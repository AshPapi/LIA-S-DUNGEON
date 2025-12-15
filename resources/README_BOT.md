# Prolog-бот для Mire Dungeon

## 1. Модуль и экспорт

```prolog
:- module(bot_agent, [
    reset_world/0,
    sync_state/9,
    choose_action/1,
    parse_action/2,
    action_to_string/2,
    apply_action/1
]).
```

Объявляется модуль `bot_agent` с экспортируемыми предикатами — это API для вызова из Clojure.

---

## 2. Хранение фактов (динамическая база знаний)

```prolog
:- dynamic current_room/1.      % Текущая комната игрока
:- dynamic exit/3.              % exit(Комната, Направление, Назначение)
:- dynamic item_in_room/2.      % item_in_room(Комната, Предмет)
:- dynamic mob_in_room/5.       % mob_in_room(Комната, Id, HP, MaxHP, Damage)
:- dynamic inventory_item/1.    % inventory_item(Предмет)
:- dynamic stats/4.             % stats(HP, MaxHP, Damage, Resist)
```

### Как это работает:

| Директива | Что хранит | Пример факта |
|-----------|------------|--------------|
| `current_room/1` | Где сейчас бот | `current_room(hallway)` |
| `exit/3` | Выходы из комнаты | `exit(hallway, north, promenade)` |
| `item_in_room/2` | Предметы в комнате | `item_in_room(hallway, 'hp-small')` |
| `mob_in_room/5` | Мобы в комнате | `mob_in_room(hallway, goblin, 30, 50, 10)` |
| `inventory_item/1` | Инвентарь бота | `inventory_item('hp-medium')` |
| `stats/4` | Характеристики бота | `stats(80, 100, 25, 10)` |

---

## 3. Синхронизация состояния

```prolog
reset_world :-
    retractall(current_room(_)),
    retractall(exit(_, _, _)),
    retractall(item_in_room(_, _)),
    retractall(mob_in_room(_, _, _, _, _)),
    retractall(inventory_item(_)),
    retractall(stats(_, _, _, _)).

sync_state(Room, Exits, Items, Mobs, Inventory, HP, MaxHP, Damage, Resist) :-
    reset_world,
    assertz(current_room(Room)),
    forall(member(DestDir, Exits), assert_exit(Room, DestDir)),
    forall(member(Item, Items), assertz(item_in_room(Room, Item))),
    forall(member(mob(Id, MobHp, MobMax, MobDamage), Mobs), 
           assertz(mob_in_room(Room, Id, MobHp, MobMax, MobDamage))),
    forall(member(Inv, Inventory), assertz(inventory_item(Inv))),
    assertz(stats(HP, MaxHP, Damage, Resist)).

assert_exit(Room, Dir-Dest) :- assertz(exit(Room, Dir, Dest)).
```

**Поток данных:**
```
Clojure (игровое состояние) 
    → sync_state/9 
    → reset_world (очистка) 
    → assertz (заполнение фактами)
```

---

## 4. Логика принятия решений (choose_action)

Правила проверяются **сверху вниз**, первое успешное — выполняется (`!` = cut):

```prolog
% 1. Лечиться, если нужно и есть зелье
choose_action(use(Heal)) :-
    stats(HP, MaxHP, _, _),
    mob_count(MobCount),
    heal_needed(HP, MaxHP, MobCount),    % HP <= 50% или (мобов >= 2 и HP <= 65%)
    best_heal_potion(Heal),               % hp-medium > hp-small
    inventory_item(Heal),
    !.

% 2. Использовать зелье сопротивления если много мобов
choose_action(use('resist')) :-
    mob_count(MCount), MCount >= 2,
    stats(_, _, _, Resist), Resist < 15,
    inventory_item('resist'),
    !.

% 3. Убежать если много мобов и нет зелья resist
choose_action(move(Dir)) :-
    mob_count(MCount), MCount >= 2,
    stats(_, _, _, Resist), Resist < 15,
    \+ inventory_item('resist'),
    current_room(Room),
    exit(Room, Dir, _),
    !.

% 4. Убежать если высокий риск
choose_action(move(Dir)) :-
    stats(HP, MaxHP, Damage, Resist),
    high_risk(HP, MaxHP, Damage, Resist),
    current_room(Room),
    exit(Room, Dir, _),
    !.

% 5. Атаковать моба которого можно убить одним ударом
choose_action(attack(Mob)) :-
    stats(_, _, Damage, _),
    killable_mob(Damage, Mob, _, _),      % HP моба <= Damage
    !.

% 6. Атаковать если один моб и HP > 40%
choose_action(attack(Mob)) :-
    mob_count(1),
    stats(HP, MaxHP, _, _),
    Percent is (HP * 100) // MaxHP,
    Percent > 40,
    weakest_mob(Mob, _, _),
    !.

% 7. Подобрать предмет если нет мобов
choose_action(grab(Item)) :-
    current_room(Room),
    \+ mob_in_room(Room, _, _, _, _),
    item_in_room(Room, Item),
    !.

% 8. Убежать при низком HP
choose_action(move(Dir)) :-
    stats(HP, MaxHP, _, _),
    Percent is (HP * 100) // MaxHP,
    Percent =< 35,
    mob_count(MCount), MCount > 0,
    current_room(Room),
    exit(Room, Dir, _),
    !.

% 9. Просто двигаться куда-нибудь
choose_action(move(Dir)) :-
    current_room(Room),
    exit(Room, Dir, _),
    !.

% 10. Осмотреться (fallback)
choose_action(look).
```

### Приоритеты действий:
```
1. Лечение      ───┐
2. Resist зелье ───┤ Выживание
3. Побег        ───┘
4. Атака killable моба
5. Атака одного моба
6. Подбор предметов
7. Побег при низком HP
8. Исследование
9. look (ничего не делать)
```

---

## 5. Вспомогательные предикаты

```prolog
% Когда нужно лечиться
heal_needed(HP, MaxHP, MobCount) :-
    Percent is (HP * 100) // MaxHP,
    (Percent =< 50 ; (MobCount >= 2, Percent =< 65)).

% Высокий риск
high_risk(HP, MaxHP, Damage, Resist) :-
    total_incoming(Inc),                           % Суммарный урон мобов
    Eff is max(1, (Inc * (100 - Resist)) // 100),  % С учётом resist
    (Eff >= HP ;                                   % Убьют за ход
     (highest_mob_hp(HighHp), Damage * 3 < HighHp, % Или моб слишком жирный
      (HP * 100) // MaxHP =< 70)).

% Агрегатные функции
mob_count(Count) :-
    current_room(Room),
    aggregate_all(count, mob_in_room(Room, _, _, _, _), Count).

total_incoming(Sum) :-
    current_room(Room),
    aggregate_all(sum(Dmg), mob_in_room(Room, _, _, _, Dmg), Sum).

weakest_mob(Id, Hp, MaxHp) :-
    current_room(Room),
    findall([Hp, Id, MaxHp], mob_in_room(Room, Id, Hp, MaxHp, _), Mobs),
    sort(Mobs, [[Hp, Id, MaxHp] | _]).  % sort сортирует по первому элементу
```

---

## 6. Грамматика DCG для парсинга

```prolog
parse_action(String, Action) :-
    string_lower(String, Lower),                    % К нижнему регистру
    split_string(Lower, " ", "\s\t\r\n", Tokens),   % Разбить на токены
    phrase(action(Action), Tokens).                 % Применить грамматику

% DCG-правила (Definite Clause Grammar)
action(look)            --> ["look"].
action(stats)           --> ["stats"].
action(inventory)       --> ["inventory"].
action(move(Dir))       --> ["move", DirS], {atom_string(Dir, DirS)}.
action(attack(Mob))     --> ["attack", MobS], {atom_string(Mob, MobS)}.
action(grab(Item))      --> ["grab", ItemS], {atom_string(Item, ItemS)}.
action(use(Item))       --> ["use", ItemS], {atom_string(Item, ItemS)}.
```

### Как работает DCG:

```
Вход: "move north"
    ↓
string_lower → "move north"
    ↓
split_string → ["move", "north"]
    ↓
phrase(action(Action), ["move", "north"])
    ↓
Сопоставление с: action(move(Dir)) --> ["move", DirS]
    ↓
DirS = "north", atom_string(Dir, "north") → Dir = north
    ↓
Результат: Action = move(north)
```

DCG — это синтаксический сахар. Правило:
```prolog
action(move(Dir)) --> ["move", DirS], {atom_string(Dir, DirS)}.
```
Эквивалентно:
```prolog
action(move(Dir), ["move", DirS | Rest], Rest) :- atom_string(Dir, DirS).
```

---

## 7. Применение действий (локальное обновление)

```prolog
apply_action(use(Item)) :-
    retract(inventory_item(Item)), !.

apply_action(attack(Mob)) :-
    current_room(Room),
    retract(mob_in_room(Room, Mob, _, _, _)), !.

apply_action(grab(Item)) :-
    current_room(Room),
    retract(item_in_room(Room, Item)),
    assertz(inventory_item(Item)), !.

apply_action(move(Dir)) :-
    current_room(Room),
    exit(Room, Dir, Dest),
    retractall(current_room(_)),
    assertz(current_room(Dest)), !.

apply_action(_).  % Fallback — ничего не делать
```

---

## 8. Сериализация действий

```prolog
action_to_string(look, "look").
action_to_string(move(Dir), S) :- format(string(S), "move ~w", [Dir]).
action_to_string(attack(Mob), S) :- format(string(S), "attack ~w", [Mob]).
action_to_string(grab(Item), S) :- format(string(S), "grab ~w", [Item]).
action_to_string(use(Item), S) :- format(string(S), "use ~w", [Item]).
```

---

## 9. Общая архитектура

```
┌─────────────────────────────────────────────────────────────┐
│                     Clojure (bot.clj)                       │
│  1. Получить состояние игры                                 │
│  2. Вызвать sync_state/9 → передать в Prolog                │
│  3. Вызвать choose_action/1 → получить решение              │
│  4. Вызвать action_to_string/2 → строка команды             │
│  5. Выполнить команду в игре                                │
└──────────────────────────┬──────────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────────┐
│                     Prolog (bot_agent.pl)                   │
│                                                             │
│  ┌─────────────────┐    ┌─────────────────────────────┐    │
│  │ Динамические    │    │ Правила choose_action       │    │
│  │ факты (память)  │───▶│ (логика принятия решений)   │    │
│  └─────────────────┘    └─────────────────────────────┘    │
│                                                             │
│  ┌─────────────────┐    ┌─────────────────────────────┐    │
│  │ DCG-грамматика  │    │ Вспомогательные предикаты   │    │
│  │ (парсинг)       │    │ (агрегация, риск)           │    │
│  └─────────────────┘    └─────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

---

## Ключевые концепции Prolog в этом боте

| Концепция | Использование |
|-----------|---------------|
| **dynamic** | Объявление изменяемых фактов |
| **assertz** | Добавление факта в конец базы |
| **retract** | Удаление одного факта |
| **retractall** | Удаление всех подходящих фактов |
| **DCG (-->)** | Грамматика для парсинга команд |
| **cut (!)** | Отсечение альтернатив после успеха |
| **findall** | Сбор всех решений в список |
| **aggregate_all** | Агрегация (count, sum) |
| **forall** | Итерация без сбора результатов |
