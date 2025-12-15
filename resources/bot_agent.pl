:- module(bot_agent, [
    reset_world/0,
    sync_state/9,
    choose_action/1,
    parse_action/2,
    action_to_string/2,
    apply_action/1
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(aggregate)).

:- dynamic current_room/1.
:- dynamic exit/3.              
:- dynamic item_in_room/2.      
:- dynamic mob_in_room/5.       
:- dynamic inventory_item/1.    
:- dynamic stats/4.             


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
    forall(member(mob(Id, MobHp, MobMax, MobDamage), Mobs), assertz(mob_in_room(Room, Id, MobHp, MobMax, MobDamage))),
    forall(member(Inv, Inventory), assertz(inventory_item(Inv))),
    assertz(stats(HP, MaxHP, Damage, Resist)).

assert_exit(Room, Dir-Dest) :- assertz(exit(Room, Dir, Dest)).


potion('hp-small').
potion('hp-medium').
potion('resist').

healing_order(['hp-medium', 'hp-small']).


choose_action(use(Heal)) :-
    stats(HP, MaxHP, _, _),
    mob_count(MobCount),
    heal_needed(HP, MaxHP, MobCount),
    best_heal_potion(Heal),
    inventory_item(Heal),
    !.
choose_action(use('resist')) :-
    mob_count(MCount),
    MCount >= 2,
    stats(_, _, _, Resist),
    Resist < 15,
    inventory_item('resist'),
    !.
choose_action(move(Dir)) :-
    mob_count(MCount),
    MCount >= 2,
    stats(_, _, _, Resist),
    Resist < 15,
    \+ inventory_item('resist'),
    current_room(Room),
    exit(Room, Dir, _),
    !.
choose_action(move(Dir)) :-
    stats(HP, MaxHP, Damage, Resist),
    high_risk(HP, MaxHP, Damage, Resist),
    current_room(Room),
    exit(Room, Dir, _),
    !.
choose_action(attack(Mob)) :-
    stats(_, _, Damage, _),
    killable_mob(Damage, Mob, _, _),
    !.
choose_action(attack(Mob)) :-
    mob_count(1),
    stats(HP, MaxHP, _, _),
    Percent is (HP * 100) // MaxHP,
    Percent > 40,
    weakest_mob(Mob, _, _),
    !.
choose_action(grab(Item)) :-
    current_room(Room),
    \+ mob_in_room(Room, _, _, _, _),
    item_in_room(Room, Item),
    !.
choose_action(move(Dir)) :-
    stats(HP, MaxHP, _, _),
    Percent is (HP * 100) // MaxHP,
    Percent =< 20,
    mob_count(MCount),
    MCount > 0,
    current_room(Room),
    exit(Room, Dir, _),
    !.
choose_action(move(Dir)) :-
    current_room(Room),
    exit(Room, Dir, _),
    !.
choose_action(look).

heal_needed(HP, MaxHP, MobCount) :-
    Percent is (HP * 100) // MaxHP,
    (Percent =< 50 ; (MobCount >= 2, Percent =< 65)).

high_risk(HP, MaxHP, Damage, Resist) :-
    total_incoming(Inc),
    Eff is max(1, (Inc * (100 - Resist)) // 100),
    (Eff >= HP ;
     (highest_mob_hp(HighHp), Damage * 3 < HighHp, (HP * 100) // MaxHP =< 70)).

best_heal_potion(P) :-
    healing_order(Order),
    member(P, Order),
    potion(P).

mob_count(Count) :-
    current_room(Room),
    aggregate_all(count, mob_in_room(Room, _, _, _, _), Count).

total_incoming(Sum) :-
    current_room(Room),
    aggregate_all(sum(Dmg), mob_in_room(Room, _, _, _, Dmg), Sum).

weakest_mob(Id, Hp, MaxHp) :-
    current_room(Room),
    findall([Hp, Id, MaxHp], mob_in_room(Room, Id, Hp, MaxHp, _), Mobs),
    Mobs \= [],
    sort(Mobs, [[Hp, Id, MaxHp] | _]).

killable_mob(Damage, Id, Hp, MaxHp) :-
    weakest_mob(Id, Hp, MaxHp),
    Hp =< Damage.

highest_mob_hp(Hp) :-
    current_room(Room),
    findall(H, mob_in_room(Room, _, H, _, _), Hps),
    Hps \= [],
    max_list(Hps, Hp).


apply_action(use(Item)) :-
    retract(inventory_item(Item)),
    writeln(action_used(Item)),
    !.
apply_action(attack(Mob)) :-
    current_room(Room),
    retract(mob_in_room(Room, Mob, _, _, _)),
    !.
apply_action(grab(Item)) :-
    current_room(Room),
    retract(item_in_room(Room, Item)),
    assertz(inventory_item(Item)),
    !.
apply_action(move(Dir)) :-
    current_room(Room),
    exit(Room, Dir, Dest),
    retractall(current_room(_)),
    assertz(current_room(Dest)),
    !.
apply_action(_).


parse_action(String, Action) :-
    string_lower(String, Lower),
    split_string(Lower, " ", "\s\t\r\n", Tokens),
    phrase(action(Action), Tokens).

action(look)            --> ["look"].
action(stats)           --> ["stats"].
action(inventory)       --> ["inventory"].
action(move(Dir))       --> ["move", DirS], {atom_string(Dir, DirS)}.
action(attack(Mob))     --> ["attack", MobS], {atom_string(Mob, MobS)}.
action(grab(Item))      --> ["grab", ItemS], {atom_string(Item, ItemS)}.
action(use(Item))       --> ["use", ItemS], {atom_string(Item, ItemS)}.


action_to_string(look, "look").
action_to_string(stats, "stats").
action_to_string(inventory, "inventory").
action_to_string(move(Dir), S) :-
    format(string(S), "move ~w", [Dir]).
action_to_string(attack(Mob), S) :-
    format(string(S), "attack ~w", [Mob]).
action_to_string(grab(Item), S) :-
    format(string(S), "grab ~w", [Item]).
action_to_string(use(Item), S) :-
    format(string(S), "use ~w", [Item]).