:- use_module(library(socket)).


:- dynamic exit/1.
:- dynamic hello/1.
:- dynamic weapon/1.
:- dynamic armor/1.
:- dynamic players/1.

e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].

w(blade) --> [blade].
w(staff) --> [staff].
w(sword) --> [sword].

a(magic) --> [magic].
a(divine) --> [divine].
a(heavy) --> [heavy].

hello -->[what],[is],[your],[name],['?'].
enter -->['>'].
player --> ['name'].
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits),['.'].

%ïàðñèì íà íàëè÷èå âûõîäîâ
parse(Tokens) :-
  append(Tokens,['.'],Tokens_2),
  phrase(parse_exits(Exits), Tokens_2, Rest),
  retractall(exit(_)),
  assert(exit(Exits)).
parse(_).

weapons([Weapon]) --> w(Weapon).
weapons([Weapon|Weapons]) --> w(Weapon), weapons(Weapons).

parse_weapons(Weapons) --> [weapons], weapons(Weapons),['.'].
%ïàðñèì íà ïðåäìåò îðóæèÿ
parse_2(Tokens) :-
  append(Tokens,['.'],Tokens_2),
  phrase(parse_weapons(Weapons), Tokens_2, Rest),
  retractall(weapon(_)),
  assert(weapon(Weapons)).

parse_2(_).


armors([Armor]) --> a(Armor).
armors([Armor|Armors]) --> a(Armor), armors(Armors).

parse_armors(Armors) --> [armors], armors(Armors),['.'].
%ïàðñèì íà ïðåäìåò áðîíè
parse_3(Tokens) :-
  append(Tokens,['.'],Tokens_2),
  phrase(parse_armors(Armors), Tokens_2, Rest),
  retractall(armor(_)),
  assert(armor(Armors)).

parse_3(_).

% Convert to lower case if necessary,
%skips some characters,
%works with non latin characters in SWI Prolog. */
filter_codes([], []).
filter_codes([H|T1], T2) :-
  char_code(C, H),
  member(C, ['(', ')', ':', ',']),
  filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
  code_type(F, to_lower(H)),
  filter_codes(T1, T2).


%ïåðåõîä â äðóãóþ êîìíàòó
process(Stream) :-
  %exit([_,Direction]),
  exit([Direction|_]),
  exit(Directions), write('All directions: '), write(Directions),
  write('\n'),
  write('Current direction: '),write(Direction),write('\n'),%%%
  format(atom(Command), 'move ~w~n', [Direction]),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream),
  retractall(exit(_)),
   read_loop(Stream).
process(_).

%áåðåì îðóæèå
process_2(Stream) :-
  %exit([_,Direction]),
  weapon([Kind|_]),
  weapon(Kinds), write('All kinds of weapon: '), write(Kinds),
  write('\n'),
  write('Current kind: '),write(Kind),write('\n'),%%%
  format(atom(Command), 'grab ~w~n', [Kind]),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream),
  retractall(weapon(_)),
   read_loop(Stream).
process_2(_).

%áåðåì áðîíþ
process_3(Stream) :-
  %exit([_,Direction]),
  armor([Kind|_]),
  armor(Kinds), write('All kinds of armor: '), write(Kinds),
  write('\n'),
  write('Current kind: '),write(Kind),write('\n'),%%%
  format(atom(Command), 'grab ~w~n', [Kind]),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream),
  retractall(armor(_)),
   read_loop(Stream).

process_3(_).

%ïðåäñòàâëÿåìñÿ
parse_hello(Stream):-
  read_line_to_codes(Stream, Codes),
  filter_codes(Codes, Filtered),
  atom_codes(Atom, Filtered),
  tokenize_atom(Atom, Tokens),
  write(Tokens),
  phrase(hello,Tokens,_),assert(hello('Bot')),

  %Èçìåíèòå 'Bot' íà ñâîå èìÿ(~n íå óáèðàéòå)

  format(atom(Command), 'Bot~n',[]),
  write(Stream,Command),
  write(Command),
  flush_output(Stream).

grab(Stream):-
  format(atom(Command), 'grab 1~n', []),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream).

% êîìàíäà players
players_comm(Stream):-
  format(atom(Command), 'players~n', []),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream).

stats(Stream):-
  format(atom(Command), 'stats~n', []),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream).

say(Stream):-
  format(atom(Command), 'say i am a bot HA-HA~n', []),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream).

hit(Stream):-
  players(All_players),write(All_players),
  players([Player_name|_]),
  format(atom(Command), 'hit ~w~n', [Player_name]),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream),
  read_loop(Stream).
hit(_).
%----
read_stream(Stream,Tokens):-
  read_line_to_codes(Stream, Codes),
  filter_codes(Codes, Filtered),
  atom_codes(Atom, Filtered),
  tokenize_atom(Atom, Tokens),
  write(Tokens),
  parse(Tokens),
  parse_2(Tokens),
  parse_3(Tokens),
  phrase_players(Tokens),
  nl,
  flush().

first_element([First|_],[First]).
phrase_players(Tokens):-
  first_element(Tokens,First),
  phrase(player, First, _),
  last(Tokens,Last_el),
  retractall(players(_)),
  assert(players([Last_el])).
phrase_players(_).

%÷èòàåì ïîòîê ïîêà íå íàñòóïèò ìîìåíò äåéñòâîâàòü (>)
read_loop(Stream):-
  read_stream(Stream,Tokens),
  phrase(enter, Tokens, _),!;
  read_loop(Stream).
%---
% ìåíÿéòå ïîðÿäîê êîìàíä çäåñü
%
% âûçûâàéòå players_comm ïåðåä hit
%   èíà÷å ñïèñîê èãðîêîâ áóäåò ïóñòîé èëè óñòàðåâøèé
%   â players_comm ïàðñåð èãðîêîâ, êîòîðûå íàõîäÿòñÿ â êîìíàòå
%
% ïîñëå ïîñëåäíåé êîìàíäû â äàííîì áëîêå
%   íå ñòàâüòå read_loop(Stream)
%
% êîìàíäû process,process_2,process_3, hit ñîäåðæàò read_loop âíóòðè =>
%   èõ íåëüçÿ ñòàâèòü ïîñëåäíèìè
%
% êîãäà ïðèõîäèò íåçàïëàíèðîâàííîå ñîîáùåíèå(áîòà óäàðèëè èëè êòî-òî
% ñêàçàë ÷òî-òî), òî âûâîä ïîòîêà â ïðîëîãå ñáèâàåòñÿ -  íà ðàáîòó íå
% âëèÿåò
command_in_room(Stream):-
  sleep(3),
  say(Stream),
  read_loop(Stream),

  %áåðåì îðóæèå
  sleep(3),
  process_2(Stream),
  %áåðåì áðîíþ
  sleep(3),
  process_3(Stream),

  sleep(3),
  players_comm(Stream),
  read_loop(Stream),

  sleep(3),
  hit(Stream),

  sleep(3),
  grab(Stream),
  read_loop(Stream),
  %ïåðåõîä â äðóãóþ êîìíàòó
  sleep(3),
  process(Stream),

  sleep(3),
  stats(Stream).

loop(Stream) :-
  read_loop(Stream),
  command_in_room(Stream),
  loop(Stream).

main :-
   setup_call_cleanup(
     tcp_connect(localhost:3333, Stream, []),
     (parse_hello(Stream),
     loop(Stream)),
     close(Stream)).