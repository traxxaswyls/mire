:- use_module(library(socket)).

:- dynamic exit/1.
:- dynamic hello/1.

e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
hello -->[what],[is],[your],[name],['?'].

parse(Tokens) :- phrase(parse_exits(Exits), Tokens, Rest), retractall(exit(_)), assert(exit(Exits)).

parse(_).

parse_hello(Stream):-
  read_line_to_codes(Stream, Codes),
  filter_codes(Codes, Filtered),
  atom_codes(Atom, Filtered),
  tokenize_atom(Atom, Tokens),
  write(Tokens),
  phrase(hello,Tokens,_),assert(hello('Bot')),

  format(atom(Command), 'Bot~n',[]),
  write(Stream,Command),
  write(Command),
  flush_output(Stream).

/* Convert to lower case if necessary,
skips some characters,
works with non latin characters in SWI Prolog. */
filter_codes([], []).
filter_codes([H|T1], T2) :-
  char_code(C, H),
  member(C, ['(', ')', ':']),
  filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :- 
  code_type(F, to_lower(H)),
  filter_codes(T1, T2).


process(Stream) :-
  format(atom(Command), 'say i am a bot HA-HA~n', []),
  write('Command: '),write(Command),write('\n'),
  write(Stream, Command),
  flush_output(Stream).

process(_).

loop(Stream) :-
  flush(),
  sleep(1),
  process(Stream),
  loop(Stream).
 
 main :-
   setup_call_cleanup(
     tcp_connect(localhost:3333, Stream, []),
     (parse_hello(Stream),
     loop(Stream)),
     close(Stream)).

