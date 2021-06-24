%! Programa de la máquina de Turing
%inst(q0, 1, q0, 0, der).
%inst(q0, v, qf, 1, detener).

inst(q0,0,q0,0,der).
inst(q0,1,q1,1,der).
inst(q1,1,q1,1,der).
inst(q1,0,q2,0,izq).
inst(q2,1,qf,0,detener).

% Simulador
main :-
	getLines(L),
	turing(L,C),
	writeResult(C),
	turing(busy_beaver_config, busy_beaver, C, TapeOut),
	write(C),
	write('\nUTM: '),
	write(TapeOut).


turing(CintaActual, CintaFinal) :-
    ejecute(q0, [], CintaActual, Ls, Rs),
    reverse(Ls, Ls1),
    append(Ls1, Rs, CintaFinal).

ejecute(qf, Ls, Rs, Ls, Rs) :- !.
ejecute(Q0, Ls0, Rs0, Ls, Rs) :-
    simbolo(Rs0, SimboloLeido, RsRest),
    once(inst(Q0, SimboloLeido, Q1, NuevoSimb, Action)),
    accion(Action, Ls0, [NuevoSimb|RsRest], Ls1, Rs1),
    ejecute(Q1, Ls1, Rs1, Ls, Rs).
% ------------------------------------------------------------------------
simbolo([], v, []).
simbolo([Simb|Rs], Simb, Rs).

accion(izq, Ls0, Rs0, Ls, Rs) :- izq(Ls0, Rs0, Ls, Rs).
accion(detener, Ls, Rs, Ls, Rs).
accion(der, Ls0, [Sym|Rs], [Sym|Ls0], Rs).

izq([], Rs0, [], [v|Rs0]).
izq([L|Ls], Rs, Ls, [L|Rs]).
% ------------------------------------------------------------------------

action(left,  {Lin, Rin},  {Lout, Rout}, B) :- left(Lin, Rin, Lout, Rout, B).
action(stay,  Tape,        Tape,         _).
action(right, {Lin, Rin},  {Lout, Rout}, B) :- right(Lin, Rin, Lout, Rout, B).

left([],     Rs, [], [B|Rs], B).
left([L|Ls], Rs, Ls, [L|Rs], _).

right(L, [],     [B|L], [], B).
right(L, [S|Rs], [S|L], Rs, _).
% ------------------------------------------------------------------------


% https://stackoverflow.com/questions/26826470/reading-lines-into-lists-with-prolog/26826824

getLines(L):-
  setup_call_cleanup(
    open('cintaInicial.txt', read, In),
    readData(In, L),
    close(In)
  ).

readData(In, L):-
  read_term(In, H, []),
  (   L=H
  ).

% https://stackoverflow.com/questions/4736384/writing-in-file-swi-prolog-windows

writeResult(C):-
   open('cintaFinal.txt',write,OS),
   (

   write(OS,C),nl(OS),
   false
   ;
   close(OS)
   ).

turing(Config, Rules, TapeIn, TapeOut) :-
    call(Config, IS, _, _, _, _),
    perform(Config, Rules, IS, {[], TapeIn}, {Ls, Rs}),
    reverse(Ls, Ls1),
    append(Ls1, Rs, TapeOut).

perform(Config, Rules, State, TapeIn, TapeOut) :-
    call(Config, _, FS, RS, B, Symbols),
    ( memberchk(State, FS) ->
        TapeOut = TapeIn
    ; memberchk(State, RS) ->
        {LeftIn, RightIn} = TapeIn,
        simbolo(RightIn, Symbol, RightRem),
        memberchk(Symbol, Symbols),
        once(call(Rules, State, Symbol, NewSymbol, Action, NewState)),
        memberchk(NewSymbol, Symbols),
        action(Action, {LeftIn, [NewSymbol|RightRem]}, {LeftOut, RightOut}, B),
        perform(Config, Rules, NewState, {LeftOut, RightOut}, TapeOut) ).


busy_beaver_config(IS, FS, RS, B, S) :-
    IS = 'A',               % initial state
    FS = ['HALT'],          % halting states
    RS = [IS, 'B', 'C'],    % running states
    B  = 0,                 % blank symbol
    S  = [B, 1].            % valid symbols
busy_beaver('A', 0, 1, right, 'B').
busy_beaver('A', 1, 1, left,  'C').
busy_beaver('B', 0, 1, left,  'A').
busy_beaver('B', 1, 1, right, 'B').
busy_beaver('C', 0, 1, left,  'B').
busy_beaver('C', 1, 1, stay,  'HALT').

%turing(busy_beaver_config, busy_beaver, [], TapeOut).
