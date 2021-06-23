%! Programa de la máquina de Turing
%inst(q0, 1, q0, 0, der).
%inst(q0, v, qf, 1, detener).

% b
inst(a0,b,a1,0,der).
inst(a1,b,a2,0,der).
inst(a2,b,a3,0,der).
inst(a3,b,a4,b,der).
inst(a4,b,a5,0,izq).
inst(a5,b,q5,b,izq).



% o

inst(q1,0,q5,0,stay).
inst(q1,1,q2,1,der).
inst(q1,w,qf,w,detener).

inst(q2,b,q3,1,izq).

inst(q2,w,qf,w,detener).


inst(q3,0,q4,0,izq).
inst(q3,1,q4,1,izq).
inst(q2,w,qf,w,detener).

inst(q4,b,q1,b,izq).

inst(q4,w,qf,w,detener).

%q
inst(q5,b,q7,1,izq).
inst(q5,0,q6,0,der).
inst(q5,1,q6,1,der).
inst(q5,w,qf,w,detener).

inst(q6,b,q5,b,der).

inst(q6,1,q5,1,der).
inst(q6,w,qf,w,detener).

% p
inst(q7,b,q8,b,izq).
inst(q7,0,q9,0,der).
inst(q7,1,q5,b,der).
inst(q7,w,qf,w,detener).


inst(q8,0,q7,0,izq).
inst(q8,1,q7,1,izq).
inst(q8,w,qf,w,detener).

% f
inst(q9,b,q11,0,izq).
inst(q9,0,q10,0,der).
inst(q9,1,q10,1,der).
inst(q9,w,qf,w,detener).

inst(q10,b,q9,b,der).

inst(q10,w,qf,w,detener).

inst(q11,b,q1,b,izq).

inst(q11,w,qf,w,detener).

% inst(q2,w,qf,0,detener).

% Simulador

  

main :-
	getLines(L),
	turing(L,C),
	write(C).

turing(CintaActual, CintaFinal) :-
    ejecute(a0, [], CintaActual, Ls, Rs),
    reverse(Ls, Ls1),
    append(Ls1, Rs, CintaFinal).
 
ejecute(qf, Ls, Rs, Ls, Rs) :- !.
ejecute(Q0, Ls0, Rs0, Ls, Rs) :-
    simbolo(Rs0, SimboloLeido, RsRest),
    once(inst(Q0, SimboloLeido, Q1, NuevoSimb, Action)),
    accion(Action, Ls0, [NuevoSimb|RsRest], Ls1, Rs1),
    ejecute(Q1, Ls1, Rs1, Ls, Rs).
 
simbolo([], b, []).
simbolo([Simb|Rs], Simb, Rs).
 
accion(izq, Ls0, Rs0, Ls, Rs) :- izq(Ls0, Rs0, Ls, Rs).
accion(detener, Ls, Rs, Ls, Rs).
%
accion(stay, Ls, Rs, Ls, Rs).
accion(der, Ls0, [Sym|Rs], [Sym|Ls0], Rs).
 
izq([], Rs0, [], [b|Rs0]).
izq([L|Ls], Rs, Ls, [L|Rs]).


% https://stackoverflow.com/questions/26826470/reading-lines-into-lists-with-prolog/26826824

getLines(L):-
  setup_call_cleanup(
    open('cintaSecuencia.txt', read, In),
    readData(In, L),
    close(In)
  ).

readData(In, L):-
  read_term(In, H, []),
  (   L=H
  ).
  

