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
	write(C).

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
 
simbolo([], v, []).
simbolo([Simb|Rs], Simb, Rs).
 
accion(izq, Ls0, Rs0, Ls, Rs) :- izq(Ls0, Rs0, Ls, Rs).
accion(detener, Ls, Rs, Ls, Rs).
accion(der, Ls0, [Sym|Rs], [Sym|Ls0], Rs).
 
izq([], Rs0, [], [v|Rs0]).
izq([L|Ls], Rs, Ls, [L|Rs]).


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
  
