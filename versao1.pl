% --- Conhecimento de fundo (Background Knowledge) ---

fato(pai(joao, maria)).
fato(pai(maria, ana)).
fato(pai(joao, carlos)).
fato(pai(carlos, pedro)).
fato(mae(ana, julia)).

% --- Exemplos positivos (E+) ---
positivo(avo(joao, ana)).
positivo(avo(joao, pedro)).

% --- Exemplos negativos (E-) ---
negativo(avo(maria, julia)).
negativo(avo(carlos, joao)).

% --- Representação das regras ---
% Uma regra é representada como: [Cabeça, [Corpo1, Corpo2, ...]]

% Executa o sistema
run :-
    induce_all(Regras),
    writeln('Regras induzidas:'),
    print_rules(Regras).

% Indução de regras
induce_all(Regras) :-
    setof([HeadCopy, BodyCopy],
        induce_one_rule([HeadCopy, BodyCopy]),
    Regras), !.
induce_all([]).

% Tenta induzir uma regra que cubra todos os positivos e nenhum negativo
induce_one_rule([HeadCopy, BodyCopy]) :-
    % estrutura genérica com variáveis
    Head = avo(X, Y),
    generate_body(X, Y, Body),
    prove_all(Body),                     % existe alguma prova?
    not((negativo(avo(A,B)),
         X = A, Y = B,
         prove_all(Body)                 % se cobre negativo, rejeita
        )),
    HeadCopy = Head,
    BodyCopy = Body.

% Gera um corpo de regra candidato (duas etapas pai/mae)
generate_body(X, Y, [L1, L2]) :-
    member(P1, [pai, mae]),
    member(P2, [pai, mae]),
    L1 =.. [P1, X, Z],
    L2 =.. [P2, Z, Y].

% Prova um corpo de regra com encadeamento
prove_all([]).
prove_all([G|Gs]) :-
    fato(F),
    copy_term(F, FC),
    FC = G,
    prove_all(Gs).

% Impressão de regras
print_rules([]).
print_rules([[Head, Body]|Rest]) :-
    format('~w <- ~w~n', [Head, Body]),
    print_rules(Rest).