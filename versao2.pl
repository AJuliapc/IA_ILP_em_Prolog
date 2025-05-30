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
    max_body_length(MaxLen),
    iter_deep(1, MaxLen).

% Busca iterativa em profundidade para induzir regras
iter_deep(Len, MaxLen) :-
    Len =< MaxLen,
    findall(Regra, induce_one_rule(Len, Regra), Regras),
    Regras \= [],
    writeln('Regras induzidas:'),
    print_rules(Regras), !.
iter_deep(Len, MaxLen) :-
    Len < MaxLen,
    NextLen is Len + 1,
    iter_deep(NextLen, MaxLen).

max_body_length(3).

% Induz uma regra com corpo de tamanho Len
induce_one_rule(Len, [Head, Body]) :-
    Head = avo(X, Y),
    generate_body(X, Y, Len, Body),
    covers_all_positives(Head, Body),
    covers_no_negatives(Head, Body).

% Gera corpo de regra com encadeamento correto e tamanho variável
generate_body(X, Y, 1, [Lit]) :-
    member(Pred, [pai, mae]),
    Lit =.. [Pred, X, Y].

generate_body(X, Y, Len, [Lit|Rest]) :-
    Len > 1,
    member(Pred, [pai, mae]),
    Len1 is Len - 1,
    generate_body(Z, Y, Len1, Rest),
    Lit =.. [Pred, X, Z].

% Verifica se a regra cobre todos os exemplos positivos
covers_all_positives(Head, Body) :-
    \+ (positivo(Ex),
        Ex =.. [Pred|Args],
        Head =.. [Pred|Args],
        \+ prove_body(Body)
       ).

% Verifica se a regra não cobre nenhum exemplo negativo
covers_no_negatives(Head, Body) :-
    \+ (negativo(Ex),
        Ex =.. [Pred|Args],
        Head =.. [Pred|Args],
        prove_body(Body)
       ).

% Prova o corpo da regra (lista de literais)
prove_body([]).
prove_body([Lit|Rest]) :-
    fato(F),
    copy_term(F, FC),
    FC = Lit,
    prove_body(Rest).

% Impressão de regras
print_rules([]).
print_rules([[Head, Body]|Rest]) :-
    format('~w <- ~w~n', [Head, Body]),
    print_rules(Rest).