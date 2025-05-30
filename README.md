# Gerar um código que implemente ILP em Prolog usando chatBots com LLM

**Equipe:**
* Alberth Viana de Lima
* Ana Júlia Pereira Corrêa
* Guilherme Sahdo Maciel

##

Faça um código em Prolog que seja próximo ao do Mini-Hyper do Bratko.

1. Teste primeiro top-down
2. Dê exemplos curtos de BK com E+ e E- e exemplo de regras induzidas
3. Sugira uma represenrtacao de regra, como a de lista [A, [B1,...,Bk]] para representar A <- B1 e ... Bk

##

Com base nessas instruções foram geradas duas versões de código implementado um código básico com ILP. Para compilar `versao1.pl` basta no SWISH utilizar o seguinte comando:

```prolog
run.
```

A saída será:

```prolog
Regras induzidas:
avo(joao,ana) <- [pai(joao,maria),pai(maria,ana)]
avo(joao,pedro) <- [pai(joao,carlos),pai(carlos,pedro)]
true
```

Ainda na `versao1.pl` é possível usar para indução de apenas uma regra.:

```prolog
induce_one_rule(R).
```
 
A saída será: 

```prolog
induce_one_rule(R).
R = [avo(joao,ana), [pai(joao,maria), pai(maria,ana)]]
R = [avo(joao,pedro), [pai(joao,carlos), pai(carlos,pedro)]]
```

A `versao2.pl` é uma versão adpatada da `versao1.pl`, deixando-a mais próxima do código do Mini-Hyper do Brakto. Para compilá-la basta usar a linha de comando:

```prolog
run
```

A saída será:

```prolog
Regras induzidas:
avo(_3716,_3718) <- [pai(_3716,_3736),pai(_3736,_3718)]
true
```
