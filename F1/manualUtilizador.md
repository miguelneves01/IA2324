# Manual de Utilizador - 1ª Fase do Projeto

![Logótipo do Politécnico de Setúbal](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c8/Log%C3%B3tipo_do_Politecnico_de_Setubal.png/1600px-Log%C3%B3tipo_do_Politecnico_de_Setubal.png)

## Unidade Curricular de Inteligência Artificial
Docentes:
* Prof. Joaquim Filipe
* Eng. Filipe Mariano

<br><br>

# Jogo do Cavalo

Projeto realizado por Grupo 51:
* Miguel Neves - 201900377
* Guilherme Ravasco - 201900646

<br><br><br><br><br><br><br><br>

# Índice
1. Introdução
2. Instalação
3. Configuração
4. Interface
5. Output

<br><br><br><br>

# 1. Introdução
Este documento é o **Manual de Utilizador** do projeto **Jogo do Cavalo**.

Este projeto foi proposto com o intuito de utilizar os conhecimentos aprendidos na Unidade Curricular de Inteligência Artificial, este documento descreve todos os passos necessários para que o utilizador consiga realizar a instalação e tirar o melhor proveito da aplicação do **Jogo do Cavalo** da forma mais eficiente possível.


# 2. Instalação
Para executar a aplicação do **Jogo do Cavalo** é necessário instalar o IDE **Lisp Works**, este IDE utiliza a linguagem **Commomn Lisp**, desta forma é necessário compilar o ficheiro abaixo e executar a função **(init)**, que vai dar início à aplicação, aparecendo o menu inicial.

O ficheiro necessário para compilar é o, **projeto.lisp**, neste ficheiro é onde se situa toda a interação com o utilizador e que chama as funções de outros ficheiros com os algoritmos.


# 3. Configuração
Não é necessário configurar um Path para dar load do projeto, basta apenas ter a pasta do projeto no seu computador e abrir o IDE e carregar o ficheiro explicado no ponto anterior. Este processo de compilar o ficheiro **projeto.lisp** assegura que o projeto está devidamente configurado e pronto a ser utilizado em cada máquina.


# 4. Interface
Com a instalação e configuração já devidamente realizadas é possível correr a aplicação, sem problemas.

## Menu Principal
O menu principal exibe as opções dos algoritmos disponíveis para resolver o **Jogo do Cavalo**.

```
Escolha o algoritmo
    1. DFS
    2. BFS
    3. A*
```

## Resolver os problemas
Consoante a escolha do algoritmo irá aparecer que tabuleiro é que deseja resolver, os tabuleiros vão de A a F. O utilizador deve escrever a letra do tabuleiro que prefere resolver e de seguida clicar na tecla "Enter".

Irá aparecer os tabuleiros todos disponíveis, e o utilizador consegue ver como são os tabuleiros.


## Escolher um problema
Ao escolher uma tabuleiro para resolver, irá ser pedido o objetivo de pontos do tabuleiro. O utilizador deve digitar o número do objetivo e clicar na tecla "Enter" para prosseguir.
 
Após estes passos o jogo irá ser resolvido pelo algoritmo escolhido e com o tabuleiro escolhido.

O algoritmo **A*** tem ainda a opção de escolher se o utilizador deseja resolver os tabuleiros pela Heurística Base ou pela Heurística de Maior Valor (Heurística Criada).

# Output
O Output é o resultado do jogo. Os resultados são exibidos no **Listener**. Um exemplo de output é o seguinte:

```
Timing the evaluation of (ALGORITMO SORTING (CAR INICIAIS) (CDR INICIAIS) OBJETIVO)

User time    =        0.031
System time  =        0.000
Elapsed time =        0.008
Allocation   = 1131272 bytes
0 Page faults
Calls to %EVAL    20817
GC time      =        0.000
Profundidade: 3
Nos Gerados: 5
Nos Expandidos: 4
Fator de Ramificacao Medio: 1.2
Penetrancia: 0.6
Pontuacao: 72

Solucao:

(OPERADOR-INICIAL (1 0) OPERADOR-2 OPERADOR-2)
(2 NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL 3 NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL T NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
```