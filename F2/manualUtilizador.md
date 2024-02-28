# Manual de Utilizador - 2ª Fase do Projeto

![Logótipo do Politécnico de Setúbal](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c8/Log%C3%B3tipo_do_Politecnico_de_Setubal.png/1600px-Log%C3%B3tipo_do_Politecnico_de_Setubal.png)

## Unidade Curricular de Inteligência Artificial
Docentes:
* Prof. Joaquim Filipe
* Eng. Filipe Mariano

# Jogo do Cavalo

## Manual de Utilizador
Projeto realizado por Grupo 51:
* Miguel Neves - 201900377
* Guilherme Ravasco - 201900646

<br><br><br><br><br><br>

# Indice 

1. Introdução
2. Instalação
3. Configuração
4. Interface
5. Output

<br><br><br><br><br><br>

# 1. Introdução
Este documento é o **Manual de Utilizador** do projeto **Jogo do Cavalo**.

Este projeto foi proposto com o intuito de utilizar os conhecimentos aprendidos na Unidade Curricular de Inteligência Artificial, este documento descreve todos os passos necessários para que o utilizador consiga realizar a instalação e tirar o melhor proveito da aplicação do **Jogo do Cavalo** da forma mais eficiente possível.

<br><br><br><br><br><br>

# 2. Instalação
Para executar a aplicação do **Jogo do Cavalo** é necessário instalar o IDE **Lisp Works**, este IDE utiliza a linguagem **Commomn Lisp**, desta forma é necessário compilar o ficheiro abaixo e executar uma das seguintes funções **(init)**, **(init tabuleiro)** e **(init tabuleiro tempo)**,o parâmemtro **tempo** é o máximo tempo de resposta que quer dar ao AI, que vai dar início à aplicação, aparecendo o menu inicial.

Também é possível executar apenas a melhor jogada com o algoritmo **ALFABETA** execuntando **(jogar tabuleiro tempo &optional player)**. Tendo em conta que o tabuleiro tem que ser um tabuleiro em que ambos os jogadores já realizaram a sua primeiro jogada e o player caso nao seja especificado assume-se como **-1**.

O ficheiro necessário para compilar é o, **interact.lisp**, neste ficheiro é onde se situa toda a interação com o utilizador e que chama as funções de outros ficheiros com os algoritmos.

# 3. Configuração
Não é necessário configurar um Path para dar load do projeto, basta apenas ter a pasta do projeto no seu computador e abrir o IDE e carregar o ficheiro explicado no ponto anterior. Este processo de compilar o ficheiro **interact.lisp** assegura que o projeto está devidamente configurado e pronto a ser utilizado em cada máquina.

# 4. Inteface
Não é necessário configurar um Path para dar load do projeto, basta apenas ter a pasta do projeto no seu computador e abrir o IDE e carregar o ficheiro explicado no ponto anterior. Este processo de compilar o ficheiro **projeto.lisp** assegura que o projeto está devidamente configurado e pronto a ser utilizado em cada máquina.