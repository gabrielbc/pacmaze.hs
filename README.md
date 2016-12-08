# Pacmaze.hs

Implementação do Pacmaze com três algoritmos de busca diferentes, em Haskell. No Pacmaze o objetivo é levar o pac até a última pastilha.

## Modelagem

A entrada é um mundo bidimensional, sendo os caracteres # - P as paredes, caminho e a posição do Pac respectivamente. O mundo representa o estado corrente, e as ações do Pac são: acima, abaixo, esquerda e direita. Após uma ação, o mundo alterado é o novo estado.

## Algoritmos

Os algoritmos para escolha são: Busca em Largura, Busca em Profundidade e A* com Distância de Manhattan, utilizando uma estrutura de Heap para a implementação da fronteira de estados a serem expandidos.

## Utilização

As instruções seguintes se referem a utilização do programa.

### Pré-Requisito

Instalação do compilador [GHC](https://www.haskell.org/ghc/download_ghc_7_6_1)

### Execução

```
runghc pacmaze.hs {algoritmo} {arquivo} {x} {y}  
```

Os parâmetros para execução são os seguintes:
* algoritmo: 'astar', 'bfs', ou 'dfs'
* arquivo: nome do arquivo, ex: pacmaze-01-tiny.txt
* x e y: posição inicial do Pac.

Ex:

```
runghc pacmaze.hs astar pac-01-tiny.txt 5 1  
```
