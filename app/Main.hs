import GUI.GUI

main :: IO ()
main = startGUI

{-
Tarefas ---------------------------------------------------------------------
*Quando editar o nome de uma aresta ou nodo no grafo de tipos, fazer update em todos os grafos que usam o objeto
*permitir que o usuário escolha como será vista a regra (formato L->R, ou L<-K->R)
*permitir a reordenação das regras por drag n' drop
*modificar sistema undo/redo para funcionar com todo o projeto ao invés de grafos individuais
*Refatorar funções do módulo UpdateInspector

Ideias -------------------------------------------------------------------------
*criação de nodos através de clique duplo em um espaço em branco
*criação de arestas dando clique duplo em um nodo e arrastando mouse para outro nodo
*Modificar interface para que o painel do inspector não seja mostrado a toda hora.
  *Mostra-lo ao dar 2 cliques em um elemento.
  *mostrar um campo de digitação em cima do elemento quando o usuario apertar F2 para renomeá-lo
*mostrar menu de contexto ao clicar em um elemento com o botão direito

Progresso -------------------------------------------------------------------
*Permitir que arestas no typeGraph tenham o mesmo nome, desde que tenham origem e destino diferentes

Feito ------------------------------------------------------------------------
*Inferência de tipos de arestas
  *Criação de arestas com tipos automáticos (dando preferência pelo tipo selecionado na comboBox, se for válido)
  *Quando o tipo de um nodo das extremidades de uma aresta for modificado
  *Modificar lista de elementos mostrados na comboBox de tipos de arestas de acordo com a aresta selecionada
-}
