import Editor.GraphEditor

main :: IO ()
main = startGUI

{-
Tarefas ---------------------------------------------------------------------
*Adicionar diferentes tipos de grafos, mudando o inspector (painel a direita) quando o usuario clicar nele
  *Host  - grafo em que elementos são associados a um tipo
  *Rule  - grafo em que elementos indicam transformações sobre grafos Host


Progresso -------------------------------------------------------------------
*Adicionar diferentes tipos de grafos, mudando o inspector (painel a direita) quando o usuario clicar nele
  *Host  - grafo em que elementos são associados a um tipo


Feito -----------------------------------------------------------------------
*Melhorar menu de Propriedades
 *3 aparencias diferentes para nodos, edges e nodos+edges
*Corrigir Zoom para ajustar o Pan quando ele for modificado
*Copy/Paste/Cut
*Corrigir arestas não sendo coladas com Cut/Paste
*Corrigir movimento das arestas quando mover um nodo
*corrigir bug no copiar/colar que ocorre quando a seleção é movida antes de copiar
*Novo Arquivo
*Separar a estrutura do grafo das estruturas gráficas
*Estilos diferentes para as arestas
*Criar uma janela de mensagens de erros para substituir prints
*Mudar para que quando o usuario clique em um nodo, ele não invalide toda a seleção se o nodo for parte da seleção
*Fazer com que duplo-clique em um nodo ou aresta ou pressionando F2 com nodos/arestas selecionados, o dialogo nome seja focado
*Mudar estrutura do grafo para estrutura usada no verigraph
*Editar multiplos grafos no mesmo projeto
  *Criar uma arvore de grafos
  *Consertar Undo/Redo
*Espaçar edges quando houver mais de uma aresta entre dois nodos e ela estiver centralizada
*Removida a opção "Insert Emoji" do menu da treeView, porque a ativação estava fazendo o programa encerrar.
*Arrumado bug que fazia o programa encerrar ao salvar com algum grafo que não o primeiro selecionado.
*Indicar em qual grafo está a mudança do projeto
*Mudar a linguagem dos comentários para inglês
*Perguntar se o usuario quer salvar o grafo no caso de ativar a ação 'new'
*Mudar a linguagem da interface toda para inglês
*Mudar o modelo da treeview para uma treeStore
*Modificar a aplicação para aceitar ramificações da treestore
  *Durante a edição
  *Salvar/Carregar
  *Adicionar diferentes tipos de grafos, mudando o inspector (painel a direita) quando o usuario clicar nele
    *Typed - grafo com mais liberdade de opções, define os tipos dos elementos
-}
