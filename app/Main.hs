import Editor.GraphEditor

main :: IO ()
main = startGUI

{-
Tarefas ---------------------------------------------------------------------
*Adicionar diferentes tipos de grafos, mudando o inspector (painel a direita) quando o usuario clicar nele
  *Rule  - grafo em que elementos indicam transformações sobre grafos Host
* Informar em tempo real quando erros nos host e rulesGraph se o typeGraph for modificado


Progresso -------------------------------------------------------------------
*Adicionar coluna na treeview para atributo de ativação de ruleGraphs





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
  *Host - grafos em que elementos são associados a um tipo e herdam a informação visual desses elementos
   - fazer inspector mudar para hostInspector
   - carregar comboboxes com informações de tipo
   - fazer elementos do diagrama serem associados aos tipos
      - fazer com que a informação de um elemento (nodeInfo, edgeInfo) seja uma string no formato " label { tipo }"
   - mudar a aparência dos elementos do diagrama conforme seus tipos
   - fazer atualização do hostInspector com a função updateInspector
   - indicar erro caso o tipo do elemento não esteja especificado no typeGraph
*Verificar se um elemento do hostGraph foi mapeado corretamente para TypedGraph
*Adicionar coluna na treeView para indicar mudança
*Atualizar informações do typegraph ativado automaticamente durante a edição
*Arrumar função para indicar mudanças - ela está indicando mudanças em nodos do tipo Topic
*Indicar conflitos no typeGraph
-}
