-- Este módulo não é chamado "Simple" porque é simples. Chama-se "Simples" porque
-- faz coisas complicadas para um software simples.
import Distribution.Simple

-- Uma implementação simples de "main" para um script de instalação do Cabal.
-- Ele lê o arquivo de descrição do pacote usando IO e executa a ação especificada
-- na linha de comando.
main = defaultMain 
