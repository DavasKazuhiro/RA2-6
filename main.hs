module Main where

import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)

import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (sort, group, maximumBy, isInfixOf)
import Data.Ord  (comparing)

-- TIPOS DE DADOS

-- data cria um novo tipo de dado
-- deriving (Show, Read) - para permitir a serialização e desserialização de/para o disco.
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read)

-- para armazenar os itens.
-- type nao cria um novo tipo, so fala pro compilador que 
-- quando eu escrever Inventario ele vai interpretar como o tipo

-- Map.Map cria uma estrutura chave valor
-- string é o tipo da chave 
-- Item é o tipo do valor
type Inventario = Map.Map String Item

-- ADT (Algebraic Data Type) — um tipo enumerado com várias formas possíveis.
data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read)

-- Representa o resultado de uma operação — sucesso ou erro.
-- É um adt mas com variacao simples
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read)

-- registro para o log
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)

type ResultadoOperacao = (Inventario, LogEntry)


-- FUNCOES PURAS

addItem :: UTCTime -> Inventario -> Item -> Either String ResultadoOperacao
addItem t inventario item
    | Map.member (itemID item) inventario =
        Left "Item já existe no inventário"
    | otherwise =
        let inventarioNovo = Map.insert (itemID item) item inventario
            logEntry = LogEntry
                { timestamp = t,
                acao      = Add,
                detalhes  = "Adicionado: " ++ show item,
                status    = Sucesso
                }
        in Right (inventarioNovo, logEntry)



removeItem :: UTCTime -> Inventario -> String -> Either String ResultadoOperacao
removeItem t inventario chave
    | not (Map.member chave inventario) =
        Left "Item não encontrado para remoção"
    | otherwise =
        let inventarioNovo = Map.delete chave inventario
            logEntry = LogEntry
                { timestamp = t,
                acao      = Remove,
                detalhes  = "Item removido: " ++ chave,
                status    = Sucesso
                }
        in Right (inventarioNovo, logEntry)
     
     

updateQty :: UTCTime -> Inventario -> String -> Int -> Either String ResultadoOperacao
updateQty t inventario chave novaQtd
    | not (Map.member chave inventario) =
        Left "Item não encontrado para atualização"
        
    | novaQtd < 0 =
      Left "Estoque insuficiente (resultado negativo)"
      
      
    | otherwise =
      let Just item = Map.lookup chave inventario
          itemAtualizado = item { quantidade = novaQtd }
          inventarioNovo = Map.insert chave itemAtualizado inventario
          logEntry = LogEntry
            { timestamp = t,
            acao      = Update,
            detalhes  = "Quantidade atualizada para " ++ show novaQtd ++ " em " ++ chave,
            status    = Sucesso
            }
      in Right (inventarioNovo, logEntry)
      
-- FUNCAO DE RELATORIO
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro allLogs = filter ehErro allLogs
  where
    ehErro log = case status log of
      Falha _ -> True
      Sucesso -> False

--- dar uma olhada nessas funçoes: 

-- FUNCAO PURA DE RELATORIO
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemID allLogs = filter (ehDoItem itemID) allLogs
  where
    -- Verifica se o 'detalhe' do log contém o ID do item.
    -- Esta é uma forma simples; pode falhar se IDs forem substrings de outros.
    ehDoItem :: String -> LogEntry -> Bool
    ehDoItem id log = id `isInfixOf` (detalhes log)




-- FUNCAO PURA: Pega o ID da última palavra do 'detalhes'
extrairID :: LogEntry -> Maybe String
extrairID log = case acao log of
    Remove -> Just (last (words (detalhes log)))
    Update -> Just (last (words (detalhes log)))
    _      -> Nothing -- Ignora 'Add' e 'QueryFail'

-- FUNCAO PURA: Acha o item mais movimentado
itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado allLogs =
    let ids     = mapMaybe extrairID allLogs
        grupos  = group (sort ids)
    in case grupos of
        [] -> Nothing
        _  -> listToMaybe (maximumBy (comparing length) grupos)
              
              
              
main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="
    inventario <- lerInventario
    logs <- lerAuditoria
    loop inventario logs
  where
    -- FUNCOES DE INICIALIZACAO

    lerAuditoria :: IO [LogEntry]
    lerAuditoria = do
            conteudo <- readFile "Auditoria.log" `catch` handler
            let linhas = lines conteudo
            let logs = mapMaybe readMaybe linhas
            putStrLn ("Logs de auditoria carregados: " ++ show (length logs))
            return logs
        where
            handler :: IOException -> IO String
            handler _ = do
                putStrLn "Arquivo Auditoria.log não encontrado. Iniciando log vazio..."
                return ""

    lerInventario :: IO Inventario
    lerInventario = do
        conteudo <- readFile "Inventario.dat" `catch` handler
        case readMaybe conteudo of
            Just inv -> return inv
            Nothing  -> return Map.empty
      where
        handler :: IOException -> IO String
        handler _ = do
            putStrLn "Arquivo Inventario.dat não encontrado. Iniciando inventário vazio..."
            return ""

    -- FUNCOES DE PERSISTENCIA

    salvarInventario :: Inventario -> IO ()
    salvarInventario inv = writeFile "Inventario.dat" (show inv)

    salvarLog :: LogEntry -> IO ()
    salvarLog logEntry = appendFile "Auditoria.log" (show logEntry ++ "\n")

    -- LOOP PRINCIPAL

    loop :: Inventario -> [LogEntry] -> IO ()
    loop inventario logs = do
        putStr "\nComando (add/remove/update/show/report/exit): "
        hFlush stdout
        comando <- getLine
        chamarComando comando inventario logs

    chamarComando :: String -> Inventario -> [LogEntry] -> IO ()
    chamarComando comando inventario logs
        | comando == "add"    = chamarAdd inventario logs
        | comando == "remove" = chamarRemove inventario logs
        | comando == "update" = chamarUpdate inventario logs
        | comando == "report" = chamarReport inventario logs 
        | comando == "show"   = chamarShow inventario logs
        | comando == "exit"   = chamarExit inventario logs
        | otherwise           = chamarInvalido inventario logs

    -- COMANDOS DO LOOP

-- CHAMAR ADICIONAR
    chamarAdd :: Inventario -> [LogEntry] -> IO ()
    chamarAdd inventario logs = do
        putStr "ID: " >> hFlush stdout
        idItem <- getLine
        putStr "Nome: " >> hFlush stdout
        nome <- getLine
        putStr "Quantidade: " >> hFlush stdout
        qtdStr <- getLine
        putStr "Categoria: " >> hFlush stdout
        categoria <- getLine

        case readMaybe qtdStr :: Maybe Int of
            Nothing -> do
                putStrLn "Erro: Quantidade inválida. Insira um número."
                loop inventario logs-- Volta ao menu sem salvar

            Just qtd -> do -- 'qtd' agora é um Int 
                -- função original vai aqui dentro
                tempo <- getCurrentTime
                let item = Item idItem nome qtd categoria
                case addItem tempo inventario item of
                    Left erro -> do
                        putStrLn ("Erro: " ++ erro)
                        let logFalha = LogEntry tempo Add ("Falha ao adicionar: " ++ idItem) (Falha erro)
                        salvarLog logFalha
                        loop inventario (logFalha : logs)
                    Right (novoInv, logEntry) -> do
                        salvarInventario novoInv
                        salvarLog logEntry
                        putStrLn "Item adicionado com sucesso!"
                        loop novoInv (logEntry : logs)

    -- CHAMAR REMOVER
    chamarRemove :: Inventario -> [LogEntry] -> IO ()
    chamarRemove inventario logs = do
        putStr "ID a remover: " >> hFlush stdout
        idRemover <- getLine
        tempo <- getCurrentTime
        case removeItem tempo inventario idRemover of
            Left erro -> do
                putStrLn ("Erro: " ++ erro)
                let logFalha = LogEntry tempo Remove ("Falha ao remover: " ++ idRemover) (Falha erro)
                salvarLog logFalha
                loop inventario (logFalha : logs)
            Right (novoInv, logEntry) -> do
                salvarInventario novoInv
                salvarLog logEntry
                putStrLn "Item removido com sucesso!"
                loop novoInv (logEntry : logs)

-- CHAMAR ATUALIZAR
    chamarUpdate :: Inventario -> [LogEntry] -> IO ()
    chamarUpdate inventario logs = do
        putStr "ID: " >> hFlush stdout
        idItem <- getLine
        putStr "Nova quantidade: " >> hFlush stdout
        qtdStr <- getLine

        case readMaybe qtdStr :: Maybe Int of
            Nothing -> do
                putStrLn "Erro: Quantidade inválida. Insira um número."
                loop inventario logs

            Just novaQtd -> do -- 'novaQtd' agora é um Int
                -- função original 
                tempo <- getCurrentTime
                case updateQty tempo inventario idItem novaQtd of
                    Left erro -> do
                        putStrLn ("Erro: " ++ erro)
                        let logFalha = LogEntry tempo Update ("Falha ao atualizar: " ++ idItem) (Falha erro)
                        salvarLog logFalha
                        loop inventario (logFalha : logs)
                    Right (novoInv, logEntry) -> do
                        salvarInventario novoInv
                        salvarLog logEntry
                        putStrLn "Quantidade atualizada!"
                        loop novoInv (logEntry : logs)

-- CHAMAR RELATORIO
    chamarReport :: Inventario -> [LogEntry] -> IO ()
    chamarReport inventario logs = do
        putStrLn "\n--- Módulo de Relatórios ---"
        putStr "Qual relatório (erros / historico / movimentado): " >> hFlush stdout
        tipoReport <- getLine

        case tipoReport of
            "erros"       -> reportarErros logs
            "historico"   -> reportarHistorico logs
            "movimentado" -> reportarMaisMovimentado logs
            _             -> putStrLn "Relatório inválido."


        loop inventario logs -- Volta ao loop principal
      where
        -- Função auxiliar para o relatório de erros
        reportarErros :: [LogEntry] -> IO ()
        reportarErros logs = do
            putStrLn "\n--- Relatório de Erros de Operação ---"
            let erros = logsDeErro logs
            if null erros
                then putStrLn "Nenhum erro encontrado."
                else mapM_ (print . formatarLog) erros

        -- Função auxiliar para o histórico de item
        reportarHistorico :: [LogEntry] -> IO ()
        reportarHistorico logs = do
            putStr "Qual o ID do item: " >> hFlush stdout
            idItem <- getLine
            putStrLn ("\n--- Histórico para o item: " ++ idItem ++ " ---")
            let historico = historicoPorItem idItem logs
            if null historico
                then putStrLn "Nenhuma movimentação encontrada para este item."
                else mapM_ (print . formatarLog) historico

        -- Função auxiliar para o item mais movimentado
        reportarMaisMovimentado :: [LogEntry] -> IO ()
        reportarMaisMovimentado logs = do
            putStrLn "\n--- Item Mais Movimentado (Remove/Update) ---"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "Nenhuma movimentação (Remove/Update) registrada."
                Just idItem -> putStrLn ("O item mais movimentado é: " ++ idItem)

        -- Função auxiliar genérica para formatar
        formatarLog :: LogEntry -> String
        formatarLog log = show (acao log) ++ " em " ++ show (timestamp log) ++ ": " ++ detalhes log
        
        
        
    -- CHAMAR EXIBIR
    chamarShow :: Inventario -> [LogEntry] -> IO ()
    chamarShow inventario logs = do
        putStrLn "Inventário atual:"
        print inventario
        loop inventario logs

    -- CHAMAR SAIR
    chamarExit :: Inventario -> [LogEntry] -> IO ()
    chamarExit inventario logs = do
        putStrLn "Saindo e salvando dados..."
        salvarInventario inventario
        exitSuccess

    -- CHAMAR COMANDO INVALIDO
    chamarInvalido :: Inventario -> [LogEntry] -> IO ()
    chamarInvalido inventario logs = do
        putStrLn "Comando inválido!"
        loop inventario logs
