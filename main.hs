module Main where

import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)


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


main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="
    inventario <- lerInventario
    lerAuditoria
    loop inventario
  where
    -- FUNCOES DE INICIALIZACAO

    lerAuditoria :: IO ()
    lerAuditoria = do
        existe <- doesFileExist "Auditoria.log"
        if existe
          then putStrLn "Arquivo de Auditoria encontrado."
          else putStrLn "Arquivo de Auditoria não encontrado."

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

    loop :: Inventario -> IO ()
    loop inventario = do
        putStr "\nComando (add/remove/update/show/exit): "
        hFlush stdout
        comando <- getLine
        chamarComando comando inventario

    chamarComando :: String -> Inventario -> IO ()
    chamarComando comando inventario
        | comando == "add"    = chamarAdd inventario
        | comando == "remove" = chamarRemove inventario
        | comando == "update" = chamarUpdate inventario
        | comando == "show"   = chamarShow inventario
        | comando == "exit"   = chamarExit inventario
        | otherwise           = chamarInvalido inventario

    -- COMANDOS DO LOOP

    -- CHAMAR ADICIONAR
    chamarAdd :: Inventario -> IO ()
    chamarAdd inventario = do
        putStr "ID: " >> hFlush stdout
        idItem <- getLine
        putStr "Nome: " >> hFlush stdout
        nome <- getLine
        putStr "Quantidade: " >> hFlush stdout
        qtdStr <- getLine
        putStr "Categoria: " >> hFlush stdout
        categoria <- getLine
        let qtd = read qtdStr :: Int
        tempo <- getCurrentTime
        let item = Item idItem nome qtd categoria
        case addItem tempo inventario item of
            Left erro -> do
                putStrLn ("Erro: " ++ erro)
                let logFalha = LogEntry tempo Add ("Falha ao adicionar: " ++ idItem) (Falha erro)
                salvarLog logFalha
                loop inventario
            Right (novoInv, logEntry) -> do
                salvarInventario novoInv
                salvarLog logEntry
                putStrLn "Item adicionado com sucesso!"
                loop novoInv

    -- CHAMAR REMOVER
    chamarRemove :: Inventario -> IO ()
    chamarRemove inventario = do
        putStr "ID a remover: " >> hFlush stdout
        idRemover <- getLine
        tempo <- getCurrentTime
        case removeItem tempo inventario idRemover of
            Left erro -> do
                putStrLn ("Erro: " ++ erro)
                let logFalha = LogEntry tempo Remove ("Falha ao remover: " ++ idRemover) (Falha erro)
                salvarLog logFalha
                loop inventario
            Right (novoInv, logEntry) -> do
                salvarInventario novoInv
                salvarLog logEntry
                putStrLn "Item removido com sucesso!"
                loop novoInv

    -- CHAMAR ATUALIZAR
    chamarUpdate :: Inventario -> IO ()
    chamarUpdate inventario = do
        putStr "ID: " >> hFlush stdout
        idItem <- getLine
        putStr "Nova quantidade: " >> hFlush stdout
        qtdStr <- getLine
        tempo <- getCurrentTime
        let novaQtd = read qtdStr :: Int
        case updateQty tempo inventario idItem novaQtd of
            Left erro -> do
                putStrLn ("Erro: " ++ erro)
                let logFalha = LogEntry tempo Update ("Falha ao atualizar: " ++ idItem) (Falha erro)
                salvarLog logFalha
                loop inventario
            Right (novoInv, logEntry) -> do
                salvarInventario novoInv
                salvarLog logEntry
                putStrLn "Quantidade atualizada!"
                loop novoInv

    -- CHAMAR EXIBIR
    chamarShow :: Inventario -> IO ()
    chamarShow inventario = do
        putStrLn "Inventário atual:"
        print inventario
        loop inventario

    -- CHAMAR SAIR
    chamarExit :: Inventario -> IO ()
    chamarExit inventario = do
        putStrLn "Saindo e salvando dados..."
        salvarInventario inventario
        exitSuccess

    -- CHAMAR COMANDO INVALIDO
    chamarInvalido :: Inventario -> IO ()
    chamarInvalido inventario = do
        putStrLn "Comando inválido!"
        loop inventario
