module Main where

import Data.Time (UTCTime)
import qualified Data.Map as Map


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



additem :: UTCTime -> Inventario -> Item -> Either String ResultadoOperacao
additem t inventario item
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



removeltem :: UTCTime -> Inventario -> String -> Either String ResultadoOperacao
removeltem t inventario chave
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
  putStrLn "tipos basicos"
