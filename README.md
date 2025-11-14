# Sistema de Gerenciamento de Inventário em Haskell
Atividade Avaliativa – RA2 — Programação Lógica e Funcional

---

## Instituição e Disciplina
**Instituição:** Pontifícia Universidade Católica do Paraná (PUCPR)  
**Disciplina:** Programação Lógica e Funcional (Turma 4º U) — Ciência da Computação — Noite  
**Professor:** Frank Coelho de Alcantara

---

## Integrantes do Grupo 
- **Davi Kazuhiro Natume** — GitHub: DavasKazuhiro  
- **Felipe Melink Maestri** — GitHub: Fmmaestri  
- **Lucas Antonio Pelanda** — GitHub: LucasPelanda  
- **Pedro Henrique Valente Favero** — GitHub: pedrofavero  

---

## Objetivo Geral
Este projeto implementa um **sistema de gerenciamento de inventário** em **Haskell**, aplicando conceitos de programação funcional, persistência de dados e separação rigorosa entre lógica pura e operações de I/O.

O sistema funciona via **terminal**, registra todo evento em um **arquivo de auditoria** e mantém estado entre execuções através de persistência em disco.

---

## Ambiente de Execução Online
O programa pode ser rodado diretamente no navegador, sem instalações.

**Link para execução no Online GDB:**  
https://onlinegdb.com/iI2mUr1qEi

### Como executar
1. Acesse o link acima  
2. Clique em **Run**  
3. Utilize os comandos no terminal inferior

---

## Comandos Disponíveis

| Comando | Função |
|---------|--------|
| `add` | Adiciona um item |
| `remove` | Remove certa quantidade de um item |
| `update` | Atualiza a quantidade total do item |
| `listar` | Lista todos os itens cadastrados |
| `report` | Acessa o módulo de relatórios |
| `exit` | Salva o estado atual e encerra |

---

## Exemplo de sessão
=== Sistema de Inventario ===
Comando: add
ID: teclado01
Nome: Teclado Mecânico
Quantidade: 10
Categoria: Periféricos
Item adicionado com sucesso!

Comando: listar
ID: teclado01
Nome: Teclado Mecânico
Quantidade: 10
Categoria: Periféricos
---

## Estrutura Lógica do Sistema

### **Tipos de Dados**
- **Item** — contém itemID, nome, quantidade e categoria  
- **Inventario** — Map String Item  
- **AcaoLog** — Add | Remove | Update | QueryFail  
- **StatusLog** — Sucesso | Falha String  
- **LogEntry** — timestamp, acao, detalhes, status  
- **ResultadoOperacao** — Inventário atualizado + registro de log

### **Funções puras**
- addItem  
- removeItem  
- updateQty  
- logsDeErro  
- historicoPorItem  
- itemMaisMovimentado  

Todas retornam `Either` para sinalizar falhas sem I/O.

---

## 💾 Persistência e Auditoria
O sistema mantém estado entre execuções utilizando dois arquivos:

| Arquivo | Função |
|---------|--------|
| `Inventario.dat` | Persistência do inventário |
| `Auditoria.log` | Registro de todas as operações, sucesso ou falha |

📌 Em operações bem-sucedidas, o inventário é salvo e o log é gravado  
📌 Em erro lógico, o inventário é mantido e um LogEntry de falha é registrado

---

## Dados de Teste (10 itens mínimos exigidos pela RA2)
| ID | Nome | Qtd | Categoria |
|----|------|-----|-----------|
| teclado01 | Teclado Mecânico | 10 | Periféricos |
| mouse01 | Mouse Óptico | 15 | Periféricos |
| monitor01 | Monitor 24" | 5 | Monitores |
| headset01 | Headset Gamer | 8 | Áudio |
| notebook01 | Notebook i5 | 3 | Computadores |
| ssd01 | SSD 1TB | 12 | Armazenamento |
| hd01 | HD 2TB | 7 | Armazenamento |
| impressora01 | Impressora Laser | 4 | Impressão |
| cadeira01 | Cadeira Ergonômica | 6 | Mobiliário |
| webcam01 | Webcam Full HD | 9 | Vídeo |
 Inserção dos 10 itens iniciais no inventário

 Inserção realizada manualmente com o comando `add`.

---

## Conteúdo do arquivo `Auditoria.log`

```haskell
LogEntry {timestamp = 2025-11-14 20:28:02 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"teclado01\", nome = \"Teclado Mecanico\", quantidade = 10, categoria = \"Perifericos\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:28:28 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"mouse01\", nome = \"Mouse Optico\", quantidade = 15, categoria = \"Perifericos\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:28:56 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"monitor01\", nome = \"Monitor 24 polegadas\", quantidade = 5, categoria = \"Monitores\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:29:09 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"headset01\", nome = \"Headset Gamer\", quantidade = 8, categoria = \"Audio\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:29:24 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"notebook01\", nome = \"Notebook i5\", quantidade = 3, categoria = \"Computadores\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:29:46 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"ssd01\", nome = \"SSD 1TB\", quantidade = 12, categoria = \"Armazenamento\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:30:01 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"hd01\", nome = \"HD 2TB\", quantidade = 7, categoria = \"Armazenamento\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:30:19 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"impressora01\", nome = \"Impressora Laser\", quantidade = 4, categoria = \"Impressao\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:30:42 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"cadeira01\", nome = \"Cadeira Ergonomica\", quantidade = 6, categoria = \"Mobiliario\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:30:56 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"webcam01\", nome = \"Webcam Full HD\", quantidade = 9, categoria = \"Video\"}", status = Sucesso}
```
📂 Conteúdo do arquivo Inventario.dat após inserção
```haskell
fromList
  [ ("cadeira01", Item { itemID = "cadeira01", nome = "Cadeira Ergonomica", quantidade = 6, categoria = "Mobiliario" })
  , ("hd01", Item { itemID = "hd01", nome = "HD 2TB", quantidade = 7, categoria = "Armazenamento" })
  , ("headset01", Item { itemID = "headset01", nome = "Headset Gamer", quantidade = 8, categoria = "Audio" })
  , ("impressora01", Item { itemID = "impressora01", nome = "Impressora Laser", quantidade = 4, categoria = "Impressao" })
  , ("monitor01", Item { itemID = "monitor01", nome = "Monitor 24 polegadas", quantidade = 5, categoria = "Monitores" })
  , ("mouse01", Item { itemID = "mouse01", nome = "Mouse Optico", quantidade = 15, categoria = "Perifericos" })
  , ("notebook01", Item { itemID = "notebook01", nome = "Notebook i5", quantidade = 3, categoria = "Computadores" })
  , ("ssd01", Item { itemID = "ssd01", nome = "SSD 1TB", quantidade = 12, categoria = "Armazenamento" })
  , ("teclado01", Item { itemID = "teclado01", nome = "Teclado Mecanico", quantidade = 10, categoria = "Perifericos" })
  , ("webcam01", Item { itemID = "webcam01", nome = "Webcam Full HD", quantidade = 9, categoria = "Video" })
  ]
```

## Cenários de Teste Manuais (Exigidos pela RA2)

### **Cenário 1 — Persistência**
1. Rodar o programa sem arquivos existentes
2. Adicionar 3 itens
3. Sair com `exit`
4. Abrir novamente e digitar `listar`

📍 **Resultado esperado:** itens persistem
Adição dos itens e fechamento do programa
<img width="769" height="623" alt="image" src="https://github.com/user-attachments/assets/c8f831e1-767f-4c2a-b2b8-f24102860010" />

Reabertura do programa e listagem

<img width="666" height="519" alt="image" src="https://github.com/user-attachments/assets/a2813c11-2a99-49fb-b7c3-29ed7976ec9a" />



---

### **Cenário 2 — Erro de lógica**
1. Adicionar teclado01 com quantidade 10
2. `remove teclado01 15`

📍 **Resultado esperado:** erro exibido, inventário mantido, log de falha salvo
<img width="672" height="416" alt="image" src="https://github.com/user-attachments/assets/ad3be5bc-e527-4825-af0b-e07c62860f0f" />

Log de auditoria:
```haskell
LogEntry {timestamp = 2025-11-14 20:46:17.231669296 UTC, acao = Add, detalhes = "Adicionado: Item {itemID = \"teclado01\", nome = \"Teclado Mecanico\", quantidade = 10, categoria = \"Perifericos\"}", status = Sucesso}
LogEntry {timestamp = 2025-11-14 20:46:23.939517971 UTC, acao = Remove, detalhes = "Falha ao remover: ID teclado01", status = Falha "Estoque insuficiente"}
```

---

### **Cenário 3 — Relatório de erros**
1. Executar `report`
2. Selecionar `erros`

📍 **Resultado esperado:** falha do cenário 2 listada no relatório

<img width="785" height="283" alt="image" src="https://github.com/user-attachments/assets/8e31ac09-4a68-4d8c-a0af-ce77ebf82f96" />


---

## Conclusão
Este trabalho demonstra domínio de:
- Programação funcional com Haskell
- ADTs e expressões condicionais puras
- Persistência e estados imutáveis
- Operações de I/O encapsuladas corretamente
- Sistema funcional completo e testado em ambiente real

---
