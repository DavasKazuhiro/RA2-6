# Sistema de Gerenciamento de Inventário em Haskell
Atividade Avaliativa – RA2 — Programação Lógica e Funcional

---

## 🏫 Instituição e Disciplina
**Instituição:** Pontifícia Universidade Católica do Paraná (PUCPR)  
**Disciplina:** Programação Lógica e Funcional (Turma 4º U) — Ciência da Computação — Noite  
**Professor:** Frank Coelho de Alcantara

---

## 🧑‍💻 Integrantes do Grupo (ordem alfabética)
- **Davi Kazuhiro Natume** — GitHub: https://github.com/DavasKazuhiro  
- **Felipe Melink Maestri** — GitHub: https://github.com/Fmmaestri  
- **Lucas Antonio Pelanda** — GitHub: https://github.com/LucasPelanda  
- **Pedro Henrique Valente Favero** — GitHub: https://github.com/pedrofavero  

---

## 🎯 Objetivo Geral
Este projeto implementa um **sistema de gerenciamento de inventário** em **Haskell**, aplicando conceitos de programação funcional, persistência de dados e separação rigorosa entre lógica pura e operações de I/O.

O sistema funciona via **terminal**, registra todo evento em um **arquivo de auditoria** e mantém estado entre execuções através de persistência em disco.

---

## 🌐 Ambiente de Execução Online
O programa pode ser rodado diretamente no navegador, sem instalações.

🔗 **Link para execução no Online GDB:**  
https://onlinegdb.com/iI2mUr1qEi

### Como executar
1. Acesse o link acima  
2. Clique em **Run**  
3. Utilize os comandos no terminal inferior

---

## 💻 Comandos Disponíveis

| Comando | Função |
|---------|--------|
| `add` | Adiciona um item |
| `remove` | Remove certa quantidade de um item |
| `update` | Atualiza a quantidade total do item |
| `listar` | Lista todos os itens cadastrados |
| `report` | Acessa o módulo de relatórios |
| `exit` | Salva o estado atual e encerra |

---

## 🧾 Exemplo de sessão
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

## 🧠 Estrutura Lógica do Sistema

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

## 📦 Dados de Teste (10 itens mínimos exigidos pela RA2)
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

---

## 🧪 Cenários de Teste Manuais (Exigidos pela RA2)

### **Cenário 1 — Persistência**
1. Rodar o programa sem arquivos existentes
2. Adicionar 3 itens
3. Sair com `exit`
4. Abrir novamente e digitar `listar`

📍 **Resultado esperado:** itens persistem

---

### **Cenário 2 — Erro de lógica**
1. Adicionar teclado01 com quantidade 10
2. `remove teclado01 15`

📍 **Resultado esperado:** erro exibido, inventário mantido, log de falha salvo

---

### **Cenário 3 — Relatório de erros**
1. Executar `report`
2. Selecionar `erros`

📍 **Resultado esperado:** falha do cenário 2 listada no relatório

---

## 🏁 Conclusão
Este trabalho demonstra domínio de:
- Programação funcional com Haskell
- ADTs e expressões condicionais puras
- Persistência e estados imutáveis
- Operações de I/O encapsuladas corretamente
- Sistema funcional completo e testado em ambiente real

---

## 📂 Repositório GitHub
🔗 https://github.com/DavasKazuhiro/RA2-6

---
