Sistema de Gerenciamento de Inventário em Haskell
Atividade Avaliativa – RA2 — Programação Lógica e Funcional

======================================================================
Instituição e Disciplina
======================================================================
Instituição: Pontifícia Universidade Católica do Paraná (PUCPR)
Disciplina: Programação Lógica e Funcional (Turma 4º U) — Ciência da Computação — Noite
Professor: Frank Coelho de Alcantara

======================================================================
Integrantes do Grupo (ordem alfabética)
======================================================================
Davi Kazuhiro Natume — GitHub: https://github.com/DavasKazuhiro
Felipe Melink Maestri — GitHub: https://github.com/Fmmaestri
Lucas Antonio Pelanda — GitHub: https://github.com/LucasPelanda
Pedro Henrique Valente Favero — GitHub: https://github.com/pedrofavero

======================================================================
Objetivo Geral
======================================================================
Este projeto tem como objetivo implementar um sistema de gerenciamento de inventário utilizando Haskell, aplicando princípios de programação funcional, manipulação de estado puro e persistência de dados. O sistema deve ser executado via terminal e registrar toda interação em um log de auditoria, mantendo histórico entre execuções.

======================================================================
Requisitos Atendidos
======================================================================
O sistema desenvolvido atende aos requisitos da Atividade Avaliativa RA2:

- interação via terminal
- lógica de negócio implementada exclusivamente em funções puras
- manipulação imutável de estado com Map
- persistência em Inventario.dat e Auditoria.log
- log de todas as tentativas de operação (sucesso e falha)
- leitura do estado salvo ao iniciar o programa
- relatórios gerados a partir dos logs
- suporte a comandos interativos
- documentação completa com cenários reais de teste

====================================================================
Ambiente de Execução Online
====================================================================
Link de Execução (Online GDB):
https://onlinegdb.com/iI2mUr1qEi

Como executar:
1) Abrir o link
2) Clicar em Run
3) Usar os comandos no terminal

====================================================================
Comandos Disponíveis
====================================================================
add — adiciona um item
remove — remove quantidade especificada
update — redefine a quantidade
listar — lista itens existentes
report — módulo de relatórios
exit — salva e encerra o programa

====================================================================
Exemplo de Sessão
====================================================================
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

====================================================================
Estrutura Lógica do Sistema — Descrição Técnica
====================================================================
Tipos de dados:
Item — contém itemID, nome, quantidade e categoria
Inventario — Map String Item
AcaoLog — Add, Remove, Update, QueryFail
StatusLog — Sucesso ou Falha String
LogEntry — timestamp, acao, detalhes, status
ResultadoOperacao — tupla contendo novo inventário e registro de log

Funções puras principais:
addItem
removeItem
updateQty
logsDeErro
historicoPorItem
itemMaisMovimentado

====================================================================
Persistência e Auditoria
====================================================================
Inventario.dat — sobrescrito após operações bem sucedidas
Auditoria.log — recebe linha nova a cada tentativa de operação

Sucesso:
- inventário atualizado
- log registrado

Falha:
- estado não é alterado
- log é registrado

====================================================================
Dados de Teste (10 itens utilizados)
====================================================================
teclado01 — 10 — Periféricos
mouse01 — 15 — Periféricos
monitor01 — 5 — Monitores
headset01 — 8 — Áudio
notebook01 — 3 — Computadores
ssd01 — 12 — Armazenamento
hd01 — 7 — Armazenamento
impressora01 — 4 — Impressão
cadeira01 — 6 — Mobiliário
webcam01 — 9 — Vídeo

====================================================================
Cenários de Testes Manuais (Exigência RA2)
====================================================================

Cenário 1 — Persistência
Executar sem arquivos existentes
Adicionar 3 itens
Encerrar com exit
Reabrir e listar
Resultado esperado: dados persistem

Cenário 2 — Estoque insuficiente
Adicionar teclado01 com quantidade 10
Tentar remover 15
Resultado esperado: erro, inventário permanece intacto, log registrado

Cenário 3 — Relatório de erros
Executar report
Selecionar erros
Deve mostrar a falha do cenário 2

====================================================================
Observações Finais
====================================================================
O projeto segue padrões acadêmicos de modelagem de dados e separação funcional entre lógica pura e operações IO. Todas as operações são registradas, persistência é confiável e os relatórios fornecem análises úteis de auditoria.

====================================================================
Repositório GitHub
====================================================================
https://github.com/DavasKazuhiro/RA2-6
