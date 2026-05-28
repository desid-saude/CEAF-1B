# Painel CEAF 1B

Dashboard interativo para monitoramento das aquisições, preços, fornecedores e repasses federais relacionados ao **Componente Especializado da Assistência Farmacêutica — Grupo 1B**.

O projeto consolida diferentes fontes de informação sobre compras públicas de medicamentos do CEAF 1B, permitindo acompanhar a evolução dos gastos, comparar preços entre unidades federativas, identificar padrões de concentração de mercado e apoiar análises sobre eficiência alocativa, transparência e planejamento da assistência farmacêutica.

---

## Visão geral

O **Painel CEAF 1B** foi desenvolvido em `R/Shiny` com o objetivo de organizar, padronizar e visualizar dados de aquisição de medicamentos do Grupo 1B do CEAF. A aplicação integra registros provenientes do **Portal Nacional de Contratações Públicas — PNCP**, respostas obtidas via **Lei de Acesso à Informação — LAI**, informações complementares de **portais estaduais de transparência**, dados do **Banco de Preços em Saúde — BPS** e bases de **repasses federais**.

A ferramenta foi estruturada para apoiar análises exploratórias e comparativas sobre as aquisições realizadas pelos entes federativos, oferecendo uma visão consolidada sobre valores gastos, quantidades adquiridas, fornecedores, medicamentos, apresentações farmacêuticas e distribuição territorial das compras.

---

## Principais funcionalidades

O painel reúne diferentes módulos analíticos voltados ao acompanhamento das aquisições do CEAF 1B:

* **Visão geral das aquisições**, com tabelas consolidadas, ranking de medicamentos e evolução dos valores acumulados;
* **Comparativos periódicos**, com agregações mensais, trimestrais e anuais;
* **Classificação ABC/Pareto**, permitindo identificar os itens de maior peso no gasto total;
* **Base completa**, com visualização tabular e possibilidade de consulta dos registros tratados;
* **Previsão de gastos**, com projeções a partir das séries históricas disponíveis;
* **Evolução e distribuição espacial dos preços**, com rankings estaduais, mapas e séries temporais;
* **Economia potencial**, estimada a partir de benchmarks de preço;
* **Perfil de fornecedores**, com rankings por valor contratado, quantidade adquirida e medidas de preço;
* **Concentração de mercado**, com indicadores como curva de Lorenz, market share e medidas de concentração;
* **Gastos estaduais**, com comparação entre unidades federativas;
* **Repasses federais**, permitindo confrontar os valores repassados com os gastos observados nas bases de aquisição.

---

## Estrutura do repositório

```text
Painel CEAF 1B/
│
├── codigo/
│   ├── funções.R              # Funções auxiliares de limpeza, padronização e harmonização
│   ├── lai.R                  # Tratamento dos dados obtidos via LAI
│   ├── pncp.R                 # Tratamento dos dados do PNCP, PTE e BPS
│   ├── repasses.R             # Tratamento dos dados de repasses federais
│   └── mapa.R                 # Geração da malha geográfica simplificada
│
├── dashboard/
│   ├── global.R               # Configurações globais, tema, funções de UI e carregamento de pacotes
│   ├── ui.R                   # Interface do dashboard
│   ├── server.R               # Lógica reativa e renderização dos módulos analíticos
│   │
│   ├── df/
│   │   ├── base_completa.csv  # Base tratada utilizada pelo dashboard
│   │   ├── repasses.csv       # Base tratada de repasses federais
│   │   └── mapa_brasil_leve.rds
│   │
│   └── www/
│       ├── regua.svg
│       └── regua2.svg
│
├── docs/
│   ├── auxiliares/            # Bases auxiliares, deflatores, listas de medicamentos e população
│   ├── dados/                 # Bases brutas e intermediárias
│   ├── despachos/             # Documentos e respostas estaduais
│   └── Apresentação CEAF 1B.pptx
│
├── output/
│   ├── base_completa.xlsx     # Base consolidada final
│   ├── repasses.xlsx          # Repasses tratados
│   ├── LAI/                   # Saídas do tratamento da LAI
│   └── PNCP/                  # Saídas do tratamento do PNCP
```

---

## Fontes de dados

O projeto consolida informações de múltiplas fontes, com diferentes níveis de padronização e granularidade:

| Fonte                              | Descrição                                                                                             |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------- |
| PNCP                               | Registros de contratações públicas relacionados aos medicamentos do CEAF 1B                           |
| LAI                                | Informações enviadas por governos estaduais em resposta a solicitações via Lei de Acesso à Informação |
| Portais estaduais de transparência | Dados complementares de aquisições obtidos em bases estaduais                                         |
| BPS                                | Informações do Banco de Preços em Saúde utilizadas como referência complementar                       |
| Repasses federais                  | Dados anuais de repasses federais associados aos medicamentos do CEAF                                 |
| Bases auxiliares                   | Lista de medicamentos do Grupo 1B, CATMAT, deflator, estimativas populacionais e malha geográfica     |

Os valores monetários são tratados e padronizados para permitir comparabilidade entre registros, fontes, medicamentos, apresentações e unidades federativas.

---

## Pipeline de tratamento

O fluxo de tratamento dos dados está organizado em scripts independentes, mas complementares.

O script `codigo/lai.R` realiza a leitura, limpeza e padronização dos dados enviados pelos estados via LAI, harmonizando nomes de variáveis, medicamentos, apresentações, datas, quantidades e valores.

O script `codigo/pncp.R` trata os registros extraídos do PNCP e de fontes complementares, realiza a compatibilização com a lista de medicamentos do CEAF 1B, aplica deflacionamento, incorpora informações populacionais e consolida a base final utilizada pelo dashboard.

O script `codigo/repasses.R` reorganiza os dados de repasses federais, transforma os arquivos anuais em uma base longitudinal e integra essas informações com os gastos observados nas bases de aquisição.

O script `codigo/mapa.R` gera uma versão simplificada da malha estadual brasileira, utilizada nos mapas interativos do painel.

---

## Como executar o dashboard

Para executar a aplicação localmente, abra o projeto no R ou RStudio e rode:

```r
shiny::runApp("dashboard")
```

O dashboard utiliza os arquivos já tratados disponíveis em:

```text
dashboard/df/base_completa.csv
dashboard/df/repasses.csv
dashboard/df/mapa_brasil_leve.rds
```

Portanto, não é necessário reprocessar toda a base para abrir a aplicação, desde que esses arquivos estejam presentes no diretório `dashboard/df/`.

---

## Pacotes necessários

O projeto utiliza principalmente os seguintes pacotes em R:

```r
pacman::p_load(
  tidyverse,
  bslib,
  shiny,
  DT,
  ggplot2,
  lubridate,
  scales,
  plotly,
  bsicons,
  writexl,
  sf,
  geobr,
  prophet,
  shinyjs,
  shinycssloaders
)
```

Alguns scripts de tratamento também utilizam pacotes adicionais, como:

```r
janitor
readxl
stringr
stringi
rmapshaper
```

---

## Reprodução das bases

Para reproduzir integralmente as bases tratadas, recomenda-se executar os scripts na seguinte ordem:

```r
source("codigo/lai.R")
source("codigo/pncp.R")
source("codigo/repasses.R")
source("codigo/mapa.R")
```

Antes da execução, verifique os caminhos definidos nos scripts e adapte-os ao ambiente local. Algumas rotinas foram originalmente estruturadas para diretórios internos de trabalho e podem exigir ajuste do `setwd()` ou adoção de caminhos relativos.

---

## Produtos gerados

A execução dos scripts produz arquivos intermediários e finais utilizados pelo painel:

```text
output/LAI/LAI.xlsx
output/PNCP/PNCP.xlsx
output/base_completa.xlsx
output/repasses.xlsx
dashboard/df/base_completa.csv
dashboard/df/repasses.csv
dashboard/df/mapa_brasil_leve.rds
```

A base `base_completa.csv` é o principal insumo do dashboard e contém registros padronizados de aquisições, com variáveis como data, estado, fornecedor, CNPJ, CATMAT, medicamento, apresentação, quantidade, valor, fonte da informação e população.

---

## Organização metodológica

A construção do painel envolveu quatro etapas principais:

1. **Coleta e consolidação das informações**, a partir de fontes públicas, respostas estaduais e bases auxiliares;
2. **Padronização dos registros**, com harmonização de nomes de medicamentos, apresentações, unidades federativas, fornecedores, CATMAT, datas, quantidades e valores;
3. **Integração das bases**, compatibilizando registros de compras, fontes complementares e repasses federais;
4. **Visualização analítica**, por meio de um dashboard interativo com filtros, gráficos, mapas, tabelas e indicadores sintéticos.

Essa organização busca garantir rastreabilidade, comparabilidade e transparência no uso das informações.

---

## Autores

Projeto desenvolvido por:

- **Theo da Fonseca Torres**
- **Felipe Duplat Luz**

**Ministério da Saúde**  
Secretaria Executiva  
Departamento de Economia e Investimentos em Saúde  
Coordenação de Gestão de Dados Estatísticos em Saúde

---

## Observações sobre uso dos dados

Este repositório contém bases tratadas, documentos auxiliares e arquivos derivados de diferentes fontes públicas e institucionais. Antes de disponibilizar o projeto em repositório público, recomenda-se revisar os arquivos presentes em `docs/`, `output/` e `dashboard/df/`, especialmente documentos recebidos via LAI, planilhas estaduais e eventuais informações administrativas sensíveis.

Caso o repositório seja publicado em ambiente aberto, avalie a possibilidade de manter apenas os scripts, arquivos agregados e bases anonimizadas ou já publicáveis, documentando separadamente o processo de obtenção dos dados brutos.