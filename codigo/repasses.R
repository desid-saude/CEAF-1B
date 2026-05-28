# ===================================================================================
# CABEÇALHO
# ===================================================================================
#
#   AUTOR:       Theo da Fonseca Torres
#   CONTATO:     theo.torres@saude.gov.br
#   DATA:        08/12/2025 (Data de criação)
#   VERSÃO:      [4.5.2]
#
#   DESCRIÇÃO:   [Limpeza e reshape dados repasses CEAF]
#
#  
#
# ===================================================================================

# ---
# 0. CONFIGURAÇÃO DO AMBIENTE 
# ---

# Limpa todos os objetos do ambiente de trabalho 
rm(list = ls())

# Força a coleta de lixo 
gc()

# Define opções globais 
options(
  scipen = 999
)

# ---
# 1. CARREGAMENTO DE PACOTES 
# ---
# Lista de pacotes necessários para este script
pacotes_necessarios <- c(
  "janitor",
  "tidyverse",
  "readxl",
  "writexl",
  "stringr"
)

# Função para verificar, instalar (se necessário) e carregar pacotes
carregar_pacotes <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Aplica a função para carregar todos os pacotes da lista
lapply(pacotes_necessarios, carregar_pacotes)
options(warn = -1)

# Carregar funções:
source("codigo/funções.R")


# ---
# 2. DEFINIÇÃO DE CAMINHOS (Paths)
# ---
#--- DEFINIR DIRETÓRIO ---
if (Sys.getenv("USERNAME") == "theo.torres") { # Theo
  
  setwd("C:/Users/theo.torres/Desktop/CEAF")

} else { # Felipe

  setwd(file.path("C:/Users", Sys.info()[["user"]], "OneDrive - Ministério da Saúde/- Atividades/DESID/Demandas/2025-XX-XX- CEAF 1B"))

}


# ---
# 3. Wrangling
# ---
# 1. Definir o vetor de anos que queremos processar
anos_para_ler <- 2020:2025

# 2. Criar o loop que lê e empilha tudo
dados_consolidados <- map_df(anos_para_ler, function(ano_atual) {
  
  # Cria o nome do arquivo dinamicamente 
  nome_arquivo <- paste0("docs/auxiliares/Repasses federais/", ano_atual, ".csv")
  
  message(paste("Processando:", nome_arquivo)) 
  
  read.csv(nome_arquivo, fileEncoding = 'latin1', sep = ";") |>
    filter(str_detect(`Produção.Ambulatorial.do.SUS...Brasil...por.local.de.atendimento`, "^[0-9]|^Procedimento")) |>
    row_to_names(row_number = 1) |>
    select(-29) |> 
    pivot_longer(
      cols = c(2:28), 
      names_to = "Estado",                    
      values_to = "Valor"                            
    ) |>
    extract(
      col = Procedimento,                  
      into = c("Codigo_Procedimento", "Medicamento", "Apresentacao"), 
      regex = "^(\\d+)\\s+(.*?)\\s+(\\d.*$)", 
      remove = TRUE                         
    ) |>
    mutate(
      Ano = ano_atual, 
      Medicamento = str_to_title(Medicamento),
      Apresentacao = purrr::map_chr(
        str_to_title(Apresentacao),
        ~ padronizar_apresentacao(.x)
      ),
      Valor = ifelse(Valor == "-", 0, Valor),
      Valor = parse_number(Valor, locale = locale(decimal_mark = ",")),
      Codigo_Procedimento = as.numeric(Codigo_Procedimento)
    ) |>
    select(Ano, Estado, Medicamento, Apresentacao, Valor)
})

# Visualizar o resultado final
glimpse(dados_consolidados)


#--- JUNTAR COM LAI E PNCP ---

# Importar base:
ceaf = read_xlsx("output/base_completa.xlsx") %>%
  mutate(Ano = year(Data)) %>%
  group_by(Ano, Estado, Medicamento, Apresentacao) %>%
  summarise(LAI  = sum(Valor[Base == "LAI"],  na.rm = TRUE),
            PNCP = sum(Valor[Base == "PNCP"], na.rm = TRUE))

# Juntar com Repasses:
base = left_join(dados_consolidados, ceaf, by = c("Ano", "Estado", "Medicamento", "Apresentacao"))

# Exportar dados:
write_xlsx(dados_consolidados, "output/repasses.xlsx")
write.csv(base, "dashboard/df/repasses.csv")


