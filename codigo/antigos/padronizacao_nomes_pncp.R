library(readxl)
library(lubridate)
library(readr)
library(tidyverse)
library(openxlsx)

options(warn = -1)

setwd("C:\\Users\\theo.torres\\Desktop\\dashboard\\dashboard\\")

base_pncp <- read_xlsx("C:\\Users\\theo.torres\\Desktop\\Med1B\\PNCP.xlsx") |>
  mutate(data = ymd(data)) |>
  select(1, 4, 10:13) |>
  drop_na(data) |>
  rename(Data = 1, Estado = 2, Medicamento = 3,
         Apresentacao = 4, Quantidade = 5, Valor = 6) |>
  select(Data, Estado, Medicamento, Apresentacao, Quantidade, Valor)

# =======================================================
# --- BLOCO DA PÁGINA: APRESENTAÇÃO DOS MEDICAMENTOS  ---
# =======================================================
# Função para padronizar um único texto
padronizar_texto <- function(texto) {
  
  # Verificação de segurança para valores nulos
  if (is.na(texto)) return(NA)
  
  # 1. Limpeza básica
  texto <- str_to_lower(str_trim(texto))
  
  # 2. Extração da Dosagem
  # Regex adaptado para R (precisa de escapes duplos \\)
  # Procura número (com ponto ou vírgula) seguido da unidade
  padrao_dosagem <- "(\\d+[\\.,]?\\d*)\\s*(mg/ml|mcg/ml|ui/ml|u\\.usp|mg|ml|mcg|g|ui)"
  
  match <- str_match(texto, padrao_dosagem)
  
  dosagem <- ""
  # Se encontrou correspondência (match[1] é o match completo, [2] é o numero, [3] é a unidade)
  if (!is.na(match[1])) {
    numero_str <- str_replace(match[2], ",", ".")
    unidade <- match[3]
    
    # Converter para numérico e voltar para string remove o ".0" automaticamente
    # Ex: "50.0" vira 50, mas "3.6" continua 3.6
    numero_limpo <- as.character(as.numeric(numero_str))
    
    dosagem <- paste0(numero_limpo, unidade)
  }
  
  # 3. Identificação da Forma Farmacêutica
  forma <- "Não Especificado"
  
  # str_detect verifica se ALGUM dos padrões (separados por |) está presente
  if (str_detect(texto, "comprimido|comp|capsula|cap|dragea|cápsula|revestido| comprimido|vo")) {
    forma <- "Comprimido/Cápsula"
  } else if (str_detect(texto, "inj|ampola|fa|seringa|frasco-ampola|implante|injetável|solução|liofinj")) {
    forma <- "Injetável"
  } else if (str_detect(texto, "sol") && str_detect(texto, "oral|xarope|susp")) {
    forma <- "Solução Oral"
  } else if (str_detect(texto, "creme|pomada|topico|tópico")) {
    forma <- "Tópico"
  } else if (str_detect(texto, "inalacao|inalação|inal")) {
    forma <- "Inalação"
  }
  
  # Monta o resultado final
  if (dosagem != "") {
    return(paste(dosagem, "-", forma))
  } else {
    return(forma) # Retorna forma se não achou dosagem
  }
}

# Aplicar a função
# 'rowwise()' é usado porque nossa função processa um texto por vez
df_padronizado <- base_pncp %>%
  rowwise() %>%
  mutate(Apresentacao = padronizar_texto(Apresentacao)) %>%
  ungroup() # Desfaz o agrupamento rowwise para voltar ao normal

df_padronizado <- df_padronizado |>
  mutate(Apresentacao = ifelse(is.na(Apresentacao), "Não Especificado", Apresentacao)) |>
  mutate(Base = "PNCP")

write_csv(df_padronizado, "base_pncp_corrigidos.csv")
