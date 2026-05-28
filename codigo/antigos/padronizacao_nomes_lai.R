library(tidyverse)
library(utils)

setwd("C:\\Users\\theo.torres\\Desktop\\dashboard\\dashboard\\")
df <- read.csv("base_completa.csv", sep = ",") |>
  select(-1) |>
  mutate(Apresentacao = str_remove_all(Apresentacao,"\\."))


# =======================================================
# --- BLOCO DA PÁGINA: NOMES DOS MEDICAMENTOS  ---
# =======================================================
# 1. Definir o Mapa de Correções (De -> Para)
# A estrutura é c("Nome Original Ruim" = "Nome Novo Bom", ...)
mapa_medicamentos <- c(
  # Prefixos e Sais
  "Acetato de Ciproterona" = "Ciproterona",
  "Acetato de Gosserrelina" = "Gosserrelina",
  "Acetato de Leuprorrelina" = "Leuprorrelina",
  "Acetato de Triptorrelina" = "Triptorrelina",
  "Embonato Triptorrelina" = "Triptorrelina",
  "Embonato de Triptorrelina" = "Triptorrelina",
  "Cloridrato Amantadina" = "Amantadina",
  "Cloridrato de Amantadina" = "Amantadina",
  "Cloridrato de Selegilina" = "Selegilina",
  "Cloridrato de Triexifenidil" = "Triexifenidil",
  "Triexifenidila" = "Triexifenidil",
  "Mesilato de Deferoxamina" = "Desferroxamina",
  "Deferiprona" = "Deferiprona",
  "Risedronato de Sodio" = "Risedronato",
  "Dicloridrato Sapropterina" = "Sapropterina",
  
  # Erros de Digitação e Variações
  "Acitetrina" = "Acitretina",
  "Control Acitretina" = "Acitretina",
  "Control Risperidona" = "Risperidona",
  "Acido Ursodesoxicolico" = "Ácido Ursodesoxicólico",
  "Ursodesoxicolico" = "Ácido Ursodesoxicólico",
  "Eltrombopag Olamina" = "Eltrombopague Olamina",
  "Eltrombopague" = "Eltrombopague Olamina",
  "Eltrombopague Olamina" = "Eltrombopague Olamina",
  "Olamina Eltrombopague" = "Eltrombopague Olamina",
  
  
  # Inversões e Formas
  "Humana Imunoglobulina" = "Imunoglobulina Humana",
  "Rivastigmina Sol. Oral" = "Rivastigmina",
  
  # Nomes Compostos e Complexos
  "Brometo Tiotropio + Olodaterol Cloridrato" = "Tiotrópio + Olodaterol",
  "Brometo de Tiotropio + Cloridato de Olodaterol" = "Tiotrópio + Olodaterol",
  "Brometo de Tiotropio Monoidratado + Cloridrato de Olodaterol" = "Tiotrópio + Olodaterol",
  "Tiotropio + Olodaterol" = "Tiotrópio + Olodaterol",
  "Brometo de Tiotropio" = "Tiotrópio",
  "Brometo de Umeclidinio + Trifenatato de Vilanterol" = "Umeclidínio + Vilanterol",
  
  "Sacubitril Valsartana" = "Sacubitril + Valsartana",
  "Sacubitril Valsartana Sodica Hidratada" = "Sacubitril + Valsartana",
  "Valsartana Sodica Hidratada Sacubitril" = "Sacubitril + Valsartana",
  "Sacubitril" = "Sacubitril + Valsartana",
  
  "Sacarato de Hidroxido  Ferrico" = "Sacarato de Hidróxido Férrico",
  "Sacarato de Hidroxido Ferrico" = "Sacarato de Hidróxido Férrico",
  "Sacarato de Oxido Ferrico" = "Sacarato de Hidróxido Férrico",
  "Sacarato de hidróxido ferrico injetável" = "Sacarato de Hidróxido Férrico"
)

# 2. Aplicar a Padronização
df_corrigido <- df %>%
  mutate(
    # str_squish remove espaços extras (inicio, fim e duplos no meio)
    # Isso ajuda a garantir que o nome bata com a chave do dicionário
    Medicamento_Limpo = str_squish(Medicamento),
    
    # A mágica acontece aqui:
    # Tentamos buscar o nome no mapa. Se não encontrar (retornar NA), usamos o nome original.
    Medicamento = coalesce(mapa_medicamentos[Medicamento_Limpo], Medicamento_Limpo)
  ) |>
  select(-7)

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
df_padronizado <- df_corrigido %>%
  rowwise() %>%
  mutate(Apresentacao = padronizar_texto(Apresentacao)) %>%
  mutate(Base = "LAI") |>
  ungroup() # Desfaz o agrupamento rowwise para voltar ao normal


# Salvar o arquivo final
write_csv(df_padronizado, "base_nomes_corrigidos.csv")


base_total <- rbind(df_padronizado, df_padronizado1)
write_csv(base_total, "base_total.csv")
