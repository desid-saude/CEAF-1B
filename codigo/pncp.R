
######################################################################################################
# MINISTÉRIO DA SAÚDE (MS)                                                                           #
# SECRETARIA EXECUTIVA (SE)                                                                          #
# DEPARTAMENTO DE ECONOMIA E INVESTIMENTOS EM SAÚDE (DESID)                                          #
# COORDENAÇÃO DE GESTÃO DE DADOS ESTATÍSTICOS EM SAÚDE (COEST)                                       #
#----------------------------------------------------------------------------------------------------#
# DESCRIÇÃO DA ATIVIDADE:                                                                            #
#                                                                                                    #
# Limpar e padronizar os dados extraídos do site do Portal Nacional de Contratações Públicas (PNCP)  #
# e dos dados enviados pelos governos estaduais através da Lei de Acesso à Informação (LAI).         #
#----------------------------------------------------------------------------------------------------#
# Autores: Theo da Fonseca Torres e Felipe Duplat Luz                                                #
# Data: 09/12/2025                                                                                   #
# Versão: 1.0                                                                                        #
#----------------------------------------------------------------------------------------------------#

#--- DEFINIR DIRETÓRIO ---
if (Sys.getenv("USERNAME") == "theo.torres") {
  
  setwd("C:/Users/theo.torres/Desktop/dashboard")

} else {

  setwd(file.path("C:/Users", Sys.info()[["user"]], "OneDrive - Ministério da Saúde/- Atividades/DESID/Demandas/2026-XX-XX- CEAF 1B"))

}


#--- CARREGAR OS PACOTES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               writexl,
               stringi,
               lubridate)


#--- AUXILIARES ---
options(scipen = 999)

#--- CARREGAR FUNÇÕES ---
source("codigo/funções.R")



#--- IMPORTAR DADOS ------------------------

#--- AUXILIARES ---

# Lista CEAF 1B:
ceaf = read_excel("docs/auxiliares/lista_1B.xlsx") %>% select(medicamento) %>% mutate(medicamento = stri_trans_general(medicamento, "Latin-ASCII")) %>% unique()

# Dicionário dos medicamentos:
dic = ceaf %>% mutate(base = word(medicamento, 1)) %>% rename(medicamento_ref = medicamento)

# Deflator:
deflator = read_xlsx("docs/auxiliares/deflator.xlsx")

# População:
pop = read_xlsx("docs/auxiliares/Estimativas da população.xlsx", sheet = "UF")


#--- PNCP --- 
pncp = read_excel("docs/dados/pncp_dados_extraidos.xlsx")


#--- PORTAIS DA TRANSPARÊNCIA ESTADUAIS ---
pte = read.csv("docs/dados/PTE/PR.csv", sep = ";")



#--- TRATAR DADOS --------------------------

#--- PNCP --- 
pncp_clean = pncp %>%

  # "Quebrar" variáveis:
  mutate(uf      = substr(Local, nchar(Local) - 1, nchar(Local)),
         unidade = trimws(substr(`Unidade compradora`, 10, nchar(`Unidade compradora`))),
         empresa = sub(".*social:\\s*", "", FORNECEDOR),
         cnpj    = sub(".*CPF:\\s*(.*?)(\n).*", "\\1", FORNECEDOR),
         data    = substr(`Data fim de recebimento de propostas`, 1, 10)) %>%
  
  # Ajustar classe:
  mutate(valor_total    = parse_number(`Valor total estimado`,    locale = locale(decimal_mark = ",", grouping_mark = ".")),
         valor_unitario = parse_number(`Valor unitário estimado`, locale = locale(decimal_mark = ",", grouping_mark = ".")),
         data           = dmy(data)) %>%

  # Limpar nome e apresentação:
  mutate(temp = stri_trans_general(sub("^\\((ID-)?[0-9]+\\)\\s*-?\\s*", "", Descrição), "Latin-ASCII")) %>%
  
  # Separar nome de apresentação:
  separate(col   = temp, 
           into  = c("medicamento", "apresentacao"),
           sep   = "(?<=\\D)(?=\\d)",
           extra = "merge") %>%
  
  # Padronizar nome:
  mutate(medicamento = sub("(,| - |:|PRINCIPIO|concentracao|caracteristicas|acessorio|composicao|tipo|dosagem|[0-9]|;).*", "", medicamento)) %>%

  # Limpar nome:
  mutate(medicamento = stri_trans_general(sub("^\\((ID-)?[0-9]+\\)\\s*-?\\s*", "", medicamento), "Latin-ASCII")) %>%
    
  # Puxar termos em parênteses para frente:
  mutate(medicamento = stri_trans_totitle(trimws(gsub("[()]", "", medicamento)))) %>%
  
  # Ajuste ordem do medicamento:
  mutate(medicamento = sub("^(.+)\\s+[Dd]e\\s+(.+)$", "\\2 \\1", medicamento),
         base        = word(medicamento, 1)) %>%
  left_join(dic, by = "base") %>%
  mutate(medicamento = ifelse(medicamento == base & !is.na(medicamento_ref), medicamento_ref, medicamento)) %>%
   
  # Criar variáveis:
  mutate(ano         = year(data),
         mes         = month(data),
         quantidade  = parse_number(Quantidade, locale = locale(decimal_mark = ",", grouping_mark = "."))) %>%
  
  # Deflacionar para dez/25:
  left_join(deflator, by = c("ano", "mes")) %>%
  mutate(valor_unitario = valor_unitario * (deflator$ipca[deflator$ano == 2025 & deflator$mes == 12][1] / ipca),
         valor_total    = valor_total    * (deflator$ipca[deflator$ano == 2025 & deflator$mes == 12][1] / ipca)) %>%
    
  # Finalizar manipulação:
  mutate(medicamento = sub("^Eltrombopag\\b", "Eltrombopague", medicamento)) %>%
  select(data, ano, mes, uf, unidade, modalidade = `Modalidade da contratação`, contrato = `Id contratação PNCP`,
         cnpj, empresa, medicamento, apresentacao, quantidade, valor_unitario, valor_total) %>%
  rename_with(tolower, everything()) %>%

  # Ajuste manual:
  mutate(medicamento = if_else(grepl("Hidroxido", medicamento, ignore.case = TRUE), "Ferrico Hidroxido",medicamento))


#--- PORTAIS DA TRANSPARÊNCIA ESTADUAIS ---
pte_clean = pte %>%
       mutate(
    apresentacao = if_else(
                           str_detect(item, ","),
                           str_trim(str_replace(item, "^[^,]*,", "")),
                           NA_character_),
    item = str_trim(str_replace(item, ",.*$", "")),
    item = str_trim(str_remove(item, "^\\d+\\s+"))) %>%
       select()






#--- UNIR BASES ----------------------------

#--- UNIR ---
pncp_full = semi_join(pncp_clean, ceaf, by = "medicamento")


#--- TRATAMENTO ---
pncp_full = pncp_full %>%
  mutate(
    apresentacao = apresentacao %>%
      tolower() %>%
      gsub(
        "(forma farmaceutica|para solucao injetavel|solucao injetavel|apresentacao|
        componente|equivalencia|caracteristica adicional|unidade|uso|fornecimento|
        via de administracao|fornecimento|equivalencia|via de adminis)",
        "",
        .,
        ignore.case = TRUE
      ) %>%
      gsub(
       "\\b(ampo|compo|compon|-|com|pr|co|po lio|pre|apre|para apres|para apr|apr|fras)\\b",
       "",
       .,
       ignore.case = TRUE) %>%
      gsub("(componente|apres)", "", ., ignore.case = TRUE) %>%
      gsub("de triptorrelina", "", ., ignore.case = TRUE) %>%
      gsub("[,.;:]", "", .) %>%
      gsub("\\s+", " ", .) %>%
      trimws())



#--- ADAPTAR PARA DASHBOARD ----------------------------
dashboard_pncp = pncp_full %>%
       select(Data = data, Estado = uf, CNPJ = cnpj, Fornecedor = empresa, Medicamento = medicamento,
              Apresentacao = apresentacao, Quantidade = quantidade, Valor = valor_total) %>%
       rowwise() %>%
       mutate(Apresentacao = padronizar_apresentacao(Apresentacao)) %>%
       ungroup() %>%
       mutate(Apresentacao = ifelse(is.na(Apresentacao), "Não Especificado", Apresentacao)) |>
       mutate(Base = "PNCP")



#--- JUNTAR LAI E PNCP ------------------------

#--- JUNTAR ---
bps = read_excel("docs/dados/BPS/BPS_clean.xlsx")
dashboard_total = bind_rows(dashboard_lai, dashboard_pncp, bps) %>%
  group_by(Medicamento, Apresentacao) %>%
  mutate(CATMAT = coalesce(CATMAT, first(na.omit(CATMAT))),
         Data   = as.Date(Data),
         ano    = year(Data)) %>%
  ungroup() %>%
  left_join(pop, by = c("ano", "Estado" = "uf")) %>%
  rename(Populacao = pop) %>% select(-ano)


#--- EXPORTAR ---
write_xlsx(pncp_full, "output/PNCP/PNCP.xlsx")
write_xlsx(dashboard_total, "output/base_completa.xlsx")
write_csv(dashboard_total, "dashboard/df/base_completa.csv")


