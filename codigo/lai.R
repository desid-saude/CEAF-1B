
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
               janitor,
               stringi)


#--- AUXILIARES ---
options(scipen = 999)

#--- CARREGAR FUNÇÕES ---
source("codigo/funções.R")



#--- IMPORTAR DADOS ------------------------

#--- AUXILIARES ---
deflator = read_xlsx("docs/auxiliares/deflator.xlsx")
xlsx = list.files(path = "docs/despachos", pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
ceaf = read_excel("docs/auxiliares/lista_1B.xlsx") %>% 
  mutate(medicamento = stri_trans_general(medicamento, "Latin-ASCII"),
         base        = stri_trans_general(base, "Latin-ASCII"),
         base2       = as.character(base2))


#--- CRIAR LISTA ---
lai = xlsx %>%
  setNames(basename(dirname(xlsx))) %>%
  map(~ {
    arquivo <- .x
    sheets  <- excel_sheets(arquivo)

    map_dfr(
      sheets,
      ~ read_xlsx(arquivo, sheet = .x) %>%
          clean_names() #%>%
          #mutate(ano = .x)
    )
  }) #%>%
  #purrr::map(~ .x %>%
  #  mutate(across(
  #    matches("(^|_)data($|_)"),
  #    ~ {
  #      v <- as.character(.) %>% na_if("-") %>% na_if("")
  #      as.Date(parse_date_time(v, orders = c("ymd", "dmy", "dmy HMS", "ymd HMS")))
  #    }
  #  ))
  #)



#--- TRATAR DADOS ------------------------

#--- HARMONIZAR LISTA ---

# Ajustes manuais:
#lai$AL$data_da_aquisicao_publicacao = as.numeric(lai$AL$data_da_aquisicao_publicacao)

# Rodar função:
lai = lai %>% map(~ padronizar_var(.x, dic_var)) %>%
  map(~ {df = .x
    for (v in intersect(names(df), names(dic_class))) {
    
      df[[v]] = padronizar_class(df[[v]], dic_class[[v]])}
    
    df})


#--- UNIR BASES ---
lai_clean = lai %>%
  imap(~ {
    bind_rows(.x, .id = "id_arquivo") %>%
      mutate(uf = .y)
  }) %>%
  bind_rows()


#--- PADRONIZAR VARIÁVEIS ---
lai_clean = lai_clean %>%
  
  # Ajustes:
  rename_with(tolower, everything()) %>%
  
  # Ajuste da data:
  mutate(data = if_else(year(data) < 100, make_date(year = ano, month = month(data), day = day(data)), data)) %>%
    
  # Criar variáveis:
  mutate(ano            = if_else(!is.na(data) & !ano %in% names(.), year(data),  ano),
         mes            = if_else(!is.na(data) & !mes %in% names(.), month(data), mes),
         valor_total    = if_else(is.na(valor_total), valor_unitario * quantidade, valor_total),
         valor_unitario = if_else(is.na(valor_unitario), valor_total / quantidade, valor_unitario)) %>%
  
  # Deflacionar para dez/25:
  left_join(deflator, by = c("ano", "mes")) %>%
  mutate(valor_unitario = valor_unitario * (deflator$ipca[deflator$ano == 2025 & deflator$mes == 12][1] / ipca),
         valor_total    = valor_total    * (deflator$ipca[deflator$ano == 2025 & deflator$mes == 12][1] / ipca)) %>%
  
  # Limpar medicamento:
  mutate(medicamento = stri_trans_totitle(stri_trans_general(medicamento, "Latin-ASCII"))) %>%
  
  # criar apresentação:
  separate(col   = medicamento, 
           into  = c("medicamento", "apresentacao2"),
           sep   = "(?<=\\D)(?=\\d)",
           extra = "merge") %>%
  
  # Condensar apresentação:
  mutate(apresentacao = case_when(is.na(apresentacao) & !is.na(apresentacao2) ~ apresentacao2,
                                 !is.na(apresentacao) & is.na(apresentacao2)  ~ apresentacao,
                                 !is.na(apresentacao) & !is.na(apresentacao2) ~ paste(apresentacao, apresentacao2, sep = " "),
                                 TRUE ~ NA_character_)) %>%
  
  # Tirar numeração:
  mutate(medicamento = str_remove(medicamento, "^\\d+\\s*-\\s*")) %>%
  
  # Padronizar nome:
  mutate(medicamento = sub("(,| - |:|Dosagem|Comprimido|Capsula|Po Para|Po Liofilizado|\\(Lipase|Solucao|Sol\\.).*", "", medicamento)) %>%
  
  # Ajustes:
  mutate(medicamento = trimws(str_remove_all(medicamento, "\\b(Gerais|Control|Lar)\\b"))) %>%
  
  # Ajustar ordem dos nomes:
  mutate(medicamento = map_chr(
    medicamento, ~ {
      hit = ceaf %>% filter(str_detect(.x, fixed(base, ignore_case = TRUE)))

      if (nrow(hit) > 0) {
        hit$medicamento[1]
      } else {
        .x
      }
    }
  )) %>%
  
  # Ajustar apresentação:
  mutate(
    apresentacao = map2_chr(
      apresentacao, medicamento,
      ~ {
        hit = ceaf %>%
          filter(
            medicamento == .y,
            str_detect(.x, fixed(base2))
          )

        if (nrow(hit) > 0) {
          hit$apresentacao[1]
        } else {
          .x
        }
      }
    )
  ) %>%
  
  # Ajustar CNPJ e empresa (1/2):
  mutate(
    cnpj_empresa = if_else(
      is.na(cnpj_empresa) & str_detect(empresa, "^\\d"),
      str_trim(str_extract(empresa, "^\\d+[^-]*")),
      cnpj_empresa
    ),
    
    empresa = if_else(
      str_detect(empresa, "^\\d"),
      str_trim(str_remove(empresa, "^\\d+[^-]*-")),
      empresa)
  ) %>%
  
  # Ajustar CNPJ e empresa (2/2):
  mutate(
    # 1) Extrair CNPJ do texto da empresa, apenas se cnpj_empresa for NA
    cnpj_empresa = if_else(
      is.na(cnpj_empresa) &
        str_detect(empresa, regex("CNPJ", ignore_case = TRUE)),
      {
        # extrai algo do tipo 05.049.432/ 0001-00 ou 05049432000100
        cnpj_extraido <- str_extract(
          empresa,
          "\\d{2}\\.?\\d{3}\\.?\\d{3}\\s*/?\\s*\\d{4}-?\\d{2}|\\d{14}"
        )

        # remove tudo que não for número
        cnpj_digits <- str_replace_all(cnpj_extraido, "\\D", "")

        # só aceita se tiver exatamente 14 dígitos
        if_else(
          !is.na(cnpj_digits) & str_length(cnpj_digits) == 14,
          str_replace(
            cnpj_digits,
            "^(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})$",
            "\\1.\\2.\\3/\\4-\\5"
          ),
          NA_character_
        )
      },
      cnpj_empresa
    ),

    # 2) Limpar o nome da empresa
    empresa = str_trim(
      str_remove(
        empresa,
        regex(
          "\\s*CNPJ\\s*:?.*$",
          ignore_case = TRUE
        )
      )
    )
  ) %>%
  
  # Padronizar nome da empresa:
  mutate(
    empresa_pad = empresa %>%
      str_to_upper() %>%                                    # caixa
      str_replace_all("\\s+", " ") %>%                      # espaços múltiplos
      str_trim() %>%
      str_replace_all("\\s*-\\s*$", "") %>%                 # remove hífen no final (" -", "- ")
      str_replace_all("[\\.,;:]+", " ") %>%                 # pontuação vira espaço
      str_replace_all("[/\\\\]+", " ") %>%                  # barras viram espaço
      str_replace_all("\\s+", " ") %>%                      # colapsa de novo
      str_trim() %>%
      stri_trans_general("Latin-ASCII") %>%                 # remove acentos

      # ---- normalizações “jurídicas”/abreviações comuns ----
      str_replace_all("\\bS\\s*\\.\\s*A\\s*\\.?\\b", "SA") %>%   # S/A, S.A, S A, S.A.
      str_replace_all("\\bL\\s*T\\s*D\\s*A\\b", "LTDA") %>%      # L T D A -> LTDA
      str_replace_all("\\bL\\s*T\\s*D\\s*A\\.?\\b", "LTDA") %>%  # LTDA. -> LTDA
      str_replace_all("\\bE\\b", "E") %>%                        # mantém E isolado
      str_replace_all("\\s+", " ") %>%
      str_trim()
  ) %>%
  
  # Adicionar caracteres ao CNPJ:
  mutate(
    cnpj_empresa = str_trim(cnpj_empresa),
    cnpj_empresa = na_if(cnpj_empresa, ""),
    cnpj_empresa = case_when(
      
      # já tem mais de 14 caracteres -> assume que já está formatado/certo, mantém
      !is.na(cnpj_empresa) & str_length(cnpj_empresa) > 14 ~ cnpj_empresa,

      # tem exatamente 14 dígitos (só número) -> formata como CNPJ
      !is.na(cnpj_empresa) & str_detect(cnpj_empresa, "^\\d{14}$") ~ str_replace(
        cnpj_empresa,
        "^(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})$",
        "\\1.\\2.\\3/\\4-\\5"
      ),

      # tem menos de 14 caracteres -> NA
      !is.na(cnpj_empresa) & str_length(cnpj_empresa) < 14 ~ NA_character_,

      # exatamente 14 mas contém letras/símbolos -> mantém (você pediu só formatar os 14 dígitos puros)
      TRUE ~ cnpj_empresa
    )
  ) %>%
  
  # Preencher missing de CNPJ (1/2):
  mutate(
    primeira_palavra = empresa %>%
      str_trim() %>%
      str_extract("^\\S+") %>%
      str_to_upper()
  ) %>%
  group_by(primeira_palavra) %>%
  mutate(
    cnpj_empresa = coalesce(cnpj_empresa, first(na.omit(cnpj_empresa)))
  ) %>%
  ungroup() %>%
  select(-primeira_palavra) %>%
  
  # Preencher missing de CNPJ (2/2):
  group_by(empresa) %>%
  mutate(
    cnpj_empresa = coalesce(cnpj_empresa, first(na.omit(cnpj_empresa)))
  ) %>%
  ungroup() %>%

  # Preencher missing de CATMAT:
  group_by(medicamento, apresentacao) %>%
  mutate(catmat = coalesce(catmat, first(na.omit(catmat)))) %>%
  ungroup() %>%
  
  # Selecionar variáveis:
  select(data, ano, mes, contrato, modalidade, uf, catmat, medicamento, apresentacao,
         quantidade, valor_unitario, valor_total, cnpj_empresa, empresa) %>%
  
  # Ajuste manual:
  mutate(medicamento = if_else(medicamento == "Mesalazina De", "Mesalazina", medicamento)) %>%
  filter(medicamento != "G") %>%
  mutate(cnpj_empresa = if_else(str_detect(cnpj_empresa, "^3MED"), NA_character_, cnpj_empresa),
         empresa      = if_else(empresa == "-", NA_character_, empresa))



#--- ADAPTAR PARA DASHBOARD ------------------------
dashboard_lai = lai_clean %>%
  select(Data = data, Estado = uf, CNPJ = cnpj_empresa, Fornecedor = empresa, CATMAT = catmat, Medicamento = medicamento, Apresentacao = apresentacao, Quantidade = quantidade, Valor = valor_total) %>%
  rowwise() %>%
  mutate(Apresentacao = padronizar_apresentacao(Apresentacao)) %>%
  ungroup() %>%
  mutate(Apresentacao = ifelse(is.na(Apresentacao), "Não Especificado", Apresentacao)) %>%
  mutate(Base = "LAI")



#--- EXPORTAR BASES ------------------------
write_xlsx(lai_clean, "output/LAI/LAI.xlsx")


