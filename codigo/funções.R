
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

#---  DICIONÁRIOS --------------------------
dic_var = c(data_da_aquisicao_publicacao = "data",
            assinatura_da_afm_aps        = "data",
            data_de_recebimento          = "data",
            dt_entrada                   = "data",
            data_emissao                 = "data",
            data_da_aquisicao            = "data",
            data_empenho                 = "data",

            identificacao_do_contrato_de_aquisicao = "contrato",
            afm                                    = "contrato",
            processo                               = "contrato",
            pe_e_arp                               = "contrato",
            numero_da_arp                          = "contrato",
            contrato_da_aquisicao                  = "contrato",

            modalidade_do_processo_aquisitivo      = "modalidade",
            forma_de_contratacao                   = "modalidade",
            modalidade_da_aquisicao                = "modalidade",
            modalidade_de_compra                   = "modalidade",
            modalidade_aquisicao                   = "modalidade",
            modalidade_licitacao                   = "modalidade",
            modalidade_processo_aquisitivo         = "modalidade",
            tipo_licitacao                         = "modalidade",

            fornecedor = "empresa",
            nome_empresa = "empresa",

            cnpj = "cnpj_empresa",
            
            medicamento                      = "medicamento",
            item                             = "medicamento",
            item_material                    = "medicamento",
            produto                          = "medicamento",
            nome_simplificado                = "medicamento",
            produto_especificacoes           = "medicamento",
            denominacao_comum_brasileira_dcb = "medicamento",

            dosagem = "apresentacao",
            concentracao = "apresentacao",
            forma_farmaceutica = "apresentacao",

            quantidade_adquirida_por_unidade_farmaceutica_quantidade_por_ata = "quantidade",
            qtd_contratada                                                   = "quantidade",
            quant                                                            = "quantidade",
            qtde                                                             = "quantidade",
            quantidade_adquirida_por_unidade_farmaceutica                    = "quantidade",
            qtd_adquirida                                                    = "quantidade",
            quantidade_adquirida                                             = "quantidade",
            quantidade_item                                                  = "quantidade",

            preco_por_unidade_farmaceutica_praticado_nas_aquisicoes = "valor_unitario",
            preco_por_unidade_farmaceutica_r = "valor_unitario",
            preco_registrado = "valor_unitario",
            preco_unitario = "valor_unitario",
            vlr_unitario = "valor_unitario",

            valor         = "valor_total",
            valor_r       = "valor_total",
            vlr_total     = "valor_total",
            preco_total_r = "valor_total",
            valor_total   = "valor_total",
            preco_total   = "valor_total")

dic_class = list(data          = "Date",
                 contrato      = "character",
                 modalidade    = "character",
                 empresa       = "character",
                 cnpj_empresa  = "character",
                 medicamento   = "character",
                 apresentacao  = "character",
                 quantidade    = "numeric",
                 valor_unitario= "numeric",
                 valor_total   = "numeric")



#--- FUNÇÕES -------------------------------

#--- PADRONIZAR TEXTO DA APRESENTAÇÃO ---
padronizar_apresentacao <- function(texto) {
  
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


#--- PADRONIZAR VARIÁVEIS ---

# Nomes:
padronizar_var = function(df, dict) {
  nomes = names(df)

  novos_nomes = ifelse(
    nomes %in% names(dict),
    dict[nomes],
    nomes
  )

  names(df) = make.unique(novos_nomes)
  df
}

# Classes:
padronizar_class = function(x, classe) {
  if (inherits(x, classe)) return(x)

  switch(
    classe,
    Date      = as.Date(x, origin = "1899-12-30"),
    numeric   = as.numeric(gsub(",", ".", x)),
    character = as.character(x),
    logical   = as.logical(x),
    x
  )
}


