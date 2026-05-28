
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

  setwd(file.path("C:/Users", Sys.info()[["user"]], "OneDrive - Ministério da Saúde/- Atividades/DESID/Demandas/2025-XX-XX- CEAF 1B"))

}


#--- CARREGAR OS PACOTES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               geobr,
               rmapshaper,
               sf)



#---  BAIXAR MAPA --------------------------

#--- VERSÃO ORIGINAL ---
mapa_original <- read_state(year = 2020, showProgress = TRUE)


#--- VERSÃO REDUX ---
mapa_leve <- sf::st_cast(ms_simplify(mapa_original, keep = 0.02, keep_shapes = TRUE), "MULTIPOLYGON")



#---  EXPORTAR MAPA --------------------------
saveRDS(mapa_leve, "dashboard/df/mapa_brasil_leve.rds")


