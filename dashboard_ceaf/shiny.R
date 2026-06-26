

######################################################################################################
# MINISTÉRIO DA SAÚDE (MS)                                                                           #
# SECRETARIA EXECUTIVA (SE)                                                                          #
# DEPARTAMENTO DE ECONOMIA E INVESTIMENTOS EM SAÚDE (DESID)                                          #
# COORDENAÇÃO DE GESTÃO DE DADOS ESTATÍSTICOS EM SAÚDE (COEST)                                       #
#----------------------------------------------------------------------------------------------------#
# DESCRIÇÃO DA ATIVIDADE:                                                                            #
#                                                                                                    #
# Elaboração de dashboard para monitoramento das aquisições do CEAF 1B.                              #
#                                                                                                    #
#----------------------------------------------------------------------------------------------------#
# Autores: Theo da Fonseca Torres e Felipe Duplat Luz                                                #
# Data: 09/12/2025                                                                                   #
# Versão: 2.0                                                                                        #
#----------------------------------------------------------------------------------------------------#

#--- CARREGAR OS PACOTES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               rsconnect)



#--- SUBIR NOVO DASHBOARD ------------------------

#--- DEPLOY ---
deployApp(
  appDir = "Github/CEAF-1B/dashboard_ceaf",
  forceUpdate = TRUE
)


