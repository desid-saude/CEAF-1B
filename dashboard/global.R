
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
options(scipen = 999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
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
               shinycssloaders)

#--- CONFIGURAÇÃO GLOBAL DOS SPINNERS ---
options(spinner.color = "#001F3F", spinner.type = 8) 



#--- INÍCIO DO DASHBOARD -------------------

#--- CARREGAR BASE DE DADOS ---
NOME_ARQUIVO <- "df/base_completa.csv"


#--- MELHORAR MAPA ---
if (file.exists("df/mapa_brasil_leve.rds")) {
  malha_brasil_global <- readRDS("df/mapa_brasil_leve.rds")
} else {
  
  # Fallback de segurança:
  tryCatch({
    raw <- geobr::read_state(year = 2020, showProgress = FALSE)
    malha_brasil_global <- rmapshaper::ms_simplify(raw, keep = 0.02)
  }, error = function(e) return(NULL))
}

DT_PTBR <- list(
  sEmptyTable = "Nenhum registro encontrado",
  sInfo = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
  sInfoEmpty = "Mostrando 0 até 0 de 0 registros",
  sInfoFiltered = "(Filtrados de _MAX_ registros)",
  sInfoPostFix = "",
  sInfoThousands = ".",
  sLengthMenu = "_MENU_ resultados por página",
  sLoadingRecords = "Carregando...",
  sProcessing = "Processando...",
  sZeroRecords = "Nenhum registro encontrado",
  sSearch = "Pesquisar",
  oPaginate = list(
    sNext = "Próximo",
    sPrevious = "Anterior",
    sFirst = "Primeiro",
    sLast = "Último"
  ),
  oAria = list(
    sSortAscending = ": Ordenar colunas de forma ascendente",
    sSortDescending = ": Ordenar colunas de forma descendente"
  )
)


#--- CONFIGURAÇÕES DO DASHBOARD ---------------------------

#--- GERAIS ---
options(warn = -1,
        scipen = 999,
        shiny.maxRequestSize = 30*1024^2)


#--- TEMA ---
tema_farma <- bs_theme(version = 5,
                       bootswatch = "cosmo", 
                       base_font = font_google("Inter")) %>% 
              bs_add_rules(".navbar { background-color: #001F3F !important; }
                            .navbar-brand { color: #FFFFFF !important; }
                            .btn-intro {background-color: #001F3F;color: white;border: none;}
                            .btn-intro:hover {background-color: #001F3F;color: white;}
                            
                            .navbar-nav .nav-link {
                            margin-left: 20px;  /* Espaço à esquerda de cada botão */
                            margin-right: 20px; /* Espaço à direita de cada botão */
                            font-size: 1.05rem; /* Opcional: Aumenta levemente a letra */
                            }
                            
                            /* --- ESTILO DO MENU SUSPENSO --- */
                            .dropdown-menu {
                              border-radius: 16px !important;  /* Bordas bem arredondadas */
                              border: none !important;         /* Remove borda padrão feia */
                              box-shadow: 0 10px 30px rgba(0,0,0,0.15); /* Sombra elegante */
                              padding: 10px;
                              margin-top: 10px !important;     /* Espaço entre o topo e o menu */
                            }
                            
                            .dropdown-item {
                              border-radius: 10px;
                              padding: 10px 15px;
                              font-weight: 500;
                              color: #FFFFFF;
                              transition: all 0.2s ease-in-out;
                            }
                            
                            .dropdown-item:hover {
                              background-color: #001f3f !important; /* Fundo cinza/azulado suave ao passar o mouse */
                              color: #FFFFFF !important;            /* Texto na cor da marca */
                              transform: translateX(5px); /* Leve movimento para a direita */
                            }
                            
                           .bslib-value-box {
                            border-radius: 30px !important; /* Aumentei para ficar bem visível */
                            overflow: hidden !important;    /* OBRIGATÓRIO: Corta a cor que vaza */
                            box-shadow: 0 4px 10px rgba(0,0,0,0.1); /* Sombra mais elegante */
                            }
                          .card {
                            border-radius: 15px;
                            border: none; /* Remove a borda cinza padrão para um look mais limpo */
                            box-shadow: 0 2px 5px rgba(0,0,0,0.05);
                          }
                          
 ")


#--- FUNÇÕES DE UI (Intro e Dashboard) ---
ui_intro <- function() {
  div(
    class = "d-flex flex-column justify-content-center align-items-center vh-100", 
    style = "background-color: #f8f9fa;",
    
    card(
      width = 600,
      class = "shadow-lg p-4 text-center mb-5",
      style = "max-width: 90%;", 
      
      div(bs_icon("bar-chart-fill", size = "4rem", class = "text-primary mb-3")),
      
      h2(
          HTML(
            "<span style='font-weight: 400;'>Monitoramento do Grupo 1B</span><br>
            <span style='font-weight: 700;'>Componente Especializado da Assistência Farmacêutica</span>"
          ),
          class = "mb-4",
          style = "color: #001f3f;"
        ),
      
      p("Este painel apresenta um levantamento detalhado das compras públicas de medicamentos pertencentes ao Grupo 1B do Componente Especializado da Assistência Farmacêutica (CEAF 1B). A iniciativa, desenvolvida pelo Departamento de Economia e Investimentos em Saúde (DESID/SE/MS), surge diante da recente elevação observada no valor total dessas aquisições, reforçando a necessidade de um monitoramento contínuo e transparente.", class = "text-muted mb-4", style = "text-align: justify"),

      p("A partir da consolidação e análise dos dados disponibilizados pelos governos estaduais e coletados no Portal Nacional de Contratações Públicas (PNCP), esse painel permite acompanhar tendências, identificar padrões de compra por unidade federativa e apoiar tomadas de decisão estratégica na gestão do CEAF 1B. O objetivo é oferecer uma ferramenta acessível e objetiva para que gestores e analistas possam compreender melhor a dinâmica dessas aquisições e antecipar possíveis movimentos que impactem a programação e o financiamento do componente especializado da assistência farmacêutica.", class = "text-muted mb-4", style = "text-align: justify"),

      hr(),
      
      div(style = "display: flex;
                   justify-content: center;
                   gap: 20px;",

          actionButton("btn_metod", "Metodologia",
                       class = "btn btn-outline-secondary rounded-pill w-25 mb-3",
                       icon = icon("book")),

          actionButton("btn_entrar", "Dashboard",
                       class = "btn btn-outline-secondary rounded-pill w-25 mb-3",
                       icon = icon("arrow-right-from-bracket")),

          actionButton("btn_contato", "Contato",
                       class = "btn btn-outline-secondary rounded-pill w-25 mb-3",
                       icon = icon("address-book"))
        )
      ),
    
    tags$footer(
  class = "fixed-bottom py-3",
  style = "background-color: #001f3f; color: white; text-align: center; font-size: 0.9rem;",

  div(
    tags$img(
      src = "regua.svg",
      style = "height: 75px; margin-bottom: 2px;"
    )
  )
)

  )
}


ui_dashboard <- function() {
  page_navbar(
    title = "",
    window_title = "Monitoramento do CEAF 1B",
    id = "nav_dashboard",
    bg = "#001f3f",
    inverse = TRUE,
    
    # --- CAMADAS DE OVERLAY (Intro, Contato, Metodologia) ---
    header = tagList(
      useShinyjs(), 
      tags$style(HTML("
        /* Forçar redução do TÍTULO do Card (Ex: 'Gasto Total') */
        .bslib-value-box .value-box-title,
        .bslib-value-box .card-title {
            font-size: 0.70rem !important; /* Tamanho do texto */
            text-transform: uppercase;     /* Opcional: Deixa em caixa alta */
            letter-spacing: 0.5px;
            margin-bottom: 2px !important;
            opacity: 0.9;
        }

        /* Forçar redução do VALOR do Card (Ex: 'R$ 1.500,00') */
        .bslib-value-box .value-box-value, 
        .bslib-value-box h2, 
        .bslib-value-box .h2 {
            font-size: 1.2rem !important; /* Reduzi bastante (o padrão é ~2.5rem) */
            font-weight: 700 !important;
        }
        
        /* Ajustar o tamanho do ÍCONE para ficar proporcional */
        .bslib-value-box .bsicons, 
        .bslib-value-box svg {
            width: 2.5rem !important;
            height: 2.5rem !important;
        }
        
        /* Ajustar o padding interno para o card ficar mais compacto */
        .bslib-value-box .card-body {
            padding: 1rem !important; 
        }
        /* 1. Força o container do spinner a não ter rolagem e herdar altura correta */
          .shiny-spinner-output-container {
              overflow: hidden !important;
              height: 100% !important;
          }
          
          /* 3. Ajuste específico para Plotly dentro de Spinners */
          /* Remove a margem extra que o Plotly às vezes adiciona, causando rolagem */
          .js-plotly-plot {
              margin-bottom: 0 !important;
          }
      ")),
      div(id = "page_intro", 
          style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 9999; background-color: #f8f9fa; overflow-y: auto;",
          ui_intro()
      ),
      hidden(div(id = "page_contato", 
                 style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 9999; background-color: white; overflow-y: auto;",
                 ui_contato()
      )),
      hidden(div(id = "page_metodologia", 
                 style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 9999; background-color: white; overflow-y: auto;",
                 ui_metodologia()
      ))
    ),
    
    nav_spacer(),
    
    # --- GRUPO 1: ANÁLISE DE GASTOS ---
    nav_menu(
      title = "Análise de Gastos",
      icon = bs_icon("graph-up-arrow"),
      
      # Item 1.1: Visão Geral
      nav_panel(
        title = "Visão Geral",
        icon = bs_icon("speedometer2"),
        
        page_sidebar(
          class = "p-0",
          sidebar = sidebar(
            width = 300,
            open = "desktop",
            title = "Parâmetros",
            tags$style(HTML(".sidebar .shiny-input-container { margin-bottom: 30px !important; }")),
            uiOutput("filtros_ui"),
            div(class = "mt-auto"), 
            hr(), 
            div(
              style = "text-align: center; font-size: 0.8rem; color: #001f3f;",
              bs_icon("building-fill-add", size = "1.5rem", class = "mb-2"),
              br(),
              span(style = "font-size: 0.7rem;", "Versão 2.0"),
              br(), br(),
              strong("Coordenação de Gestão de Dados Estatísticos em Saúde"),
              br(),
              "(COEST/DESID/SE/MS)",
              br(), br(),
              strong("Coordenação de Estudos ", br(), "Econômicos em Saúde"),
              br(),
              "(COES/CGAPS/DESID/SE/MS)"
            )
          ),
          div(class = "mt-2"),
          div(class = "px-3 py-1", 
              uiOutput("kpi_cards")
          ),
          navset_card_underline(
            title = "",
            nav_panel("Visão Geral",
                      layout_columns(
                        col_widths = c(12, 12),
                        card(card_header("Dados agrupados"), DTOutput("tabela_agregada")),
                        card(card_header("Top 5 - Maior Valor Gasto"), plotlyOutput("grafico_top5", height = "300px"))
                      )
            ),
            nav_panel("Evolução Acumulada",
                      card(full_screen = TRUE,
                           card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                                       div(bsicons::bs_icon("graph-up-arrow", class = "me-2"), "Crescimento do Gasto"),
                                       div(class = "d-flex align-items-center",
                                           span("Visualizar:", style = "font-size: 0.9rem; margin-right: 10px; font-weight: normal;"),
                                           selectInput("periodo_acumulado", label = NULL, choices = c("Todo o Período" = 0, "Últimos 6 Meses" = 6, "Últimos 12 Meses" = 12, "Últimos 18 Meses" = 18,"Últimos 24 Meses" = 24), selected = 0, width = "170px"))),
                           plotlyOutput("grafico_acumulado", height = "400px"))
            ),
            nav_panel("Comparativo Periódico",
                      card(card_header("Consolidado Trimestral"), withSpinner(plotlyOutput("grafico_trimestral", height = "300px"), size = 0.5), fill = FALSE),
                      
                      # Layout responsivo (empilha em mobile, divide em desktop):
                      layout_columns(
                        col_widths = breakpoints(sm = 12, md = 6),
                        card(card_header("Consolidado Mensal"), withSpinner(plotlyOutput("grafico_mensal", height = "300px"), size = 0.5), fill = FALSE),
                        card(card_header("Consolidado Anual"), withSpinner(plotlyOutput("grafico_anual", height = "300px"), size = 0.5), fill = FALSE)
                      )
            ),
            nav_panel("Classificação ABC (Pareto)",
                      card(full_screen = TRUE,
                           card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2", "Classificação ABC",
                                       div(class = "d-flex align-items-center gap-2",
                                           actionButton("btn_ant", label = NULL, icon = icon("chevron-left"), class = "btn-sm btn-outline-secondary"),
                                           span(uiOutput("texto_paginacao", inline = TRUE), style = "font-size: 0.9rem; font-weight: bold; min-width: 100px; text-align: center;"),
                                           actionButton("btn_prox", label = NULL, icon = icon("chevron-right"), class = "btn-sm btn-outline-secondary"))),
                           withSpinner(plotlyOutput("grafico_abc", height = "600px"), size = 0.5),
                           card_footer(div(class = "d-flex justify-content-center align-items-center gap-3 flex-wrap", 
                                           strong("Legenda:"), 
                                           span("■ Classe A (<80%)", style = "color:#dc3545"), 
                                           span("■ Classe B (<95%)", style = "color:#ffc107"), 
                                           span("■ Classe C (<100%)", style = "color:#198754"))), fill = FALSE)
            ),
            nav_panel("Base Completa",
                      tags$head(
                        tags$style(HTML("
                                        /* Botão Baixar arredondado */
                                        .download-dropdown .btn {
                                          border-radius: 999px;
                                          padding: 6px 16px;
                                          font-size: 14px;
                                        }
                                          
                                        /* Dropdown exclusivo do download */
                                        .download-dropdown .dropdown-menu {
                                          background-color: #ffffff;
                                          border: 1px solid #ced4da;
                                          border-radius: 12px;
                                          box-shadow: 0 6px 18px rgba(0,0,0,0.15);
                                          padding: 6px 0;
                                        }

                                        .download-dropdown .dropdown-item {
                                          color: #495057 !important;
                                          font-size: 14px;
                                          padding: 8px 16px;
                                          opacity: 1;
                                        }

                                        .download-dropdown .dropdown-item:hover {
                                          background-color: #f1f3f5;
                                          color: #212529 !important;
                                        }

                                        /* Hover dos itens do dropdown */
                                        .download-dropdown .dropdown-item:hover,
                                        .download-dropdown .dropdown-item:focus {
                                          background-color: #0d6efd; /* azul Bootstrap */
                                          color: #ffffff !important;
                                        }

                                        /* Ajustes do dropdown de download */
                                        .download-dropdown .dropdown-menu {
                                          min-width: 140px;
                                          text-align: center;
                                        }

                                        .download-dropdown .dropdown-item {
                                          text-align: center;
                                          padding: 8px 12px;
                                        }
                                      "))
                      ),
                      card(card_header(class = "d-flex justify-content-between align-items-center",
                                        span("Visualização dos Dados", class = "fw-bold"),
                                        div(class = "d-flex align-items-center gap-2",
                                            div(
                                                class = "dropdown download-dropdown",
                                                tags$button(
                                                  class = "btn btn-sm btn-light dropdown-toggle",
                                                  type = "button",
                                                  `data-bs-toggle` = "dropdown",
                                                  `aria-expanded` = "false",
                                                  icon("download"),
                                                  " Baixar"
                                                ),
                                                tags$ul(
                                                  class = "dropdown-menu dropdown-menu-end",
                                                  tags$li(downloadButton("download_xlsx", "Excel (.xlsx)", class = "dropdown-item")),
                                                  tags$li(downloadButton("download_csv",  "CSV (.csv)",    class = "dropdown-item"))
                                            )
                                          )
                                        )
                                      ),
                           DTOutput("tabela_bruta"))
            )
          )
        )
      ),
      
      # Item 1.2: Previsão
      nav_panel(
        title = "Previsão de Gastos",
        icon = bs_icon("magic"),
        
        page_fillable(
          padding = 20,
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
            card(
              card_header("Parâmetros"),
              selectizeInput("sel_base_forecast", "Selecione a Fonte de Dados:", choices = c("LAI", "PNCP", "BPS"), selected = "LAI", options = list(placeholder = "Selecione a fonte de dados")),
              selectizeInput("sel_med_forecast",  "Selecione o Medicamento:", choices = NULL, options = list(placeholder = "Vazio")),
              sliderInput("horizonte_meses", "Selecione o Tempo de Projeção:", min = 3, max = 12, value = 6, step = 1),
              uiOutput("card_kpi_previsao")
            ),
            card(
              card_header("Projeção dos Gastos"),
              withSpinner(plotlyOutput("grafico_forecast", height = "520px"), size = 0.5), fill = FALSE
            )
          ),
          card(
            card_header("Detalhamento dos Valores Projetados"),
            DTOutput("tabela_forecast")
          )
        )
      ),
    ),
    
    # --- GRUPO 2: ANÁLISE DE PREÇOS ---
    nav_menu(
      title = "Análise de Preços",
      icon = bs_icon("tags-fill"),
      
      # Item 2.1: Painel Unificado (Variação + Mapa)
      nav_panel(
        title = "Evolução e Distribuição",
        icon = bs_icon("globe-americas"),
        
        page_fillable(
          padding = 20,
          
          layout_columns(
            # Layout responsivo
            col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
            
            # --- ESQUERDA ---
            tagList(
              layout_columns(
                col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                card(
                  card_header("Parâmetros"),
                  selectizeInput("sel_med_base", "Selecione a Fonte de Dados:", choices = c("LAI", "PNCP", "BPS"), selected = "LAI", options = list(placeholder = "Selecione a fonte de dados")),
                  selectizeInput("sel_med_preco", "Selecione o Medicamento:", choices = NULL, options = list(placeholder = "Vazio")),
                  hr(),
                  div(
                    class = "alert alert-primary p-3 text-center mb-0",
                    style = "border-radius: 25px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); border: none;",
                    span("Média dos Preços:", style = "font-size: 1.0rem; opacity: 0.9;"), br(),
                    strong(textOutput("kpi_media_preco", inline = TRUE), style = "font-size: 2.0rem; line-height: 1.2;")
                  )
                ),
                card(
                  card_header("Ranking por Estado (UF)"),
                  withSpinner(plotlyOutput("grafico_ranking_preco", height = "100%"), size = 0.5), 
                  fill = FALSE,
                  style = "min-height: 465px;"
                )
              ),
              card(
                card_header("Evolução do Preço Médio (R$)"),
                withSpinner(plotlyOutput("grafico_dist_preco", height = "350px"), size = 0.5), fill = FALSE
              )
            ),
            
            # --- DIREITA (Mapa) ---
            card(
              card_header("Distribuição Espacial"),
              style = "min-height: 500px;", 
              withSpinner(plotlyOutput("mapa_brasil_preco", height = "100%"))
            )
          )
        )
      ),
      
      # Item 2.2: Economia Potencial
      nav_panel(
        title = "Economia Potencial",
        icon = bs_icon("piggy-bank-fill"),
        
        page_fillable(
          padding = 20,
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
            card(
              card_header("Parâmetros"),
              
              # Base:
              selectizeInput("sel_base_eco", "Selecione a Fonte de Dados:", choices = c("LAI", "PNCP", "BPS"), selected = "LAI"),

              # Medicamento:
              selectizeInput("sel_med_eco", "Selecione o Medicamento:", choices = NULL, options = list(placeholder = "Vazio")),
              
              # Ano:
              selectInput("sel_ano_eco", "Selecione o Ano:", choices = c("Todos"), selected = "Todos"),
              
              hr(),
              value_box(title = "Economia Estimada", value = uiOutput("kpi_total_economia"), showcase = bs_icon("currency-dollar", size = "0.75em"), bg = "#198754", fg = "white"),
              uiOutput("info_benchmark_detalhe")
            ),
            card(
              card_header("Economia Potencial por Estado (R$)"),
              plotlyOutput("grafico_economia", height = "550px")
            )
          ),
          navset_card_tab(
            nav_panel(
              "Detalhamento do Benchmark",
              DTOutput("tabela_economia")
            ),
            nav_panel(
              "Ranking de Economia Potencial",
              DTOutput("tabela_ranking_eco")
            )
          )
        )
      )
    ),

    # --- GRUPO 3: FORNECEDORES ---
        nav_menu(
      title = "Análise de Fornecedores",
      icon  = bs_icon("people-fill"),

      # =========================
      #         Perfil
      # =========================
      nav_panel(
        title = "Perfil",
        icon  = bs_icon("person-square"),

        page_sidebar(
          padding = 20,

          sidebar = sidebar(
            width = 320,
            open = "desktop",
            title = "Parâmetros",

            selectizeInput("sel_base_forn", "Selecione a Fonte de Dados:",
                          choices = c("LAI", "PNCP", "BPS"), selected = "LAI"),

            selectizeInput("sel_ano_forn", "Selecione o Ano:",
                          choices = NULL),

            selectizeInput("sel_uf_forn", "Selecione o Estado (UF):",
                          choices = NULL, multiple = TRUE,
                          options = list(placeholder = "Todos", plugins = list("remove_button"))),

            selectizeInput("sel_med_forn", "Selecione o Medicamento:",
                          choices = NULL, options = list(placeholder = "Vazio"))
          ),

          # 4 quadrantes
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12, 12), lg = c(6, 6, 12)),

            card(
              card_header("Maiores Fornecedores por Valor (R$)"),
              plotlyOutput("grafico_fornecedor_valor", height = "420px")
            ),

            card(
              card_header("Maiores Fornecedores por Quantidade"),
              plotlyOutput("grafico_fornecedor_qtd", height = "420px")
            ),

            card(
              card_header("Medidas de Preços"),
              DTOutput("tabela_medidas_preco_dropdown")
            )
          )
        )
      ),

      # =========================
      #       Concentração
      # =========================
      nav_panel(
        title = "Concentração de Mercado",
        icon  = bs_icon("stack"),

        page_sidebar(
          padding = 20,

          sidebar = sidebar(
            width = 320,
            open = "desktop",
            title = "Parâmetros",

            selectizeInput("sel_base_conc", "Selecione a Fonte de Dados:",
                          choices = c("LAI", "PNCP", "BPS"), selected = "LAI"),

            selectizeInput("sel_ano_conc", "Selecione o Ano:",
                          choices = NULL, options = list(placeholder = "Vazio")),

            selectizeInput("sel_uf_conc", "Selecione o Estado (UF):",
                          choices = NULL, multiple = TRUE,
                          options = list(placeholder = "Todos", plugins = list("remove_button"))),

            selectizeInput("sel_med_conc", "Selecione o Medicamento:",
                          choices = NULL, options = list(placeholder = "Todos"))
          ),

          # 4 quadrantes
          layout_columns(
            col_widths = breakpoints(
              sm = c(12, 12, 12),
              lg = c(6, 6, 12)
            ),

            card(
              card_header("Curva de Lorenz"),
              plotlyOutput("grafico_curva_lorenz", height = "420px")
            ),

            card(
              card_header("Indicadores de Concentração de Mercado"),
              DTOutput("grafico_indicadores_conc")
            ),

            card(
              card_header("Market-Share"),
              plotlyOutput("grafico_market_share", height = "420px")
            )
          )
        )
      ),
    ),

    # --- GRUPO 4: COMPARATIVO ENTRE ESTADOS ---
    nav_menu(
      title = "Comparativo entre Estados",
      icon = bs_icon("arrow-left-right"),

      nav_panel(
        title = "Gastos Estaduais",
        icon = bs_icon("graph-up-arrow"),
        
        page_fillable(
          padding = 20,
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
            
            # Painel Lateral de Filtros Específicos
            card(
              card_header("Parâmetros"),
              selectizeInput("comp_base", "Selecione a Fonte de Dados:", choices = NULL),
              selectizeInput("comp_ano", "Selecione o Ano:", choices = NULL),
              selectizeInput("comp_ufs", "Selecione os Estados (UFs):", 
                             choices = NULL, 
                             multiple = TRUE,
                             options = list(
                             placeholder = "Vazio", 
                             plugins = list("remove_button"),
                             maxItems = 10 # limita a 10 para não travar o gráfico visualmente
                )
              ),
              selectizeInput("comp_med", "Selecione o Medicamento:", choices = NULL, selected = "Todos", options = list(placeholder = "Todos")),
            ),
            
            # Área Visual Principal
            layout_columns(
              col_widths = 12,
              
              # Gráfico de Evolução (Linha do Tempo)
              card(
                card_header("Evolução Mensal do Gasto (R$)"),
                plotlyOutput("comp_evolucao_mensal", height = "350px")
              ),
              
              layout_columns(
                col_widths = c(6, 6), # Divide em duas colunas iguais
                
                # Gráfico 1: Gasto Total (Existente)
                card(
                  card_header("Gasto Total no Ano (R$)"),
                  plotlyOutput("comp_barras_total", height = "300px")
                ),
                
                # Gráfico 2: Gasto Per Capita
                card(
                  card_header("Gasto Total per Capita (R$)"),
                  plotlyOutput("comp_barras_percapita", height = "300px")
                )
              )
            )
          )
        )
      ),

      nav_panel(
        title = "Repasses Federais",
        icon = bs_icon("bank"),

        page_fillable(
          padding = 20,
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),

            # --- PARÂMETROS ---
            card(
              card_header("Parâmetros"),
              selectizeInput("rep_ano", "Selecione o Ano:", choices = NULL, options = list(placeholder = "Todos")),
              selectizeInput(
                "rep_ufs", "Selecione os Estados (UFs):",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Todos", plugins = list("remove_button"))
              ),
              selectizeInput("rep_med", "Selecione o Medicamento:", choices = NULL, options = list(placeholder = "Todos")),
              selectizeInput("rep_apres", "Selecione a Apresentação:", choices = NULL, options = list(placeholder = "Todos"))
            ),

            # --- VISUALIZAÇÕES ---
            tagList(
              card(
                card_header("Evolução Anual do Repasse (R$)"),
                withSpinner(plotlyOutput("rep_serie", height = "320px"), size = 0.5),
                fill = FALSE
              ),

              layout_columns(
                col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                    div("Repasses por Estado (UF)"),
                    selectInput(
                      "rep_viz_tipo", label = NULL,
                      choices = c("Ranking" = "ranking", "Mapa" = "mapa"),
                      selected = "ranking",
                      width = "120px"
                    )
                  ),
                  withSpinner(plotlyOutput("rep_viz", height = "520px"), size = 0.5),
                  fill = FALSE
                ),

                # (2) Repasse / Gasto por UF
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                    div("Repasse x Gasto por Estado (UF)"),
                    selectInput(
                      "rep_gasto_base", label = NULL,
                      choices  = c("LAI" = "LAI", "PNCP" = "PNCP", "BPS" = "BPS"),
                      selected = "LAI",
                      width    = "120px"
                    )
                  ),
                  withSpinner(plotlyOutput("rep_gasto_repasse", height = "520px"), size = 0.5),
                  fill = FALSE
                )
              )
            )
          )
        )
      )
    ),
    
    nav_spacer(), 
    
    nav_item(
      actionButton("btn_voltar_header", " Sair / Início", 
                   icon = icon("house"), 
                   class = "btn btn-outline-light btn-sm",
                   style = "border: 1px solid rgba(255,255,255,0.5);")
    )
  )
}


ui_contato <- function() {
  
  fluidPage(
    
    tags$head(
      tags$style(HTML("
        .contact-card {
          max-width: 650px;
          margin: 50px auto;
          background: #ffffff;
          border-radius: 16px;
          padding: 45px;
          box-shadow: 0px 4px 18px rgba(0,0,0,0.10);
        }
 
        .contact-title {
          font-size: 2rem;
          font-weight: 700;
          text-align: center;
          margin-bottom: 30px;
          color: #001f3f;
        }
 
        .contact-section-title {
          font-size: 1.25rem;
          font-weight: 600;
          margin-top: 30px;
          margin-bottom: 12px;
          color: #001f3f;
        }
 
        .contact-text {
          font-size: 1rem;
          text-align: justify;
          color: #333;
          line-height: 1.55;
        }
 
        .contact-icon {
          color: #001f3f;
          margin-right: 8px;
        }
 
        .section-divider {
          border-bottom: 1px solid #e5e5e5;
          padding-bottom: 12px;
          margin-bottom: 15px;
        }
 
        .contact-footer {
          text-align: center;
          margin-top: 45px;
          margin-bottom: 20px;
          color: #6c757d;
          font-size: 0.9rem;
        }
 
      "))
    ),
    
    div(class = "contact-card",
        
        div(class = "contact-title", "Contato"),
        
        p(
          class = "contact-text",
          HTML(
            "Esta página reúne os principais canais de contato das equipes da COEST/DESID/SE/MS e COES/CGAPS/DESID/SE/MS, responsáveis pelo desenvolvimento, manutenção e aprimoramento deste painel.<br><br>
            Em caso de dúvidas, sugestões ou identificação de eventuais inconsistências nos dados apresentados, colocamo-nos à disposição para receber seu retorno."
          )),
        
        hr(),
        
        div(class = "contact-section-title",
            icon("comment-dots", class = "contact-icon"), "Fale conosco"),
        
        p(
          class = "contact-text",
          HTML('
            <i class="fa fa-envelope" style="margin-right:6px;"></i>
            <a href="mailto:desid@saude.gov.br">desid@saude.gov.br</a><br>
 
            <i class="fa fa-phone" style="margin-right:6px;"></i>
            +55 61 3315-3172 | +55 61 3315-8957
          ')
        ),
        
        div(class = "contact-section-title",
            icon("people-group", class = "contact-icon"), "Equipe responsável"),
        
        div(
          style = "display: flex; gap: 40px;",
          
          # Coluna COEST:
          div(
            style = "flex: 1;",
            HTML(paste(
              c(
                "<div style='text-align: center; font-weight: 700; margin-bottom: 10px;'>COEST</div>",
                
                "<strong>Coordenadores:</strong><br>",
                "Sérgio Lucio Nunes da Silva<br>",
                "Wendell R. Oliveira da Silva<br><br>",
                
                "<strong>Analistas:</strong><br>",
                "Theo da Fonseca Torres<br>",
                "Felipe Duplat Luz"
              ),
              collapse = ""
            ))
          ),
          
          # Coluna COES:
          div(
            style = "flex: 1;",
            HTML(paste(
              c(
                "<div style='text-align: center; font-weight: 700; margin-bottom: 10px;'>COES</div>",
                
                "<strong>Coordenadores:</strong><br>",
                "Gustavo Laine Araújo de Oliveira<br><br><br>",
                
                "<strong>Analistas:</strong><br>",
                "Antônio Angelo Menezes Barreto<br>",
                "Diego de S. G. da Anunciação"
              ),
              collapse = ""
            ))
          )
        ),
        
        br(),
        
        hr(),
        
        div(
          style = "text-align:center;",
          actionButton("btn_voltar_contato", " Voltar ao Início",
                       icon = icon("house"),
                       class = "btn btn-outline-secondary rounded-pill",
                       style = "padding: 10px 24px; font-size: 1rem;")
        )
    ),
    
    div(class = "contact-footer",
        tags$img(
          src = "regua2.svg",
          style = "height: 85px; display:block; margin: 0 auto 10px; "
        )
    )
  )
}


ui_metodologia <- function() {
  
  fluidPage(
    
    tags$head(
      tags$style(HTML("

        .method-card {
          max-width: 850px;
          margin: 40px auto;
          background: #ffffff;
          border-radius: 16px;
          padding: 40px 50px;
          box-shadow: 0px 4px 18px rgba(0,0,0,0.08);
        }

        .method-title {
          font-size: 2rem;
          font-weight: 600;
          text-align: center;
          margin-bottom: 30px;
          color: #001f3f;
        }

        .method-section-title {
          font-size: 1.35rem;
          font-weight: 600;
          margin-top: 35px;
          margin-bottom: 12px;
          color: #001f3f;
        }

        .method-text {
          font-size: 1.05rem;
          line-height: 1.6;
          text-align: justify;
          color: #444;
        }

        .method-list {
          font-size: 1.05rem;
          color: #444;
          margin-left: 20px;
        }

        .method-footer {
          text-align: center;
          margin-top: 45px;
          font-size: 0.85rem;
          color: #6c757d;
        }
      "))
    ),
    
    
    div(class = "method-card",
        
        div(class = "method-title", "Metodologia"),
        
        p(class = "method-text",
          "Esta seção descreve, de forma detalhada, os procedimentos técnicos adotados para o tratamento, a organização, a construção e a atualização das informações apresentadas no painel. Seu objetivo é garantir a transparência metodológica e permitir que os usuários compreendam as decisões adotadas, bem como as limitações inerentes à natureza dos dados utilizados."),
        
        
        
        ## --- Seção 1: Fonte dos Dados ---
        div(class = "method-section-title", 
            icon("database", class = "contact-icon"), "Fontes de Dados"),
        
        p(class = "method-text",
          "Este painel foi construído e é atualizado a partir de informações das seguintes fontes:"
        ),

        tags$ul(
          class = "method-text",
          tags$li("Informações de aquisições de medicamentos fornecidas pelos governos estaduais por meio da Lei de Acesso à Informação (LAI) - Lei nº 12.527, de 18 de novembro de 2011); e"),
          tags$li("Informações coletadas do ", tags$a(href = "https://www.gov.br/pncp/pt-br", target = "_blank", "Portal Nacional de Contratações Públicas"), " (PNCP)."),
          tags$li("Informações coletadas do ", tags$a(href = "https://www.gov.br/saude/pt-br/acesso-a-informacao/banco-de-precos", target = "_blank", "Banco de Preços em Saúde"), " (BPS).")
        ),

        p(
          class = "method-text",
          "Embora ambas as fontes se refiram à compra pública, elas não são diretamente comparáveis, razão pela qual sua utilização deve ocorrer de forma separada."
        ),
        
        

        ## --- Seção 2: Processamento ---
        div(class = "method-section-title",
            icon("gears", class = "contact-icon"), "Processamento e Tratamento"),
        
        p(
          class = "method-text",
          "Após a coleta, realiza-se um processo automatizado de padronização dos nomes e das apresentações dos medicamentos, conforme as normas da ",
          tags$a(
            href = "https://www.gov.br/saude/pt-br/composicao/sectics/rename",
            target = "_blank",
            "RENAME"
          ),
          ", com o objetivo de reduzir inconsistências e eliminar duplicações de registros."
        ),
        
        p(
          class = "method-text",
          "As informações de preços foram deflacionadas para valores de dezembro de 2025, utilizando o ",
          tags$a(
            href = "https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html%22",
            target = "_blank",
            "Índice Nacional de Preços ao Consumidor Amplo"
          ),
          " (IPCA), a fim de garantir comparabilidade dos valores ao longo do tempo."
        ),

        

        ## --- Seção 3: Cálculos ---
        div(class = "method-section-title", 
            icon("chart-line", class = "contact-icon"), "Cálculos e Indicadores"),
        
        tags$ul(
          class = "method-text",
          tags$li(
            tags$b("Evolução acumulada dos medicamentos por UF:"),
            tags$br(),
            "Indicador que permite acompanhar, ao longo do tempo, a dinâmica dos gastos e das quantidades adquiridas por unidade da federação, evidenciando tendências e variações regionais."
          ),

          br(),

          tags$li(
            tags$b("Classificação ABC (Análise de Pareto):"),
            tags$br(),
            "Os medicamentos são ordenados conforme sua participação no valor total das compras, identificando os itens que concentram a maior parcela do gasto, auxiliando a priorização da análise e da gestão dos medicamentos."
          ),

          br(),

          tags$li(
            tags$b("Previsão de gastos por medicamento:"),
            tags$br(),
            "A previsão é realizada por meio de um modelo de séries temporais baseado no algoritmo Prophet, que decompõe a série em tendência e sazonalidade e gera projeções acompanhadas de intervalos de incerteza (cenários mínimo e máximo)."
          ),

          br(),

          tags$li(
            tags$b("Economia potencial:"),
            tags$br(),
            "Estima quanto cada unidade da federação poderia ter economizado caso tivesse adquirido cada medicamento pelo menor preço médio observado na amostra utilizada, funcionando como um indicador de eficiência relativa e de oportunidades de racionalização do gasto público num cenário sem assimetria de informação."
          ),
          
          br(),
          
          tags$li(
            tags$b("Curva de Lorenz"),
            tags$br(),
            "É uma representação gráfica utilizada para analisar a desigualdade na distribuição de uma variável dentro de uma população de agentes. No contexto deste dashboard, ela relaciona a porcentagem acumulada de empresas com a porcentagem acumulada do valor total do mercado. Quanto mais a curva se afasta da linha diagonal de 45 graus (que representa a perfeita igualdade), maior é o grau de concentração e desigualdade do setor analisado."
          ), 
          
          br(),
          
          tags$li(
            tags$b("CR4"),
            tags$br(),
            "Mensura o grau de domínio das quatro maiores empresas do setor sobre o mercado total. É calculado através do somatório das participações de mercado dos quatro principais agentes. Este índice fornece uma visão rápida sobre a estrutura de oligopólio: valores elevados indicam um mercado altamente concentrado no topo, onde poucas empresas detêm a maior parte da oferta ou faturamento, podendo influenciar diretamente a dinâmica de preços e competição."
          ),
          
          br(),
          
          tags$li(
            tags$b("CR8"),
            tags$br(),
            "Expande a análise para as oito maiores empresas do setor. Ele é obtido pela soma das participações de mercado dos oito principais agentes. A comparação entre o CR4 e o CR8 é fundamental para entender a dinâmica competitiva do mercado: se a diferença entre os dois indicadores for pequena, significa que o poder de mercado está quase inteiramente retido nos quatro primeiros colocados; se a diferença for significativa, indica a presença de competidores relevantes logo abaixo do topo, sugerindo uma estrutura de mercado mais gradativa."
          ),
          
          br(),
          
          tags$li(
            tags$b("Índice Herfindahl-Hirschman (IHH)"),
            tags$br(),
            "Medida abrangente de concentração de mercado, amplamente utilizada por órgãos de defesa da concorrência. considera todas as empresas do setor e é calculado somando-se os quadrados das participações de mercado de cada uma. Ao elevar as participações ao quadrado, o índice atribui um peso muito maior às empresas com grandes fatias de mercado. O resultado varia de próximo a zero (concorrência perfeita) a 10.000 (monopólio puro), permitindo identificar com precisão cenários de alta concentração que poderiam passar despercebidos por métricas lineares."
          )
        ),

        
        
        
        ## --- Seção 4: Atualização ---
        div(class = "method-section-title", 
            icon("clock-rotate-left", class = "contact-icon"), "Periodicidade de Atualização"),
        
        p(class = "method-text",
          "O painel será atualizado frequentemente com dados e informações colhidas por meio de Portal da Transparência, FTP ou APIs disponibilizados pelos governos estaduais, e quando estas fontes foram indisponíveis, os dados serão obtidos via Lei de Acesso à Informação, enquanto os dados do PNCP seguem sendo coletados de forma sistemática por meio de raspagem da base pública do próprio PNCP."

        ),        
        
        ## Botão Voltar
        br(),
        div(
          style = "text-align:center;",
          actionButton(
            "btn_voltar_metod", " Voltar ao Início",
            icon = icon("house"),
            class = "btn btn-outline-secondary mb-3 rounded-pill"
          )
        )
    ),
    
    
    ## Rodapé
    div(class = "method-footer",
        tags$img(
          src = "regua2.svg",
          style = "height: 85px; margin-bottom: 5px;"
        )
    )
  )
}



