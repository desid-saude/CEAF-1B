
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

###################################
#                                 #
#      SERVER DO DASHBOARD        #
#                                 #
###################################


#--- FUNÇÃO DO SERVER ---
function(input, output, session) {
  
  # --- CONTROLE DE NAVEGAÇÃO (VISIBILIDADE) ---
  
  # 1. Entrar no Dashboard
  observeEvent(input$btn_entrar, {
    shinyjs::hide("page_intro")
    shinyjs::hide("page_contato")
    shinyjs::hide("page_metodologia")
    
    bslib::nav_select(id = "nav_dashboard", selected = "Visão Geral")
  })
  
  # 2. Botão Voltar (Dentro do Dashboard)
  observeEvent(input$btn_voltar_header, {
    shinyjs::show("page_intro")
  })
  
  # 3. Botões da Intro para outras páginas
  observeEvent(input$btn_contato, {
    shinyjs::hide("page_intro") # Esconde intro
    shinyjs::show("page_contato") # Mostra contato
  })
  
  observeEvent(input$btn_metod, {
    shinyjs::hide("page_intro")
    shinyjs::show("page_metodologia")
  })
  
  observeEvent(input$btn_voltar_contato, {
    shinyjs::hide("page_contato")
    shinyjs::show("page_intro")
  })
  
  # Botão Voltar da página de METODOLOGIA
  observeEvent(input$btn_voltar_metod, {
    shinyjs::hide("page_metodologia")
    shinyjs::show("page_intro")
  })
  
  # 1. Leitura
  dados_raw <- reactive({
    
    if (!file.exists(NOME_ARQUIVO)) {
      showNotification(paste("Arquivo não encontrado:", NOME_ARQUIVO), type = "error", duration = NULL)
      return(NULL)
    }
    
    tryCatch({
      df <- read.csv(NOME_ARQUIVO, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
      
      if (ncol(df) < 2) {
        df <- read.csv(NOME_ARQUIVO, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
      }
      
      colunas_necessarias <- c("Medicamento", "CATMAT", "Apresentacao", "Data", "Valor", "Quantidade", "Estado", "Base", "Fornecedor", "CNPJ")
      if (!all(colunas_necessarias %in% names(df))) {
        showNotification("Colunas incorretas. Verifique se a coluna 'Estado' existe.", type = "error")
        return(NULL)
      }
      
      df <- df %>%
        mutate(
          Data = parse_date_time(Data, orders = c("dmy", "ymd", "mdy")),
          Data = as.Date(Data),
          Valor = as.numeric(gsub(",", ".", as.character(Valor))),
          Quantidade = as.numeric(gsub(",", ".", as.character(Quantidade))),
          Estado = as.character(Estado),
          Fornecedor = as.character(Fornecedor),
          CNPJ = as.character(CNPJ)
        ) %>%
        filter(!is.na(Data))
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("Erro ao ler dados:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # 2. Renderização Dinâmica dos Filtros
  output$filtros_ui <- renderUI({
    req(dados_raw())
    df <- dados_raw()
    
    # Opções ordenadas
    opcoes_uf <- sort(unique(df$Estado))
    opcoes_med <- sort(unique(df$Medicamento))
    opcoes_base <- sort(unique(df$Base))
    opcoes_catmat <- sort(unique(df$CATMAT))
    
    # Data mínima e máxima
    min_data <- min(df$Data, na.rm = TRUE)
    max_data <- max(df$Data, na.rm = TRUE)
    
    tagList(

      # Filtro de Data:
      div(class = "mb-4",
          dateRangeInput("filtro_data", "Selecione o Período:",
                         start = min_data, end = max_data,
                         min = min_data, max = max_data,
                         format = "dd/mm/yyyy", language = "pt-BR", separator = " até ")
      ),

      # Filtro de Dados:
      div(
        class = "mb-4",
        selectizeInput(
          "filtro_base",
          "Selecione a Fonte de Dados:",
          choices  = opcoes_base,
          selected = "LAI",
          multiple = FALSE,
          options  = list(plugins = list("remove_button"))
        )
      ),
      
      # Filtro de UF:
      div(class = "mb-4",
          selectizeInput("filtro_uf", "Selecione o Estado (UF):",
                         choices = opcoes_uf, multiple = TRUE,
                         options = list(placeholder = "Todos", plugins = list("remove_button")))
      ),
      
      # Filtro de Medicamento:
      div(class = "mb-4",
          selectizeInput("filtro_med", "Selecione o Medicamento:",
                         choices = opcoes_med, multiple = TRUE,
                         options = list(placeholder = "Todos", plugins = list("remove_button")))
      )
    )

  })
  
  # 3. Filtros
  dados_filtrados <- reactive({
    req(dados_raw())
    
    # Verificação de segurança para inicialização
    if (is.null(input$filtro_data)) return(NULL)
    if (any(is.na(input$filtro_data))) return(NULL)
    
    df <- dados_raw()
    
    # 1. Filtro de Data
    df <- df %>% filter(Data >= input$filtro_data[1] & Data <= input$filtro_data[2])
    
    # 2. Filtro de UF
    if (!is.null(input$filtro_uf)) {
      df <- df %>% filter(Estado %in% input$filtro_uf)
    }
    
    # 3. Filtro de Medicamento
    if (!is.null(input$filtro_med)) {
      df <- df %>% filter(Medicamento %in% input$filtro_med)
    }
    
    # 4. Filtro de Base:
    if (!is.null(input$filtro_base)) {
      df <- df %>% filter(Base %in% input$filtro_base)
    }

    # 5. Filtro de CATMAT:
    if (!is.null(input$filtro_catmat)) {
      df <- df %>% filter(CATMAT %in% input$filtro_catmat)
    }
    
    df
    
  })

  # 4. Agregações
  # 4. Agregações (Tabela Agregada por Medicamento)
  dados_agregados <- reactive({
    req(dados_filtrados())
    
    dados_filtrados() %>%
      # Garante que só calcularemos com linhas válidas (Valor e Qtd existem)
      dplyr::filter(!is.na(Valor), !is.na(Quantidade), Quantidade > 0) %>%
      
      group_by(CNPJ, Fornecedor, CATMAT, Medicamento, Apresentacao) %>%
      summarise(
        Compras = n(),
        
        # Somatórios Simples
        Qtd_Total = sum(Quantidade, na.rm = TRUE),
        Valor_Total = sum(Valor, na.rm = TRUE),
        
        # Preço Médio Ponderado: (Total Gasto / Quantidade Total)
        # Isso garante que compras maiores tenham "peso" maior no preço final
        Preco_Medio = sum(Valor, na.rm = TRUE) / sum(Quantidade, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      arrange(desc(Valor_Total))
  })
  
  # 5. KPIs
  output$kpi_cards <- renderUI({
    req(dados_filtrados())
    df <- dados_filtrados()
    
    val_total <- sum(df$Valor, na.rm = TRUE)
    qtd_total <- sum(df$Quantidade, na.rm = TRUE)
    n_meds <- length(unique(df$Medicamento))
    
    layout_columns(
      fill = FALSE,
      value_box(title = "Gasto Total", value = paste("R$", format(round(val_total, 2), big.mark=".", decimal.mark=",", nsmall = 2)), theme = value_box_theme(bg = "#003F1F"), showcase = bsicons::bs_icon("cash-coin")),
      value_box(title = "Itens Comprados", value = format(qtd_total, big.mark="."), theme = value_box_theme(bg = "#003F3F"), showcase = bsicons::bs_icon("box-seam")),
      value_box(title = "Medicamentos Distintos", value = n_meds, theme = value_box_theme(bg = "#3F1F00"), showcase = bsicons::bs_icon("capsule"))
    )
  })
  
  # --- GRÁFICOS E TABELAS ---
  
  output$tabela_agregada <- renderDT({
    req(dados_agregados())
    datatable(dados_agregados(),
              rownames = FALSE,
              colnames = c("CNPJ", "Fornecedor", "CATMAT", "Medicamento", "Apresentação", "Nº Compras", "Qtd. Total", "Valor Total", "Preço Médio"),
              options = list(
                pageLength = 10, 
                dom = 'tp',
                language = DT_PTBR  # Usa a variável do global.R
              )) %>%
      formatCurrency(columns = c("Valor_Total", "Preco_Medio"), currency = "R$ ", interval = 3, mark = ".", dec.mark = ",") %>%
      formatRound(columns = c("Qtd_Total"), digits = 0, mark = ".", dec.mark = ",") 
  })
  
  output$grafico_top10 <- renderPlotly({
    req(dados_agregados())
    
    top_df <- head(dados_agregados(), 5)
    if(nrow(top_df) == 0) return(NULL)
    
    # ID único para o ggplot não somar itens diferentes com mesmo nome
    top_df$Item_Completo <- paste(top_df$Medicamento, "-", top_df$Apresentacao)
    
    # "dicionário" para os Rótulos do Eixo:
    # Chave = ID Único (Com apresentação) -> Valor = Nome Limpo (Só Medicamento)
    labels_eixo <- setNames(top_df$Medicamento, top_df$Item_Completo)
    
    top_df <- top_df %>%
      mutate(Rotulo = paste0(
        "<b>Fornecedor:<b> ", Fornecedor, "<br>",
        "<b>Medicamento:</b> ", Medicamento, "<br>",
        "<b>Apresentação:</b> ", Apresentacao, "<br>", 
        "<b>Valor Total:</b> R$ ", format(Valor_Total, big.mark=".", decimal.mark=",", nsmall=2), "<br>",
        "<b>Qtd:</b> ", Qtd_Total
      ))
    
    p <- ggplot(top_df, aes(x = reorder(Item_Completo, Valor_Total), 
                            y = Valor_Total, 
                            text = Rotulo,
                            fill = Valor_Total)) +
      
      geom_col(show.legend = FALSE, alpha = 0.9, width = 0.6) +
      
      coord_flip() +
      
      scale_fill_gradient(low = "#88d8b0", high = "#198754") +
      
      labs(x = NULL, y = NULL) +
      
      scale_x_discrete(labels = labels_eixo) + 
      
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        axis.text.y = element_text(color = "#2c3e50"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 150)) 
  })
  
  output$grafico_acumulado <- renderPlotly({
    req(dados_filtrados())
    
    # 1. Base inicial
    df_base <- dados_filtrados()
    
    # 2. Lógica do Filtro de Tempo e Definição do Intervalo do Eixo
    meses_selecionados <- as.numeric(input$periodo_acumulado)
    
    # Variável para controlar o intervalo das legendas no gráfico
    intervalo_eixo <- waiver() # 'waiver()' é o automático do ggplot (para o caso 0)
    
    if (meses_selecionados > 0) {
      data_maxima <- max(df_base$Data, na.rm = TRUE)
      data_corte <- data_maxima %m-% months(meses_selecionados)
      df_base <- df_base %>% filter(Data >= data_corte)
      
      # Define a frequência das legendas baseada na seleção para não poluir
      if (meses_selecionados == 6) {
        intervalo_eixo <- "1 month"
      } else if (meses_selecionados == 12) {
        intervalo_eixo <- "2 months"
      } else {
        intervalo_eixo <- "3 months" 
      }
    }
    
    # 3. Cálculo do Acumulado
    df_tempo <- df_base %>%
      arrange(Data) %>%
      group_by(Data) %>%
      summarise(Valor_Dia = sum(Valor, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        Valor_Acumulado = cumsum(Valor_Dia),
        Rotulo = paste0("<b>Data:</b> ", format(Data, "%d/%m/%Y"), "<br>",
                        "<b>Acumulado:</b> R$ ", format(Valor_Acumulado, big.mark=".", decimal.mark=",", nsmall=2))
      )
    
    if(nrow(df_tempo) == 0) return(NULL)
    
    p <- ggplot(df_tempo, aes(x = Data, y = Valor_Acumulado)) +
      geom_area(fill = "#0d6efd", alpha = 0.2) +
      geom_line(color = "#0d6efd", linewidth = 1) + 
      labs(x = NULL, y = NULL) +
      geom_point(aes(text = Rotulo), color = "#0a58ca", size = 2) + 
      
      # --- CONFIGURAÇÃO DO EIXO X ---
      scale_x_date(
        date_labels = "%b/%Y",      
        date_breaks = intervalo_eixo 
      ) +
      
      scale_y_continuous(labels = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráficos de Comparativo
  # Gráfico Consolidado Trimestral (Eixo X Ajustado)
  output$grafico_trimestral <- renderPlotly({
    req(dados_filtrados())
    
    df_trim <- dados_filtrados() %>%
      mutate(Data_Ref = floor_date(Data, "quarter")) %>%
      group_by(Data_Ref) %>%
      summarise(Total = sum(Valor, na.rm = TRUE)) %>%
      mutate(
        Rotulo = paste0(
          "<b>Período:</b> ", quarter(Data_Ref), "º Tri ", year(Data_Ref), "<br>",
          "<b>Total:</b> R$ ", format(Total, big.mark=".", decimal.mark=",", nsmall=2)
        )
      )
    
    p <- ggplot(df_trim, aes(x = Data_Ref, y = Total, text = Rotulo)) +
      geom_col(fill = "#001F3F", alpha = 0.8, width = 60) + 
      labs(x = NULL, y = NULL) +
      
      # --- ALTERAÇÃO AQUI: Formatação personalizada do Eixo X ---
      scale_x_date(
        date_breaks = "3 months",
        labels = function(x) paste0(lubridate::quarter(x), "º Tri ", lubridate::year(x))
      ) +
      
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      
      # Rotação para o texto longo não encavalar
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico Consolidado Mensal (Eixo X apenas Ano)
  output$grafico_mensal <- renderPlotly({
    req(dados_filtrados())
    
    df_mes <- dados_filtrados() %>%
      mutate(Mes_Ano = floor_date(Data, "month")) %>%
      group_by(Mes_Ano) %>%
      summarise(Total = sum(Valor, na.rm = TRUE)) %>%
      mutate(Rotulo = paste0("<b>Mês:</b> ", format(Mes_Ano, "%b/%Y"), "<br>",
                             "<b>Total:</b> R$ ", format(Total, big.mark=".", decimal.mark=",", nsmall=2)))
    
    p <- ggplot(df_mes, aes(x = Mes_Ano, y = Total, text = Rotulo)) +
      geom_col(fill = "#3F001F", alpha = 0.8) +
      labs(x = NULL, y = NULL) +
      
      # --- ALTERAÇÃO AQUI ---
      scale_x_date(
        date_breaks = "1 year",  # Força apenas uma marcação por ano
        date_labels = "%Y"       # Formata o texto para mostrar apenas o ano (ex: 2025)
      ) +
      
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico Consolidado Anual (Todos os anos no eixo X)
  output$grafico_anual <- renderPlotly({
    req(dados_filtrados())
    
    df_ano <- dados_filtrados() %>%
      mutate(Ano = floor_date(Data, "year")) %>%
      group_by(Ano) %>%
      summarise(Total = sum(Valor, na.rm = TRUE)) %>%
      mutate(Rotulo = paste0("<b>Ano:</b> ", format(Ano, "%Y"), "<br>",
                             "<b>Total:</b> R$ ", format(Total, big.mark=".", decimal.mark=",", nsmall=2)))
    
    p <- ggplot(df_ano, aes(x = Ano, y = Total, text = Rotulo)) +
      # width = 100 define a largura da barra em dias (aprox 3 meses visualmente)
      geom_col(fill = "#3F1F00", alpha = 0.8, width = 100) + 
      labs(x = NULL, y = NULL) +
      
      # --- ALTERAÇÃO AQUI: Força a quebra anual ---
      scale_x_date(
        date_breaks = "1 year", 
        date_labels = "%Y"
      ) +
      
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Tabela Bruta
  output$tabela_bruta <- renderDT({
    req(dados_filtrados())
    df_exibir <- dados_filtrados()
    df_exibir$Data <- format(df_exibir$Data, "%d/%m/%Y")
    
    df_exibir <- df_exibir %>% select(Data, Estado, CATMAT, CNPJ, Fornecedor, Medicamento, Apresentacao, Quantidade, Valor, Base)
    
    datatable(df_exibir, 
              rownames = FALSE,
              options = list(
              pageLength = 10, 
              scrollX = TRUE,
              language = DT_PTBR)) %>%
      formatCurrency("Valor", currency = "R$ ", interval = 3, mark = ".", dec.mark = ",")
  })
  
# Curva ABC
dados_abc <- reactive({
  req(dados_filtrados())
  
  df_base <- dados_filtrados() %>%
    group_by(Medicamento, Apresentacao) %>%
    summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop")
  
  df_ordenacao <- df_base %>%
    group_by(Medicamento) %>%
    summarise(Total_Med = sum(Valor_Total), .groups = "drop") %>%
    arrange(desc(Total_Med))
  
  df_base %>%
    arrange(desc(Valor_Total)) %>% 
    mutate(
      Acumulado = cumsum(Valor_Total),
      Total_Geral = sum(Valor_Total),
      Perc_Acumulado = Acumulado / Total_Geral,
      
      Classe = case_when(
        Perc_Acumulado <= 0.80 ~ "A",
        Perc_Acumulado <= 0.95 ~ "B",
        TRUE ~ "C"),
      
      Total_Med_Sort = df_ordenacao$Total_Med[match(Medicamento, df_ordenacao$Medicamento)],
      
      Nome_Tooltip = paste(Medicamento, "-", Apresentacao),
      
      Rotulo = paste0(
        "<b>Item:</b> ", Nome_Tooltip, "<br>", 
        "<b>Classe:</b> ", Classe, "<br>",
        "<b>Valor Apresentação:</b> R$ ", format(Valor_Total, big.mark=".", decimal.mark=",", nsmall=2), "<br>",
        "<b>% Acumulado:</b> ", scales::percent(Perc_Acumulado, accuracy = 0.1))) %>%
    arrange(desc(Total_Med_Sort), desc(Valor_Total)) 
})

# 1. Variável reativa para controlar a página atual
paginacao <- reactiveValues(pagina = 1, itens_por_pagina = 6)

# 2. Reset de página ao mudar filtros
observeEvent(dados_abc(), {
  paginacao$pagina <- 1
})

# 3. Controles dos Botões (Lógica baseada em MEDICAMENTOS ÚNICOS)
observeEvent(input$btn_ant, {
  if (paginacao$pagina > 1) {
    paginacao$pagina <- paginacao$pagina - 1
  }
})

observeEvent(input$btn_prox, {
  req(dados_abc())
  # Conta quantos medicamentos únicos existem, não quantas linhas
  total_meds <- length(unique(dados_abc()$Medicamento))
  total_paginas <- ceiling(total_meds / paginacao$itens_por_pagina)
  
  if (paginacao$pagina < total_paginas) {
    paginacao$pagina <- paginacao$pagina + 1
  }
})

# 4. Texto informativo
output$texto_paginacao <- renderUI({
  req(dados_abc())
  total_meds <- length(unique(dados_abc()$Medicamento))
  if(total_meds == 0) return("Sem dados")
  
  total_paginas <- ceiling(total_meds / paginacao$itens_por_pagina)
  paste("Página", paginacao$pagina, "de", total_paginas)
})

# 5. Gráfico ABC com Paginação por Medicamento Agrupado
output$grafico_abc <- renderPlotly({
  req(dados_abc())
  
  df_completo <- dados_abc()
  if(nrow(df_completo) == 0) return(NULL)
  
  # Lista de medicamentos únicos na ordem correta (do maior total para o menor)
  lista_meds <- unique(df_completo$Medicamento) 
  max_valor_global <- max(df_completo$Total_Med_Sort, na.rm = TRUE)
  
  # --- Lógica de Paginação (Fatiando a lista de Medicamentos) ---
  n_slots <- paginacao$itens_por_pagina
  inicio <- (paginacao$pagina - 1) * n_slots + 1
  fim <- inicio + n_slots - 1
  
  if (fim > length(lista_meds)) fim <- length(lista_meds)
  
  # Medicamentos que aparecerão nesta página
  meds_da_pagina <- lista_meds[inicio:fim]
  
  # Filtra o dataframe para ter apenas as linhas desses medicamentos
  df_plot <- df_completo %>% 
    filter(Medicamento %in% meds_da_pagina)
  
  # --- PADDING (PREENCHIMENTO) ---
  # Se a página tiver menos medicamentos que o slot (ex: última página), preenche com vazios
  n_faltam <- n_slots - length(meds_da_pagina)
  
  meds_finais <- meds_da_pagina 
  
  if (n_faltam > 0) {
    dummies <- paste0("dummy_", 1:n_faltam)
    
    df_dummy <- data.frame(
      Medicamento = dummies,
      Apresentacao = " ",
      Valor_Total = 0, 
      Classe = "C",
      Rotulo = NA,
      Total_Med_Sort = 0
    )
    # Adiciona ao plot e à lista de fatores
    df_plot <- bind_rows(df_plot, df_dummy)
    meds_finais <- c(meds_finais, dummies)
  }
  
  # Transforma em Fator para respeitar a ordem (Inverte a ordem para o ggplot desenhar de cima para baixo)
  df_plot$Medicamento <- factor(df_plot$Medicamento, levels = rev(meds_finais))
  
  # Cores
  cores_abc <- c("A" = "#dc3545", "B" = "#ffc107", "C" = "#198754")
  
  p <- ggplot(df_plot, aes(x = Medicamento, y = Valor_Total, fill = Classe, text = Rotulo)) +
    # color = "white" cria a separação visual entre as apresentações na mesma barra
    geom_col(alpha = 0.85, width = 0.6, color = "white", size = 0.2) + 
    coord_flip() + 
    scale_fill_manual(values = cores_abc) +
    
    # Label visual: Se for dummy, mostra vazio, senão mostra o nome formatado
    scale_x_discrete(labels = function(x) ifelse(grepl("dummy_", x), "", stringr::str_to_title(x))) +
    
    labs(x = NULL, y = NULL) +
    
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","), 
      limits = c(0, max_valor_global * 1.05),
      expand = expansion(mult = c(0, 0))
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 11, color = "#2c3e50"),
      legend.position = "top"
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      margin = list(l = 400), 
      showlegend = FALSE    
    )
})
  
output$download_csv <- downloadHandler(
  filename = function() {
    paste0("ceaf_1b_", format(Sys.Date(), "%d-%m-%Y"), ".csv")
  },
  content = function(file) {
    req(dados_filtrados())
    df_baixar <- dados_filtrados()

    if ("Data" %in% names(df_baixar)) {
      df_baixar$Data <- format(df_baixar$Data, "%d/%m/%Y")
    }

    write.csv2(df_baixar, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)

output$download_xlsx <- downloadHandler(
  filename = function() {
    paste0("ceaf_1b_", format(Sys.Date(), "%d-%m-%Y"), ".xlsx")
  },
  content = function(file) {
    req(dados_filtrados())
    df_baixar <- dados_filtrados()

    if ("Data" %in% names(df_baixar)) {
      df_baixar$Data <- format(df_baixar$Data, "%d/%m/%Y")
    }

    writexl::write_xlsx(df_baixar, file)
  }
)


# =======================================================
# --- BLOCO DA PÁGINA: ANÁLISE DE PREÇOS ---
# =======================================================

# 1. Atualizar Filtros em Cascata (Base -> Medicamento -> UF)
observe({
  req(dados_raw(), input$sel_med_base)
  
  # A. Filtra a Base selecionada (LAI ou PNCP)
  df_base <- dados_raw() %>%
    dplyr::filter(Base == input$sel_med_base)
  
  # B. Atualiza lista de Medicamentos
  lista_meds <- sort(unique(df_base$Medicamento))
  
  # Lógica para manter seleção ou pegar o primeiro
  med_atual <- isolate(input$sel_med_preco)
  sel_med <- if(!is.null(med_atual) && med_atual %in% lista_meds) med_atual else lista_meds[1]
  
  updateSelectInput(session, "sel_med_preco", choices = lista_meds, selected = sel_med)
  
  # C. Atualiza lista de UFs (baseada no medicamento vigente)
  df_med <- df_base %>% dplyr::filter(Medicamento == sel_med)
  lista_ufs <- sort(unique(df_med$Estado))
  
  # Lógica para manter UFs selecionadas que ainda sejam válidas
  uf_atual <- isolate(input$sel_uf_preco)
  sel_uf <- uf_atual[uf_atual %in% lista_ufs]
  
  updateSelectizeInput(session, "sel_uf_preco", choices = lista_ufs, selected = sel_uf)
})


# 2. KPI: MÉDIA PONDERADA FILTRADA
output$kpi_media_preco <- renderText({
  req(dados_raw(), input$sel_med_base, input$sel_med_preco)
  
  # Filtra Base e Medicamento diretamente do RAW
  df_kpi <- dados_raw() %>% 
    dplyr::filter(
      Base == input$sel_med_base,
      Medicamento == input$sel_med_preco
    )
  
  # Filtro de UF (Opcional)
  if (!is.null(input$sel_uf_preco) && length(input$sel_uf_preco) > 0) {
    df_kpi <- df_kpi %>% dplyr::filter(Estado %in% input$sel_uf_preco)
  }
  
  if(nrow(df_kpi) == 0 || sum(df_kpi$Quantidade, na.rm=TRUE) == 0) return("R$ 0,00")
  
  # CÁLCULO DA MÉDIA PONDERADA
  media_ponderada <- sum(df_kpi$Valor, na.rm=TRUE) / sum(df_kpi$Quantidade, na.rm=TRUE)
  
  paste("R$", format(round(media_ponderada, 2), big.mark=".", decimal.mark=",", nsmall=2))
})


# 3. GRÁFICO DE LINHAS: EVOLUÇÃO DO PREÇO MÉDIO
output$grafico_dist_preco <- renderPlotly({
  req(dados_raw(), input$sel_med_base, input$sel_med_preco)
  
  # A. Filtra dados
  df_viz <- dados_raw() %>%
    dplyr::filter(
      Base == input$sel_med_base,
      Medicamento == input$sel_med_preco
    )
  
  if (!is.null(input$sel_uf_preco) && length(input$sel_uf_preco) > 0) {
    df_viz <- df_viz %>% dplyr::filter(Estado %in% input$sel_uf_preco)
  }
  
  if(nrow(df_viz) == 0) return(NULL)
  
  # B. Agrega por Ano/Estado
  df_viz <- df_viz %>%
    mutate(Ano = lubridate::year(Data)) %>%
    group_by(Ano, Estado) %>%
    summarise(
      Preco_Medio = sum(Valor, na.rm = TRUE) / sum(Quantidade, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Rotulo = paste0(
        "<b>Estado:</b> ", Estado, "<br>",
        "<b>Ano:</b> ", Ano, "<br>",
        "<b>Preço Médio (Pond.):</b> R$ ", format(Preco_Medio, big.mark=".", decimal.mark=",", nsmall=2)
      )
    )
  
  # C. Plot
  p <- ggplot(df_viz, aes(x = Ano, y = Preco_Medio, color = Estado, group = Estado)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    geom_point(aes(text = Rotulo), size = 2, shape = 21, fill = "white", stroke = 1.5) +
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = unique(df_viz$Ano)) +
    labs(x = NULL, y = NULL, color = "UF") +
    theme_minimal() +
    theme(legend.position = "right")
  
  ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "x unified")
})


# 4. GRÁFICO DE RANKING: PREÇO MÉDIO POR ESTADO
output$grafico_ranking_preco <- renderPlotly({
  req(dados_raw(), input$sel_med_base, input$sel_med_preco)
  
  df_rank <- dados_raw() %>%
    dplyr::filter(
      Base == input$sel_med_base,
      Medicamento == input$sel_med_preco
    )
  
  if (!is.null(input$sel_uf_preco) && length(input$sel_uf_preco) > 0) {
    df_rank <- df_rank %>% dplyr::filter(Estado %in% input$sel_uf_preco)
  }
  
  # Agrega por Estado
  df_rank <- df_rank %>%
    group_by(Estado) %>%
    summarise(
      Preco_Medio = sum(Valor, na.rm = TRUE) / sum(Quantidade, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Preco_Medio) 
  
  if(nrow(df_rank) == 0) return(NULL)
  
  df_rank$Estado <- factor(df_rank$Estado, levels = df_rank$Estado)
  
  df_rank <- df_rank %>%
    mutate(Rotulo = paste0(
      "<b>Estado:</b> ", Estado, "<br>",
      "<b>Média (Pond.):</b> R$ ", format(Preco_Medio, big.mark=".", decimal.mark=",", nsmall=2)
    ))
  
  p <- ggplot(df_rank, aes(x = Preco_Medio, 
                           y = reorder(Estado, Preco_Medio), 
                           text = Rotulo,
                           fill = Preco_Medio)) + 
    
    geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) +
    scale_fill_gradient(low = "#C5E8B7", high = "#198754") +
    
    labs(x = NULL, y = NULL) +
    scale_x_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
      axis.text.y = element_text(size = 9, color = "gray30")
    )
  
  ggplotly(p, tooltip = "text")
})


# =======================================================
# --- BLOCO: MAPA INTERATIVO (ATUALIZADO) ---
# =======================================================

output$mapa_brasil_preco <- renderPlotly({
  req(dados_raw(), input$sel_med_base, input$sel_med_preco)
  
  # A. Dados de Negócio (Base RAW + Filtros Locais)
  df_mapa_data <- dados_raw() %>%
    dplyr::filter(
      Base == input$sel_med_base,
      Medicamento == input$sel_med_preco
    ) %>%
    group_by(Estado) %>%
    summarise(
      Preco_Medio = sum(Valor, na.rm = TRUE) / sum(Quantidade, na.rm = TRUE),
      .groups = "drop"
    )
  
  # B. Cruzamento com Malha Global (Certifique-se que malha_brasil_global existe no global.R ou início do server)
  df_final <- malha_brasil_global %>%
    left_join(df_mapa_data, by = c("abbrev_state" = "Estado")) %>%
    mutate(
      Texto_Tooltip = ifelse(
        is.na(Preco_Medio),
        paste0("<b>Estado:</b> ", name_state, "<br>Sem dados registrados"),
        paste0(
          "<b>Estado:</b> ", name_state, " (", abbrev_state, ")<br>",
          "<b>Preço Médio:</b> R$ ", format(Preco_Medio, big.mark=".", decimal.mark=",", nsmall=2)
        )
      )
    )
  
  # C. Plotagem Base
  p <- ggplot(df_final) +
    geom_sf(aes(fill = Preco_Medio, text = Texto_Tooltip), color = "white", size = 0.1) +
    
    scale_fill_gradient(
      low = "#C5E8B7",   
      high = "#198754",  
      na.value = "#e9ecef",
      name = "Preço Médio"
    ) +
    
    theme_minimal() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
  
  fig <- ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  
  plotly::partial_bundle(fig)
})


# =======================================================
# --- BLOCO DA PÁGINA: ECONOMIA POTENCIAL (COMPLETO) ---
# =======================================================

# 1. Atualizar Filtros (Cascata: Base -> Medicamento -> Ano)
observe({
  req(dados_raw())
  
  # Inicialização de segurança para o filtro de Base
  if (is.null(input$sel_base_eco)) {
    updateSelectInput(session, "sel_base_eco", choices = c("LAI", "PNCP"), selected = "LAI")
    return()
  }
  
  # A. Filtra dados pela Base selecionada
  df_base <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_eco)
  
  # B. Atualiza Medicamentos disponíveis nesta Base
  lista_meds <- sort(unique(df_base$Medicamento))
  
  # Se não houver medicamentos na base (ex: erro de carga), para aqui
  if (length(lista_meds) == 0) return()
  
  # Tenta manter o medicamento atual, senão pega o primeiro da lista
  med_atual <- isolate(input$sel_med_eco)
  sel_med <- if(!is.null(med_atual) && med_atual %in% lista_meds) med_atual else lista_meds[1]
  
  updateSelectInput(session, "sel_med_eco", choices = lista_meds, selected = sel_med)
  
  # C. Atualiza Anos (Baseado no medicamento definido)
  df_med <- df_base %>% dplyr::filter(Medicamento == sel_med)
  lista_anos <- sort(unique(lubridate::year(df_med$Data)), decreasing = TRUE)
  
  updateSelectInput(session, "sel_ano_eco", 
                    choices = c("Todos", lista_anos),
                    selected = "Todos")
})


# 2. Reativo: Cálculos da Economia
dados_economia_calc <- reactive({
  # Requisição dos inputs fundamentais
  req(dados_raw(), input$sel_base_eco, input$sel_med_eco, input$sel_ano_eco)
  
  # Garante que o input de medicamento não está vazio
  if (input$sel_med_eco == "") return(NULL)
  
  # A. Filtra Base e Medicamento diretamente do RAW
  df_foco <- dados_raw() %>%
    dplyr::filter(
      Base == input$sel_base_eco,
      Medicamento == input$sel_med_eco
    )
  
  # B. Filtro de Ano (Opcional)
  if (input$sel_ano_eco != "Todos") {
    df_foco <- df_foco %>% 
      dplyr::filter(lubridate::year(Data) == as.numeric(input$sel_ano_eco))
  }
  
  if(nrow(df_foco) == 0) return(NULL)
  
  # C. Calcula a Economia para TODOS os estados (filtro de UF removido)
  df_estados <- df_foco %>%
    group_by(Estado) %>%
    summarise(
      Qtd_Total = sum(Quantidade, na.rm = TRUE),
      Gasto_Real = sum(Valor, na.rm = TRUE),
      Preco_Medio_Estado = sum(Valor, na.rm = TRUE) / sum(Quantidade, na.rm = TRUE),
      .groups = "drop"
    )
  
  # D. Encontra o Benchmark (Menor Preço Médio > 0)
  row_benchmark <- df_estados %>%
    dplyr::filter(Preco_Medio_Estado > 0) %>%
    slice_min(Preco_Medio_Estado, n = 1) %>%
    head(1) 
  
  if(nrow(row_benchmark) == 0) return(NULL)
  
  preco_ref <- row_benchmark$Preco_Medio_Estado
  estado_ref <- row_benchmark$Estado 
  
  # E. Tabela Final
  df_final <- df_estados %>%
    mutate(
      Preco_Referencia = preco_ref,
      Gasto_Otimizado = Qtd_Total * Preco_Referencia, 
      Economia_Potencial = Gasto_Real - Gasto_Otimizado,
      
      Economia_Potencial = pmax(0, Economia_Potencial),
      
      Rotulo = paste0(
        "<b>Estado:</b> ", Estado, "<br>",
        "<b>Gasto Real:</b> R$ ", format(Gasto_Real, big.mark=".", decimal.mark=","), "<br>",
        "<b>Preço Pago:</b> R$ ", format(round(Preco_Medio_Estado, 2), nsmall=2, decimal.mark=","), "<br>",
        "<b>Benchmark (", estado_ref, "):</b> R$ ", format(round(Preco_Referencia, 2), nsmall=2, decimal.mark=","), "<br>",
        "<b>Desperdício:</b> R$ ", format(round(Economia_Potencial, 2), big.mark=".", decimal.mark=",")
      )
    ) %>%
    arrange(desc(Economia_Potencial))
  
  return(list(tabela = df_final, preco_ref = preco_ref, estado_ref = estado_ref))
})

# 3. KPI: Total da Economia
output$kpi_total_economia <- renderUI({
  req(dados_economia_calc())
  total <- sum(dados_economia_calc()$tabela$Economia_Potencial, na.rm = TRUE)
  texto_valor <- paste("R$", format(round(total, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
  
  div(style = "font-size: 1.2rem; font-weight: bold; white-space: nowrap;", texto_valor)
})

# 4. Texto informativo do Benchmark
output$info_benchmark_detalhe <- renderUI({
  req(dados_economia_calc())
  
  dados <- dados_economia_calc()
  uf <- dados$estado_ref
  preco <- format(round(dados$preco_ref, 2), big.mark=".", decimal.mark=",", nsmall=2)
  
  div(
    style = "text-align: center; margin-top: 10px; font-size: 0.90rem; color: #666;",
    span("Benchmark: ", style = "font-weight: normal;"),
    strong(uf),
    span(paste0("(R$ ", preco, ")"), style = "font-size: 0.85rem;")
  )
})

# 5. Gráfico de Barras (Ranking de Economia)
output$grafico_economia <- renderPlotly({
  req(dados_economia_calc())
  
  # Filtra para mostrar apenas quem tem economia potencial relevante (> R$ 1)
  df <- dados_economia_calc()$tabela %>%
    dplyr::filter(Economia_Potencial > 1) 
  
  if(nrow(df) == 0) return(NULL)
  
  # Ordenação para o gráfico ficar bonito
  df$Estado <- factor(df$Estado, levels = df$Estado)
  
  p <- ggplot(df, aes(x = reorder(Estado, Economia_Potencial), 
                      y = Economia_Potencial, 
                      text = Rotulo,
                      fill = Economia_Potencial)) +
    
    geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) + 
    
    coord_flip() +
    
    scale_fill_gradient(low = "#88d8b0", high = "#198754") +
    
    labs(x = NULL, y = NULL) +
    
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
      axis.text.y = element_text(size = 9, color = "gray30")
    )
  
  ggplotly(p, tooltip = "text")
})

# 6. Tabela de Detalhes
output$tabela_economia <- renderDT({
  req(dados_economia_calc())
  
  df <- dados_economia_calc()$tabela %>%
    select(Estado, Qtd_Total, Preco_Medio_Estado, Gasto_Otimizado, Gasto_Real, Economia_Potencial)
  
  datatable(df, rownames = FALSE,
            colnames = c("UF", "Qtd. Comprada", "Preço Médio Pago", "Gasto Potencial (Benchmark)", "Gasto Real", "Economia Potencial"),
            options = list(
              pageLength = 10, 
              scrollX = TRUE,
              language = DT_PTBR
            )) %>%
    formatCurrency(columns = c("Preco_Medio_Estado", "Gasto_Otimizado", "Gasto_Real", "Economia_Potencial"), currency = "R$ ", interval = 3, mark = ".", dec.mark = ",") %>%
    formatRound(columns = "Qtd_Total", digits = 0, mark = ".") %>%
    formatStyle(
      'Economia_Potencial',
      color = styleInterval(0, c('gray', 'green')),
      fontWeight = 'bold'
    )
})

# =======================================================
# --- BLOCO DA PÁGINA: PREVISÃO DE GASTOS (PROPHET) ---
# =======================================================

# [CORREÇÃO SIMPLIFICADA] Atualizar Filtro de Medicamentos (Previsão)
observe({
  # Continua usando dados_raw() para ter acesso a TODOS os dados
  req(dados_raw(), input$sel_base_forecast)
  
  # Filtra a base bruta localmente
  df <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_forecast)
  
  # Cria a lista de opções
  novas_opcoes <- sort(unique(df$Medicamento))
  
  # Lógica de segurança para a seleção
  selecao_atual <- isolate(input$sel_med_forecast)
  
  nova_selecao <- if (is.null(selecao_atual) || !(selecao_atual %in% novas_opcoes)) {
    novas_opcoes[1]
  } else {
    selecao_atual
  }
  
  # USAMOS updateSelectInput (sem server = TRUE e sem freeze)
  # Isso força o envio da lista completa para o navegador, garantindo que apareça.
  updateSelectInput(
    session,
    "sel_med_forecast",
    choices = novas_opcoes,
    selected = nova_selecao
  )
})


# 2. Reativo: Cálculos com PROPHET (Aba Previsão)
dados_previsao <- reactive({
  # Dependência direta de dados_raw()
  req(dados_raw(), input$sel_base_forecast, input$sel_med_forecast)
  
  # Aplica o filtro da base LOCALMENTE, ignorando o global
  df_base <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_forecast)
  
  # Agora filtra o medicamento selecionado
  df_hist <- df_base %>%
    dplyr::filter(Medicamento == input$sel_med_forecast) %>%
    mutate(Mes_Ref = floor_date(Data, "month")) %>%
    group_by(Mes_Ref) %>%
    summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(Mes_Ref)
  
  # Verificação de segurança para o modelo rodar
  if (nrow(df_hist) < 2) return(NULL)
  
  # --- O restante do código do Prophet permanece inalterado ---
  df_prophet <- df_hist %>% rename(ds = Mes_Ref, y = Valor_Total)
  
  m <- prophet::prophet(df_prophet,
                        daily.seasonality = FALSE,
                        weekly.seasonality = FALSE,
                        yearly.seasonality = TRUE)
  
  futuro <- prophet::make_future_dataframe(m, periods = input$horizonte_meses, freq = "month")
  previsao <- predict(m, futuro)
  
  df_final <- previsao %>%
    select(ds, yhat, yhat_lower, yhat_upper) %>%
    mutate(
      ds = as.Date(ds),
      yhat = pmax(0, yhat),
      yhat_lower = pmax(0, yhat_lower),
      yhat_upper = pmax(0, yhat_upper)
    ) %>%
    left_join(df_prophet, by = "ds") %>%
    mutate(
      Tipo = ifelse(is.na(y), "Previsão", "Histórico"),
      Valor_Plot = ifelse(!is.na(y), y, yhat)
    )
  
  df_final
})


# 3. Gráfico de Previsão (Atualizado para ler estrutura do Prophet)
output$grafico_forecast <- renderPlotly({
  req(dados_previsao())
  
  df <- dados_previsao()
  
  # Tooltip
  df <- df %>%
    mutate(Rotulo = paste0(
      "<b>Mês:</b> ", format(ds, "%b/%Y"), "<br>",
      "<b>Situação:</b> ", Tipo, "<br>",
      "<b>Valor:</b> R$ ", format(round(Valor_Plot, 2), big.mark=".", decimal.mark=",")
    ))
  
  cols <- c("Histórico" = "#183EFF", "Previsão" = "#fd7e14")
  
  p <- ggplot(df, aes(x = ds, y = Valor_Plot)) +
    
    # Área de Confiança (Somente onde é previsão)
    # Usamos geom_ribbon filtrando apenas linhas de previsão para não sujar o histórico
    geom_ribbon(data = subset(df, Tipo == "Previsão"), 
                aes(ymin = yhat_lower, ymax = yhat_upper), 
                fill = "#fd7e14", alpha = 0.2) +
    
    # Linha e Pontos
    geom_line(aes(color = Tipo, linetype = Tipo), linewidth = 1) +
    geom_point(aes(color = Tipo, text = Rotulo), size = 2) +
    
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c("Histórico" = "solid", "Previsão" = "dashed")) +
    
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months") + # Ajustei break para não encavalar
    
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      margin = list(b = 160)  # (Opcional) Garante que a legenda fique um pouco mais para cima
    )
})

# 4. Tabela de Previsão
output$tabela_forecast <- renderDT({
  req(dados_previsao())
  
  df_exibir <- dados_previsao() %>%
    filter(Tipo == "Previsão") %>%
    select(ds, yhat, yhat_lower, yhat_upper)
  
  datatable(df_exibir, rownames = FALSE,
            colnames = c("Mês Referência", "Valor Previsto", "Cenário Mínimo", "Cenário Máximo"),
            options = list(
              dom = 't',
              language = DT_PTBR
            )) %>%
    formatDate("ds", method = "toLocaleDateString", params = list('pt-BR', list(month = 'long', year = 'numeric'))) %>%
    formatCurrency(c("yhat", "yhat_lower", "yhat_upper"), currency = "R$ ", mark = ".", dec.mark = ",")
})

# Somatório da Previsão
output$card_kpi_previsao <- renderUI({
  req(dados_previsao())
  
  # 1. Cálculo do Somatório:
  total_previsto <- dados_previsao() %>%
    filter(Tipo == "Previsão") %>%
    summarise(Total = sum(yhat, na.rm = TRUE)) %>%
    pull(Total)
  
  # 2. Formatação (Moeda Brasileira)
  valor_formatado <- scales::dollar(total_previsto, 
                                    prefix = "R$ ", 
                                    big.mark = ".", 
                                    decimal.mark = ",",
                                    accuracy = 0.01)
  
  # 3. Renderização do Cartão Estilizado:
  div(
    class = "alert alert-info p-3 text-center mb-0", 
    style = "background-color: #fd7e14; color: white; border-radius: 25px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); border: none;",
    
    span("Gasto Total Previsto:", style = "font-size: 0.9rem; opacity: 0.9;"), 
    br(),
    
    # Texto grande com o valor
    strong(valor_formatado, style = "font-size: 1.6rem; line-height: 1.2;")
  )
})

  # =================================================================
  #     --- ANÁLISE DE FORNECEDORES: PERFIL DOS FORNECEDORES ---
  # =================================================================

  # ============================
  #    Gráfico: Market Share 
  # ============================

  # Atualiza filtros em cascata (Base -> Medicamento/Ano/UF)
  observe({
    req(dados_raw())

    # Inicialização de segurança para a Base
    if (is.null(input$sel_base_conc) || input$sel_base_conc == "") {
      updateSelectInput(session, "sel_base_conc", choices = c("LAI", "PNCP"), selected = "LAI")
      return()
    }

    df_base <- dados_raw() %>%
      dplyr::filter(Base == input$sel_base_conc)

    # Opções disponíveis (com proteção contra NA)
    lista_meds <- sort(unique(df_base$Medicamento))
    lista_anos <- sort(unique(lubridate::year(df_base$Data)), decreasing = TRUE)
    lista_ufs  <- sort(unique(df_base$Estado))

    # Medicamento (mantém seleção se possível)
    med_atual <- isolate(input$sel_med_conc)
    sel_med <- if (!is.null(med_atual) && med_atual %in% lista_meds) med_atual else if (length(lista_meds) > 0) lista_meds[1] else ""
    updateSelectInput(session, "sel_med_conc", choices = lista_meds, selected = sel_med)

    # Ano (inclui "Todos")
    ano_atual <- isolate(input$sel_ano_conc)
    choices_anos <- c("Todos", lista_anos)
    sel_ano <- if (!is.null(ano_atual) && ano_atual %in% choices_anos) ano_atual else "Todos"
    updateSelectInput(session, "sel_ano_conc", choices = choices_anos, selected = sel_ano)

    # UF (múltiplo) - mantém apenas válidas
    ufs_atuais <- isolate(input$sel_uf_conc)
    ufs_validas <- if (is.null(ufs_atuais)) character(0) else intersect(ufs_atuais, lista_ufs)
    updateSelectizeInput(session, "sel_uf_conc", choices = lista_ufs, selected = ufs_validas, server = TRUE)
  })

output$grafico_market_share <- renderPlotly({
  req(dados_raw(), input$sel_base_conc)

  # 1) Filtra conforme parâmetros da aba
  df <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_conc) %>%
    dplyr::filter(!is.na(Fornecedor), Fornecedor != "") %>%
    dplyr::filter(!is.na(Valor))

  # Ano
  if (!is.null(input$sel_ano_conc) && input$sel_ano_conc != "Todos") {
    df <- df %>%
      dplyr::filter(!is.na(Data)) %>%
      dplyr::filter(lubridate::year(Data) == as.integer(input$sel_ano_conc))
  }

  # UF
  if (!is.null(input$sel_uf_conc) && length(input$sel_uf_conc) > 0) {
    df <- df %>% dplyr::filter(Estado %in% input$sel_uf_conc)
  }

  # Medicamento
  if (!is.null(input$sel_med_conc) && nzchar(input$sel_med_conc)) {
    df <- df %>% dplyr::filter(Medicamento == input$sel_med_conc)
  }

  validate(need(nrow(df) > 0, "Sem dados para exibir com os filtros selecionados."))

  # 2) market share por fornecedor (por VALOR)
  agg <- df %>%
    dplyr::group_by(Fornecedor) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Share = Valor_Total / sum(Valor_Total, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(Share)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::mutate(
      Fornecedor = forcats::fct_reorder(Fornecedor, Share),
      Share_pct = 100 * Share
    )

  validate(need(nrow(agg) > 0, "Sem dados suficientes para calcular market share."))

  # 3) Plota (donut):
  p <- plotly::plot_ly(
    data = agg,
    labels = ~Fornecedor,
    values = ~Share_pct,
    type = "pie",
    hole = 0.55,
    textinfo = "none",
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Share: %{value:.2f}%<br>",
      "Valor: R$ %{customdata:,.2f}",
      "<extra></extra>"
    ),
    customdata = ~Valor_Total,
    marker = list(
      line = list(
        color = "black",  # cor da borda
        width = 1         # espessura
      )
    )
  )  
})


# ============================
#    Gráfico: Curva de Lorenz
# ============================
output$grafico_curva_lorenz <- renderPlotly({
  req(dados_raw(), input$sel_base_conc)

  # 1) Filtra conforme parâmetros da aba
  df <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_conc) %>%
    dplyr::filter(!is.na(Fornecedor), Fornecedor != "") %>%
    dplyr::filter(!is.na(Valor))

  # Ano
  if (!is.null(input$sel_ano_conc) && input$sel_ano_conc != "Todos") {
    df <- df %>%
      dplyr::filter(!is.na(Data)) %>%
      dplyr::filter(lubridate::year(Data) == as.integer(input$sel_ano_conc))
  }

  # UF
  if (!is.null(input$sel_uf_conc) && length(input$sel_uf_conc) > 0) {
    df <- df %>% dplyr::filter(Estado %in% input$sel_uf_conc)
  }

  # Medicamento
  if (!is.null(input$sel_med_conc) && nzchar(input$sel_med_conc)) {
    df <- df %>% dplyr::filter(Medicamento == input$sel_med_conc)
  }

  validate(need(nrow(df) > 0, "Sem dados para exibir com os filtros selecionados."))

  # 2) Agrega por fornecedor e calcula participações
  agg <- df %>%
    dplyr::group_by(Fornecedor) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(Valor_Total), Valor_Total > 0)

  validate(need(nrow(agg) > 1, "Sem fornecedores suficientes para calcular a Curva de Lorenz."))

  total_valor <- sum(agg$Valor_Total, na.rm = TRUE)
  validate(need(total_valor > 0, "Sem valor total positivo para calcular a Curva de Lorenz."))

  lorenz_df <- agg %>%
    dplyr::arrange(Valor_Total) %>%
    dplyr::mutate(
      Share = Valor_Total / total_valor,
      Cum_Valor = cumsum(Share),
      Cum_Fornecedores = dplyr::row_number() / dplyr::n()
    ) %>%
    dplyr::mutate(
      Rotulo = paste0(
        "<b>Fornecedor:</b> ", Fornecedor, "<br>",
        "<b>Valor:</b> R$ ", scales::comma(Valor_Total, big.mark = ".", decimal.mark = ","), "<br>",
        "<b>% acumulado do valor:</b> ", sprintf("%.1f", 100 * Cum_Valor), "%<br>",
        "<b>% acumulado de fornecedores:</b> ", sprintf("%.1f", 100 * Cum_Fornecedores), "%"
      )
    )

  # Inclui origem (0,0)
  lorenz_plot <- dplyr::bind_rows(
    tibble::tibble(Cum_Fornecedores = 0, Cum_Valor = 0, Rotulo = "Início"),
    lorenz_df %>% dplyr::select(Cum_Fornecedores, Cum_Valor, Rotulo)
  )

  # 3) Plota
  p <- ggplot2::ggplot(lorenz_plot, ggplot2::aes(x = Cum_Fornecedores, y = Cum_Valor, text = Rotulo)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
    )

  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(margin = list(l = 30, r = 10, t = 10, b = 30))
})


# =========================================================
#   Tabela: Indicadores (CR4 / CR8 / IHH)
# =========================================================
indicadores_conc_tbl <- reactive({
  req(dados_raw(), input$sel_base_conc)

  # 1) Filtra conforme parâmetros da aba
  df <- dados_raw() %>%
    dplyr::filter(Base == input$sel_base_conc) %>%
    dplyr::filter(!is.na(Fornecedor), Fornecedor != "") %>%
    dplyr::filter(!is.na(Valor))

  # Ano
  if (!is.null(input$sel_ano_conc) && input$sel_ano_conc != "Todos") {
    df <- df %>%
      dplyr::filter(!is.na(Data)) %>%
      dplyr::filter(lubridate::year(Data) == as.integer(input$sel_ano_conc))
  }

  # UF
  if (!is.null(input$sel_uf_conc) && length(input$sel_uf_conc) > 0) {
    df <- df %>% dplyr::filter(Estado %in% input$sel_uf_conc)
  }

  # Medicamento
  if (!is.null(input$sel_med_conc) && nzchar(input$sel_med_conc)) {
    df <- df %>% dplyr::filter(Medicamento == input$sel_med_conc)
  }

  validate(need(nrow(df) > 0, "Sem dados para calcular os indicadores com os filtros selecionados."))

  # 2) Agrega e calcula shares
  agg <- df %>%
    dplyr::group_by(Fornecedor) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(Valor_Total), Valor_Total > 0) %>%
    dplyr::arrange(dplyr::desc(Valor_Total))

  validate(need(nrow(agg) > 0, "Sem fornecedores suficientes para calcular os indicadores."))
  total_valor <- sum(agg$Valor_Total, na.rm = TRUE)
  validate(need(total_valor > 0, "Sem valor total positivo para calcular os indicadores."))

  shares <- agg$Valor_Total / total_valor

  cr4 <- sum(utils::head(shares, 4), na.rm = TRUE)
  cr8 <- sum(utils::head(shares, 8), na.rm = TRUE)

  ihh_10000 <- sum((100 * shares)^2, na.rm = TRUE)
  ihh_1 <- sum(shares^2, na.rm = TRUE)

  out <- data.frame(
    Indicador = c("CR4", "CR8", "IHH (0–10.000)", "IHH (0–1)"),
    Valor = c(cr4, cr8, ihh_10000, ihh_1),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      `Nº de Fornecedores` = nrow(agg),
      `Unidade` = dplyr::if_else(Indicador %in% c("CR4", "CR8"), "%", "Índice")
    ) %>%
    dplyr::mutate(
      `Valor` = dplyr::case_when(
        Indicador %in% c("CR4", "CR8") ~ round(100 * Valor, 2),
        Indicador == "IHH (0–10.000)"  ~ round(Valor, 0),
        TRUE                           ~ round(Valor, 4)
      )
    ) %>%
    dplyr::select(Indicador, Unidade, `Valor`, `Nº de Fornecedores`)

  out
})

output$grafico_indicadores_conc <- renderDT({
  req(indicadores_conc_tbl())
  DT::datatable(
    indicadores_conc_tbl(),
    rownames = FALSE,
    options = list(dom = 't', language = DT_PTBR)
  )
})

# Se algum lugar ainda usar os IDs antigos, renderiza a mesma tabela
output$grafico_cr4_cr8 <- renderDT({
  req(indicadores_conc_tbl())
  DT::datatable(
    indicadores_conc_tbl(),
    rownames = FALSE,
    options = list(dom = 't', language = DT_PTBR)
  )
})

output$grafico_ihh <- renderDT({
  req(indicadores_conc_tbl())
  DT::datatable(
    indicadores_conc_tbl(),
    rownames = FALSE,
    options = list(dom = 't', language = DT_PTBR)
  )
})

# =================================================================
#            --- LÓGICA DO COMPARATIVO ENTRE ESTADOS  ---
# =================================================================

# 1. Opções dos Filtros (Inicialização + Cascata)
observe({
  req(dados_raw())
  
  # --- PASSO 1: INICIALIZAÇÃO DA BASE ---
  if (is.null(input$comp_base) || input$comp_base == "") {
    updateSelectInput(session, "comp_base", 
                      choices = c("LAI", "PNCP"), 
                      selected = "LAI")
    return()
  }
  
  # --- PASSO 2: CASCATA ---
  req(input$comp_base)
  
  # Filtra a base bruta pela Fonte selecionada
  df_filtrado_por_base <- dados_raw() %>%
    filter(Base == input$comp_base)
  
  # Extraímos as opções disponíveis nesta base
  lista_meds <- sort(unique(df_filtrado_por_base$Medicamento))
  lista_anos <- sort(unique(year(df_filtrado_por_base$Data)), decreasing = TRUE)
  lista_ufs  <- sort(unique(df_filtrado_por_base$Estado))
  
  # Atualiza Medicamento (com opção "Todos")
  meds_choices <- c("Todos", lista_meds)

  med_atual <- isolate(input$comp_med)
  sel_med <- if (!is.null(med_atual) && med_atual %in% meds_choices) med_atual else "Todos"

  updateSelectInput(session, "comp_med", choices = meds_choices, selected = sel_med)

  # Atualiza Medicamento (com opção "Todos")
  ano_choices <- c("Todos", lista_anos)

  ano_atual <- isolate(input$comp_ano)
  sel_ano <- if(!is.null(ano_atual) && ano_atual %in% ano_choices) ano_atual else "Todos"
  
  updateSelectInput(session, "comp_ano", choices = ano_choices, selected = sel_ano)
  
  # Atualiza Estados (Sem server = TRUE para evitar sumiço das opções)
  ufs_atuais <- isolate(input$comp_ufs)
  ufs_validas <- intersect(ufs_atuais, lista_ufs)
  
  # Seleciona os válidos ou os 3 primeiros se estiver vazio
  sel_ufs <- if (length(ufs_validas) > 0) ufs_validas else NULL
  
  updateSelectizeInput(session, "comp_ufs", 
                       choices = lista_ufs, 
                       selected = sel_ufs) # server = TRUE removido por segurança
})

# 2. Dados Reativos (Com filtro de Base)
dados_comparativo <- reactive({
  req(dados_raw(), input$comp_med, input$comp_ano, input$comp_ufs, input$comp_base)

  if (length(input$comp_ufs) == 0) return(NULL)

  df <- dados_raw() %>%
    mutate(Ano = year(Data)) %>%
    filter(
      Base == input$comp_base,
      Estado %in% input$comp_ufs
    )

  # Ano ("Todos" = não filtra)
  if (!is.null(input$comp_ano) && input$comp_ano != "Todos") {
    df <- df %>% filter(Ano == as.numeric(input$comp_ano))
  }
  
  # Medicamento (opção "Todos" = não filtra)
  if (!is.null(input$comp_med) && input$comp_med != "Todos") {
    df <- df %>% filter(Medicamento == input$comp_med)
  }

  df
})

# --- Definição Dinâmica de Cores ---
cores_estados <- reactive({
  req(input$comp_ufs)
  
  estados_sel <- sort(unique(input$comp_ufs))
  n_estados <- length(estados_sel)
  
  if (n_estados == 0) return(NULL)
  
  # Gera uma paleta de cores distinta baseada no número de estados
  # Usamos a paleta 'Set1' ou 'Dark2' do RColorBrewer para bom contraste, ou hue_pal padrão
  cols <- scales::hue_pal()(n_estados)
  
  # Nomeia o vetor para garantir que cada Estado tenha sua cor fixa
  names(cols) <- estados_sel
  return(cols)
})

# 3. Gráfico 1: Evolução Mensal (Linhas Múltiplas)
output$comp_evolucao_mensal <- renderPlotly({
  req(dados_comparativo(), cores_estados())
  
  df_viz <- dados_comparativo() %>%
    mutate(Mes = floor_date(Data, "month")) %>%
    group_by(Mes, Estado) %>%
    summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Rotulo = paste0(
        "<b>Estado:</b> ", Estado, "<br>",
        "<b>Mês:</b> ", format(Mes, "%b/%Y"), "<br>",
        "<b>Gasto:</b> R$ ", format(Total, big.mark=".", decimal.mark=",", nsmall=2)
      )
    )
  
  if(nrow(df_viz) == 0) return(NULL)
  
  p <- ggplot(df_viz, aes(x = Mes, y = Total, color = Estado, group = Estado)) +
    geom_line(linewidth = 1, alpha = 0.9) +
    # Pontos levemente menores para não poluir se tiver muitos estados
    geom_point(aes(text = Rotulo), size = 2, shape = 21, fill = "white", stroke = 1) +
    
    scale_color_manual(values = cores_estados()) +
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    
    labs(x = NULL, y = NULL, color = "UF") + # Legenda ativada
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text = element_text(color = "#495057")
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "x unified")
})

# 4. Gráfico 2: Comparativo de Total Gasto (Barras Múltiplas)
output$comp_barras_total <- renderPlotly({
  req(dados_comparativo(), cores_estados())
  
  df_bar <- dados_comparativo() %>%
    group_by(Estado) %>%
    summarise(Total = sum(Valor, na.rm = TRUE)) %>%
    mutate(Rotulo = paste0("<b>Estado:</b> ", Estado, "<br>",
                           "<b>Total Gasto:</b> R$ ", format(Total, big.mark=".", decimal.mark=",", nsmall=2)))
  
  if(nrow(df_bar) == 0) return(NULL)
  
  p <- ggplot(df_bar, aes(x = Estado, y = Total, fill = Estado, text = Rotulo)) +
    geom_col(width = 0.6, alpha = 0.9, color = "white") + # Adicionei o "+" que faltava aqui também
    
    scale_fill_manual(values = cores_estados()) +
    # Ajuste scale se quiser remover o sufixo "M" ou manter
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",", scale = 1e-6, suffix = " M")) +
    
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  ggplotly(p, tooltip = "text")
})

output$comp_barras_percapita <- renderPlotly({
  
  # 1. Requerimentos: Dados filtrados e cores definidas
  req(dados_comparativo(), cores_estados())
  
  # 2. Preparação dos dados
  df_percapita <- dados_comparativo() %>%
    group_by(Estado) %>%
    summarise(
      Total_Valor = sum(Valor, na.rm = TRUE),
      
      # Importante: Usamos max() pois a população repete em todas as linhas do estado.
      # Se somássemos, o valor ficaria incorreto.
      Populacao = max(as.numeric(Populacao), na.rm = TRUE) 
    ) %>%
    mutate(
      # Cálculo do indicador
      Gasto_Per_Capita = Total_Valor / Populacao,
      
      # Texto para o Tooltip interativo
      Rotulo = paste0(
        "<b>Estado:</b> ", Estado, "<br>",
        "<b>População:</b> ", format(Populacao, big.mark=".", decimal.mark=","), "<br>",
        "<b>Total:</b> R$ ", format(Total_Valor, big.mark=".", decimal.mark=",", scale=1e-6, suffix="M"), "<br>",
        "<b>Per Capita:</b> R$ ", format(round(Gasto_Per_Capita, 2), big.mark=".", decimal.mark=",")
      )
    ) %>%
    # Remove casos onde não há população ou valor (evita erros no gráfico)
    filter(Gasto_Per_Capita > 0 & !is.na(Gasto_Per_Capita))
  
  if(nrow(df_percapita) == 0) return(NULL)
  
  # 3. Construção do Gráfico (ggplot2)
  p <- ggplot(df_percapita, aes(x = Estado, y = Gasto_Per_Capita, fill = Estado, text = Rotulo)) +
    geom_col(width = 0.6, alpha = 0.9, color = "white") +
    
    # Mantém a mesma paleta de cores dos outros gráficos da aba
    scale_fill_manual(values = cores_estados()) +
    
    # Formatação do eixo Y em Reais
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    
    labs(x = NULL, y = NULL) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  # 4. Conversão para Plotly
  ggplotly(p, tooltip = "text")
})

outputOptions(output, "grafico_top10", suspendWhenHidden = FALSE)
outputOptions(output, "grafico_acumulado", suspendWhenHidden = FALSE)
outputOptions(output, "grafico_dist_preco", suspendWhenHidden = FALSE)
  

  # =========================
  #  ABA: PERFIL DOS FORNECEDORES
  # =========================

  # --- Atualiza filtros (Fonte -> Ano -> Medicamento -> UF) ---

  # 1) Fonte -> Ano
  observeEvent(list(dados_raw(), input$sel_base_forn), {
    req(dados_raw())
    df <- dados_raw()

    base_sel <- input$sel_base_forn
    if (is.null(base_sel) || !base_sel %in% unique(df$Base)) {
      base_sel <- sort(unique(df$Base))[1]
    }

    df_base <- df %>% dplyr::filter(Base == base_sel)

    anos_disp <- df_base %>%
      dplyr::filter(!is.na(Data)) %>%
      dplyr::mutate(ano = as.character(lubridate::year(Data))) %>%
      dplyr::pull(ano) %>%
      unique() %>%
      sort()

    ano_atual <- isolate(input$sel_ano_forn)
    if (is.null(ano_atual) || !(ano_atual %in% anos_disp)) {
      ano_atual <- if (length(anos_disp) > 0) max(anos_disp) else NULL
    }

    shiny::freezeReactiveValue(input, "sel_ano_forn")
    updateSelectizeInput(
      session, "sel_ano_forn",
      choices  = anos_disp,
      selected = ano_atual,
      server   = TRUE
    )
  }, ignoreInit = FALSE)

  # 2) Ano -> Medicamento
  observeEvent(list(dados_raw(), input$sel_base_forn, input$sel_ano_forn), {
    req(dados_raw())
    df <- dados_raw()

    req(input$sel_base_forn)
    req(input$sel_ano_forn)

    df_ano <- df %>%
      dplyr::filter(
        Base == input$sel_base_forn,
        lubridate::year(Data) == as.numeric(input$sel_ano_forn)
      )

    meds_disp <- sort(unique(df_ano$Medicamento))
    med_atual <- isolate(input$sel_med_forn)
    if (is.null(med_atual) || !(med_atual %in% meds_disp)) {
      med_atual <- if (length(meds_disp) > 0) meds_disp[1] else NULL
    }

    shiny::freezeReactiveValue(input, "sel_med_forn")
    updateSelectizeInput(
      session, "sel_med_forn",
      choices  = meds_disp,
      selected = med_atual,
      server   = TRUE
    )
  }, ignoreInit = FALSE)

  # 3) Medicamento -> UF
  observeEvent(list(dados_raw(), input$sel_base_forn, input$sel_ano_forn, input$sel_med_forn), {
    req(dados_raw())
    df <- dados_raw()

    req(input$sel_base_forn)
    req(input$sel_ano_forn)
    req(input$sel_med_forn)

    df_filtro <- df %>%
      dplyr::filter(
        Base == input$sel_base_forn,
        lubridate::year(Data) == as.numeric(input$sel_ano_forn),
        Medicamento == input$sel_med_forn
      )

    ufs_disp <- sort(unique(df_filtro$Estado))
    uf_atual <- isolate(input$sel_uf_forn)
    if (!is.null(uf_atual) && length(uf_atual) > 0) {
      uf_atual <- intersect(uf_atual, ufs_disp)
    }

    shiny::freezeReactiveValue(input, "sel_uf_forn")
    updateSelectizeInput(
      session, "sel_uf_forn",
      choices  = ufs_disp,
      selected = uf_atual,
      server   = TRUE
    )
  }, ignoreInit = FALSE)

  # --- Base filtrada para a aba ---
  dados_forn <- reactive({
    req(dados_raw())
    df <- dados_raw()

    req(input$sel_base_forn)
    df <- df %>% filter(Base == input$sel_base_forn)

    req(input$sel_ano_forn)
    df <- df %>% filter(lubridate::year(Data) == as.numeric(input$sel_ano_forn))

    req(input$sel_med_forn)
    df <- df %>% filter(Medicamento == input$sel_med_forn)

    # UF pode ser "todas"
    if (!is.null(input$sel_uf_forn) && length(input$sel_uf_forn) > 0) {
      df <- df %>% filter(Estado %in% input$sel_uf_forn)
    }

    # preço unitário
    df <- df %>%
      mutate(
        preco_unit = dplyr::if_else(!is.na(Quantidade) & Quantidade > 0,
                                    Valor / Quantidade,
                                    NA_real_)
      )

    df
  })

  # 1) Maiores fornecedores por valor
  output$grafico_fornecedor_valor <- renderPlotly({
    req(dados_forn())
    df <- dados_forn()

    top_valor <- df %>%
      group_by(Fornecedor) %>%
      summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Valor_Total)) %>%
      slice_head(n = 10)

    req(nrow(top_valor) > 0)

    top_valor <- top_valor %>%
      arrange(Valor_Total) %>%
      mutate(
        Fornecedor = factor(Fornecedor, levels = Fornecedor)
    )

    plot_ly(
      data = top_valor,
      x = ~Valor_Total,
      y = ~Fornecedor,
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Valor total: R$ %{x:,.2f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(l = 220, r = 20, t = 20, b = 40)
      )
  })

  # 2) Maiores fornecedores por quantidade
  output$grafico_fornecedor_qtd <- renderPlotly({
    req(dados_forn())
    df <- dados_forn()

    top_qtd <- df %>%
      group_by(Fornecedor) %>%
      summarise(Quantidade_Total = sum(Quantidade, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Quantidade_Total)) %>%
      slice_head(n = 10)

    req(nrow(top_qtd) > 0)

    top_qtd <- top_qtd %>%
      arrange(Quantidade_Total) %>%
      mutate(
        Fornecedor = factor(Fornecedor, levels = Fornecedor)
    )

    plot_ly(
      data = top_qtd,
      x = ~Quantidade_Total,
      y = ~Fornecedor,
      type = "bar",
      orientation = "h",
      marker = list(color = "orange"),
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Quantidade total: %{x:,.0f}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(l = 220, r = 20, t = 20, b = 40)
      )
  })

  # 3) Medidas de preço:
  output$tabela_medidas_preco_dropdown <- DT::renderDT({
    req(dados_forn())
    df <- dados_forn()

    tab <- df %>%
      dplyr::filter(!is.na(preco_unit), is.finite(preco_unit)) %>%
      dplyr::group_by(Fornecedor) %>%
      dplyr::summarise(
        n = dplyr::n(),
        preco_medio   = mean(preco_unit, na.rm = TRUE),
        mediana       = stats::median(preco_unit, na.rm = TRUE),
        minimo        = min(preco_unit, na.rm = TRUE),
        maximo        = max(preco_unit, na.rm = TRUE),
        desvio_padrao = stats::sd(preco_unit, na.rm = TRUE),
        cv            = dplyr::if_else(is.finite(preco_medio) & preco_medio != 0, desvio_padrao / preco_medio, NA_real_),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(preco_medio), !is.na(preco_medio)) %>%
      dplyr::arrange(preco_medio) %>%
      dplyr::mutate(
        preco_medio   = round(preco_medio, 2),
        mediana       = round(mediana, 2),
        minimo        = round(minimo, 2),
        maximo        = round(maximo, 2),
        desvio_padrao = round(desvio_padrao, 2),
        cv            = round(cv, 2)) %>%
      rename("Observações" = n,
             "Preço Médio" = preco_medio,
             "Mediana"     = mediana,
             "Preço Mínimo" = minimo,
             "Preço Máximo" = maximo,
             "Desvio Padrão" = desvio_padrao,
             "Coeficiente de Variação" = cv)

    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthChange = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/pt-BR.json"
        )
      )
    )
  })


# =====================================================================================
# --- BLOCO DA PÁGINA: REPASSES FEDERAIS ---
# =====================================================================================

# 0) Leitura da base de repasses (anual, por UF e medicamento)
repasses_raw <- reactive({

  caminho_repasse <- "df/repasses.csv"

  if (!file.exists(caminho_repasse)) {
    showNotification(paste("Arquivo não encontrado:", caminho_repasse), type = "error", duration = NULL)
    return(NULL)
  }

  tryCatch({
    df <- read.csv(caminho_repasse, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

    # Padronizações mínimas
    df <- df %>%
      dplyr::mutate(
        Ano = as.integer(Ano),
        Estado = toupper(trimws(as.character(Estado))),
        Medicamento = toupper(trimws(as.character(Medicamento))),
        Apresentacao = as.character(Apresentacao),
        Valor = suppressWarnings(as.numeric(Valor))
      )

    df
  }, error = function(e) {
    showNotification(paste("Erro ao ler repasses.csv:", e$message), type = "error", duration = NULL)
    return(NULL)
  })
})

# Função auxiliar: Gini
gini_coef <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  if (sum(x) == 0) return(0)
  x <- sort(x)
  n <- length(x)
  G <- (2 * sum((1:n) * x)) / (n * sum(x)) - (n + 1) / n
  as.numeric(G)
}

# 1) Atualizar filtros (cascata: Medicamento -> Apresentação -> Ano -> UFs)
observe({
  req(repasses_raw())

  meds <- sort(unique(repasses_raw()$Medicamento))
  choices <- c("Todos", meds)

  sel <- isolate(input$rep_med)
  if (is.null(sel) || !(sel %in% choices)) sel <- "Todos"

  updateSelectizeInput(session, "rep_med", choices = choices, selected = sel, server = TRUE)
})

observe({
  req(repasses_raw(), input$rep_med)

  df <- repasses_raw()
  if (!is.null(input$rep_med) && input$rep_med != "Todos") {
    df <- df %>% dplyr::filter(Medicamento == input$rep_med)
  }

  apres <- sort(unique(df$Apresentacao))
  choices <- c("Todos", apres)

  sel <- isolate(input$rep_apres)
  if (is.null(sel) || !(sel %in% choices)) sel <- "Todos"

  updateSelectizeInput(session, "rep_apres", choices = choices, selected = sel, server = TRUE)
})

observe({
  req(repasses_raw(), input$rep_med, input$rep_apres)

  df <- repasses_raw()
  if (!is.null(input$rep_med) && input$rep_med != "Todos") {
    df <- df %>% dplyr::filter(Medicamento == input$rep_med)
  }
  if (!is.null(input$rep_apres) && input$rep_apres != "Todos") {
    df <- df %>% dplyr::filter(Apresentacao == input$rep_apres)
  }

  anos <- sort(unique(df$Ano), decreasing = TRUE)
  choices <- c("Todos", anos)

  sel <- isolate(input$rep_ano)
  if (is.null(sel) || !(sel %in% as.character(choices))) sel <- "Todos"

  updateSelectizeInput(session, "rep_ano", choices = choices, selected = sel, server = TRUE)
})

observe({
  req(repasses_raw(), input$rep_med, input$rep_apres, input$rep_ano)

  df <- repasses_raw()
  if (!is.null(input$rep_med) && input$rep_med != "Todos") {
    df <- df %>% dplyr::filter(Medicamento == input$rep_med)
  }
  if (!is.null(input$rep_apres) && input$rep_apres != "Todos") {
    df <- df %>% dplyr::filter(Apresentacao == input$rep_apres)
  }
  if (!is.null(input$rep_ano) && input$rep_ano != "Todos") {
    df <- df %>% dplyr::filter(Ano == as.integer(input$rep_ano))
  }

  ufs <- sort(unique(df$Estado))
  sel <- isolate(input$rep_ufs)
  sel <- if (!is.null(sel)) intersect(sel, ufs) else character(0)

  updateSelectizeInput(session, "rep_ufs", choices = ufs, selected = sel, server = TRUE)
})

# 2) Base filtrada
repasses_filtrados <- reactive({
  req(repasses_raw(), input$rep_med, input$rep_apres, input$rep_ano)

  df <- repasses_raw()

  if (!is.null(input$rep_med) && input$rep_med != "Todos") {
    df <- df %>% dplyr::filter(Medicamento == input$rep_med)
  }

  if (!is.null(input$rep_apres) && input$rep_apres != "Todos") {
    df <- df %>% dplyr::filter(Apresentacao == input$rep_apres)
  }

  if (!is.null(input$rep_ano) && input$rep_ano != "Todos") {
    df <- df %>% dplyr::filter(Ano == as.integer(input$rep_ano))
  }

  if (!is.null(input$rep_ufs) && length(input$rep_ufs) > 0) {
    df <- df %>% dplyr::filter(Estado %in% input$rep_ufs)
  }

  df %>%
    dplyr::filter(!is.na(Valor), Valor > 0)
})

# 3) Série anual do repasse
output$rep_serie <- renderPlotly({
  req(repasses_filtrados())

  df <- repasses_filtrados() %>%
    dplyr::group_by(Ano) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(Ano) %>%
    dplyr::mutate(
      Rotulo = paste0(
        "<b>Ano:</b> ", Ano, "<br>",
        "<b>Repasse:</b> R$ ", scales::comma(Valor_Total, big.mark = ".", decimal.mark = ",")
      )
    )

  validate(need(nrow(df) > 0, "Sem dados para a série temporal."))

  p <- ggplot(df, aes(x = Ano, y = Valor_Total)) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    geom_point(aes(text = Rotulo), size = 2) +
    scale_y_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = df$Ano) +
    labs(x = NULL, y = NULL) +
    theme_minimal()

  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 40, r = 10, t = 10, b = 35))
})

# Funções auxiliares que retornam o objeto plotly ----
plot_rep_ranking <- function(df_rep) {
  df <- df_rep %>%
    dplyr::group_by(Estado) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(Valor_Total))

  validate(need(nrow(df) > 0, "Sem dados para o ranking."))

  df <- df %>%
    dplyr::mutate(
      Estado = factor(Estado, levels = rev(Estado)),
      Rotulo = paste0(
        "<b>UF:</b> ", Estado, "<br>",
        "<b>Repasse:</b> R$ ", scales::comma(Valor_Total, big.mark = ".", decimal.mark = ",")
      )
    )

  p <- ggplot(df, aes(x = Valor_Total, y = Estado, text = Rotulo)) +
    geom_col(alpha = 0.85, width = 0.7, show.legend = FALSE) +
    scale_x_continuous(labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  ggplotly(p, tooltip = "text") %>%
    layout(margin = list(l = 50, r = 10, t = 10, b = 25))
}

plot_rep_mapa <- function(df_rep, malha_brasil_global) {
  df_uf <- df_rep %>%
    dplyr::group_by(Estado) %>%
    dplyr::summarise(Valor_Total = sum(Valor, na.rm = TRUE), .groups = "drop")

  df_map <- malha_brasil_global %>%
    dplyr::left_join(df_uf, by = c("abbrev_state" = "Estado")) %>%
    dplyr::mutate(
      Texto_Tooltip = ifelse(
        is.na(Valor_Total),
        paste0("<b>Estado:</b> ", name_state, " (", abbrev_state, ")<br>Sem dados"),
        paste0(
          "<b>Estado:</b> ", name_state, " (", abbrev_state, ")<br>",
          "<b>Repasse:</b> R$ ", scales::comma(Valor_Total, big.mark = ".", decimal.mark = ",")
        )
      )
    )

  p <- ggplot(df_map) +
    geom_sf(aes(fill = Valor_Total, text = Texto_Tooltip), color = "white", size = 0.1) +
    scale_fill_gradient(
      low  = "#C5E8B7",
      high = "#198754",
      na.value = "#e9ecef",
      name = "Repasse Federal (R$)",
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 8)
    )

  ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = FALSE) %>%
    plotly::partial_bundle()
}

# Output único que alterna pelo dropdown ----
output$rep_viz <- renderPlotly({
  req(repasses_filtrados())
  req(input$rep_viz_tipo)

  df_rep <- repasses_filtrados()

  if (identical(input$rep_viz_tipo, "mapa")) {
    req(malha_brasil_global)
    return(plot_rep_mapa(df_rep, malha_brasil_global))
  }

  # default: ranking
  plot_rep_ranking(df_rep)
})
  
# =======================================================
# --- REPASSE x GASTO (LAI/PNCP) POR UF ---
# =======================================================

output$rep_gasto_repasse <- renderPlotly({
  req(repasses_filtrados(), dados_raw())
  req(input$rep_gasto_base)  # vem do selectInput no global.R

  # 1) Repasse (já vem filtrado pelos mesmos inputs rep_* que você usa na aba)
  df_rep <- repasses_filtrados() %>%
    dplyr::group_by(Estado) %>%
    dplyr::summarise(Repasse = sum(Valor, na.rm = TRUE), .groups = "drop")

  # Helper: aplica os MESMOS filtros do repasse na base de gastos (dados_raw)
  filtra_gasto <- function(base) {
    df <- dados_raw() %>%
      dplyr::filter(Base == base)

    # Medicamento
    if (!is.null(input$rep_med) && input$rep_med != "Todos") {
      df <- df %>% dplyr::filter(Medicamento == input$rep_med)
    }

    # Apresentação (se existir na base completa; se não existir, isso simplesmente não roda)
    if (!is.null(input$rep_apres) && input$rep_apres != "Todos" && "Apresentacao" %in% names(df)) {
      df <- df %>% dplyr::filter(Apresentacao == input$rep_apres)
    }

    # Ano (na base completa normalmente é Data; então filtramos pelo year(Data))
    if (!is.null(input$rep_ano) && input$rep_ano != "Todos" && "Data" %in% names(df)) {
      df <- df %>%
        dplyr::filter(!is.na(Data)) %>%
        dplyr::filter(lubridate::year(Data) == as.integer(input$rep_ano))
    }

    # UF
    if (!is.null(input$rep_ufs) && length(input$rep_ufs) > 0) {
      df <- df %>% dplyr::filter(Estado %in% input$rep_ufs)
    }

    df %>%
      dplyr::group_by(Estado) %>%
      dplyr::summarise(Gasto = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.na(Gasto), Gasto > 0)
  }

  df_lai  <- filtra_gasto("LAI")  %>% dplyr::rename(LAI  = Gasto)
  df_pncp <- filtra_gasto("PNCP") %>% dplyr::rename(PNCP = Gasto)

  # 3) Junta tudo por UF e calcula proporções
  df <- df_rep %>%
    dplyr::left_join(df_lai,  by = "Estado") %>%
    dplyr::left_join(df_pncp, by = "Estado") %>%
    dplyr::mutate(
      prop_LAI  = dplyr::if_else(!is.na(LAI)  & LAI  > 0, Repasse / LAI,  NA_real_),
      prop_PNCP = dplyr::if_else(!is.na(PNCP) & PNCP > 0, Repasse / PNCP, NA_real_)
    )

  validate(need(nrow(df) > 0, "Sem dados para calcular a relação Repasse/Gasto com os filtros selecionados."))

  # Tooltips
  df <- df %>%
    dplyr::mutate(
      tip_LAI = paste0(
        "<b>UF:</b> ", Estado, "<br>",
        "<b>Repasse:</b> R$ ", scales::comma(Repasse, big.mark=".", decimal.mark=","), "<br>",
        "<b>Gasto (LAI):</b> R$ ", scales::comma(LAI, big.mark=".", decimal.mark=","), "<br>",
        "<b>Proporção:</b> ", scales::percent(prop_LAI, accuracy = 0.1)
      ),
      tip_PNCP = paste0(
        "<b>UF:</b> ", Estado, "<br>",
        "<b>Repasse:</b> R$ ", scales::comma(Repasse, big.mark=".", decimal.mark=","), "<br>",
        "<b>Gasto (PNCP):</b> R$ ", scales::comma(PNCP, big.mark=".", decimal.mark=","), "<br>",
        "<b>Proporção:</b> ", scales::percent(prop_PNCP, accuracy = 0.1)
      )
    )

  # 4) Escolhe a base (LAI/PNCP) pelo selectInput do header e ordena do maior pro menor
  if (identical(input$rep_gasto_base, "PNCP")) {
    df_plot <- df %>%
      dplyr::filter(!is.na(prop_PNCP)) %>%
      dplyr::arrange(dplyr::desc(prop_PNCP)) %>%
      dplyr::mutate(
        Estado = factor(Estado, levels = Estado),
        y = prop_PNCP,
        tip = tip_PNCP
      )
  } else {
    df_plot <- df %>%
      dplyr::filter(!is.na(prop_LAI)) %>%
      dplyr::arrange(dplyr::desc(prop_LAI)) %>%
      dplyr::mutate(
        Estado = factor(Estado, levels = Estado),
        y = prop_LAI,
        tip = tip_LAI
      )
  }

  validate(need(nrow(df_plot) > 0, "Sem dados (na base selecionada) para calcular a proporção com os filtros atuais."))

  # 5) Plot (um trace só)
  plotly::plot_ly(
    data = df_plot,
    x = ~Estado, y = ~y,
    type = "bar",
    text = ~tip,
    hoverinfo = "text",
    textposition = "none"
  ) %>%
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "", tickformat = ".0%", rangemode = "tozero"),
      margin = list(l = 40, r = 15, t = 40, b = 90),
      title = list(text = "", x = 0)
    ) %>%
    plotly::config(displayModeBar = FALSE)
})


}


