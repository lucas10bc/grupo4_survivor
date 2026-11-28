library(shiny)
library(tidyverse)

# ---------------------------------------------------------------
# Dados
# ---------------------------------------------------------------
base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-06-01/"

summary_df <- read_csv(paste0(base_url, "summary.csv"), show_col_types = FALSE)
viewers    <- read_csv(paste0(base_url, "viewers.csv"), show_col_types = FALSE)


temporadas <- sort(unique(viewers$season))

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- navbarPage(
  title = "Survivor",
  
  # Audiência por episódio ---------------------------------------
  tabPanel("Audiência por temporada",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("temps_aud", "Temporadas:", choices = temporadas,
                              selected = c(1, 20, 40), multiple = TRUE)
             ),
             mainPanel(
               uiOutput("medias_aud"),
               plotOutput("plot_audiencia", click = "click_ep"),
               helpText("Clique em um ponto para ver os detalhes do episódio."),
               uiOutput("info_episodio")
             )
           )
  ),
  
  # Estreia x final x reunião ------------------------------------
  tabPanel("Estreia, final e reunião",
           sidebarLayout(
             sidebarPanel(
               radioButtons("var_y", "Comparar a audiência da estreia com:",
                            choices = c("Final" = "viewers_finale",
                                        "Reunião" = "viewers_reunion",
                                        "Média da temporada" = "viewers_mean")),
               checkboxInput("mostrar_reta", "Mostrar reta de regressão", TRUE),
               radioButtons("metodo_cor", "Método de correlação:",
                            choices = c("Pearson" = "pearson", "Spearman" = "spearman")),
               sliderInput("temp_min", "Usar temporadas a partir da:",
                           min = min(temporadas), max = max(temporadas) - 5,
                           value = min(temporadas), step = 1)
             ),
             mainPanel(
               plotOutput("plot_estreia", click = "click_temp"),
               helpText("Clique em um ponto para ver os detalhes da temporada."),
               uiOutput("info_temporada"),
               uiOutput("cor_estreia")
             )
           )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # audiência por temporada 
  dados_aud <- reactive({
    req(input$temps_aud)
    viewers %>%
      filter(season %in% input$temps_aud) %>%
      mutate(temporada = factor(season))
  })
  
  # Visor com as médias das temporadas selecionadas
  output$medias_aud <- renderUI({
    d <- dados_aud()
    media_viewers <- mean(d$viewers, na.rm = TRUE)
    media_nota    <- mean(d$rating_18_49, na.rm = TRUE)
    
    caixa <- function(valor, legenda) {
      div(
        style = "flex:1; text-align:center; padding:12px; background:#f5f5f5;
                 border-radius:8px;",
        div(style = "font-size:40px; font-weight:bold; line-height:1;", valor),
        div(style = "font-size:14px; color:#666; margin-top:6px;", legenda)
      )
    }
    
    n_temp <- n_distinct(d$season)
    div(
      style = "margin-bottom:15px;",
      div(
        style = "display:flex; gap:12px;",
        caixa(format(round(media_viewers, 2), nsmall = 2),
              "Média de audiência (milhões)"),
        caixa(format(round(media_nota, 2), nsmall = 2),
              "Média da nota (rating 18-49)")
      ),
      div(style = "font-size:12px; color:#888; margin-top:4px; text-align:center;",
          paste0("Baseado em ", n_temp, ifelse(n_temp == 1, " temporada", " temporadas"),
                 " (", nrow(d), " episódios)"))
    )
  })
  
  output$plot_audiencia <- renderPlot({
    ggplot(dados_aud(), aes(episode, viewers, color = temporada, group = temporada)) +
      geom_line() + geom_point(size = 3) +
      labs(x = "Episódio", y = "Audiência (milhões)", color = "Temporada") +
      theme_minimal(base_size = 14)
  })
  
  # Detalhes do episódio clicado
  output$info_episodio <- renderUI({
    req(input$click_ep)
    ep <- nearPoints(dados_aud(), input$click_ep,
                     xvar = "episode", yvar = "viewers",
                     threshold = 15, maxpoints = 1)
    req(nrow(ep) == 1)
    
    wellPanel(
      fluidRow(
        column(5,
               if (tem_img) {
                 tags$img(src = arquivo_img, style = "max-width:100%; border-radius:6px;")
               } else {
                 div(style = "padding:30px; text-align:center; background:#eee; border-radius:6px;",
                     em("Imagem não encontrada:"), br(), code(paste0("www/", arquivo_img)))
               }
        ),
        column(7,
               h4(ep$title),
               p(strong("Temporada: "), ep$season, " (", ep$season_name, ")"),
               p(strong("Episódio na temporada: "), ep$episode,
                 " | ", strong("Episódio geral: "), ep$episode_number_overall),
               p(strong("Data de exibição: "), format(ep$episode_date, "%d/%m/%Y")),
               p(strong("Audiência: "), ep$viewers, " milhões"),
               p(strong("Rating (18-49): "), ep$rating_18_49),
               p(strong("Share (18-49): "), ep$share_18_49)
        )
      )
    )
  })
  
  # estreia, final e reunião
  dados_estreia <- reactive({
    summary_df %>%
      filter(season >= input$temp_min) %>%
      drop_na(viewers_premier, all_of(input$var_y))
  })
  
  output$plot_estreia <- renderPlot({
    p <- ggplot(dados_estreia(), aes(viewers_premier, .data[[input$var_y]], color = season)) +
      geom_point(size = 3) +
      geom_text(aes(label = season), vjust = -0.8, size = 3, show.legend = FALSE) +
      scale_color_viridis_c() +
      labs(x = "Audiência da estreia (milhões)", y = input$var_y, color = "Temporada") +
      theme_minimal(base_size = 14)
    if (input$mostrar_reta) p <- p + geom_smooth(method = "lm", se = TRUE, color = "black")
    p
  })
  
  # Detalhes da temporada clicada
  output$info_temporada <- renderUI({
    req(input$click_temp)
    t <- nearPoints(dados_estreia(), input$click_temp,
                    xvar = "viewers_premier", yvar = input$var_y,
                    threshold = 15, maxpoints = 1)
    req(nrow(t) == 1)
    
    wellPanel(
      h4(paste0("Temporada ", t$season, ": ", t$season_name)),
      p(strong("Local: "), t$location, " (", t$country, ")"),
      p(strong("Formato das tribos: "), t$tribe_setup),
      p(strong("Vencedor(a): "), t$full_name),
      p(strong("Audiência da estreia: "), t$viewers_premier, " milhões"),
      p(strong("Audiência do final: "), t$viewers_finale, " milhões"),
      p(strong("Audiência da reunião: "), t$viewers_reunion, " milhões")
    )
  })
  
  # Número da correlação em destaque
  output$cor_estreia <- renderUI({
    d <- dados_estreia()
    req(nrow(d) >= 3)
    r <- cor(d$viewers_premier, d[[input$var_y]], method = input$metodo_cor)
    
    div(
      style = "text-align:center; padding:15px; margin-top:10px;
               background:#f5f5f5; border-radius:8px;",
      div(style = "font-size:56px; font-weight:bold; line-height:1;",
          format(round(r, 2), nsmall = 2)),
      div(style = "font-size:14px; color:#666; margin-top:6px;",
          paste("Correlação de", tools::toTitleCase(input$metodo_cor)))
    )
  })
}

shinyApp(ui, server)
