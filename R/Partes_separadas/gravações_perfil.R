library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(plotly)
library(DT)

regiao_de <- function(pais) {
  case_when(
    pais %in% c("Malaysia", "Thailand", "Cambodia", "China", "Philippines") ~ "Ásia",
    pais %in% c("Australia", "Polynesia", "Vanuatu", "Islands", "Cook Islands",
                "Samoa", "Fiji", "Palau") ~ "Oceania",
    pais %in% c("Kenya", "Gabon") ~ "África",
    pais %in% c("Brazil", "Panama", "Guatemala", "Nicaragua") ~ "Américas",
    TRUE ~ "Outras"
  )
}

preparar_temporadas <- function(dados) {
  dados |>
    mutate(
      dias_gravacao    = as.numeric(filming_ended - filming_started) + 1,
      dias_exibicao    = as.numeric(ended - premiered),
      dias_ate_estreia = as.numeric(premiered - filming_ended),
      regiao           = regiao_de(country)
    )
}

preparar_elenco <- function(dados) {
  dados |>
    mutate(
      resultado_grupo = case_when(
        str_detect(result, "Sole Survivor") ~ "Vencedor(a)",
        str_detect(result, "unner-up")      ~ "Finalista",
        !is.na(jury_status)                 ~ "Jurado",
        TRUE                                ~ "Eliminado(a) antes do júri"
      ),
      resultado_grupo = factor(resultado_grupo,
                               levels = c("Vencedor(a)", "Finalista", "Jurado", "Eliminado(a) antes do júri"))
    )
}

df_summary   <- readRDS("data/summary.rds") |> preparar_temporadas()
df_castaways <- readRDS("data/castaways.rds") |> preparar_elenco()

# Tema compartilhado

cores <- list(
  primaria   = "#C2410C",  
  secundaria = "#0F766E",  
  escura     = "#1F2A37",  
  destaque   = "#F5B700",  
  floresta   = "#0B3D3A", 
  areia      = "#FAF7F2",  
  neutra     = "#CBD5E1",  
  vazio      = "#EADFCB"   #bege pra aqueles que não tem participante, todas as outras cores são temporárias
)

tema_survivor <- bs_theme(
  version      = 5,
  bg           = cores$areia,
  fg           = cores$escura,
  primary      = cores$primaria,
  secondary    = cores$secundaria,
  dark         = cores$floresta,
  base_font    = font_google("Poppins"),
  heading_font = font_google("Poppins")
) |>
  bs_add_rules(paste0("
    .navbar { background-color: ", cores$floresta, " !important; }
    .navbar .navbar-brand { color: #F5EFE0 !important; font-weight: 600; }
    .navbar .nav-link { color: #CFE3DF !important; }
    .navbar .nav-link.active {
      color: ", cores$destaque, " !important;
      border-bottom: 2px solid ", cores$destaque, ";
    }
    .sidebar { background-color: #F3EDE0 !important; }
    .card {
      background-color: #FFFFFF;
      border: none;
      box-shadow: 0 2px 10px rgba(31, 42, 55, 0.08);
    }
    .card-header { background-color: transparent; font-weight: 600; }
    .nav-tabs .nav-link.active {
      color: ", cores$primaria, ";
      border-bottom: 2px solid ", cores$primaria, ";
    }
    table.dataTable thead th {
      background-color: ", cores$floresta, ";
      color: #FFFFFF;
    }
  "))

opcoes_dt <- list(
  pageLength = 10,
  scrollX    = TRUE,
  language   = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json")
)


paleta <- function(n) {
  grDevices::colorRampPalette(
    c("#0F766E", "#5EAAA8", "#F5B700", "#E8590C", "#9A3412")
  )(max(n, 2))
}

estilo_plotly <- function(p) {
  p |>
    layout(
      font          = list(family = "Poppins", color = cores$escura),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      hoverlabel    = list(font = list(family = "Poppins")),
      xaxis         = list(gridcolor = "#E9ECEF", zerolinecolor = "#E9ECEF"),
      yaxis         = list(gridcolor = "#E9ECEF", zerolinecolor = "#E9ECEF")
    ) |>
    config(displaylogo = FALSE)
}

# primeira aba aba produção das temporadas
# coordenadas países
coords_pais <- data.frame(
  country = c("Malaysia", "Australia", "Kenya", "Polynesia", "Thailand", "Brazil",
              "Panama", "Vanuatu", "Guatemala", "China", "Palau", "Nicaragua",
              "Samoa", "Gabon", "Islands", "Cook Islands", "Philippines",
              "Cambodia", "Fiji"),
  lat = c(6.3, -19.0, 0.6, -8.9, 7.0, -3.0, 8.5, -17.7, 16.9, 29.3, 7.5, 12.5,
          -13.9, -1.0, -18.9, -18.9, 13.8, 10.7, -17.7),
  lon = c(117.0, 145.0, 37.9, -140.1, 99.0, -60.0, -79.0, 168.3, -89.9, 115.5,
          134.6, -87.0, -171.8, 11.5, -159.8, -159.8, 123.9, 103.3, 177.1)
)

metricas_globo <- c(
  "Número de temporadas"                      = "temporadas",
  "Intervalo médio gravação → estreia (dias)" = "gap_medio",
  "Primeira temporada gravada no país"        = "primeira"
)

ordem_tipo <- c("Novatos", "Veteranos", "Novatos e veteranos", "Outro")

mod_temporadas_ui <- function(id, dados) {
  ns <- NS(id)
  
  div(
    class = "p-3",
    
    card(
      card_body(
        layout_columns(
          col_widths = c(3, 3, 2, 2, 2),
          sliderInput(ns("temporadas"), "Temporadas",
                      min = min(dados$season), max = max(dados$season),
                      value = range(dados$season), step = 1, sep = ""),
          selectizeInput(ns("paises"), "País de gravação",
                         choices = sort(unique(dados$country)),
                         multiple = TRUE,
                         options = list(placeholder = "Todos os países")),
          selectInput(ns("metrica_globo"), "filtro das bolhas", choices = metricas_globo),
          radioButtons(ns("projecao"), "Mapa",
                       choices = c("Globo 3D" = "globo", "Plano" = "plano"),
                       inline = TRUE)
        )
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Onde 'Survivor' foi gravado"),
      p(class = "text-muted px-3 mb-0",
        "Arraste para girar o globo. Tamanho da bolha = temporadas gravadas no país."),
      textOutput(ns("nota_mapa"), container = function(...) p(class = "text-muted px-3 mb-0", ...)),
      plotlyOutput(ns("globo"), height = "600px")
    ),
    
    card(
      full_screen = TRUE,
      card_header("Painel por país de gravação"),
      DTOutput(ns("tabela"))
    )
  )
}

mod_temporadas_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    filtrado <- reactive({
      req(input$temporadas)
      d <- dados |>
        filter(season >= input$temporadas[1], season <= input$temporadas[2])
      if (!is.null(input$paises)) d <- d |> filter(country %in% input$paises)
      if (!is.null(input$tipo))   d <- d |> filter(tipo_elenco %in% input$tipo)
      d
    })
    
    # Países do filtro que não têm coordenada cadastrada
    output$nota_mapa <- renderText({
      sem <- setdiff(unique(filtrado()$country), coords_pais$country)
      if (length(sem) == 0) "" else
        paste("Sem coordenada no mapa (aparecem só na tabela):", paste(sem, collapse = ", "))
    })
    
    # globo 3d
    output$globo <- renderPlotly({
      d <- filtrado() |> arrange(season); req(nrow(d) > 0)
      met <- input$metrica_globo
      rot <- names(metricas_globo)[metricas_globo == met]
      
      p <- d |>
        group_by(country) |>
        summarise(
          temporadas = n(),
          lista      = paste(season, collapse = ", "),
          local      = first(location),
          gap_medio  = round(mean(dias_ate_estreia, na.rm = TRUE)),
          primeira   = min(season),
          .groups = "drop"
        ) |>
        left_join(coords_pais, by = "country") |>
        filter(!is.na(lat))
      req(nrow(p) > 0)
      
      p$z   <- p[[met]]
      p$tam <- 14 + 5 * p$temporadas
      p$tip <- paste0("<b>", p$country, "</b><br>", p$local,
                      "<br>Temporadas (", p$temporadas, "): ", p$lista,
                      "<br>Intervalo médio até a estreia: ", p$gap_medio, " dias")
      
      proj <- if (input$projecao == "globo") {
        list(type = "orthographic", rotation = list(lon = 140, lat = -5))
      } else {
        list(type = "natural earth")
      }
      
      plot_ly(p, type = "scattergeo", mode = "markers",
              lat = ~lat, lon = ~lon, text = ~tip, hoverinfo = "text",
              marker = list(size = ~tam, color = ~z, colorscale = escala_calor,
                            showscale = TRUE, colorbar = list(title = rot, len = 0.7),
                            opacity = 0.92,
                            line = list(color = "white", width = 1.5))) |>
        layout(geo = list(
          projection = proj,
          showland = TRUE, landcolor = "#E9E1D0",
          showocean = TRUE, oceancolor = "#D6ECEA",
          showcountries = TRUE, countrycolor = "#FFFFFF",
          showcoastlines = TRUE, coastlinecolor = "#C9BFA8",
          bgcolor = "rgba(0,0,0,0)"),
          margin = list(l = 0, r = 0, t = 0, b = 0)) |>
        estilo_plotly()
    })
    
    # tabela
    output$tabela <- renderDT({
      d <- filtrado() |> arrange(season); req(nrow(d) > 0)
      
      res <- d |>
        group_by(country) |>
        summarise(
          temporadas = n(),
          lista      = paste(season, collapse = ", "),
          locais     = paste(unique(location), collapse = " | "),
          gap        = round(mean(dias_ate_estreia, na.rm = TRUE)),
          grav       = round(mean(dias_gravacao, na.rm = TRUE), 1),
          .groups = "drop"
        ) |>
        arrange(desc(temporadas), country) |>
        select(
          "País"                                 = country,
          "Temporadas"                           = temporadas,
          "Quais temporadas"                     = lista,
          "Locais de gravação"                   = locais,
          "Intervalo médio até a estreia (dias)" = gap,
          "Duração média da gravação (dias)"     = grav
        )
      
      maxi <- function(x) max(1, x, na.rm = TRUE)
      barra <- function(tab, coluna, cor) {
        formatStyle(tab, coluna,
                    background = styleColorBar(c(0, maxi(res[[coluna]])), cor),
                    backgroundSize = "98% 70%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
      }
      
      datatable(res, rownames = FALSE, class = "hover compact",
                options = modifyList(opcoes_dt, list(pageLength = 8, dom = "tp"))) |>
        barra("Temporadas", "#F5B700") |>
        barra("Intervalo médio até a estreia (dias)", "#9CCFC9") |>
        barra("Duração média da gravação (dias)", "#E7D6BD")
    })
  })
}
# perfil do elenco
cores_res <- c(
  "Vencedor(a)"                = "#F5B700",  # todas as cores são temporárias
  "Finalista"                  = "#C2410C",  
  "Jurado"                     = "#0F766E",  
  "Eliminado(a) antes do júri" = "#CBD5E1"   
)

escala_calor <- list(
  list(0,    "#FFF7E6"),
  list(0.25, "#FDD069"),
  list(0.5,  "#F28C28"),
  list(0.75, "#C2410C"),
  list(1,    "#7C2D12")
)

metricas_mapa <- c(
  "Número de participantes" = "participantes",
  "Idade média"             = "idade_media",
  "% que chegou ao júri"    = "pct_juri",
  "Número de vencedores"    = "vencedores"
)

# Resume o elenco por estado 
resumir_estados <- function(d) {
  d |>
    filter(!is.na(state)) |>
    mutate(sigla = ifelse(state == "District of Columbia", "DC",
                          state.abb[match(state, state.name)])) |>
    filter(!is.na(sigla)) |>
    group_by(state, sigla) |>
    summarise(
      participantes = n(),
      idade_media   = round(mean(age, na.rm = TRUE), 1),
      vencedores    = sum(resultado_grupo == "Vencedor(a)"),
      pct_juri      = round(mean(resultado_grupo != "Eliminado(a) antes do júri") * 100, 1),
      .groups = "drop"
    )
}

mod_elenco_ui <- function(id, dados) {
  ns <- NS(id)
  idades <- range(dados$age, na.rm = TRUE)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Filtros",
      sliderInput(ns("temporadas"), "Temporadas",
                  min = min(dados$season), max = max(dados$season),
                  value = range(dados$season), step = 1, sep = ""),
      sliderInput(ns("idade"), "Faixa de idade",
                  min = idades[1], max = idades[2], value = idades, step = 1),
      selectizeInput(ns("personalidade"), "Tipo de personalidade",
                     choices = sort(unique(na.omit(dados$personality_type))),
                     multiple = TRUE,
                     options = list(placeholder = "Todos os tipos")),
      selectizeInput(ns("resultado"), "Resultado",
                     choices = names(cores_res),
                     multiple = TRUE,
                     options = list(placeholder = "Todos os resultados")),
      hr(),
      selectInput(ns("metrica_mapa"), "Mapa: o que colorir", choices = metricas_mapa),
      radioButtons(ns("modo_calor"), "Grade: mostrar",
                   choices = c("Percentual da linha" = "pct", "Número de pessoas" = "n")),
      checkboxInput(ns("pontos"), "Violino: mostrar cada participante", value = TRUE)
    ),
    
    layout_columns(
      fill = FALSE,
      value_box("Participantes", textOutput(ns("vb_n")), theme = "primary"),
      value_box("Idade média", textOutput(ns("vb_media")), theme = "secondary"),
      value_box("Estado mais frequente", textOutput(ns("vb_estado")), theme = "dark"),
      value_box("Personalidade mais comum", textOutput(ns("vb_pers")), theme = "primary")
    ),
    
    navset_card_tab(
      full_screen = TRUE,
      title = "Quem participa de 'Survivor'?",
      
      nav_panel(
        "De onde vêm",
        p(class = "text-muted",
          "Mapa de calor dos estados de origem. Passe o mouse para ver os detalhes de cada estado. Estados em bege não tiveram participantes com os filtros atuais."),
        plotlyOutput(ns("mapa"), height = "460px"),
        h6("Ranking de estados"),
        DTOutput(ns("tabela_estados"))
      ),
      
      nav_panel(
        "Personalidade × resultado",
        p(class = "text-muted",
          "Cada linha é um tipo de personalidade. A cor mostra a intensidade relativa dentro de cada coluna; os números são os valores reais."),
        plotlyOutput(ns("mapa_calor"), height = "600px"),
        h6("Resumo por tipo de personalidade"),
        DTOutput(ns("tabela_pers"))
      ),
      
      nav_panel(
        "Idade e resultado",
        p(class = "text-muted",
          "Distribuição de idade de quem venceu, chegou à final, ao júri ou foi eliminado antes. A linha tracejada é a média."),
        plotlyOutput(ns("violino"), height = "460px"),
        h6("Participantes"),
        DTOutput(ns("tabela"))
      )
    )
  )
}

mod_elenco_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    filtrado <- reactive({
      req(input$temporadas, input$idade)
      d <- dados |>
        filter(season >= input$temporadas[1], season <= input$temporadas[2],
               age >= input$idade[1], age <= input$idade[2])
      if (!is.null(input$personalidade)) {
        d <- d |> filter(personality_type %in% input$personalidade)
      }
      if (!is.null(input$resultado)) {
        d <- d |> filter(resultado_grupo %in% input$resultado)
      }
      d
    })
    
    por_estado <- reactive(resumir_estados(filtrado()))
    
    # caixas de resumo
    output$vb_n <- renderText(nrow(filtrado()))
    
    output$vb_media <- renderText({
      d <- filtrado(); req(nrow(d) > 0)
      paste(round(mean(d$age, na.rm = TRUE), 1), "anos")
    })
    
    output$vb_estado <- renderText({
      est <- na.omit(filtrado()$state); req(length(est) > 0)
      names(sort(table(est), decreasing = TRUE))[1]
    })
    
    output$vb_pers <- renderText({
      p <- na.omit(filtrado()$personality_type); req(length(p) > 0)
      names(sort(table(p), decreasing = TRUE))[1]
    })
    
    # heatmap
    output$mapa <- renderPlotly({
      e <- por_estado(); req(nrow(e) > 0)
      met <- input$metrica_mapa
      rot <- names(metricas_mapa)[metricas_mapa == met]
      e$z <- e[[met]]
      e$tip <- paste0("<b>", e$state, "</b><br>",
                      "Participantes: ", e$participantes, "<br>",
                      "Idade média: ", e$idade_media, " anos<br>",
                      "Chegaram ao júri: ", e$pct_juri, "%<br>",
                      "Vencedores: ", e$vencedores)
      
      # aqueles que estão sem nenhum participante no filtro atual
      todos <- data.frame(
        sigla = c(state.abb, "DC"),
        state = c(state.name, "District of Columbia")
      )
      vazios <- todos |> filter(!sigla %in% e$sigla)
      vazios$z <- 0
      vazios$tip <- paste0("<b>", vazios$state, "</b><br>Sem participantes")
      
      p <- plot_ly()
      
      # c1 estados sem participantes 
      if (nrow(vazios) > 0) {
        p <- p |>
          add_trace(data = vazios, type = "choropleth", locationmode = "USA-states",
                    locations = ~sigla, z = ~z,
                    colorscale = list(list(0, cores$vazio), list(1, cores$vazio)),
                    showscale = FALSE, text = ~tip, hoverinfo = "text",
                    marker = list(line = list(color = "white", width = 1)))
      }
      
      # c2 estados com participantes (escala de calor)
      p |>
        add_trace(data = e, type = "choropleth", locationmode = "USA-states",
                  locations = ~sigla, z = ~z, text = ~tip, hoverinfo = "text",
                  colorscale = escala_calor,
                  marker = list(line = list(color = "white", width = 1)),
                  colorbar = list(title = rot)) |>
        layout(geo = list(scope = "usa",
                          projection = list(type = "albers usa"),
                          showlakes = TRUE, lakecolor = "#FFFFFF",
                          bgcolor = "rgba(0,0,0,0)", landcolor = cores$vazio),
               margin = list(l = 0, r = 0, t = 0, b = 0)) |>
        estilo_plotly()
    })
    
    output$tabela_estados <- renderDT({
      d <- por_estado() |>
        arrange(desc(participantes)) |>
        select(
          "Estado"               = state,
          "Participantes"        = participantes,
          "Idade média"          = idade_media,
          "Vencedores"           = vencedores,
          "% que chegou ao júri" = pct_juri
        )
      datatable(d, rownames = FALSE, options = opcoes_dt)
    })
    
    # grade de calor 
    output$mapa_calor <- renderPlotly({
      d <- filtrado() |> filter(!is.na(personality_type)); req(nrow(d) > 0)
      niveis <- names(cores_res)
      modo <- input$modo_calor
      
      cont <- d |>
        mutate(resultado_grupo = as.character(resultado_grupo)) |>
        count(personality_type, resultado_grupo) |>
        tidyr::complete(personality_type, resultado_grupo = niveis, fill = list(n = 0)) |>
        group_by(personality_type) |>
        mutate(total = sum(n), pct = n / total * 100) |>
        ungroup()
      
      cont$valor <- if (modo == "pct") cont$pct else cont$n
      cont <- cont |>
        group_by(resultado_grupo) |>
        mutate(intensidade = ifelse(max(valor) > 0, valor / max(valor), 0)) |>
        ungroup()
      
      cont$rotulo <- if (modo == "pct") paste0(round(cont$valor), "%") else as.character(cont$n)
      cont$tip <- paste0("<b>", cont$personality_type, "</b><br>",
                         cont$resultado_grupo, ": ", cont$n, " de ", cont$total,
                         " (", round(cont$pct, 1), "%)")
      
      ordem <- cont |> distinct(personality_type, total) |>
        arrange(desc(total)) |> pull(personality_type)
      
      anot <- lapply(seq_len(nrow(cont)), function(i) {
        list(x = cont$resultado_grupo[i], y = cont$personality_type[i],
             text = cont$rotulo[i], showarrow = FALSE,
             font = list(size = 12,
                         color = if (cont$intensidade[i] > 0.55) "white" else cores$escura))
      })
      
      plot_ly(cont, x = ~resultado_grupo, y = ~personality_type, z = ~intensidade,
              type = "heatmap", colorscale = escala_calor, showscale = FALSE,
              xgap = 3, ygap = 3, text = ~tip, hoverinfo = "text") |>
        layout(annotations = anot,
               xaxis = list(title = "", side = "top",
                            categoryorder = "array", categoryarray = niveis),
               yaxis = list(title = "", categoryorder = "array",
                            categoryarray = rev(ordem))) |>
        estilo_plotly()
    })
    
    output$tabela_pers <- renderDT({
      d <- filtrado() |>
        filter(!is.na(personality_type)) |>
        group_by(personality_type) |>
        summarise(
          participantes = n(),
          vencedores    = sum(resultado_grupo == "Vencedor(a)"),
          finalistas    = sum(resultado_grupo == "Finalista"),
          jurados       = sum(resultado_grupo == "Jurado"),
          pct_juri      = round(mean(resultado_grupo != "Eliminado(a) antes do júri") * 100, 1),
          .groups = "drop"
        ) |>
        arrange(desc(participantes)) |>
        select(
          "Personalidade"        = personality_type,
          "Participantes"        = participantes,
          "Vencedores"           = vencedores,
          "Finalistas"           = finalistas,
          "Jurados"              = jurados,
          "% que chegou ao júri" = pct_juri
        )
      datatable(d, rownames = FALSE, options = opcoes_dt)
    })
    
    # violino 
    output$violino <- renderPlotly({
      d <- filtrado() |> filter(!is.na(age)); req(nrow(d) > 0)
      d$grupo <- as.character(d$resultado_grupo)
      d$tip <- paste0(d$full_name, " (temp. ", d$season, ")")
      
      plot_ly(d, x = ~grupo, y = ~age, color = ~grupo, colors = cores_res,
              type = "violin",
              box = list(visible = TRUE),
              meanline = list(visible = TRUE),
              points = if (isTRUE(input$pontos)) "all" else FALSE,
              jitter = 0.35, pointpos = 0,
              marker = list(opacity = 0.5, size = 4),
              text = ~tip, hoverinfo = "y+text") |>
        layout(showlegend = FALSE,
               xaxis = list(title = "", categoryorder = "array",
                            categoryarray = names(cores_res)),
               yaxis = list(title = "Idade")) |>
        estilo_plotly()
    })
    
    output$tabela <- renderDT({
      d <- filtrado() |>
        arrange(season, order) |>
        select(
          "Temporada"         = season,
          "Participante"      = full_name,
          "Idade"             = age,
          "Cidade"            = city,
          "Estado"            = state,
          "Personalidade"     = personality_type,
          "Resultado"         = result,
          "Dia da eliminação" = day,
          "Votos recebidos"   = total_votes_received
        )
      datatable(d, rownames = FALSE, options = opcoes_dt)
    })
  })
}

# shiny meu querido (aplicativo aqui)
ui <- page_navbar(
  title    = "Survivor | 40 temporadas em dados",
  theme    = tema_survivor,
  fillable = FALSE,
  
  nav_panel("Produção das temporadas", mod_temporadas_ui("temporadas", df_summary)),
  nav_panel("Perfil do elenco",        mod_elenco_ui("elenco", df_castaways))
)

server <- function(input, output, session) {
  mod_temporadas_server("temporadas", df_summary)
  mod_elenco_server("elenco", df_castaways)
}

shinyApp(ui, server)

