shinyServer(function(input, output, session) {
  
  ###################################
  #Login
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = base_usuarios,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  ########################################################################
  #UI
  output$ui <- renderUI({
    req(credentials()$user_auth)
    sidebarLayout(
      sidebarPanel(
        titlePanel(""),
        shinyjs::useShinyjs(),
        fluidRow(
          column(8,
                 radioButtons(
                   inputId='idServicio',
                   label = 'Seleccione el tipo de subsidio:',
                   choices = select_servicio,
                   selected = 'todos'
                 )
          )),
        fluidRow(column(8,
                        selectInput(
                          inputId = "idPorcentaje",
                          label ='CBE como Porcentaje del Ingreso:',
                          choices = select_porcentaje,
                          selected = NULL,
                          multiple = F))),
        
        fluidRow(column(8,
                        sliderInput(
                          inputId = "idIngresos",
                          label ='Decil de ingreso de la población:',
                          min = 0,
                          max = 10,
                          step = 1,
                          value = c(0,10)))
        ),
        fluidRow(
          column(6,radioButtons(inputId = "idZona",
                                label = 'Zona Bio:',
                                choices = select_zona,
                                selected = 7)),
          column(6,radioButtons(inputId = "idPersonas",
                                label = "Miembros del hogar:",
                                choices = select_personas,
                                selected = 4))
        ),
        fluidRow(
          column(6,
                 checkboxGroupInput(
                   inputId='idBienes',
                   label = 'Exclusión por Patrimonio:',
                   choices = select_bienes,
                   selected = NULL
                 )),
                 column(6,
                        radioButtons(
                          inputId='idAutos',
                          label = 'Exclusión por Auto:',
                          choices = select_autos,
                          selected = 'ninguno',
                        ))
          ),
        actionButton(inputId = "boton", label = "Calcular Gasto"),
        hr()
      ),
      mainPanel(
        fluidRow(
          column(3, offset = 9,
                 helpText("")   
          )),
        hr(),
        fluidRow(
          div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
          shinyauthr::loginUI(id = "login")
        ),
        fluidRow(column(6,
                        value_box(
                          title = tags$div("Monto promedio anual:", style = "font-size: 18px;"),
                          value = uiOutput("cant_mon"),
                          height = '100px',
                          class = 'p-0 nopad',
                          style = "background-color: #212a51 ; color: white;"
                        ),
                        br(),
                        value_box(
                          tags$div("Cantidad de hogares subsidiados:", style = "font-size: 18px;"),
                          value = uiOutput("cant_hog"),
                          height = '100px',
                          class = 'p-0 nopad',
                          style = "background-color: #212a51 ; color: white;"
                        ),
                        br()
        ),
        column(6,
               value_box(
                 tags$div("Cantidad total de hogares:", style = "font-size: 18px;"),
                 value = uiOutput("cant_hog_t"),
                 height = '100px',
                 class = 'p-0 nopad',
                 style = "background-color: #212a51 ; color: white;"
               ),
               br(),
               value_box(
                 tags$div("Porcentaje de hogares subsidiados:", style = "font-size: 18px;"),
                 value = uiOutput("porc_h"),
                 height = '100px',
                 class = 'p-0 nopad',
                 style = "background-color: #212a51 ; color: white;"
               ),
               br()
        )
        ),
        fluidRow(
          column(8,
                 plotly::plotlyOutput("barras")),
          column(4,
                 plotly::plotlyOutput("torta"))
        )))
  })
  
  
  
  
  ########################################################################
  #SERVER
    inicial <- reactiveVal()
    
    graficar <- reactiveVal(F)
    monto <- reactiveVal(0)
    hogares <- reactiveVal(0)
    hogares_total <- reactiveVal(0)
    agrupado_familia <- reactiveVal()
    agrupado_decil <- reactiveVal()
    aux <- reactiveVal(F)
    
    porcentaje_hog <- reactive(
      if(hogares_total() != 0){
        paste(f2(hogares()*100/hogares_total()),'%')
      } else
        'Sin hogares'    
    )
    
    bienes <- reactive({
        input$idBienes
    })
    
    autos <- reactive({
      if(input$idAutos == 'ninguno'){
        c() 
      } 
      else{
        input$idAutos
      }
    })
    
    
    criterio <- reactive({
      input$idPorcentaje
    })
    
    servicios <- reactive({
      if(input$idServicio != 'todos'){
        input$idServicio  
      } else{
        c('tabla_nat','tabla_luz','tabla_gar')
      }
    })
    
    tipo_zona <- reactive({
      if(input$idZona != 7){
        input$idZona
      } else {
        c(1,2,3,4,5,6)
      }
    })
    
    tipo_viv <- reactive({
      if(input$idPersonas !=4){
        input$idPersonas
      } else {
        c('1','2','3')
      }
    })
    
    monto_sub <- reactive({
      switch(
        criterio(),
        "CRITERIO_0" = "MONTO_SUBSIDIADO_0",
        "CRITERIO_2" = "MONTO_SUBSIDIADO_2",
        "CRITERIO_4" = "MONTO_SUBSIDIADO_4",
        "CRITERIO_6" = "MONTO_SUBSIDIADO_6",
        "CRITERIO_8" = "MONTO_SUBSIDIADO_8",
        "CRITERIO_10" = "MONTO_SUBSIDIADO_10"
      )
    })
    
    df_no_filt <- reactive({
      universo_filtrado <- select(inicial(),all_of(campos_basicos)) %>%
        dplyr::filter(TIPO_VIVIENDA %in% tipo_viv()) %>%
        dplyr::filter(id_zona_bio %in% tipo_zona())
    })
    
    df_filt <- reactive({
      seleccionados <- c(bienes(),criterio(),monto_sub(),autos())
      campos <- append(campos_basicos,seleccionados)
      universo_filtrado <-  select(inicial(), all_of(campos)) %>%
        dplyr::filter(TIPO_VIVIENDA %in% tipo_viv()) %>%
        dplyr::filter(id_zona_bio %in% tipo_zona()) %>%
        dplyr::filter(!!sym(criterio()) == 0) %>%
        dplyr::filter(between(DECIL_INGRESO, input$idIngresos[1], input$idIngresos[2]))
      
      for(bien in bienes()) {
        universo_filtrado <- universo_filtrado %>%
          dplyr::filter(!!sym(bien) == 0)
      }
      
      for(auto in autos()) {
        universo_filtrado <- universo_filtrado %>%
          dplyr::filter(!!sym(auto) == 0)
      }
      
      
      return(universo_filtrado)
    })
  
    

    observeEvent(input$boton, {
      monto(0)
      hogares(0)
      hogares_total(0)
      columna <- monto_sub()
      for (servicio in servicios()) {
        inicial(get(servicio))
        if(aux()){
          aux1 <- df_filt() %>%
            group_by(TIPO_VIVIENDA) %>%
            summarise(total_h = sum(CANT_TOTAL_HOGARES), .groups = "drop")
          merge1 <- merge(df1, aux1, by = "TIPO_VIVIENDA", all = TRUE)
          merge1[is.na(merge1)] <- 0
          df1$total_h <- merge1$total_h.x + merge1$total_h.y
          aux2 <- df_filt() %>%
            group_by(DECIL_INGRESO) %>%
            summarise(total_h = sum(CANT_TOTAL_HOGARES), .groups = "drop")
          merge2 <- merge(df2, aux2, by = "DECIL_INGRESO", all = TRUE)
          merge2[is.na(merge2)] <- 0
          df2$total_h <- merge2$total_h.x + merge2$total_h.y
          
          } else {
          df1 <- df_filt() %>%
                             group_by(TIPO_VIVIENDA) %>%
                             summarise(total_h = sum(CANT_TOTAL_HOGARES), .groups = "drop")
          df2 <- df_filt() %>%
                           group_by(DECIL_INGRESO) %>%
                           summarise(total_h = sum(CANT_TOTAL_HOGARES), .groups = "drop")
          aux(T)
        }
        hogares_total(hogares_total() + sum(df_no_filt()$CANT_TOTAL_HOGARES))
        monto(monto() + sum(df_filt()[[columna]]))
        hogares(hogares() + sum(df_filt()$CANT_TOTAL_HOGARES)) 
        agrupado_decil(df2)
        agrupado_familia(df1)
      }
      aux(F)
    })
    
    output$barras <- renderPlotly({
     
       validate(
        need(nrow(agrupado_decil()) > 0, "No existen datos para graficar")
      )
      
      plot_ly(agrupado_decil(), x = ~DECIL_INGRESO, y = ~total_h ,type = 'bar', marker = list(color = '#80899a', width = 0.5),hovertext= ~ paste0('<b>Cantidad: </b>', f(total_h), '<br><b>Decil de ingreso</b>: <br>', DECIL_INGRESO),
              hoverinfo = 'text') %>%
        plotly::layout(title = "Cantidad de hogares por deciles",
                       xaxis = list(title = "Deciles de ingreso",
                                    tickmode = "linear",  
                                    tick0 = 0,           
                                    dtick = 1),
                       yaxis = list(title = "Cantidad de hogares"))
      
      })
    
    
    output$torta <- renderPlotly({
      validate(
        need(nrow(agrupado_familia()) > 0, "No existen datos para graficar")
      )
      colors <- c("#212a51","#e7ba61",'#80899a')
      tipos <- c("1 o 2", "3 o 4", "Más de 4")
      tipo_labels <- c("1 o 2" = 1, "3 o 4" = 2, "Más de 4" = 3)
      
      
      
      fig <- plot_ly(agrupado_familia(), labels = ~tipos[tipo_labels[TIPO_VIVIENDA]], values = ~total_h, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste('Hogares: ', f(total_h),
                                   'Personas: ', tipos[tipo_labels[TIPO_VIVIENDA]]),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)
      fig <- fig %>% plotly::layout(title = 'Hogares por tipo de familia',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
    })
    
    
    
    output$cant_hog <- renderUI({
      tagList(
        tags$span(
          f(hogares()),
          class = "mi-clase-custom"
        )
      )
    })
    
    output$cant_hog_t <- renderUI({
      tagList(
        tags$span(
          f(hogares_total()),
          class = "mi-clase-custom"
        )
      )
    })
    
    output$porc_h <- renderUI({
      tagList(
        tags$span(
          porcentaje_hog(),
          class = "mi-clase-custom"
        )
      )
    })
    
    output$cant_mon <- renderUI({
      tagList(
        tags$span(
          paste('$',f(monto()*12)),
          class = "mi-clase-custom"
        )
      )
    })

  
  
  
})











