## Sacar datos y hacer modificaciones que aplican para todo; 
## fuera de la función para que no sea un proceso que se repita cada vez


catalogo_alcaldias <-
    read_csv("catalogo_alcaldias.csv")

postes <-
    read.csv("postes.csv", encoding = "UTF-8", stringsAsFactors = F) %>%
    tibble() %>%
    clean_names() %>%
    mutate(
        alcaldia = ifelse(alcaldia == "MAGDALENA CONTRERAS",
                          "LA MAGDALENA CONTRERAS", alcaldia),
        alcaldia = ifelse(alcaldia == "CUAJIMALPA",
                          "CUAJIMALPA DE MORELOS", alcaldia),
        longitud = gsub("\\,", "\\.", longitud),
        latitud = gsub("\\,", "\\.", latitud),
        boton_bin = ifelse(boton == "CON BOTON", 1, 0),
        altavoz_bin = ifelse(altavoz == "CON ALTAVOZ", 1, 0),
        estatus_bin = ifelse(estatus_conectividad == "WIFI GRATUITO_ACTIVO", 1, 0),
        alt_bot_bin = ifelse(boton_bin == 1 & altavoz_bin == 1, 1, 0),
        direccion_2 = paste(direccion, esquina, "COL.", colonia, "ALC.", alcaldia)
    ) %>%
    left_join(catalogo_alcaldias)

## Importar shapes
shape_alcaldias <-
    st_read("shape_alcaldias/shape_alcaldias.shp")

shape_colonias <-
    st_read("shape_colonias/shape_colonias.shp")

#### Función del servidor ####
shinyServer(function(input, output, session) {
    
    
    ## Datos "reactivos" al filtro de alcaldía
    data_filt <- reactive({
        req(input$alcaldia_sel)
        if (input$alcaldia_sel == 'todas') {
            postes
        } else {
            postes %>% 
                filter(alcaldia_clean == input$alcaldia_sel)
        }
    })
    
    ## Botón que regresa a la página principal
    observeEvent(input$main, {
        updateTabItems(session, "sidebar", "main")
    })
    
    output$total <- renderInfoBox({
        infoBox(title = tags$p("Total", br(), 
                               comma(nrow(data_filt()), digits = 0)),
                color  = "black", fill = TRUE)
    })
    
    
    #### Pantalla 2 ####
    ## Datos en el encabezado
    output$inactivos <- renderInfoBox({
        infoBox(title = tags$p("Inactivos", br(), 
                               comma(nrow(data_filt() %>% 
                                              filter(estatus_bin == 0)),
                                     digits = 0)),
                icon = icon("alert", lib = "glyphicon"),
                color  = "red", fill = TRUE) 
    })
    
    output$botones <- renderInfoBox({
        infoBox(title = tags$p("Botones", br(), 
                               comma(nrow(data_filt() %>% 
                                              filter(boton_bin == 1)),
                                     digits = 0)),
                color  = "green", fill = TRUE) 
    })
    
    output$alertas <- renderInfoBox({
        infoBox(title = tags$p("Alertas", br(), 
                               comma(nrow(data_filt() %>% 
                                              filter(altavoz_bin == 1)),
                                     digits = 0)),
                icon = icon("volume-up"),
                color  = "green", fill = TRUE) 
    })
    
    ## Tabla
    output$tabla_geo <- renderDataTable({
        if (input$alcaldia_sel == "todas") {
            tabla <- 
                data_filt() %>% 
                group_by(Alcaldia = alcaldia_clean) %>% 
                summarise(
                    Total = n(),
                    Activos = sum(estatus_bin),
                    "% inactivos" = 1 - (Activos/Total),
                    Botones = sum(boton_bin),
                    Altavoces = sum(altavoz_bin),
                    Ambos = sum(alt_bot_bin),
                    .groups = "drop"
                )
        } else {
            tabla <- 
                data_filt() %>% 
                group_by(Colonia = str_to_title(colonia)) %>% 
                summarise(
                    Total = n(),
                    Activos = sum(estatus_bin),
                    "% inactivos" = 1 - (Activos/Total),
                    Botones = sum(boton_bin),
                    Altavoces = sum(altavoz_bin),
                    Ambos = sum(alt_bot_bin),
                    .groups = "drop"
                )
        }
        
        tabla %>% 
            formattable(
                list(
                    "% inactivos" = percent
                    # "% inactivos" = color_tile("#CEE59D", "#FF5C64")
                    #         
                )
            ) %>%
            as.datatable(
                escape = FALSE, 
                rownames = FALSE,
                options(
                    autowith = FALSE,
                    compact = TRUE,
                    scrollX = 500,
                    scrollY = 500,
                    scroller = TRUE,
                    pageLength = 20,
                    lengthMenu = c(10, 20, 50)
                ),
            )
    })
    
    # Gráfica que se modifica según la variable que se quiera graficar (establecida en un selector)
    output$plot <- renderPlot(
        {
            data_filt() %>% 
                mutate(
                    boton = ifelse(boton == "CON BOTON", "CON BOTON", "SIN BOTON"),
                    altavoz = ifelse(altavoz == "CON ALTAVOZ", "CON ALTAVOZ", "SIN ALTAVOZ"),
                    across(c(boton, altavoz, tipo_de_poste, estatus_conectividad), str_to_sentence)) %>% 
                rename(
                    "Tipo de poste" = tipo_de_poste,
                    "Botón" = boton,
                    "Altavoz" = altavoz,
                    "Estatus WIFI" = estatus_conectividad
                ) %>% 
                count(!!input$variable) %>% 
                mutate(prc = n/sum(n)) %>% 
                ggplot(aes(x = !!input$variable, 
                           y = prc, 
                           fill = !!input$variable,
                           color = !!input$variable)) +
                geom_bar(stat = "identity", position = "dodge",
                         alpha = .7) +
                labs(
                    title = paste("Distribución"),
                    # x = !!input$variable,
                    y = "Porcentaje"
                ) +
                scale_y_continuous(label = percent) +
                scale_color_manual(values = c("#005f73", "#bb3e03"),
                                   aesthetics = c("color", "fill")) +
                theme_light() +
                theme(
                    legend.position = "bottom"
                )
        }
        
    )
    # output$plot <- renderPlot(
    #     {
    #         
    #         ggplot(starwars, aes(x = !!input$variable, fill = !!input$variable)) +
    #             geom_bar()
    #     }
    #     
    # )
    
    #### Mapa ####
    output$mapadin <- renderLeaflet({
        puntos <- 
            postes %>% 
            mutate(across(c(longitud, latitud), as.numeric)) %>% 
            drop_na(latitud, longitud) %>% 
            st_as_sf(coords = c("longitud", "latitud"),
                     crs = 4326)
        
        puntos_activos <- 
            puntos %>% 
            filter(estatus_bin == 1)
        
        labs_puntos_act <- 
            sprintf(
                "%s<br/>%s<br/>%s",
                paste(puntos_activos$estatus_conectividad),
                paste(puntos_activos$boton),
                paste(puntos_activos$altavoz)
            )
        
        puntos_inactivos <- 
            puntos %>% 
            filter(estatus_bin == 0)
        
        labs_puntos_inact <- 
            sprintf(
                "%s<br/>%s<br/>%s",
                paste(puntos_inactivos$estatus_conectividad),
                paste(puntos_inactivos$boton),
                paste(puntos_inactivos$altavoz)
            ) %>% 
            lapply(HTML)
        
        
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>%
            # setView(-99.136361, 19.32, 11) %>%
            addPolygons(
                data = shape_alcaldias,
                color = "black",
                weight = 1.5,
                fillOpacity =  0
            ) %>% 
            addPolygons(
                data = shape_colonias, 
                # label = labels_binomios,
                weight = 1.5,
                color = "gray",
                fillOpacity = 0,
                group = "Colonias"
            ) %>% 
            addCircles(
                data = puntos_activos,
                color = "#00b140",
                fill = TRUE,
                popup = labs_puntos_act,
                layerId = ~direccion_2,
                radius = .2,
                group = "Postes con wifi activo"
            ) %>% 
            addCircles(
                data = puntos_inactivos,
                color = "#ff4f4f",
                fill = TRUE,
                label = labs_puntos_inact,
                layerId = ~direccion_2,
                group = "Postes con falla en wifi"
            ) %>% 
            addLayersControl(
                overlayGroups = c(
                    "Postes con wifi activo",
                    "Postes con falla en wifi",
                    "Colonias"
                )
            ) %>% 
            hideGroup(c(
                "Postes con wifi activo",
                "Colonias")
            )
    })
    
    observe({
        event <- input$mapadin_shape_click
        if (is.null(event))
            return()
        
        output$Click_text <- renderText(
            sprintf(
                "%s\n%s\n%s",
                paste("Dirección:", event$id),
                paste("Longitud: ", event$lng),
                paste("Latitud", event$lat)
            )) 
    })
    
    #### Descarga de tabla ####
    
    #### Incidentes ####
    output$tabla_postes <- renderDataTable({
        postes %>% 
            # select(-contains("_bin")) %>% 
            mutate(
                boton = ifelse(boton == "CON BOTON", "CON BOTON", "SIN BOTON"),
                altavoz = ifelse(altavoz == "CON ALTAVOZ", "CON ALTAVOZ", "SIN ALTAVOZ"),
                across(c(boton, altavoz, tipo_de_poste, estatus_conectividad), str_to_sentence),
                across(c(alcaldia, boton, altavoz, tipo_de_poste, estatus_conectividad), factor)
            ) %>% 
            select(
                "Calle" = direccion,
                "Esquina" = esquina,
                "Alcaldia" = alcaldia,
                "Colonia" = colonia,
                "Tipo de poste" = tipo_de_poste,
                "Botón" = boton,
                "Altavoz" = altavoz,
                "Estatus WIFI" = estatus_conectividad
            )  %>% 
            datatable( 
                rownames = FALSE,
                filter = "top",
                extensions = c("KeyTable", "Buttons"),
                options = list(
                    hover = TRUE,
                    compact = TRUE,
                    scrollX = 400,
                    scroller = TRUE,
                    scrollY = 700,
                    columnDefs = list(list(className = 'dt-right', targets = 1:2)),
                    keys = TRUE,
                    dom = "Blfrtip",
                    buttons = c("csv", "excel"),
                    dom = "t",
                    initComplete = JS(
                        "function(settings, json) {",
                        "$('body').css({'font-family': 'Calibri'});",
                        "$(this.api().table().header()).css({'background-color': '#005f73', 'color': '#fff'})",
                        "}"
                    ),
                    pageLength = 50,
                    lengthMenu = c(10, 20, 50, 100, 200, 500, 1000)
                )
            )
    })
    
})
