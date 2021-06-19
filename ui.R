## Paqueteria ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(janitor)
library(scales)
library(sf)
library(leaflet)
library(htmltools)
library(formattable)
library(DT)




##### Funcion del encabezado ####
header <- dashboardHeader(
    title = tags$p(
        tags$img(src = 'rladiesqro.png', height = '64', width = '64'),
        'Postes de C5 en CDMX', style = "color = #fff; font-size: 3rem"),
    
    tags$li(class = "dropdown", 
            actionButton("main", "Bienvenida"),
            
            tags$style(".main-header {max-height: 60px}"),
            tags$style(".main-header .logo {height: 60px;}"),
            tags$style(".sidebar-toggle {height: 60px; padding-top: 20px !important;}"),
            tags$style(".navbar {min-height:60px !important}"),
            tags$style(".dropdown {height: 60px; padding-top: 10px !important;}")
    ),
    titleWidth = "80%"
)


#### Funcion de barra lateral ####
sidebar <- dashboardSidebar(
    sidebarMenu( #### Que sea un menu
        tags$style(HTML(".sidebar-menu li a { font-size: 1.5rem; }")),
        id = "sidebar", 
        br(),
        menuItem("Bienvenida", tabName = "main", icon = icon("home")),
        menuItem("Resumen", tabName = "plots_tablas",
                 icon = icon("th-list",  lib = "glyphicon")),
        menuItem("Mapa", tabName = "mapa", icon = icon("map-marked-alt")),
        menuItem("Tabla para descargar", tabName = "tabla_download",
                 icon = icon("download", lib = "glyphicon"))
    )
)

#### Estructura dek cuerpo ####
body <- dashboardBody(
    tabItems( ## ]El item de cada tab establecido
        
        # id = "body",
        
        tabItem(tabName = "main",
                fluidRow(
                    box(
                        title = "¡Bienvenida!",
                        status = "danger",
                        solidHeader = TRUE,
                        tags$h2("¿De qué va esto?"),
                        tags$p("Este es un tablero montado para el taller de Rladies acerca de cómo hacer tableros en shiny, particularmente usando la paquetería de shinydashboards."),
                        tags$br(),
                        tags$h3("Datos de ejemplo"),
                        tags$p("Para esto, los datos que se usaron son acerca de los postes del C5 en la CDMX que salen del", 
                               tags$a(href = "https://datos.cdmx.gob.mx/dataset/ubicacion-acceso-gratuito-internet-wifi-c5", " portal de datos abiertos "),
                               tags$p("de la CDMX.")),
                        tags$br(),
                        tags$p('"En esta base de datos se pueden consultar las ubicaciones de los postes del Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano (C5) de la Ciudad de México que cuentan con cámaras de seguridad, botón de pánico, altavoz, el tipo de poste y la georreferenciación; así como el estatus actual de la conexión a internet inalámbrico."')
                    ),
                    box(
                        title = "Algunos recursos adicionales",
                        status = "success",
                        solidHeader = TRUE,
                        tags$ul(
                            tags$li(tags$a(href = "https://shiny.rstudio.com/", "Página de Shiny")),
                            tags$li(tags$a(href = "https://rstudio.github.io/shinydashboard/", "Página de shinydashboard")),
                            tags$li(tags$a(href = "http://shinyapps.dreamrs.fr/shinyWidgets/", "Galería de shinyWidgets")),
                            tags$li(tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/reactivePoll.html", "reactivePoll para actualizaciones automáticas")),
                            tags$li(tags$a(href = "https://mastering-shiny.org/", "Libro: Mastering Shiny"))
                            )
                    )
                )
        ),
        tabItem(tabName = "plots_tablas",
                tags$style(".info-box-content p { font-size: 2.5rem; }"),
                
                fluidRow(
                    box(
                        #### Filtros ####
                        width = 4,
                        # title = "Filtro de alcaldías",
                        pickerInput(
                            inputId = "alcaldia_sel", label = "Selecciona una alcaldía",
                            c("Todas las alcaldías" = "todas",
                              c("Álvaro Obregón", "Azcapotzalco", "Benito Juárez", "Coyoacán",
                                "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero", 
                                "Iztacalco", "Iztapalapa", "La Magdalena Contreras",
                                "Miguel Hidalgo", "Milpa Alta", "Tláhuac", "Tlalpan",
                                "Venustiano Carranza", "Xochimilco")),
                            selected = "todas",
                            options = list(
                                style = "btn-primary"
                            )
                        ),
                    ),
                    
                    #### Info basica #####
                    infoBoxOutput(
                        width = 2,
                        "total"
                    ),
                    infoBoxOutput(
                        width = 2,
                        "inactivos"
                    ),
                    infoBoxOutput(
                        width = 2,
                        "botones"
                    ),
                    infoBoxOutput(
                        width = 2,
                        "alertas"
                    )
                ),
                
                
                box(
                    width = 6, 
                    height = "100%",
                    id = "tabla",
                    title = "Ubicación de los postes",
                    dataTableOutput(
                        "tabla_geo", 
                        width = "100%", 
                        height = "100%"
                    )
                ),
                
                tabBox(
                    width = 6,
                    height = "100%",
                    id = "plots",
                    tabPanel(title = "Gráfica",
                             varSelectInput("variable", "Elige qué variable graficar", 
                                      ## Cheat para tener un df con las variables
                                           tibble(`Tipo de poste` = c("x", "y"), 
                                                  `Botón` = c("x", "y"), 
                                                  `Altavoz` = c("x", "y"), 
                                                  `Estatus WIFI` = c("x", "y"))
                                            ),
                             plotOutput("plot")),
                    tabPanel(title = "Ideas nuevas!")
                )
        ),
        tabItem(tabName = "mapa",
                box(leafletOutput("mapadin")),
                box(title = "Información del poste",
                    status = "warning",
                    solidHeader = TRUE,
                    textOutput("Click_text")
                )
        ),
        
        tabItem(tabName = "tabla_download",
                dataTableOutput("tabla_postes"))
    )
)


#### Llamar todo lo anterior
shinyUI( 
    dashboardPage(
        header, 
        sidebar, 
        body)
)


