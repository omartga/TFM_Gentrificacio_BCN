#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#Codi per executar l'aplicació shiny de visualització de variables de barris de Barcelona.

#Càrrega de llibreries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(stringi)
library(tibble)
library(shiny)

#Definició de la interficie d'usuari
ui <- fluidPage(
  titlePanel("Anàlisi variables per barri"),
  sidebarLayout(
    sidebarPanel(
      selectInput("zone", "Selecciona barri:", choices = NULL),
      radioButtons("scaling", "Selecciona escala:",
                   choices = list("Escala (0-1)" = "scaled", "Valors originals" = "nonscaled"), selected = "scaled")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

#Definicio del servidor
server <- function(input, output, session) {
  #Càrrega de dades a partir del fitxers originals (raw_data)
  pmll <- read_csv("1. Dades_en_brut/Taula estadística - Preu mitjà (€) del lloguer d'habitatges.zip")
  emn <- read_csv("1. Dades_en_brut/Taula estadística - Edat mitjana de la població segons nacionalitat (Espanya_UE_Resta estranger).zip")
  pnbcn <- read_csv("1. Dades_en_brut/Taula estadística - Població per nacionalitat (Espanya_UE_Resta estranger).zip")
  epn <- read_csv("1. Dades_en_brut/Taula estadística - Població de 16 anys i més per titulació acadèmica i nacionalitat (Espanya_UE_Resta estranger).zip")
  hut <- read_csv("1. Dades_en_brut/Taula estadística - Nombre d’habitatges d’ús turístic.zip")

  #Neteja de dades
  pmll2 <- pmll %>%
    rename_all(tolower) %>% #Variables en minúscules
    rename_all(~gsub(" ", "_", .)) %>% #Espais canviats a _
    mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% #Canvi a ASCII per eliminar símbols
    mutate_all(tolower) %>% #Valors en minúscules
    mutate(across(-c(1,2), as.numeric)) %>% #Valors a numèric
    pivot_longer(cols = -c(territori, tipus_de_territori), 
                 names_to = "anys",
                 values_to = "preu_mitja_lloguer") %>% #Canvi de columna per any a fila per any
    mutate(anys = as.integer(anys)) %>% 
    mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori), #Correccio de noms ens la taula
           tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
           territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
           territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori),
           territori = ifelse(territori == "el poble sec - aei parc montjuic", "el poble-sec", territori),
           territori = ifelse(territori == "la marina del prat vermell - aei zona franca", "la marina del prat vermell", territori))
  
  emn2 <- emn %>%
    rename_all(tolower) %>%
    rename_all(~gsub(" ", "_", .)) %>%
    rename(nacionalitat = 'nacionalitat_(espanya/ue/resta_estranger)') %>%
    mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
    mutate_all(tolower) %>%
    rename_with(~ substr(., start = nchar(.) - 3, stop = nchar(.)), -c(1,2,3)) %>%
    pivot_longer(cols = -c(territori, tipus_de_territori, nacionalitat),
                 names_to = "anys",
                 values_to = "edat_mitjana") %>%
    subset(nacionalitat != "no consta") %>%
    pivot_wider(names_from = nacionalitat, names_prefix = "edat_mitjana.", values_from = edat_mitjana) %>%
    mutate(across(-c(1,2), as.numeric)) %>%
    mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
           tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
           territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
           territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori)) %>%
    mutate(anys = as.integer(anys))
  
  pnbcn2 <- pnbcn %>%
    rename_all(tolower) %>%
    rename_all(~gsub(" ", "_", .)) %>%
    rename(nacionalitat = 'nacionalitat_(espanya/ue/resta_estranger)') %>%
    mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
    mutate_all(tolower) %>%
    rename_with(~ substr(., start = nchar(.) - 3, stop = nchar(.)), -c(1,2,3)) %>%
    pivot_longer(cols = -c(territori, tipus_de_territori, nacionalitat),
                 names_to = "anys",
                 values_to = "poblacio") %>%
    subset(nacionalitat != "no consta") %>%
    pivot_wider(names_from = nacionalitat, names_prefix = "poblacio.", values_from = poblacio) %>%
    mutate(across(-c(1,2), as.numeric)) %>%
    mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
           tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
           territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
           territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori)) %>%
    mutate(anys = as.integer(anys))
  
  epn2 <- epn %>%
    rename_all(tolower) %>%
    rename_all(~gsub(" ", "_", .)) %>%
    rename_all(~stri_trans_general(., "Latin-ASCII")) %>%
    mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
    mutate_all(tolower) %>%
    pivot_longer(cols = -c(territori, tipus_de_territori, titulacio_academica), names_to = "anys", values_to = "nombre_titulacions") %>%
    subset(nombre_titulacions != "no consta") #Eliminació valors "no consta"
  
  epn21 <- epn2 %>%
    subset(tipus_de_territori == "tipus de territori") %>%
    select(-c(1:3))
  
  epn22 <- epn2 %>%
    subset(tipus_de_territori != "tipus de territori")
  
  epn3 <- epn22 %>%
    left_join(epn21, by = "anys") %>%
    rename(nombre_titulacions = nombre_titulacions.x) %>%
    rename(procedencia = nombre_titulacions.y) %>%
    mutate(anys = str_extract(anys, "\\d{4}")) %>%
    subset(procedencia != "no consta" & titulacio_academica != "no consta") %>%
    pivot_wider(names_from = c("titulacio_academica", "procedencia"), values_from = nombre_titulacions) %>%
    mutate(across(-c(1,2), as.integer)) %>%
    mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
           tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
           territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
           territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori))
  
  hut2 <- hut %>%
    rename_all(tolower) %>%
    rename_all(~gsub(" ", "_", .)) %>%
    mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
    mutate_all(tolower) %>%
    mutate(across(-c(1,2), as.numeric)) %>%
    rename_with(~ substr(., start = nchar(.) - 3, stop = nchar(.)), -c(1,2)) %>%
    mutate(tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori))
  
  hut4 <- hut2 %>%
    pivot_longer(cols = -c(territori, tipus_de_territori),
                 names_to = "anys",
                 values_to = "habitatges_us_turistic") %>%
    mutate(anys = as.integer(anys),
           habitatges_us_turistic = as.integer(habitatges_us_turistic)) %>%
    mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
           tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
           tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
           territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
           territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
           territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
           territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori))

  #Creació taula amb totes les variables
  m.df <- left_join(x = emn2, y = pmll2, by = c("territori", "tipus_de_territori", "anys")) %>%
    left_join(pnbcn2, by = c("territori", "tipus_de_territori", "anys")) %>%
    left_join(epn3, by = c("territori", "tipus_de_territori", "anys")) %>%
    left_join(hut4, by = c("territori", "tipus_de_territori", "anys"))

  #Taula que conté els valors només de barris
  m.df.barri <- m.df %>%
    subset(tipus_de_territori == "barri") %>%
    select(-c("tipus_de_territori"))

  #Funció per escalar les dades
  scale.01 <- function(x) {
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    return((x - x_min) / (x_max - x_min))
  }

  #Variable amb noms dels barris
  unique.barris <- unique(m.df.barri$territori)

  updateSelectInput(session, "zone", choices = unique.barris)

  #Definició per a la representació de les dades
  output$plot <- renderPlotly({
    req(input$zone)

    #Filtratge de dades per barri
    barri <- subset(m.df.barri, territori == input$zone)

    #Comprovació si es tracten dades escalades o no
    if (input$scaling == "scaled") {
      barri <- barri %>%
        mutate(across(where(is.numeric), scale.01))
    }
    
    barri <- barri %>%
      pivot_longer(cols = -c("territori", "anys"), names_to = "variable", values_to = "valor")

    #Definició de la gràfica
    b <- ggplot(barri, aes(x = anys, y = valor, color = variable)) +
      geom_point() +
      geom_line() +
      theme(legend.position = "bottom") +
      ggtitle(paste("Gràfic del barri: ", input$zone))

    #Representació de la gràfica
    ggplotly(b)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
