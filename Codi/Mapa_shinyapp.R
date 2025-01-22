#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#Codi per executar l'aplicació shiny de gentrificació de Barcelona.

# Llibreries

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(stringi)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(zoo)
library(tibble)
library(umap)
library(shiny)
library(sf)

# Carrega de dades

pmll <- read_csv("data/preu_lloguer.csv")

emn <- read_csv("data/edat_poblacio.csv")

pnbcn <- read_csv("data/poblacio.csv")

epn <- read_csv("data/estudis_poblacio.csv")

hut <- read_csv("data/habitatges_us_turistic.csv")

mapa <- st_read("data/BCN_UNITATS_ADM/0301040100_Barris_UNITATS_ADM.shp")

mapa.d <- st_read("data/BCN_UNITATS_ADM/0301040100_Districtes_UNITATS_ADM.shp")


#Barris i districtes
bcn_barri <- mapa[, c("NOM", "geometry")]

bcn_districte <- mapa.d[, c("NOM", "geometry")]

bcn_barri <- bcn_barri %>%
  mutate(territori = tolower(NOM),
         territori = iconv(territori, "UTF-8", "ASCII//TRANSLIT"),
         territori = ifelse(territori == "sants - badal", "sants-badal", territori),
         territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
         territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
         territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori),
         territori = ifelse(territori == "el poble sec - aei parc montjuic", "el poble sec", territori),
         territori = ifelse(territori == "la marina del prat vermell - aei zona franca", "la marina del prat vermell", territori))

bcn_districte <- bcn_districte %>%
  mutate(territori = tolower(NOM),
         territori = iconv(territori, "UTF-8", "ASCII//TRANSLIT"))

set.seed(123)

#preu lloguer
pmll2 <- pmll %>%
  rename_all(tolower) %>%
  rename_all(~gsub(" ", "_", .)) %>%
  mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  mutate_all(tolower) %>%
  mutate(across(-c(1,2), as.numeric)) %>%
  pivot_longer(cols = -c(territori, tipus_de_territori),
               names_to = "anys",
               values_to = "preu_mitja_lloguer") %>%
  mutate(anys = as.integer(anys)) %>%
  mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
         tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
         tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
         territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
         territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
         territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
         territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori),
         territori = ifelse(territori == "el poble sec - aei parc montjuic", "el poble-sec", territori),
         territori = ifelse(territori == "la marina del prat vermell - aei zona franca", "la marina del prat vermell", territori) )
llmap <- pmll2 %>%
  subset(pmll2$tipus_de_territori == "barri")
llmap <- left_join(llmap, bcn_barri, by = "territori")


#Edat mitjana
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
  pivot_wider(names_from = nacionalitat, names_prefix = "edat_mitjana.", values_from = edat_mitjana)  %>%
  mutate(across(-c(1,2), as.numeric)) %>%
  mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
         tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
         tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
         territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
         territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
         territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
         territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori)) %>%
  mutate(anys = as.integer(anys))
emn2.b <- emn2 %>%
  subset(emn2$tipus_de_territori == "barri") %>%
  select(-c(tipus_de_territori))
emap <- left_join(emn2.b, bcn_barri, by = "territori")


#Habitatges turistics
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
         territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori) )
hut.b <- hut4 %>%
  subset(hut4$tipus_de_territori == "barri") %>%
  select(-c(tipus_de_territori))
hutmap <- left_join(hut.b, bcn_barri, by = "territori")


#Estudis poblacio

epn2 <- epn %>%
  rename_all(tolower) %>%
  rename_all(~gsub(" ", "_", .)) %>%
  rename_all(~stri_trans_general(., "Latin-ASCII")) %>%
  mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  mutate_all(tolower)  %>%
  pivot_longer(cols = -c(territori, tipus_de_territori, titulacio_academica), names_to = "anys", values_to = "nombre_titulacions") %>%
  subset(nombre_titulacions != "no consta")
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
epn3.b <- epn3 %>%
  subset(epn3$tipus_de_territori == "barri") %>%
  select(-c(tipus_de_territori))
epnmap.b <- left_join(epn3.b, bcn_barri, by = "territori")


#Poblacio

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
  pivot_wider(names_from = nacionalitat, names_prefix = "poblacio.", values_from = poblacio)  %>%
  mutate(across(-c(1,2), as.numeric)) %>%
  mutate(territori = ifelse(territori == "sants - badal", "sants-badal", territori),
         tipus_de_territori = ifelse(territori == "sant andreu (districte)", "districte", tipus_de_territori),
         tipus_de_territori = ifelse(territori == "sant andreu (barri)", "barri", tipus_de_territori),
         territori = ifelse(territori == "sant andreu (districte)", "sant andreu", territori),
         territori = ifelse(territori == "sant andreu (barri)", "sant andreu", territori),
         territori = ifelse(territori == "sant gervasi - galvany", "sant gervasi-galvany", territori),
         territori = ifelse(territori == "sant gervasi - la bonanova", "sant gervasi-la bonanova", territori)) %>%
  mutate(anys = as.integer(anys))
pnbcn2.b <- pnbcn2 %>%
  subset(pnbcn2$tipus_de_territori == "barri") %>%
  select(-c(tipus_de_territori))
pobmap.b <- left_join(pnbcn2.b, bcn_barri, by = "territori")


#KMEANS
#Districte

m.df <- left_join(x = emn2, y = pmll2, by = c("territori", "tipus_de_territori", "anys")) %>%
  left_join(pnbcn2, by = c("territori", "tipus_de_territori", "anys")) %>% 
  left_join(epn3, by = c("territori", "tipus_de_territori", "anys")) %>%
  left_join(hut4, by = c("territori", "tipus_de_territori", "anys"))

m.df.districte <- m.df %>%
  subset(tipus_de_territori == "districte")  %>%
  select(-c("tipus_de_territori"))

m.df.districte2 <- m.df.districte %>% 
  mutate(id = str_c(territori, anys, sep = "_")) %>%
  mutate_all(function(x) {
    if(is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
    return(x)
  }) %>%
  mutate(across(where(is.numeric), scale))

mDF_d <- m.df.districte2 %>%
  select(!c("territori", "anys")) %>%
  column_to_rownames("id")  %>%
  as.matrix()

km_res <- kmeans(mDF_d, centers = 3, nstart = 25)
districte <- data.frame(m.df.districte[, c("territori", "anys")], cluster = km_res$cluster)

mapbcn.d <-  left_join(x = districte, y = bcn_districte, by = c("territori"))

#Barris
m.df.barri <- m.df %>%
  subset(tipus_de_territori == "barri")  %>%
  select(-c("tipus_de_territori"))

m.df.barri2 <- m.df.barri %>% 
  mutate(id = str_c(territori, anys, sep = "_")) %>%
  mutate_all(function(x) {
    if(is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
    return(x)
  }) %>%
  mutate(across(where(is.numeric), scale))

mDF_b <- m.df.barri2 %>%
  select(!c("territori", "anys")) %>%
  column_to_rownames("id")  %>%
  as.matrix()

km_res.b <- kmeans(mDF_b, centers = 3, nstart = 25)

barri <- data.frame(m.df.barri[, c("territori", "anys")], cluster = km_res.b$cluster)

mapbcn <-  left_join(x = barri, y = bcn_barri, by = c("territori"))

#Shiny

global_min_preu_mitja_lloguer <- min(llmap$preu_mitja_lloguer, na.rm = TRUE)
global_max_preu_mitja_lloguer <- max(llmap$preu_mitja_lloguer, na.rm = TRUE)

global_min_edat_mitjana <- min(emap %>% select(-geometry, -territori, -anys, -NOM), na.rm = TRUE)
global_max_edat_mitjana <- max(emap %>% select(-geometry, -territori, -anys, -NOM), na.rm = TRUE)

global_min_habitatges_us_turistic <- min(hutmap$habitatges_us_turistic, na.rm = TRUE)
global_max_habitatges_us_turistic <- max(hutmap$habitatges_us_turistic, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Evolució de BCN"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Preu mitjà lloguer", 
                 sliderInput("year1", "Any:", min = 2014, max = 2023, value = 2014, step = 1)),
        tabPanel("Edat mitjana", 
                 sliderInput("year2", "Any:", min = 1997, max = 2023, value = 1997, step = 1),
                 selectInput("column_selector2", "Escull una variable:", 
                             choices = colnames(select(emap, -c('territori', 'anys', 'NOM', 'geometry'))))),
        tabPanel("Població",
                 sliderInput("year3", "Any:", min = 1997, max = 2023, value = 1997, step = 1),
                 selectInput("column_selector3", "Escull una variable:", 
                             choices = colnames(select(pobmap.b, -c('territori', 'anys', 'NOM', 'geometry')))),
                 textOutput("panel3Text")),
        tabPanel("Estudis població",
                 sliderInput("year4", "Any:", min = 1997, max = 2023, value = 1997, step = 1),
                 selectInput("column_selector4", "Escull una variable:", 
                             choices = colnames(select(epnmap.b, -c('territori', 'anys', 'NOM', 'geometry')))),
                 textOutput("panel4Text")),
        tabPanel("Habitatges ús turístic",
                 sliderInput("year5", "Any:", min = 2014, max = 2023, value = 2014, step = 1),
                 textOutput("panel5Text")),
        tabPanel("Gentrificació Barris",
                 sliderInput("year6", "Any:", min = 1997, max = 2023, value = 1997, step = 1),
                 textOutput("panel6Text")),
        tabPanel("Gentrificació Districtes",
                 sliderInput("year7", "Any:", min = 1997, max = 2023, value = 1997, step = 1),
                 textOutput("panel7Text"))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.tabset == 'Preu mitjà lloguer'",
        plotlyOutput("mapPlot1", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Edat mitjana'",
        plotlyOutput("mapPlot2", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Població'",
        plotlyOutput("mapPlot3", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Estudis població'",
        plotlyOutput("mapPlot4", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Habitatges ús turístic'",
        plotlyOutput("mapPlot5", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Gentrificació Barris'",
        plotlyOutput("mapPlot6", width = "100%", height = "800px")
      ),
      conditionalPanel(
        condition = "input.tabset == 'Gentrificació Districtes'",
        plotlyOutput("mapPlot7", width = "100%", height = "800px")
      )
    )
  )
)

server <- function(input, output, session) {
  output$mapPlot1 <- renderPlotly({

    llly <- llmap[llmap$anys == input$year1, ]
    
    l <- ggplot(data = llly$geometry, aes(fill = llly$preu_mitja_lloguer)) +
      geom_sf() +
      scale_fill_gradient(low = "green", high = "red", name = "Preu Mitjà Lloguer", 
                          limits = c(global_min_preu_mitja_lloguer, global_max_preu_mitja_lloguer)) +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Preu Mitjà Lloguer")
    
    ggplotly(l)
  })
  
  output$mapPlot2 <- renderPlotly({
    emnly <- emap[emap$anys == input$year2, ]
    
    e <- ggplot(data = emnly$geometry, aes(fill = emnly[[input$column_selector2]], text = paste("Territori: ", emnly[["territori"]],"<br>Valor: ", sprintf("%.2f", emnly[[input$column_selector2]])))) +
      geom_sf() +
      scale_fill_gradient(low = "#6ff237", high = "#ba37f2", name = input$column_selector2, 
                          limits = c(global_min_edat_mitjana, global_max_edat_mitjana)) +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Edat Mitjana")
    
    ggplotly(e, tooltip = "text")
  })
  
  output$mapPlot3 <- renderPlotly({
    pobly <- pobmap.b[pobmap.b$anys == input$year3, ]
    
    max_poblacio_dynamic <- max(pobmap.b[[input$column_selector3]], na.rm = TRUE)
    min_poblacio_dynamic <- min(pobmap.b[[input$column_selector3]], na.rm = TRUE)
    
    pb <- ggplot(data = pobly$geometry, aes(fill = pobly[[input$column_selector3]])) +
      geom_sf() +
      scale_fill_gradient(low = "#ffffff", high = "#FF8000", name = input$column_selector3, 
                          limits = c(min_poblacio_dynamic, max_poblacio_dynamic)) +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Població")
    
    ggplotly(pb)
  })
  
  output$mapPlot4 <- renderPlotly({
    eply <- epnmap.b[epnmap.b$anys == input$year4, ]
    
    max_estudis_poblacio_dynamic <- max(epnmap.b[[input$column_selector4]], na.rm = TRUE)
    min_estudis_poblacio_dynamic <- min(epnmap.b[[input$column_selector4]], na.rm = TRUE)
    mid_estudis_poblacio_dynamic <- (min_estudis_poblacio_dynamic + max_estudis_poblacio_dynamic)/2
    
    ep <- ggplot(data = eply$geometry, aes(fill = eply[[input$column_selector4]])) +
      geom_sf() +
      scale_fill_gradient2(low = "#0000FF", mid = "#ffffff", high = "#800000", name = input$column_selector4,
                          midpoint = mid_estudis_poblacio_dynamic,
                          limits = c(min_estudis_poblacio_dynamic, max_estudis_poblacio_dynamic)) +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Estudis Població")
    
    ggplotly(ep)
  })
  
  output$mapPlot5 <- renderPlotly({
    hutly <- hutmap[hutmap$anys == input$year5, ]
    
    h <- ggplot(data = hutly$geometry, aes(fill = hutly$habitatges_us_turistic)) +
      geom_sf() +
      scale_fill_gradient(low = "#ffffff", high = "#ff2424", name = "Habitatges Ús Turístic", 
                          limits = c(global_min_habitatges_us_turistic, global_max_habitatges_us_turistic)) +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Habitatges Ús Turístic")
    

    ggplotly(h)
  })
  
  output$mapPlot6 <- renderPlotly({
    cb <- mapbcn[mapbcn$anys == input$year6, ]
    
    b <- ggplot() +
      geom_sf(data = cb$geometry, aes(fill = factor(cb$cluster))) +
      scale_fill_manual(values = c("1" = "green", "2" = "red", "3" = "yellow"), name = "Gentrificació Barris") +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Gentrificació Barris")
    
    ggplotly(b)
  })
  
  output$mapPlot7 <- renderPlotly({
    cd <- mapbcn.d[mapbcn.d$anys == input$year7, ]
    
    d <- ggplot() +
      geom_sf(data = cd$geometry, aes(fill = factor(cd$cluster))) +
      scale_fill_manual(values = c("1" = "green", "2" = "yellow", "3" = "red"), name = "Gentrificació Districtes") +
      theme_minimal() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      ggtitle("Gentrificació Districtes")
    
    ggplotly(d)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
