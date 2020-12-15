library(shiny)
library(shinydashboard)
library(knitr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tidyr)
library(stringr)
library(wordcloud2)
library(leaflet)
library(kableExtra)
library(gridExtra)
library(igraph)
library(ggraph)
library(drat)
library(rgdal)
library(tigris)
library(grid)


# Read Data
listings_ST <- readRDS("listings_ST2.rds")
listings_DS <- readRDS("listings_DS2.rds")
# listings_All <- rbind(listings_ST,listings_DS)
# saveRDS(listings_All,"listings_All.rds")
listings_All <- readRDS("listings_All.rds")

# Stop Words
data(stop_words)
mystopwords <- tibble(word = c("data","required","position","including","skills","ability","u.s","NA",stop_words$word))

# States and city
listings_ST <- listings_ST %>% separate(location,c("city","state"), sep = ", ")
listings_DS <- listings_DS %>% separate(location,c("city","state"), sep = ", ")
listings_All <- listings_All %>% separate(location,c("city","state"), sep = ", ")


ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(title = span("Search A Data Job", 
                         span("dashboard", style = "font-size: 18px; font-weight: bold"))
                    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Job Information", tabName="Table", icon = icon("scroll")),
            menuItem("Words in Descriptions", tabName="Words", icon = icon("dashboard")),
            menuItem("Word Cloud", tabName="WordC", icon = icon("cloud")),
            menuItem("Job Location", tabName="Location",icon = icon("city")),
            menuItem("Company", tabName="Company",icon = icon("chart-bar")),
            menuItem("Mapping", tabName="Mapping",icon = icon("map-marker-alt"))
        )
    ),
    
    dashboardBody(
        tags$head( 
            tags$style(HTML(".main-sidebar { font-size: 16px; }"))
        ),
        tabItems(
            tabItem("Table",
                    fluidRow(
                        column(4,
                               selectInput(
                                   "JobTitleT", "Job Title: ", c("All", "Statistician", "Data Scientist")
                               )
                        ),
                        
                        column(4,
                               selectInput(
                                   "StateT", "State: ", c("All", unique(listings_All$state))
                               )
                        ),
                        
                    ),
                    DT::dataTableOutput("Table")
            ),
            
            tabItem("Words",
                    fluidRow(
                        column(4,
                               radioButtons(
                                   "JobTitleW", "Job Title: ", c("All", "Statistician", "Data Scientist")
                               )
                        ),
                        
                        column(4,
                               radioButtons(
                                   "PlotW", "Plot Type: ", c("Word Count","2-grams","Network")
                               )
                        ),
                        column(4,
                               numericInput(
                                   "LineW","Lines to Show:",value = 15
                               )
                        ),

                    ),
                    plotOutput("p_Words",height = 700)
            ),
            
            tabItem("WordC",
                    fluidRow(
                        column(4,
                               selectInput(
                                   "JobTitleC", "Job Title: ", c("All", "Statistician", "Data Scientist")
                               )
                        ),
                    ),
                    wordcloud2Output("p_WordsC",height = 650, width = 1200)
            ),
            
            tabItem("Location",
                    fluidRow(
                        
                        column(4,
                               selectInput(
                                   "JobTitleL", "Job Title: ", c("All", "Statistician", "Data Scientist")
                               )
                        ),
                        
                        column(4,
                               selectInput(
                                   "StateL", "State: ", c("Select One", unique(listings_All$state))
                               )
                        ), 
                        
                    ),
                    plotOutput("p_Loc"),
                    plotOutput("p_Loc2")
            ),
            
            tabItem("Company",
                    fluidRow(
                        
                        column(4,
                               selectInput(
                                   "JobTitleE", "Job Title: ", c("All", "Statistician", "Data Scientist")
                               )
                        ),
                        
                        column(4,
                               selectInput(
                                   "StateE", "State: ", c("All", unique(listings_All$state))
                               )
                        ), 
                        
                        column(4,
                               numericInput(
                                   "LineE","Top Lines to Show:",value = 10
                               )
                        ), 
                        
                    ),
                    # plotOutput("count_Comp"),
                    plotOutput("count_Comp_Loc",height = 700)
            ),
            
            tabItem("Mapping",
                    
                    fluidRow(
                        column(4,
                               selectInput(
                                   "JobTitleM", "Job Title: ", c("All", "Statistician", "Data Scientist")                       )
                        ),
                    ),
                    leafletOutput("LeafletPlot", height = 650, width = 1200),
                    fluidPage(textOutput("text1"),tags$head(tags$style("#text1{color: black; font-size: 22px}")),
                              textOutput("text2"),tags$head(tags$style("#text2{color: black; font-size: 22px}"))
                              )
                               
            )
        )
    )
)

server <- function(input, output) {
    output$Table <- DT::renderDataTable(DT::datatable({
        if (input$JobTitleT == "All"){
            data <- listings_All
        }
        else if (input$JobTitleT == "Statistician"){
            data <- listings_ST
        }
        else if (input$JobTitleT == "Data Scientist"){
            data <- listings_DS
        }
        
        if (input$StateT != "All"){
            data <- filter(data,state == input$StateT)
        }
        
        data <- data[,c(1:2,8,6)]
        names(data) <- c("Title","Company","Address","Link")
        data
    }))
    
    output$p_Words <- renderPlot({
        if (input$JobTitleW == "All"){
            data <- listings_All
            cc <- "orange"
        }
        else if (input$JobTitleW == "Statistician"){
            data <- listings_ST
            cc <- "steelblue"
        }
        else if (input$JobTitleW == "Data Scientist"){
            data <- listings_DS
            cc <- "darkgreen"
        }
        
        if (input$PlotW == "Word Count"){
            text_description <- tibble(text = data$description)
            p_Words <- text_description  %>%
                unnest_tokens(word, text) %>%
                anti_join(mystopwords) %>%
                count(word, sort = TRUE) %>%
                slice_max(n, n = input$LineW) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(n, word)) +
                geom_col(fill=cc,alpha = 0.5) +
                ggtitle("Word Count") +
                theme(axis.text=element_text(size=16), plot.title = element_text(size = 22, face = "bold")) +
                labs(x = NULL) +
                labs(y = NULL)
        }
        
        else if(input$PlotW == "2-grams"){
            bigrams_filtered <- data  %>% 
                filter(!is.na(description)) %>%
                unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word2 %in% stop_words$word) %>%
                filter(!word1 %in% mystopwords$word) %>%
                filter(!word2 %in% mystopwords$word) 
            
            bigram_counts <- bigrams_filtered %>%
                count(word1, word2, sort = TRUE)
            
            bigrams_united <- bigrams_filtered %>%
                unite(bigram, word1, word2, sep = " ")
            
            p_Words <- bigrams_united %>% count(bigram, sort = TRUE) %>%
                slice_max(n, n = input$LineW) %>%
                mutate(bigram = reorder(bigram, n)) %>%
                ggplot(aes(n, bigram)) +
                geom_col(fill=cc,alpha = 0.5) +
                ggtitle("2-grams") +
                theme(axis.text=element_text(size=16), plot.title = element_text(size = 22, face = "bold")) +
                labs(x = NULL) +
                labs(y = NULL)
        }
        
        else if(input$PlotW == "Network"){
            bigrams_filtered <- data  %>% 
                filter(!is.na(description)) %>%
                unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word2 %in% stop_words$word) %>%
                filter(!word1 %in% mystopwords$word) %>%
                filter(!word2 %in% mystopwords$word) 
            
            bigram_counts <- bigrams_filtered %>%
                count(word1, word2, sort = TRUE)
            
            bigram_graph <- bigram_counts %>%
                filter(n > 20) %>%
                graph_from_data_frame()
            
            set.seed(2020)
            a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
            
            p_Words <- ggraph(bigram_graph, layout = "fr") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                               arrow = a, end_cap = circle(.07, 'inches')) +
                geom_node_point(color = cc, alpha = 0.5, size = 3) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                theme_void()
        }
        
        p_Words
    }
    )
    
    output$p_WordsC <- renderWordcloud2({
        if (input$JobTitleC == "All"){
            data <- listings_All
        }
        else if (input$JobTitleC == "Statistician"){
            data <- listings_ST
        }
        else if (input$JobTitleC == "Data Scientist"){
            data <- listings_DS
        }
        
        text_description <- tibble(text = data$description)
        cloud <- text_description  %>%
            unnest_tokens(word, text) %>%
            anti_join(mystopwords) %>%
            count(word, sort = TRUE)
        
            wordcloud2(cloud, size = 2, minRotation = -pi/6, maxRotation = -pi/6,  rotateRatio = 1)  

    }
    )
    
    
    output$p_Loc <- renderPlot({
        if (input$JobTitleL == "All"){
            data <- listings_All
            cc <- "orange"
        }
        else if (input$JobTitleL == "Statistician"){
            data <- listings_ST
            cc <- "steelblue"
        }
        else if (input$JobTitleL == "Data Scientist"){
            data <- listings_DS
            cc <- "darkgreen"
        }
        
        data_State <- na.omit(data$state)
        data_State <- tibble(state = data_State)
        p_Loc <- data_State %>% count(state, sort = TRUE) %>% filter(n > 3) %>% ggplot() +
            geom_col(aes(x = reorder(state,-n), y = n), fill=cc,alpha = 0.5) +
            theme_bw() + 
            xlab("States") +   ylab("Count of Job Information") + 
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), legend.position='none')
        
        p_Loc
    }
    )
    
    output$p_Loc2 <- renderPlot({
        if (input$JobTitleL == "All"){
            data <- listings_All
        }
        else if (input$JobTitleL == "Statistician"){
            data <- listings_ST
        }
        else if (input$JobTitleL == "Data Scientist"){
            data <- listings_DS
        }
        
        data_City <- filter(data,state == input$StateL)
        data_City <- na.omit(data_City$city)
        data_City <- tibble(city = data_City)
        p_Loc2 <- data_City %>% count(city, sort = TRUE) %>% filter(n>1) %>% ggplot() +
            geom_col(aes(x = reorder(city,-n), y = n), fill="brown2",alpha = 0.5) +
            theme_bw() + 
            xlab("City") +   ylab("Count of Job Information") + 
            theme(axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5), legend.position='none')
        p_Loc2
    }
    )
    
    output$count_Comp_Loc <- renderPlot({
        if (input$JobTitleE == "All"){
            data <- listings_All
            cc <- "orange"
        }
        else if (input$JobTitleE == "Statistician"){
            data <- listings_ST
            cc <- "steelblue"
        }
        else if (input$JobTitleE == "Data Scientist"){
            data <- listings_DS
            cc <- "darkgreen"
        }
        
        if(input$StateE != "All"){
            data <- filter(data,state == input$StateE)
        }
        
        count_Comp_Loc <- tibble(text = data$company)  %>%
            count(text, sort = TRUE) %>%
            slice_max(n, n = input$LineE) %>%
            mutate(text = reorder(text, n)) %>%
            ggplot(aes(n, text)) +
            geom_col(fill=cc,alpha = 0.5) +
            ggtitle("Companies are hiring") +
            theme(axis.text=element_text(size=16), plot.title = element_text(size = 22, face = "bold")) +
            xlab(NULL) +   ylab(NULL)
        
        count_Comp_Loc
    }
    )
    
    output$LeafletPlot <- renderLeaflet({
        if (input$JobTitleM == "All"){
            data <- listings_All
        }
        else if (input$JobTitleM == "Statistician"){
            data <- listings_ST
        }
        else if (input$JobTitleM == "Data Scientist"){
            data <- listings_DS
        }
        
        
        Link <- paste0('<a href = ', data$link,'> Details from Indeed </a>')
        i_popup <- paste0("<strong>Titel: </strong>", data$title, "<br>", 
                          "<strong>Company: </strong>", data$company, "<br>", 
                          "<strong>Location: </strong>", data$address, "<br>",
                          "<strong>Job Link: </strong>", Link)
        
        LL <- data[9:10]
        MappingLL <- LL %>%
            leaflet() %>%
            addTiles() %>%
            setView(-78, 40, zoom = 6) %>%
            addMarkers(clusterOptions = markerClusterOptions(),popup=i_popup)
        
        MappingLL
    }
    )
    
    output$text1 <- renderText({
        "You can look at the markers for title and company."
    })
    
    output$text2 <- renderText({
        "Click the link on the markers for more information."
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
