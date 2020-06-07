library(sp)
library(leaflet)
library(readxl)
library(shinythemes)
library(shiny)
library(tidyverse)
library(dplyr)
library(esquisse)

healthDf <- read_xlsx("C:/Users/31322/Desktop/disease/healthDf.xlsx")
View(healthDf)

# UI

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> India Health care from 2013 to 2017</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("yearInput", label = h3("Year"),
                                        choices = c(2013,
                                                    2014,
                                                    2015,
                                                    2016,
                                                    2017,
                                                    2018,
                                                    2019,
                                                    2020)),
                            selectInput("diseaseInput", label = h3("Diseases"),
                                        choices = c("Malaria_Cases",
                                                    "Dengue_Cases",
                                                    "TB_Cases",
                                                    "HIV_Cases",
                                                    "Swine_Flu_Cases",
                                                    "AES_Cases",
                                                    "Malaria_Deaths",
                                                    "Dengue_Deaths",
                                                    "TB_Deaths",
                                                    "HIV_Deaths",
                                                    "Swine_Flu_Deaths",
                                                    "AES_Deaths"
                                        ))),
                          mainPanel(leafletOutput(outputId = 'map', height = 
                                                    800) 
                          ))
))

print(healthDf)


# SERVER
server <- shinyServer(function(input, output, session) {
  
  selectedYear <- reactive({
    as.integer(input$yearInput)
  })
  
  selectedDisease <- reactive({
    input$diseaseInput
  })
  
  selectedData <- reactive({
    healthDf %>% 
      select("state","latitude","longitude","Year", "Malaria_Cases",
             "Malaria_Deaths","TB_Cases","TB_Deaths","HIV_Cases","HIV_Deaths","Dengue_cases",
             "Dengue_Deaths","Acute_Encephalitis_Syndrome_cases","Acute_Encephalitis_Syndrome_deaths",
             "Swine_Flu_cases","Swine_Flu_deaths","tb_case_range","tb_death_range","mal_cases_range","mal_death_range","Hiv_case_range",
             "Hiv_death_range","Deng_Case_range","Deng_death_range","sw_flu_cases_range",
             "sw_flu_cases_range","AES_case_range","AES_death_range"
      ) %>%
      
      
      filter(healthDf$Year == selectedYear())
  })
  
  pal_den_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$Deng_Case_range)
  pal_den_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$Deng_death_range)
  pal_mal_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$mal_cases_range)
  pal_mal_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$mal_death_range)
  pal_tb_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$tb_case_range)
  pal_tb_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$tb_death_range)
  pal_hiv_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$Hiv_case_range)
  pal_hiv_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$Hiv_death_range)
  pal_sw_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$sw_flu_cases_range)
  pal_sw_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$sw_flu_death_range)
  pal_aes_c=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$AES_case_range)
  pal_aes_d=colorFactor(palette = c("white","green", "yellow","red"), domain = healthDf$AES_death_range)
  
  
  output$map <- renderLeaflet({
    leaflet(healthDf) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addCircleMarkers(data = healthDf ,lat = healthDf$latitude, lng = healthDf$longitude,
                       popup = paste0("<strong>State: </strong>", 
                                      healthDf$state),
                       color = "#BDBDC3",
                       fillOpacity = 0.8,
                       weight = 1)
    
  })
  
  
  observeEvent(input$yearInput, {
    palValue <- NULL
    disValue <- NULL
    if(selectedDisease() == "Malaria_Cases"){
      palValue <- pal_mal_c
      disValue <- selectedData()$mal_cases_range
    }
    else if(selectedDisease() == "Malaria_Deaths"){
      palValue <- pal_den_c
      disValue <- selectedData()$mal_death_range
    }  
    else if(selectedDisease() == "Dengue_Cases"){
      palValue <- pal_den_c
      disValue <- selectedData()$Deng_Case_range
    }
    else if(selectedDisease() == "Dengue_Deaths"){
      palValue <- pal_den_c
      disValue <- selectedData()$Deng_death_range
    }
    else if(selectedDisease() == "TB_Cases"){
      palValue <- pal_tb
      disValue <- selectedData()$tb_case_range
    }
    else if(selectedDisease() == "TB_Deaths"){
      palValue <- pal_tb_d
      disValue <- selectedData()$tb_death_range
    }
    else if(selectedDisease() == "Swine_Flu_Cases"){
      palValue <- pal_sw_c
      disValue <- selectedData()$sw_flu_case_range
    }
    else if(selectedDisease() == "Swine_Flu_Deaths"){
      palValue <- pal_sw_d
      disValue <- selectedData()$sw_flu_death_range
    }
    else if(selectedDisease() == "AES_Cases"){
      palValue <- pal_aes_c
      disValue <- selectedData()$AES_case_range
    }
    else if(selectedDisease() == "AES_Deaths"){
      palValue <- pal_aes_d
      disValue <- selectedData()$AES_death_range
    }
    else if(selectedDisease() == "Hiv_Cases"){
      palValue <- pal_hiv_c
      disValue <- selectedData()$Hiv_case_range
    }
    else{
      palValue <- pal_hiv_d
      disValue <- selectedData()$Hiv_death_range
    }
    
    leafletProxy("map", data = selectedData()) %>%
      clearShapes() %>%
      clearPopups() %>%
      clearMarkers() %>%
      removeControl("legend") %>%
      
      addCircles(lng = selectedData()$longitude,
                 lat = selectedData()$latitude,
                 color= if(selectedDisease() == "Malaria_Cases"){
                   ~pal_mal_c(selectedData()$mal_cases_range)
                 }else if(selectedDisease() == "Malaria_Deaths"){
                   ~pal_mal_d(selectedData()$mal_death_range)
                 }else if(selectedDisease() == "Dengue_Cases"){
                   ~pal_den_c(selectedData()$Deng_Case_range)
                 }else if(selectedDisease() == "Dengue_Deaths"){
                   ~pal_den_d(selectedData()$Deng_death_range)
                 }else if(selectedDisease() == "TB_Cases"){
                   ~pal_tb_c(selectedData()$tb_case_range)
                 }else if(selectedDisease() == "TB_Deaths"){
                   ~pal_tb_d(selectedData()$tb_death_range)
                 }else if(selectedDisease() == "Swine_Flu_Cases"){
                   ~pal_sw_c(selectedData()$sw_flu_cases_range)
                 }else if(selectedDisease() == "Swine_Flu_Deaths"){
                   ~pal_sw_d(selectedData()$sw_flu_death_range)
                 }else if(selectedDisease() == "AES_Cases"){
                   ~pal_aes_c(selectedData()$AES_case_range)
                 }else if(selectedDisease() == "AES_Deaths"){
                   ~pal_aes_d(selectedData()$AES_death_range)
                 }else if(selectedDisease() == "Hiv_Cases"){
                   ~pal_hiv_c(selectedData()$Hiv_case_range)
                 }else{
                   ~pal_hiv_d(selectedData()$Hiv_death_range)
                 },
                 fillColor = "transparent",
                 highlightOptions = highlightOptions(weight = 10,
                                                     color = "brown",
                                                     fillColor = "purple"),
                 label = selectedData()$state,
      ) %>%
      
      addCircleMarkers(lat = selectedData()$latitude,
                       lng = selectedData()$longitude,
                       color = if(selectedDisease() == "Malaria_Cases"){
                         ~pal_mal_c(selectedData()$mal_cases_range)
                       }else if(selectedDisease() == "Malaria_Deaths"){
                         ~pal_mal_d(selectedData()$mal_death_range)
                       }else if(selectedDisease() == "Dengue_Cases"){
                         ~pal_den_c(selectedData()$Deng_Case_range)
                       }else if(selectedDisease() == "Dengue_Deaths"){
                         ~pal_den_d(selectedData()$Deng_death_range)
                       }else if(selectedDisease() == "TB_Cases"){
                         ~ppal_tb_c(selectedData()$tb_case_range)
                       }else if(selectedDisease() == "TB_Deaths"){
                         ~pal_tb_d(selectedData()$tb_death_range)
                       }else if(selectedDisease() == "Swine_Flu_Cases"){
                         ~pal_sw_c(selectedData()$sw_flu_cases_range)
                       }else if(selectedDisease() == "Swine_Flu_Deaths"){
                         ~pal_sw_d(selectedData()$sw_flu_death_range)
                       }else if(selectedDisease() == "AES_Cases"){
                         ~pal_aes_c_c(selectedData()$AES_case_range)
                       }else if(selectedDisease() == "AES_Deaths"){
                         ~pal_aes_d(selectedData()$AES_death_range)
                       }else if(selectedDisease() == "Hiv_Cases"){
                         ~pal_hiv_c(selectedData()$Hiv_case_range)
                       }else{
                         ~pal_hiv_d(selectedData()$Hiv_death_range)
                       },
                       label = selectedData()$state,
                       popup = paste(selectedDisease()," Cases:", disValue)
      ) %>%
      addLegend(position = "bottomright", pal = palValue, values = disValue ,
                title = "Range",
                opacity = 1,
                layerId = "legend")
    
  })
  
  # 
  # #Observe event for disease
  # 
  observeEvent(input$diseaseInput, {
    palValue <- NULL
    disValue <- NULL
    print(selectedData())
    print(selectedDisease())
    if(selectedDisease() == "Malaria_Cases"){
      palValue <- pal_mal_c
      disValue <- selectedData()$mal_cases_range
    }
    else if(selectedDisease() == "Malaria_Deaths"){
      palValue <- pal_mal_d
      disValue <- selectedData()$mal_death_range
    }
    else if(selectedDisease() == "Dengue_Cases"){
      palValue <- pal_den_c
      disValue <- selectedData()$Deng_Case_range
    }
    else if(selectedDisease() == "Dengue_Deaths"){
      palValue <- pal_den_d
      disValue <- selectedData()$Deng_death_range
    }
    else if(selectedDisease() == "TB_Cases"){
      palValue <- pal_tb_c
      disValue <- selectedData()$tb_case_range
    }
    else if(selectedDisease() == "TB_Deaths"){
      palValue <- pal_tb_d
      disValue <- selectedData()$tb_death_range
    }
    else if(selectedDisease() == "Swine_Flu_Cases"){
      palValue <- pal_sw_c
      disValue <- selectedData()$sw_flu_cases_range
    }
    else if(selectedDisease() == "Swine_Flu_Deaths"){
      palValue <- pal_sw_d
      disValue <- selectedData()$sw_flu_death_range
    }
    else if(selectedDisease() == "AES_Cases"){
      palValue <- pal_aes_c
      disValue <- selectedData()$AES_case_range
    }
    else if(selectedDisease() == "AES_Deaths"){
      palValue <- pal_aes_d
      disValue <- selectedData()$AES_death_range
    }
    else if(selectedDisease() == "Hiv_Cases"){
      palValue <- pal_hiv_c
      disValue <- selectedData()$Hiv_case_range
    }
    else{
      palValue <- pal_hiv_d
      disValue <- selectedData()$Hiv_death_range
    }
    
    
    leafletProxy("map", data = selectedData()) %>%
      clearShapes() %>%
      clearPopups() %>%
      clearMarkers() %>%
      removeControl("legend") %>% 
      
      addCircles(lng = selectedData()$longitude, 
                 lat = selectedData()$latitude,
                 # color= colorValue,
                 color = if(selectedDisease() == "Malaria_Cases"){
                   ~pal_mal_c(selectedData()$mal_cases_range)
                 }else if(selectedDisease() == "Malaria_Deaths"){
                   ~pal_mal_d(selectedData()$mal_death_range)
                 }else if(selectedDisease() == "Dengue_Cases"){
                   ~pal_den_c(selectedData()$Deng_Case_range)
                 }else if(selectedDisease() == "Dengue_Deaths"){
                   ~pal_den_d(selectedData()$Deng_death_range)
                 }else if(selectedDisease() == "TB_Cases"){
                   ~pal_tb_c(selectedData()$tb_case_range)
                 }else if(selectedDisease() == "TB_Deaths"){
                   ~pal_tb_d(selectedData()$tb_death_range)
                 }else if(selectedDisease() == "Swine_Flu_Cases"){
                   ~pal_sw_c(selectedData()$sw_flu_cases_range)
                 }else if(selectedDisease() == "Swine_Flu_Deaths"){
                   ~pal_sw_d(selectedData()$sw_flu_death_range)
                 }else if(selectedDisease() == "AES_Cases"){
                   ~pal_aes_c(selectedData()$AES_case_range)
                 }else if(selectedDisease() == "AES_Deaths"){
                   ~pal_aes_d(selectedData()$AES_death_range)
                 }else if(selectedDisease() == "Hiv_Cases"){
                   ~pal_hiv_c(selectedData()$Hiv_case_range)
                 }else{
                   ~pal_hiv_d(selectedData()$Hiv_death_range)
                 },
                 fillColor = "transparent",
                 highlightOptions = highlightOptions(weight = 10,
                                                     color = "brown",
                                                     fillColor = "purple"),
                 label = selectedData()$state,
      ) %>%
      
      addCircleMarkers(lat = selectedData()$latitude,
                       lng = selectedData()$longitude,
                       # color= colorValue,
                       color = if(selectedDisease() == "Malaria_Cases"){
                         ~pal_mal_c(selectedData()$mal_cases_range)
                       }else if(selectedDisease() == "Malaria_Deaths"){
                         ~pal_mal_d(selectedData()$mal_death_range)
                       }else if(selectedDisease() == "Dengue_Cases"){
                         ~pal_den_c(selectedData()$Deng_Case_range)
                       }else if(selectedDisease() == "Dengue_Deaths"){
                         ~pal_den_d(selectedData()$Deng_death_range)
                       }else if(selectedDisease() == "TB_Cases"){
                         ~pal_tb_c(selectedData()$tb_case_range)
                       }else if(selectedDisease() == "TB_Deaths"){
                         ~pal_tb_d(selectedData()$tb_death_range)
                       }else if(selectedDisease() == "Swine_Flu_Cases"){
                         ~pal_sw_c(selectedData()$sw_flu_cases_range)
                       }else if(selectedDisease() == "Swine_Flu_Deaths"){
                         ~pal_sw_d(selectedData()$sw_flu_death_range)
                       }else if(selectedDisease() == "AES_Cases"){
                         ~pal_aes_c(selectedData()$AES_case_range)
                       }else if(selectedDisease() == "AES_Deaths"){
                         ~pal_aes_d(selectedData()$AES_death_range)
                       }else if(selectedDisease() == "Hiv_Cases"){
                         ~pal_hiv_c(selectedData()$Hiv_case_range)
                       }else{
                         ~pal_hiv_d(selectedData()$Hiv_death_range)
                       },
                       label = selectedData()$state,
                       popup = paste(selectedDisease()," Cases:", disValue)
      ) %>%
      addLegend(position = "topright", pal = palValue, values = disValue,
                title = "Range",
                opacity = 1,
                layerId = "legend")
    
  })
  
})

# Run app! 
shinyApp(ui = ui, server = server)