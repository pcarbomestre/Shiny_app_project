library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(usmap)
library(plotly)
library(bslib)
library(kableExtra)


# Data input
# ----------------------
## Time series data
df <- read_csv(here("data","filtered_data_2000_2020.csv"), col_names = TRUE) %>% 
  mutate(gm_chemical_name = case_when(
    gm_chemical_name == "Bicarbonate Alkalinity (mg/l)" ~ "Bicarbonate Alkalinity",
    gm_chemical_name == "Potassium (mg/l)"  ~ "Potassium",
    gm_chemical_name == "Nitrate as N (mg/l)" ~ "Nitrate"))

## Map data
map <- us_map("counties",
              include = c("CA")) %>% 
  mutate(county = str_remove_all(county, " County"))

df1 <- df %>% 
  mutate(county = str_to_title(gm_gis_county), .keep="unused") %>% 
  group_by(county, gm_chemical_name, year) %>% 
  summarise(mean_gm_result=mean(mean_gm_result)) 

mapdata <- left_join(map,df1,"county") 
# ----------------------

# Create a custom theme
my_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#161853",
  primary = "#EC255A",
  base_font = font_google('Avenir')
)

ui <- fluidPage(theme=my_theme,
                navbarPage(
                  "Groundwater California",  # Title
                  
                  tabPanel("Home",fluid = TRUE, icon = icon("home"),
                           titlePanel(h2("California Groundwater Contamination", align = "center")),
                           fluidRow(column(br(),
                                           br(),
                                           tags$img(src="GAMAlogo.png",width="170"),
                                           br(),
                                           br(),
                                           tags$img(src="WBlogo.png",width="170"),align = "center", width = 3),
                                    
                                    column(
                                      p("California faces immense challenges as local groundwater users work to adhere to the mandates of the Sustainable Groundwater Management Act (SGMA) by the 2040s. With pumping reductions and land fallowing looming, many local government agencies and other stakeholders have to grapple with the increasing demands for water combined with climate change induced droughts and declining recharge rates.
In addition to issues of scarcity, increased demand for agricultural goods has led to a variety of pollutants contaminating California’s water supply. Among these, nitrate, potassium, and phosphate have had detrimental effects on groundwater across the state. These effluents can cause eutrophication in downstream systems, leading to algal blooms and decreased oxygen levels. 
To mitigate contamination, California sets standards for Maximum Contaminant Levels (MCLs) for these chemicals. This application aims to evaluate these threats facing California’s water supply. Through interactive maps, graphs, and other visualizations, this interface allows users to better understand the complex issues surrounding California water quality and scarcity.",
style="text-align:justify;align:center;color:black;background-color:white;padding:15px;border-radius:10px"),
tags$img(src="image.png",
         width="90%"),
br(),
br(),
p("Data Citations: 
  \n
  California State Water Resources Control Board, Ground Water - Water Quality Results (2022). Open Data Portal.",
  style="text-align:center;color:black"),
p("For more information please check the",em("GAMA Groundwater information system"),"page clicking",
  a(href="https://data.ca.gov/dataset/ground-water-water-quality-results/resource/be2d189b-dcb7-4c1c-b881-a52b278cf0a7", "HERE",target="_blank"),
  style="text-align:center;color:black"),
p("Data Citations:", style = "text-align: center; color: black; font-weight: bold"), 
p("1. California State Water Resources Control Board, Ground Water - Water Quality Results (2022). Open Data Portal.",
  style="text-align:center;color:black"),
p("2. California Department of Water Resources, Periodic Groundwater Level Measurements (2022). California Natural Resources Agency.",
  style = "text-align: center; color: black"),
width=6, align = "center"),


column(br(),
       br(),
       br(),
       tags$img(src="CAlogo.png",width="200"),
       br(),
       br(),
       br(),
       br(),
       tags$img(src="RSlogo.png",width="200"),
       width=3, align = "center")
                           ),

hr(),
p(em("Developed by"),br("S. Kaveh, T. Maggart & P. Carbó"),style="text-align:center"),
fluidRow(column(DT::dataTableOutput("RawData"),
                width = 12)),
hr()
                  ),

tabPanel("Temporal series",fluid = TRUE, icon = icon("chart-area"),
         fluidRow(
           column(
             p("This tool explores groundwater contaminants across California counties. 
                               The interface allows the user to select the relevant contaminant and the county of interest. 
                               The resulting figure explores monthly averages of the selected contaminant throughout time given the selected constraints.",
               style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1250px; align: center"),
             width=4)
         ),
         sidebarLayout(
           sidebarPanel(
             "What do you want to represent?",
             br(),
             hr(),
             selectInput(inputId = "pick_pollutant",
                         label = "Select pollutant",
                         choices = unique(df$gm_chemical_name),
                         selected = "50 Free"
             ), # End selectInput
             
             selectInput(inputId = "pick_county",
                         label = "Select County",
                         choices = unique(df$gm_gis_county),
                         selected = "50 Free"
             ), # End selectInput
             
             sliderInput(inputId = "pick_range",
                         label = "Time range",
                         min = min(df$date),
                         max = max(df$date),
                         value = c(min(df$date),max(df$date)),
                         timeFormat = "%m/%Y",
                         ticks = F
             ) # End sliderInput
             
           ), # End of sidebarPanel
           mainPanel(
             "Contaminant Temporal Series",
             plotOutput("gw_plot")
           ) # End of mainPanel
         ) # End of sidebarLayout
), # End of tabPanel Time Series

tabPanel("California Map",fluid = TRUE, icon = icon("map"),
         fluidRow(
           p("This tool explores annual contaminant averages across California counties. 
                                 The interface allows users to select the year of interest, and the resulting chloropleth map shows contaminant concentration across time throughout California.",
             style="text-align:justify;color:black;padding:15px;border-radius:5px;align:center;width:1250")
         ),
         sidebarLayout(
           sidebarPanel(
             "What do you want to represent?",
             hr(),
             selectInput(inputId = "pick_pollutant_map",
                         label = "Select pollutant",
                         choices = unique(df$gm_chemical_name),
                         selected = "50 Free"
             ), # End selectInput
             
             sliderInput(inputId = "pick_year_map",
                         label = "Time range",
                         min = min(mapdata$year),
                         max = max(mapdata$year),
                         value = c(min(mapdata$year), max(mapdata$year)), # how do we make this show up without columns???
                         timeFormat = "%Y",
                         ticks = T,
                         animate = T
             ) # End sliderInput
             
           ), # End of sidebarPanel
           mainPanel(
             column(
               "California Counties Map",
               plotOutput(outputId = "gw_map", width = "150%"), width = 8)
           ) # End of mainPanel
         ) # End of sidebarLayout
), # End of tabPanel map

tabPanel("Contaminant Statistics", fluid = T, icon = icon("table"),
         fluidRow(
           p("This table shows general statistics for each pollutant in a selected county at any time range.",
             style="text-align:justify;color:black;padding:15px;border-radius:5px;align:center;width:1250"),
         ),
         sidebarLayout(
           sidebarPanel(
             "What do you want to represent?",
             br(),
             hr(),
             selectInput(inputId = "pick_county",
                         label = "Select County",
                         choices = unique(df$gm_gis_county),
                         selected = "50 Free"
             ), # End selectInput
             
             selectInput(inputId = "pick_year", 
                         label = ("Select Year"), 
                         choices = list("Year" = c(min(df$year):max(df$year))),
                         selected = 1),
             
             hr(),
             fluidRow(column(3, verbatimTextOutput("value"))
             ), # end selectInput fpr year
             
             checkboxGroupInput(inputId = "pick_contaminant",
                                label = "Contaminant",
                                choices = c("Bicarbonate Alkalinity" = "Bicarbonate Alkalinity", 
                                            "Potassium" = "Potassium", 
                                            "Nitrate" = "Nitrate"),
                                selected = c("Bicarbonate Alkalinity", "Potassium", "Nitrate")

                                ), # end checkboxGroup

             
           ), # End of sidebarPanel
           mainPanel(
             column(
               "California Contaminant Statistics",
               tableOutput(outputId ="gw_stat"), width = 8
             ) # End of mainPanel
           ) # End of sidebarLayout
         ) # End of tabPanel statistics
         
) # End of tabPanel
                ) #end of navbarPage
)


server <- function(input,output) {
  ## Time series  
  gw_reactive <- reactive({
    df %>% 
      filter(gm_chemical_name %in% input$pick_pollutant,
             gm_gis_county %in% input$pick_county) %>% 
      filter(date >= input$pick_range[1]) %>%
      filter(date <= input$pick_range[2])
  }) # end gw_reactive
  
  output$gw_plot <- renderPlot(
    ggplot(data=gw_reactive(),aes(x=date,y=mean_gm_result)) +
      geom_area( fill="#FBC7D4", alpha=0.4) +
      geom_line(color="#f6809d", size=1) +
      geom_point(size=1, color="#f6809d") +
      labs(y ="mg/l", x = "Years") +
      theme_minimal() +
      labs(caption = "Data from CA State Water Resources Control Board.")
  ) # end output$gw_plot
  
  ## Map 
  map_reactive <- reactive({
    mapdata %>% 
      filter(gm_chemical_name %in% input$pick_pollutant_map) %>% 
      filter(year == input$pick_year_map[1])
  }) # end map_reactive
  
  output$gw_map <- renderPlot({
    ggplot(data=map_reactive(),aes(x=x,y=y,group=group)) +
      geom_polygon(data=mapdata,aes(x=x,y=y,group=group),color="black",fill="grey88",size = 0.2) +
      geom_polygon(aes(fill=mean_gm_result)) +
      coord_fixed(ratio = 1) +
      scale_fill_continuous("mg/l",trans = 'reverse') +
      scale_fill_gradient(low = "#FBC7D4", high = "#9796F0") +
      labs(caption = "Data from CA State Water Resources Control Board.") +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  }, height = 600
  
  ) # end output$gw_map
  
  ## statistic table
  ### the ui and reactives aren't interacting :/
  ca_stat <- reactive({
    df1 %>%
      filter(county == input$pick_county,
             year == input$pick_year,
             gm_chemical_name %in% input$pick_contaminant)
  }) # end ca_stat reactive
  
  output$gw_stat <- renderTable({
    ca_stat() %>% 
      group_by(gm_chemical_name) %>%
      summarise(mean_gm_result = mean_gm_result) %>% 
      rename("Chemical Name" = "gm_chemical_name",
             "Mean Concentration (mg/L)" = "mean_gm_result") 
  }) ### end gw_stat
  
}

shinyApp(ui=ui, server=server)