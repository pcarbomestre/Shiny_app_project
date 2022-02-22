library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(usmap)
library(plotly)
library(bslib)
library(tmap)
library(janitor)
library(sf)
library(kableExtra)

# Data input
# ----------------------
## Time series data
df <- read_csv(here("data","filtered_data_2000_2020.csv"), col_names = TRUE) 

## Map data
map <- us_map("counties",
              include = c("CA")) %>% 
  mutate(county = str_remove_all(county, " County"))

df1 <- df %>% 
  mutate(county = str_to_title(gm_gis_county), .keep="unused") %>% 
  group_by(county, gm_chemical_name, year) %>% 
  summarise(mean_gm_result=mean(mean_gm_result))

mapdata <- left_join(map,df1,"county")

county_sf <- st_read(here('data','California_County_Boundaries','CA_Counties_TIGER2016.shp')) %>% 
  clean_names() %>% 
  rename(county = name)

combined_sf_shiny <- st_read(here('data','combined_sf_shiny.shp')) %>% 
  rename(gm_chemical_name = gm_chm_,
         povall_2019 = pv_2019,
         pctpovall_2019 = pc_2019,
         mean_gm_result = mn_gm_r)

combined_well_shiny_sf <- st_read(here('data','combined_well_shiny_sf.shp')) %>% 
  rename(well_type = well_us,
         mean_water_depth = mn_wtr_,
         povall_2019 = pv_2019,
         pctpovall_2019 = pc_2019)
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
                  
                  tabPanel("Contaminant Map",fluid = TRUE, icon = icon("map"),
                           fluidRow(
                             column(
                               p("This tool explores annual contaminant averages across California counties. 
                                 The interface allows users to select the year of interest, and the resulting chloropleth map shows contaminant concentration across time throughout California.",
                                 style="text-align:justify;color:black;background-color:gainsboro;padding:15px;border-radius:5px"),
                               width=4)
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               hr(),
                               selectInput(inputId = "pick_pollutant_map",
                                           label = "Select pollutant",
                                           choices = unique(combined_sf_shiny$gm_chemical_name),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               sliderInput(inputId = "pick_year_map",
                                           label = "Time range",
                                           min = min(combined_sf_shiny$year),
                                           max = max(combined_sf_shiny$year),
                                           value = min(combined_sf_shiny$year),
                               ) # End sliderInput
                               
                             ), # End of sidebarPanel
                             mainPanel(
                               column(
                                 "California Counties Map",
                                 tmapOutput(outputId = "gw_map_con"), width = 8)
                             ) # End of mainPanel
                           ) # End of sidebarLayout
                  ), # End of tabPanel map
                  
                  tabPanel("Contaminant Statistics", fluid = T, icon = icon("chart-area"),
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
                               ) # end selectInput fpr year
                               
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
  
  ## Contaminant Map 
  map_reactive_con <- reactive({
    combined_sf_shiny %>% 
      filter(gm_chemical_name %in% input$pick_pollutant_map) %>% 
      filter(year == input$pick_year_map[1])
  }) # end map_reactive
  
  output$gw_map_con <- renderTmap({
    tm_shape(shp = map_reactive_con()) +
      tm_borders(col = 'gray') +
      tm_fill(col = 'mean_gm_result',
              title = "Mean Contaminant Concentration",
              style = 'cont',
              popup.vars = c("Population in Poverty (2019)"="povall_2019","Percent of Population in Poverty (2019)"="pctpovall_2019"),
              popup.format = list()) 
  }
  
  ) # end output$gw_map_con
  
  ## statistic table
  ### the ui and reactives aren't interacting :/
  ca_stat <- reactive({
    df1 %>%
      filter(county == input$pick_county,
             year == input$pick_year)
  }) # end ca_stat reactive
  
  output$gw_stat <- renderTable({
    ca_stat() %>% 
      group_by(gm_chemical_name) %>%
      summarise(mean_gm_result = mean_gm_result)
  }) ### end gw_stat
  
}

shinyApp(ui=ui, server=server)

