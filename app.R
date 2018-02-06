# Kentucky Election 2018
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(magrittr)
library(glue)

load('kyelect_data.rda')
# kyelect_leaflets %<>% filter(chamber %in% c('H','S'))

process_election <- function(df){
  df %>% 
    mutate(chamber = case_when(str_detect(`contest name`, 'State Representative') == T ~ 'KY House',
                               str_detect(`contest name`, 'State Senator') == T ~ 'KY Senate',
                               str_detect(`contest name`, 'US Representative') == T ~ 'US House',
                               T ~ 'other'),
           district = str_extract(`contest name`, '\\d+')) %>% 
    filter(chamber != 'other') %>% 
    select(party = `party name`, chamber, district, votes = `total votes`, pct = `percent of votes`)
}

e_2012 <- read_csv('election_2012.csv') %>% process_election() %>% mutate(year = '2012')
e_2014 <- read_csv('election_2014.csv') %>% process_election() %>% mutate(year = '2014')
e_2016 <- read_csv('election_2016.csv') %>% process_election() %>% mutate(year = '2016')

elections <- bind_rows(e_2012, e_2014, e_2016)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = 'Kentucky Election, 2018', titleWidth = 300),
  dashboardSidebar(
    selectInput(inputId = 'chamber_selection',
                label = 'Chamber:',
                choices = c('Kentucky House of Representatives' = 'ky_house',
                            'Kentucky Senate' = 'ky_senate',
                            'US House of Representatives' = 'us_house'),
                selected = 'ky_house'),
    # sidebarMenu(id = 'chamber_selector',
    #             menuItem('Kentucky House of Representatives', tabName = 'ky_house'),
    #             menuItem('Kentucky Senate', tabName = 'ky_senate'),
    #             menuItem('US House of Representatives', tabName = 'us_house'),
    #             selected = 'ky_house'
    # ),
    uiOutput('district_selection'),
    helpText('Developed by Robert Kahne')
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
   fluidRow(
     box(uiOutput('district_name'),
         width = 12, align = 'center')
     ),
   fluidRow(box(title = 'Map', status = 'primary', leafletOutput('district_map'),
            width = 12, align = 'center')
            ),
   fluidRow(box(title = 'Candidates', status = 'primary', dataTableOutput('candidates_table'),
            width = 12, align = 'center')
            ),
   fluidRow(box(title = 'History', status = 'primary', plotlyOutput('history'),
                width = 12, align = 'center')
              ),
   fluidRow(
     box(includeMarkdown('footer.md'), width = 12)
   )
   )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$district_selection <- renderUI({
     if(reactive({input$chamber_selection})() == 'ky_house'){
       vals <- as.character(1:100)
     }else if(reactive({input$chamber_selection})() == 'ky_senate'){
       vals <- as.character(which(1:28 %% 2 == 0))
     }else if(reactive({input$chamber_selection})() == 'us_house'){
       vals <- as.character(1:6)
     }
     
     selectInput('district_number', 'District:', vals)
   })
   
   output$district_name <- renderUI({
     if(reactive({input$chamber_selection})() == 'ky_house'){
       paste('Kentucky House District', reactive({input$district_number})()) %>% h1(align = 'center')
     }else if(reactive({input$chamber_selection})() == 'ky_senate'){
       paste('Kentucky Senate District', reactive({input$district_number})()) %>% h1(align = 'center')
     }else if(reactive({input$chamber_selection})() == 'us_house'){
       paste('US House District', reactive({input$district_number})()) %>% h1(align = 'center')
     }
   })
   
   output$district_map <- renderLeaflet({
     ch <- case_when(reactive({input$chamber_selection})() == 'ky_house' ~ 'H',
                          reactive({input$chamber_selection})() == 'ky_senate' ~ 'S',
                          reactive({input$chamber_selection})() == 'us_house' ~ 'US')
     
     (kyelect_leaflets %>% filter(chamber == ch, district == reactive({input$district_number})()))$district_map[[1]]
   })
   
   output$candidates_table <- renderDataTable({
     ch <- case_when(reactive({input$chamber_selection})() == 'ky_house' ~ 'State Representative',
                     reactive({input$chamber_selection})() == 'ky_senate' ~ 'State Senator',
                     reactive({input$chamber_selection})() == 'us_house' ~ 'US Representative')
     
     candidates %>% 
       filter(Office == ch,
              District == reactive({input$district_number})()) %>% 
       select(Name, Party, Website, Facebook, Twitter)%>% 
       mutate(Website = ifelse(is.na(Website), '', glue('<a href = "{Website}" target = "_blank">link</a>')),
              Facebook = ifelse(is.na(Facebook), '', glue('<a href = "{Facebook}" target = "_blank">link</a>')),
              Twitter = ifelse(is.na(Twitter), '', glue('<a href = "{Twitter}" target = "_blank">link</a>'))) %>% 
       arrange(Party) %>% 
       datatable(options = list(dom = 't'),
                 rownames = FALSE,
                 escape = F)
   })
   
   output$history <- renderPlotly({
     ch <- case_when(reactive({input$chamber_selection})() == 'ky_house' ~ 'KY House',
                     reactive({input$chamber_selection})() == 'ky_senate' ~ 'KY Senate',
                     reactive({input$chamber_selection})() == 'us_house' ~ 'US House')
     
     e <- elections %>% 
       filter(chamber == ch, district == reactive({input$district_number})()) %>% 
       select(party, votes, year) %>% 
       spread(party, votes)
     
     if(!'DEM' %in% names(e)) e %<>% mutate(DEM = 0)
     if(!'REP' %in% names(e)) e %<>% mutate(REP = 0)
     
     e %>%
       replace_na(list(DEM = 0, REP = 0)) %>%
       mutate(DEM_PCT = DEM / (DEM  + REP),
              GOP_PCT = REP / (DEM + REP),
              dem_txt = scales::comma_format()(DEM),
              gop_txt = scales::comma_format()(REP)) %>% 
       plot_ly(x = ~year, y = ~GOP_PCT, name = 'GOP', type = 'bar', marker = list(color = '#de2d26'), text = ~gop_txt) %>% 
       add_trace(y = ~DEM_PCT, name = 'Dem', marker = list(color ='#3182bd'), text = ~dem_txt) %>%
       layout(barmode = 'stack',
              xaxis = list(title = ""),
              yaxis = list(title = '',tickformat = "%"))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)
