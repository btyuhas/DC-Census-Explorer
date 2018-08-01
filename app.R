# x <- c('sf', 'leaflet', 'tidyverse', 'tidycensus', 'viridis', 'shiny', 'rvest')
# install.packages(x)
library('sf')
library('leaflet')
library('tidyverse')
library('tidycensus')
library('viridis')
library('shiny')
library('rvest')
library('magrittr')
library('shinythemes')

census_api_key('b645f9b5bbd914cdcda312d81863e843a395e291',install=TRUE,overwrite = TRUE)
readRenviron("~/.Renviron")

vars <- read.csv("vars.csv", stringsAsFactors = F)

VAlist <- c("Loudoun County", "Fairfax County", "Fairfax City", "Arlington County", "Alexandria")
MDlist <- c("Montgomery County", "Prince George")

ui <- fluidPage(
  sidebarLayout(position = 'right',
                
    #Dynamic inputs
    sidebarPanel(
      
      #Variable choices
      selectInput("variable", "Choose a variable:", choices = vars$concept, 
                  selected = "Median Home Value"),
      textInput("custom", label = NA, placeholder = "Custom Variable"),
      checkboxGroupInput('summary', label=NA, choices = c("Per Capita" = "B01003_001",
                                                          "Income Ratio" = "B19013_001"),
                         inline=T),
      #Geography choices
      radioButtons('geography', "Geography Level", choices = c('county', 'tract'),
                   selected = 'tract', inline = T),
      
      #Year choices
      sliderInput("year", "Year", min = 2010, max = 2016, value = 2016, step = 1),
      radioButtons('hide', 'Change Counties', choices = c('hide' = '0', 'show' = '1'),
                   selected = 'hide', inline = T),
      
      #County choices
      conditionalPanel('input.hide == 1',
        checkboxGroupInput("VA", "Virginia Counties", choices=VAlist, selected =
                             c("Loudoun County", "Fairfax County")),
        checkboxGroupInput("MD", "Maryland Counties", choices=MDlist,selected = "Montgomery County"),
        checkboxGroupInput("DC", "District of Columbia", choices="District of Columbia",
                           selected = "District of Columbia")
      ),
      
      #Explanation of variable used
      helpText(textOutput('explain'))),
    
    #Seperates map and density plot onto different tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("mymap", height = "850")),
        tabPanel("Distribution", plotOutput("plot")),
        tabPanel("Data", dataTableOutput('table'))
      )
    ))
)

server <- function(input, output){
  #Overides variable choice list with custom inputed variables 
  variable <- reactive({
    if(!(input$custom == "")) {
      variable <- input$custom
    }
    else{
      variable <- vars[vars$concept==input$variable,]$name
    }
  })
  
  #Gets data from census API according to user inputs
  data <- reactive({
    dataVA <- get_acs(geography = input$geography, 
                    variables = variable(), 
                    state = 'VA',
                    county = input$VA,
                    year = input$year,
                    geometry = TRUE,
                    summary_var = input$summary)
    
    dataDC <- get_acs(geography = input$geography, 
                    variables = variable(), 
                    state = 'DC',
                    county = input$DC,
                    year = input$year,
                    geometry = TRUE,
                    summary_var = input$summary)
    
    dataMD <- get_acs(geography = input$geography, 
                    variables = variable(), 
                    state = 'MD',
                    county = input$MD,
                    year = input$year,
                    geometry = TRUE,
                    summary_var = input$summary)
  
   VA <- filter(dataVA, !is.null(input$VA))
   DC <- filter(dataDC, !is.null(input$DC))
   MD <- filter(dataMD, !is.null(input$MD))
   data <- rbind(VA, DC, MD) 
   
   if(!is.null(input$summary)){
     if(input$summary == "B19013_001") {
       data %<>% mutate(summary_est = summary_est/12)
     }
     data %<>% 
       mutate(val = estimate/summary_est) %>% 
       mutate(moe = moe / summary_moe)
   }
   else{
    data %<>% mutate(val = estimate)
   }
  })
  
  title <- reactive({
    url <- "https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&var="
    url %<>%  paste(str_replace(variable(), fixed("_"), ''), sep = '')
    webpage <- read_html(url)
    node <- html_node(webpage, ".MB_Variable_Label")
    text <- html_text(node) %>% 
      strsplit(split = "\n") %>%
      unlist() %>%
      .[. != ""]
  })
  
  output$mymap <- renderLeaflet({
    
    pal <- colorNumeric(palette = "viridis", domain = data()$val)
    st_transform(data(), crs = "+init=epsg:4326") %>% 
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(popup = ~ paste(sep='<br/>',str_extract(NAME, "^([^,]*)"), 
                                  paste(sep=' ', "Estimate:", val)),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.5,
                  color = ~ pal(val)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ val,
                title = title(),
                opacity = 1)
  })
  
  #Distribution of values
  output$plot <- renderPlot({
    ggplot(data(), aes(val)) +
      geom_density(fill = 'blue', alpha = .5) +
      labs(title=paste("Distribution of", title()), x = 'Estimate', y = 'Density')
  })
  
  #Data output
  output$table <- renderDataTable({
    data() %>% 
      as.data.frame() %>% 
      select(NAME, variable, val, moe)
    
  }, options=list(pageLength=10))
  
  #Some webscraping to get a definition of the variable used
  output$explain <- renderPrint({
    url <- "https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table="
    url %<>% paste(str_extract(variable(), "[^_]+"), sep='')
    webpage <- read_html(url)
    node <- html_node(webpage, ".DocSecContent")
    text <- html_text(node) %>% 
      strsplit(split = "\n") %>%
      unlist() %>%
      .[. != ""]
    text[1]
  })
}

shinyApp(ui, server)