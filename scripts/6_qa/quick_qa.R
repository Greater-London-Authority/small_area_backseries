## rough script to QA the results etc
## some plotting, some outlier finding, etc

## 0. libraries and functions
library(data.table)
library(shiny)


## 1. data
backseries <- data.table(readRDS("output_data/revised_backseries_msoa21cd_2012_2024.rds"))


## 2. for a single lsoa, shiny app with charts showing deaths and net flows by age (because these are the two components that are estimated. We make minimal/no changes to births and population)
## the point is to cycle through to see if in general the results look like they make sense. Too many lsoas, never mind lsoa-year combinations, to do any sort of comprehensive sweep
## because performance is really slow if the entire backseries dataset is put in, the app only runs on one lad at a time

  ### 2.1 defining the UI
all_lads <- backseries[, unique(lad23cd)]
lad_sel <- all_lads[200]

lsoa_selection <- backseries[lad23cd == lad_sel, unique(msoa21cd)]

sex_selection <- c("male", "female")

year_selection <- 2012:2024

backseries_lad <- backseries[lad23cd == lad_sel, ]


ui <- fluidPage(
  
  tags$style(type='text/css', ".selectize-input { font-size: 9.5pt; line-height: 9.5pt;} .selectize-dropdown { font-size: 9.5pt; line-height: 9.5pt; }"),
  
  selectInput("sex", "Choose sex", sex_selection),
  
  selectInput("year", "Choose year", year_selection),
  
  selectizeInput("lsoa", "Choose lsoa", lsoa_selection),
  
  plotOutput("plot", height = "1000px")
  
)

  ### 2.2 defining the server
server <- function(input, output, session){
  
  output$plot <- shiny::renderPlot({
    
    #### the inputs
    lsoa_sel <- input$lsoa
    
    year_sel <- input$year
    
    sex_sel <- input$sex
    
    par(mfrow = c(2, 1))
    
    #### the flows chart
    lsoa_dat <- backseries_lad[msoa21cd == lsoa_sel & year == year_sel & sex == sex_sel, ]
    
    ymax <- max(lsoa_dat[, c("net_flows", "inflow", "outflow")])
    
    ymin <- min(-lsoa_dat[, c("outflow")])
    
    net_col <- rgb(200, 0, 200, 100, maxColorValue = 255)
    
    plot(x = 1, y = 1, type = "n", bty = "n", las = 1,
         xlim = c(0, 75), ylim = c(ymin, ymax),
         ylab = "", xlab = "")
    
    lines(x = c(0, 75), y = c(0, 0), col = "lightgrey", lty = "dotted")
    
    lines(x = lsoa_dat[, age], y = lsoa_dat[, net_flows],
          col = net_col, lwd = 1.5)
    
    lines(x = lsoa_dat[, age], y = lsoa_dat[, inflow], 
          col = "blue", lwd = 2)
    
    lines(x = lsoa_dat[, age], y = -lsoa_dat[, outflow], 
          col = "red", lwd = 2)
    
    
    #### the deaths chart
    plot(x = lsoa_dat[, age], y = lsoa_dat[, deaths], 
         col = "black", lwd = 2,
         type = "l", bty = "n", las = 1,
         ylab = "", xlab = "")
    
    grid()
    
    par(mfrow = c(1, 1))
    
    
  })
  
}

  ### 2.3 running the app
shinyApp(
  ui = ui, 
  server = server 
)



## 3. histograms of the key variables

  ### 3.1. total
plot(density(backseries[, deaths], na.rm = TRUE))
hist(backseries[, deaths], breaks = 1000)

hist(backseries[, net_flows], breaks = 1000)

hist(backseries[, net_flows], breaks = 1000)
hist(backseries[, net_flows], breaks = 1000)


## any notes from qa
## - same as for wards, something wrong with age 0 in 2022
## - 2012 in east village in Newham, still an issue because no lsoas are fitted to the msoas. But I think it's fine to leave it. Just one small bit of distorted data, and what else are we meant to do? Also, it probably accurately reflects reality. There really was no one living there in 2011.  
## hmm. 2015 in east village. Almost nothing allocated to outflows, all to inflows, and high net flows. Something like this happens occasionally elsewhere. 
## still occasionally the issue of negative outflows...sometimes higher than inflows! What do we do? Probably something we need to address. 
