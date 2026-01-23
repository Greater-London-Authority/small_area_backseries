## rough script to QA the results etc
## some plotting, some outlier finding, etc

## 0. libraries and functions
library(data.table)
library(shiny)

source("scripts/inputs.R")

## 1. data
backseries <- data.table(readRDS(file_path <- paste0("output_data/revised_backseries_", dest_geog_colname, "_", min_year + 1, "_", max_year, ".rds")))

## 2. for a single lsoa, shiny app with charts showing deaths and net flows by age (because these are the two components that are estimated. We make minimal/no changes to births and population)
## the point is to cycle through to see if in general the results look like they make sense. Too many lsoas, never mind lsoa-year combinations, to do any sort of comprehensive sweep
## because performance is really slow if the entire backseries dataset is put in, the app only runs on one lad at a time

  ### 2.1 defining the UI
#all_lads <- backseries[, unique(lad23cd)]
#lad_sel <- all_lads[200]
#lad_sel <- "E09000032"

#lsoa_selection <- backseries[lad23cd == lad_sel, unique(msoa21cd)]

sex_selection <- c("male", "female")

year_selection <- 2012:2024

#backseries_lad <- backseries[lad23cd == lad_sel, ]
all_lsoas <- backseries[, unique(msoa21cd)]

backseries_lad <- backseries[msoa21cd %in% all_lsoas[1:500] ,]

lsoa_selection <- all_lsoas[1:500]


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
         xlim = c(0, 90), ylim = c(ymin, ymax),
         ylab = "", xlab = "")
    
    lines(x = c(0, 90), y = c(0, 0), col = "lightgrey", lty = "dotted")
    
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
hist(backseries[, deaths], breaks = 1000) # most areas/ages have 0 deaths
hist(backseries[deaths != 0 & deaths <=3.1, deaths], breaks = 1000) # and beyond that, looks like a perfect poisson distribution! Although with spikes at 2, 1.5, 1, 0.5, etc....non-convergence? Or do we expect this? 


hist(backseries[, net_flows], breaks = 1000)

hist(backseries[, net_flows], breaks = 1000)
hist(backseries[, net_flows], breaks = 1000)

hist(backseries[, births], breaks = 1000)

hist(backseries[, population], breaks = 1000)


## 4. plots, or app, of time series of the key variables



## any notes from qa
## - same as for wards, something wrong with age 0 in 2022
## - 2012 in east village in Newham, still an issue because no lsoas are fitted to the msoas. But I think it's fine to leave it. Just one small bit of distorted data, and what else are we meant to do? Also, it probably accurately reflects reality. There really was no one living there in 2011.  
## hmm. 2015 in east village. Almost nothing allocated to outflows, all to inflows, and high net flows. Something like this happens occasionally elsewhere. 
## still occasionally the issue of negative outflows...sometimes higher than inflows! What do we do? Probably something we need to address. 

## for some msoas, looked like netflows at age 0 are way too low. But this isn't the case for all, or even anywhere near most, msoas, so it's not a systemic data issue. Is it of concern at all then? Or just a reflection of real high net outflows at age 0 in particular areas? 
## and then again, the issue of higher net outflows at age 85+, due to the distortion introduced by apportioning 75+ evenly into single year of age from 75 to 85+. Is it bad enough that we should do something about it? 
## is there something up with flows at ages 0 and 1? One always seems high, the other seems low...check that I have done these ones correctly. 

## msoa E02007083 looks funky, especially in later years. Very high positive netflows, and barely any outflow. But I think this makes sense - it's Nine Elms, which is an area that has sprung up out of nothing and attracted huge inflows. 
## but of course, even if the net flows here are correct (I think they broadly are), using patterns of in and out flows from 2011 for an area like this is a very flawed approach. But I suppose there isn't anything we can do about this. 

