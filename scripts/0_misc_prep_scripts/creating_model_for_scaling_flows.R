
## this script creates the model used to account for within-area flows when scaling flows between different geographies. 
## it's a very simple process, using linear regression, with a single input (population) used to predict the output (flows within that area). 
## the data used is ukmig008 from the detailed UK migration statistics from census 2011, at oa, msoa, ward, and lad level
## the reason that the only input is population is to make the process very easy to use. For any geography, the population can simply be put into the model and a reasonable estimate of the within-area flows comes out. 
## and them once you have within-area flows you can scale up and down between geographies
## the model could be improved with more variables, different forms of modelling beyond linear regression considered, etc - depends on how great the improvement would be vs time invested and increased complexity in implementing the model
## a look at the data seemed to suggest that lad seems to behave differently, but that oa, msoa, and ward are fairly similar to each other. So the final model was run without lad, and that's what will be used in the process

## 0. libraries and functions
library(data.table)


## 1. reading in the datasets
oa <- fread("input_data/raw/ukmig008_oa.csv")

msoa <- fread("input_data/raw/ukmig008_msoa.csv")

ward <- fread("input_data/raw/ukmig008_ward.csv")

lad <- fread("input_data/raw/ukmig008_lad.csv")


## 2. getting them into the same dataset
oa[, geog_type := "oa"] # adding a column for geography type
msoa[, geog_type := "msoa"]
lad[, geog_type := "lad"]
ward[, geog_type := "ward"]

oa <- oa[, 2:5] # getting rid of the first geography column in each dataset - confusing and not needed
msoa <- msoa[, 2:5]
lad <- lad[, 2:5]
ward <- ward[, 2:5]

ukmig <- rbind(oa, msoa, ward, lad)

colnames(ukmig) <- c("geog_code", "population", "moved_within", "geog_type")


## 3. exploratory data analysis
## slightly messy, with rough notes, but keeping it in to show workings etc 

  ### 3.1. calculating percentage of population who moved within the area in the past year
ukmig[, perc_moved := round(100*(moved_within/population), 1)]

  ### 3.2. a few simple scatterplots to look at the relationship between the variables
plot(x = ukmig[, population], y = ukmig[, moved_within]) # simple untransformed scatterplot of population vs moved within looks good! Fairly linear. Although when we plot population from all geographies on the same axis, we really only see variation in lad because it's so much bigger. 
plot(x = ukmig[, population], y = ukmig[, sqrt(moved_within)]) # square root transform for all geographies looks worse...break between geographies up ward and then lad? Looks like lad level behaves differently to the rest?


plot(x = ukmig[, log(population)], y = ukmig[, log(moved_within)]) # double-log also looks good, although the small counts and 0 counts especially in oa sort of messes up the logs
plot(x = ukmig[, log(population)], y = ukmig[, moved_within]) # logging just x axis to avoid that small counts issue - nah, looks like y axis needs to be logged for this relationship to work

plot(x = ukmig[, log(population)], y = ukmig[, perc_moved]) # overall, modelling as proportions or percentages seems to make it worse, so just keep it directly in units of person
plot(x = ukmig[, log(population)], y = ukmig[, log(perc_moved)])


plot(x = ukmig[geog_type %in% c("oa", "msoa", "ward"), population], y = ukmig[geog_type %in% c("oa", "msoa", "ward"), moved_within]) # narrowed to below lad, looks decent, fair amount of high outliers on the y-axis though....
plot(x = ukmig[geog_type %in% c("oa", "msoa", "ward"), population], y = ukmig[geog_type %in% c("oa", "msoa", "ward"), sqrt(moved_within)]) # square root transform on dependent variable, looks better, maybe the best so far
plot(x = ukmig[geog_type %in% c("oa", "msoa", "ward"), population], y = ukmig[geog_type %in% c("oa", "msoa", "ward"), log(moved_within)]) # nah, again clear that they'd both need to be logged for this to make sense, but also can't do that because there are so many 0s - probably ways around this but probably not worth it anyway
plot(x = ukmig[geog_type %in% c("oa", "msoa", "ward"), population]^2, y = ukmig[geog_type %in% c("oa", "msoa", "ward"), moved_within])


plot(x = ukmig[geog_type %in% c("oa"), population], y = ukmig[geog_type %in% c("oa"), moved_within])
plot(x = ukmig[geog_type %in% c("msoa"), population], y = ukmig[geog_type %in% c("msoa"), moved_within])
plot(x = ukmig[geog_type %in% c("ward"), population], y = ukmig[geog_type %in% c("ward"), moved_within])
plot(x = ukmig[geog_type %in% c("lad"), population], y = ukmig[geog_type %in% c("lad"), moved_within]) # again looks like lad behaves differently

plot(x = ukmig[geog_type %in% c("oa"), population], y = ukmig[geog_type %in% c("oa"), sqrt(moved_within)])
plot(x = ukmig[geog_type %in% c("msoa"), population], y = ukmig[geog_type %in% c("msoa"), sqrt(moved_within)])
plot(x = ukmig[geog_type %in% c("ward"), population], y = ukmig[geog_type %in% c("ward"), sqrt(moved_within)]) # (1) keep experimenting, but seems that square root dependent variable is the best fitting, (2) relationship does seem different from smaller geographies, la behaves differently, (3) the larger the geography, the better the relationship, sadly
plot(x = ukmig[geog_type %in% c("lad"), population], y = ukmig[geog_type %in% c("lad"), sqrt(moved_within)]) # again, lad looks different to the lower level geographies


  ### 3.3. checking correlation coefficients
cor(ukmig[, population], ukmig[, moved_within]) # very high at 0.94
cor(ukmig[geog_type == "lad", population], ukmig[geog_type == "lad", moved_within])

cor(ukmig[, log(population)], ukmig[, log(moved_within)]) # undefined because of all of the 0s

cor(ukmig[, population], ukmig[, sqrt(moved_within)])
cor(ukmig[geog_type %in% c("oa", "msoa", "ward"), population], ukmig[geog_type %in% c("oa", "msoa", "ward"), moved_within])
cor(ukmig[geog_type %in% c("oa", "msoa", "ward"), population]^2, ukmig[geog_type %in% c("oa", "msoa", "ward"), moved_within])
cor(ukmig[geog_type %in% c("oa", "msoa", "ward"), population], ukmig[geog_type %in% c("oa", "msoa", "ward"), sqrt(moved_within)]) # very high at 0.93 - think I'll go with this one


## 4. running the regression
ukmig_small <- ukmig[geog_type %in% c("oa", "msoa", "ward"), ] 

scaling_model <- lm(sqrt(moved_within) ~ population, data = ukmig_small) # looks fairly good, significant relationships and high r-squared values. 

summary(scaling_model) # looks fairly good, significant relationships and high r-squared value. Use this slope and intercept to model within-area flows. 
plot(scaling_model) # doesn't do amazingly well on all of the diagnostics but doesn't matter greatly, as we're interested in prediction and not inference




