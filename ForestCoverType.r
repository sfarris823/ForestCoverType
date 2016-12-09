# reset environmental
rm(list=ls())

##
## Install/Load needed libraries
##
# List of packages for session
.packages = c("dplyr",          # data manipulation
              "tidyr",          # data manipulation
              "ggplot2",        # simple plots 
              "randomForest",   # exploratory modeling
              "caret",          # cross validation
              "doSNOW",         # clustering for performance
              "rpart",          # recursive partitioning
              "rpart.plot"      # plotting decision trees  
)

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, library, character.only=TRUE)
rm(.packages)
rm(.inst)

##
## Custom functions
##

# factorit() will take a data frame column and replace NAs, empty 
# strings with another value and returns the column as a factor

factorit <- function (dframe, na="Unk") 
{
  dframe[which(is.na(dframe))] <- na
  dframe[which(dframe=="")] <- na
  return (as.factor(dframe))
}


# showhist() builds a histogram using a facetwrap

showhist <- function(data, xaxis, fillgrp, facetwrap, title) 
{
  ggplot(data, aes_string(x = xaxis, fill = fillgrp)) +
    stat_count(width = 0.5) + 
    facet_wrap(facetwrap) +
    ggtitle(title) +
    xlab(xaxis) +
    ylab("Total Count") +
    labs(fill = fillgrp)
}

# rpart.cv() trains a predictive model 
rpart.cv <- function(seed, training, labels, ctrl)
{
  set.seed(seed)
  rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  return (rpart.cv)
}


##
## Get data
##

# train.csv has 15,120 observations and 56 variables
train.raw <- read.csv("train.csv", stringsAsFactors = F)

# test.csv has 565,892 observations and 56 variables
test.raw <- read.csv("test.csv", stringsAsFactors = F)

# Combine train and test
# First add a covertype column to test
test.raw$Cover_Type <- "Unk"
# combined has a total of 581,012 observations and 56 variables
combined <- rbind(train.raw, test.raw)


##
## Explore the data
##

# Id 
summary(combined$Id)                       # Ranges from 1 - 581,000
count(combined, vars=Id)                   # 581,012 unique values
table(is.na(combined$Id))                  # No data is missing

# Elevation 
summary(combined$Elevation)                # Ranges from 1,859 - 3,858 meters
count(combined, vars=Elevation)            # 1,978 unique values
table(is.na(combined$Elevation))           # No data is missing

# Aspect
summary(combined$Aspect)                   # Ranges from 0 - 360 degrees azimuth
count(combined, vars=Aspect)               # 361 unique values
table(is.na(combined$Aspect))              # No data is missing

# Slope
summary(combined$Slope)                    # Ranges from 0 - 66 degrees
count(combined, vars=Slope)                # 67 unique values
table(is.na(combined$Slope))               # No data is missing

# Horizontal_Distance_To_Hydrology
summary(combined$Horizontal_Distance_To_Hydrology)          # Ranges from 0 - 1,397 meters?
count(combined, vars=Horizontal_Distance_To_Hydrology)      # 551 unique values
table(is.na(combined$Horizontal_Distance_To_Hydrology))     # No data is missing

# Vertical_Distance_To_Hydrology
summary(combined$Vertical_Distance_To_Hydrology)            # Ranges from -173 - 601 meters?
count(combined, vars=Vertical_Distance_To_Hydrology)        # 700 unique values
table(is.na(combined$Vertical_Distance_To_Hydrology))       # No data is missing

# Horizontal_Distance_To_Roadways 
summary(combined$Horizontal_Distance_To_Roadways)           # Ranges from 0 - 7,117 meters?
count(combined, vars=Horizontal_Distance_To_Roadways)       # 5,785 unique values
table(is.na(combined$Horizontal_Distance_To_Roadways))      # No data is missing

# Hillshade_9am  
summary(combined$Hillshade_9am )           # Ranges from 0 - 254
count(combined, vars=Hillshade_9am )       # 207 unique values
table(is.na(combined$Hillshade_9am ))      # No data is missing

# Hillshade_Noon   
summary(combined$Hillshade_Noon )          # Ranges from 0 - 254
count(combined, vars=Hillshade_Noon )      # 185 unique values
table(is.na(combined$Hillshade_Noon ))     # No data is missing

# Hillshade_3pm   
summary(combined$Hillshade_3pm )           # Ranges from 0 - 254
count(combined, vars=Hillshade_3pm )       # 255 unique values
table(is.na(combined$Hillshade_3pm ))      # No data is missing

# Horizontal_Distance_To_Fire_Points   
summary(combined$Horizontal_Distance_To_Fire_Points )       # Ranges from 0 - 7173
count(combined, vars=Horizontal_Distance_To_Fire_Points )   # 5,827 unique values
table(is.na(combined$Horizontal_Distance_To_Fire_Points ))  # No data is missing
 
# Wilderness_Area - Note: Each observation is categorized into only one Wilderness_Area.
table((combined$Wilderness_Area1 + combined$Wilderness_Area2 + combined$Wilderness_Area3 + combined$Wilderness_Area4) > 1)

table(min(combined[,12:15]))               # Ranges from 0 - 1
table(max(combined[,12:15]))
table(is.na(combined$Wilderness_Area1) &   # No data is missing
      is.na(combined$Wilderness_Area2) & 
      is.na(combined$Wilderness_Area3) & 
      is.na(combined$Wilderness_Area4))

# Soil_Type - Note: Each observation is categorized into only one Wilderness_Area.
table(min(combined[,16:55]))               # Ranges from 0 - 1
table(max(combined[,16:55]))
table((combined$Soil_Type1 + combined$Soil_Type2 + combined$Soil_Type3 + combined$Soil_Type4 + combined$Soil_Type5 +   # No data is missing
     combined$Soil_Type6 + combined$Soil_Type7 + combined$Soil_Type8 + combined$Soil_Type9 + combined$Soil_Type10 + 
     combined$Soil_Type11 + combined$Soil_Type12 + combined$Soil_Type13 + combined$Soil_Type14 + combined$Soil_Type15 + 
     combined$Soil_Type16 + combined$Soil_Type17 + combined$Soil_Type18 + combined$Soil_Type19 + combined$Soil_Type20 + 
     combined$Soil_Type21 + combined$Soil_Type22 + combined$Soil_Type23 + combined$Soil_Type24 + combined$Soil_Type25 + 
     combined$Soil_Type26 + combined$Soil_Type27 + combined$Soil_Type28 + combined$Soil_Type29 + combined$Soil_Type30 + 
     combined$Soil_Type31 + combined$Soil_Type32 + combined$Soil_Type33 + combined$Soil_Type34 + combined$Soil_Type35 + 
     combined$Soil_Type36 + combined$Soil_Type37 + combined$Soil_Type38 + combined$Soil_Type39 + combined$Soil_Type40) > 1)

# Cover_Type
summary(as.factor(combined$Cover_Type))         # Ranges from 1 - 7 with Unknowns
count(combined, vars=Cover_Type)                # 8 unique values
table(is.na(combined[1:15120, "Cover_Type"]))   # No data is missing from train set


##
## Simplify Table
##

# combine the Wilderness_AreaX variables into a single variable with a value of 1-4
combined$Wilderness_Area <- paste(combined$Wilderness_Area1, combined$Wilderness_Area2,
                                  combined$Wilderness_Area3,combined$Wilderness_Area4, sep="")
combined$Wilderness_Area[combined$Wilderness_Area == "1000"] <- "1" 
combined$Wilderness_Area[combined$Wilderness_Area == "0100"] <- "2" 
combined$Wilderness_Area[combined$Wilderness_Area == "0010"] <- "3" 
combined$Wilderness_Area[combined$Wilderness_Area == "0001"] <- "4" 

# combine the Soil_TypeX variables into a single variable with a value of 1-40
combined$Soil_Type <- factor(apply(combined[,16:55], 1, function(x) which(x==1)), labels=colnames(combined[,16:55]))
combined$Soil_Type <- gsub("^Soil_Type", x=combined$Soil_Type, replacement="")

# rebuild the table with just the data needed
simpcombined <- select(combined, Id:Horizontal_Distance_To_Fire_Points, Cover_Type:Soil_Type)


##
## Data Analysis
##

# convert a few columns to factors for plots
simpcombined$Cover_Type <- factorit(simpcombined$Cover_Type)
simpcombined$Soil_Type <- factorit(simpcombined$Soil_Type)
simpcombined$Wilderness_Area <- factorit(simpcombined$Wilderness_Area)

# play with plots
showhist(simpcombined[1:15120,], "Wilderness_Area", "Cover_Type", ~Soil_Type, "Cover_Type by Soil_Type and Wilderness_Area")
showhist(simpcombined[1:15120,], "Soil_Type", "Cover_Type", ~Wilderness_Area, "Cover_Type by Soil_Type and Wilderness_Area")

# examine data by Cover_Type
covertype.1 <- simpcombined[simpcombined$Cover_Type=="1", ]
covertype.2 <- simpcombined[simpcombined$Cover_Type=="2", ]
covertype.3 <- simpcombined[simpcombined$Cover_Type=="3", ]
covertype.4 <- simpcombined[simpcombined$Cover_Type=="4", ]
covertype.5 <- simpcombined[simpcombined$Cover_Type=="5", ]
covertype.6 <- simpcombined[simpcombined$Cover_Type=="6", ]
covertype.7 <- simpcombined[simpcombined$Cover_Type=="7", ]

# Maybe Elevation affects Cover_Type?
summary(covertype.1$Elevation)
summary(covertype.2$Elevation)
summary(covertype.3$Elevation)
summary(covertype.4$Elevation)
summary(covertype.5$Elevation)
summary(covertype.6$Elevation)
summary(covertype.7$Elevation)

# Maybe Elevation affects Cover_Type?
summary(covertype.1$Elevation)
summary(covertype.2$Elevation)
summary(covertype.3$Elevation)
summary(covertype.4$Elevation)
summary(covertype.5$Elevation)
summary(covertype.6$Elevation)
summary(covertype.7$Elevation)

# Look for correlations to target focus
corrgram(simpcombined)    #note to do: convert cover_type, soil_type, and wilderness_area to integer and rerun.