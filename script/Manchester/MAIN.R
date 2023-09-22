# Order of processing data

# Read raw data
source("script/Manchester/read.R")

# Export trip details to include JAVA-based BE attributes
source("script/Manchester/addXYcoords.R")

# Add BE / route data after JAVA-based analysis
# source("script/Manchester/addRouteData.R")

# Mode choice model 
# Step 1: compute link corridor data
# This step requires about 50 GB memory. 
# To skip this step, please use the processed data at "data/Manchester/corridor/AllShort92.csv" 
source("script/Manchester/modeChoice/prepLinkCorridorData.R")

# Step 2: process trip data for Apollo
source("script/Manchester/modeChoice/prepForApollo.R")

# Step 3: mode choice model estimation with Apollo
source("script/Manchester/modeChoice/mandatory/MNL_noModeUseFrequency.R")
source("script/Manchester/modeChoice/discretionary/MNL_noModeUseFrequency.R")