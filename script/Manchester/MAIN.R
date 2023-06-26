# Order of processing data

# Read raw data
source("script/Manchester/read.R")

# Export trip details to include JAVA-based BE attributes
source("script/Manchester/addXYcoords.R")

# Add BE / route data after JAVA-based analysis
source("script/Manchester/addRouteData.R")