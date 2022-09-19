# Order of stuff

# Read raw data
source("script/Manchester/read.R")

# Export trip details to MATSim (if necessary)
# source("script/Manchester/prepForMATSim.R")

# Add route attributes from MATSim
source("script/Manchester/addRouteAttributes.R")

# Add abm attributes
source("script/Manchester/abm.R")