# travelDiaryProcessing
R scripts for processing and modelling with travel diary data.
Currently includes diary data from Manchester (TRADS), England (NTS), and Melbourne (VISTA)

## Manchester
Scripts associated with the Greater Manchester TRADS travel diary (https://tfgm.com/trads) for years 2016-2018.

Most important scripts are described here

### script/Manchester/read.r
Reads the raw travel diary files provided by TfGM in .sav format and runs initial processing.
Saves output database in RDS format, which is used for further analysis.

### script/Manchester/routing/addXYcoords.r
Adds XY coordinates to trip origins and destinations to create a dataset suitable for routing. 
The output file is used for route-based analysis in the Java repository https://github.com/jibeproject/matsim-jibe. 
This input is specified using the property 'diary.file' in that repository.

### script/Manchester/modeChoice/prepForApollo.r
Preliminary data preparation for mode choice modelling. Runs after read.r.

### script/Manchester/modeChoice/prepForJava.r
Next stage of data preparation for mode choice modelling. Runs after prepForApollo.r. 
Creates output files suitable for mode choice estimation in the Java repository https://github.com/jibeproject/matsim-jibe.
This input is specified using the second argument in the estimation main method (https://github.com/jibeproject/matsim-jibe/blob/master/src/main/java/estimation/RunMnlManchester.java).
Also includes miscellaneous analysis scripts for this dataset

### script/Manchester/modeChoice/formatJavaResults.r
Formats outputs from mode choice estimation for presentation / publication and for incorporating into MITO.

#Melbourne

Scripts associated with the VISTA travel diary survey for Melbourne
[TBD]

#England

Scripts associated with England's National Travel Survey (NTS)
[TBD]
