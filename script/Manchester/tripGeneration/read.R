pacman::p_load(dplyr,readr,tidyr)

######################## READ IN GREAT BRITAIN DATA ########################
rawData <- list()

# Read in PSUs (for region-based analysis only)
rawData$PSUs <- read_tsv("UKDA-5340-tab/tab/psu_eul_2002-2021.tab", col_names = TRUE,
                         cols_only(PSUID = "i", PSUStatsReg_B01ID = "i", PSUCountry_B01ID = "i")) 

# Read in households
rawData$households <- read_tsv("UKDA-5340-tab/tab/household_eul_2002-2021.tab",col_names = TRUE,
                                     cols_only(PSUID = "i", SurveyYear = "i", HouseholdID = "i", HHoldCountry_B01ID = "i", HHoldNumPeople = "i", 
                                               NumBike = "i", NumCar = "i", NumMCycle = "i", 
                                               HHIncome2002_B02ID = "i", Settlement2011EW_B03ID = "i",
                                               W1 = "n",W2 = "n", W3 = "n")) %>% left_join(rawData$PSUs)

# Read in individuals
rawData$persons <- read_tsv("UKDA-5340-tab/tab/individual_eul_2002-2021.tab",col_names = TRUE,
                        cols_only(SurveyYear = "i", HouseholdID = "i", PersNo = "i", WkMode_B01ID = "i",
                                  PrivCar_B01ID = "i", OrdBusFreq_B01ID = "i", CoachFreq_B01ID = "i", TrainFreq_B01ID = "i", 
                                  TaxiCabFreq_B01ID = "i", BicycleFreq_B01ID = "i", PlaneFreq_B01ID = "i", WalkFreq_B01ID = "i",
                                  Age_B01ID = "i",Sex_B01ID = "i", DrivLic_B02ID = "i", TicketHolding_B01ID = "i", OwnCycle_B01ID = "i", CarsEas_B01ID = "i", EcoStat_B02ID="i", OftHome_B01ID="i"))

# Read in trips
rawData$trips <- read_tsv("UKDA-5340-tab/tab/trip_eul_2002-2021.tab",col_names = TRUE,
                      cols_only(SurveyYear = "i", HouseholdID = "i", PersNo = "i",TravDay = "i",JourSeq = "i",HowComp_B01ID = "i",
                                ShortWalkTrip_B01ID = "i", TripPurpFrom_B01ID = "i",TripPurpTo_B01ID = "i",TripPurpose_B01ID = "i",
                                MainMode_B03ID = "i", TripStart = "n", TripEnd = "n",
                                TripDisIncSW = "n",JD = "n",TripTotalTime = "n",JJXSC = "i", JOTXSC="n", JTTXSC = "n",W5 = "n"))

# Read in stages (for access & egress)
rawData$stages <- read_tsv("UKDA-5340-tab/tab/stage_eul_2002-2021.tab", col_names = TRUE,
                       cols_only(SurveyYear = "i", HouseholdID = "i", PersNo = "i",TravDay = "i",JourSeq = "i",StageSeq = "i",
                                 StageMode_B03ID = "i", StageDistance = "n", StageTime = "n"))

######################## CREATE WORKING DATASETS, FILTER BY AGE & SURVEY YEAR  ######################## 
# UK Data
households <- rawData$households %>% mutate(hh.id = HouseholdID)
persons    <- rawData$persons    %>% mutate(hh.id = HouseholdID, p.id = PersNo, p.ID = HouseholdID*10 + PersNo - 1)
trips      <- rawData$trips      %>% mutate(hh.id = HouseholdID, p.id = PersNo, p.ID = HouseholdID*10 + PersNo - 1, t.day.id = TravDay, t.id = JourSeq)
stages     <- rawData$stages     %>% mutate(hh.id = HouseholdID, p.id = PersNo, p.ID = HouseholdID*10 + PersNo - 1, t.day.id = TravDay, t.id = JourSeq, s.id = StageSeq)

# Filter household data by survey year (UK only), country
households <- households %>% 
  filter((SurveyYear >= 2014) & (SurveyYear < 2020), W1 == 1) %>%
  filter(PSUStatsReg_B01ID %in% c(12, 13)) # North West - metropolitan+ non-metropolitan 

# Filter person data age 10+ only.
persons <- persons %>% 
  semi_join(households["hh.id"]) #%>% filter(hh.id %in% households$PSUID) # keep only persons living in north west

#%>% filter(Age_B01ID >= 5)

trips <- trips %>% semi_join(persons[c("hh.id","p.id")]) %>% filter(!is.na(W5))
stages <- stages %>% semi_join(trips[c("hh.id","p.id","t.day.id","t.id")])

######################## ARRANGE DATA ########################
trips <- arrange(trips,hh.id,p.id,t.day.id,t.id)

######################## WEIGHTS ######################## 
# Household Weight
households[["hh.weight"]] <- with(households, W2)

# Person Weight
persons[["p.weight"]] <- 1

# Trip Weight
trips[["t.weight"]] <- trips$W5 * nrow(trips) / sum(trips$W5)

# Short Walk Factors ??
trips[["t.JJXSC"]] <- with(trips, JJXSC)
trips[["t.day_week_factor"]] <- with(trips, pmax(JJXSC,1))

######################## HOUSEHOLD VARIABLES ######################## 
# Survey year
households[["hh.year"]] <- with(households, SurveyYear)

# Household Region
households[["hh.region"]] <- with(households, PSUStatsReg_B01ID)

# Household Size
households[["hh.size"]] <- with(households, HHoldNumPeople)

# Number of children (need to use the data from before filtering)
households <- left_join(households, rawData$persons %>% group_by(HouseholdID) %>% summarise(hh.adults = sum(Age_B01ID > 5),
                                                                                        hh.children = sum(Age_B01ID <= 5)))

# Household Structure
households[["hh.structure"]] <- with(households, case_when(hh.size == 1 ~ "1A",
                                                           hh.size == 2 & hh.children == 0 ~ "2A",
                                                           hh.size >= 3 & hh.children == 0 ~ "3A",
                                                           hh.size >= 2 & (hh.size - hh.children) == 1 ~ "p",
                                                           hh.size >= 3 & (hh.size - hh.children) >= 2 ~ "p"))

# Household size (adjusted)
households[["hh.sizeAdj"]] <- with(households, (hh.size - hh.children) + 0.5*hh.children)

# Household Number of Cars
households[["hh.cars"]] <- with(households, NumCar)

# Household Number of Motorcycles
households[["hh.motorcycles"]] <- with(households, NumMCycle)

# Household Number of Bicycles
households[["hh.bicycles"]] <- with(households, NumBike)

# Household Income (DIFFERENT)
households[["hh.income"]] <- with(households, HHIncome2002_B02ID)

# Classified as Urban?
households[["hh.urban"]] <- with(households, na_if(Settlement2011EW_B03ID,-8) == 1)

######################## PERSON VARIABLES ######################## 
mode_restriction <- function(freqencyVar) {
  case_when(freqencyVar > 0 ~ freqencyVar,
            freqencyVar == -9 ~ as.integer(8))
}

persons <- persons %>% within({
  
  # Sex is female
  p.female <- Sex_B01ID == 2
  
  # Age group (1 = 4-, 2 = 5-10, 3 = 11-17, 4 = 18-29, 5 = 30-49, 6 = 50-59, 7=60-69, 8=70+)
  # NEW: Age group (1 = 4-, 2 = 5-10, 3 = 11-15, 4=16-17, 5 = 18-29, 6 = 30-49, 7 = 50-59, 8=60-69, 9=70+)
  p.age_gr <- case_when(Age_B01ID %in% c(-10, -8) ~ NA,
                        Age_B01ID <= 3 ~ 1,
                        Age_B01ID == 4 ~ 2,
                        Age_B01ID == 5 ~ 3,
                        Age_B01ID <= 7 ~ 4,
                        Age_B01ID <= 12 ~ 5,
                        Age_B01ID <= 14 ~ 6,
                        Age_B01ID <= 15 ~ 7,
                        Age_B01ID <= 17 ~ 8,
                        TRUE ~ 9)
  
  # Has driver's license?
  p.driversLicence <- na_if(DrivLic_B02ID,-8) == 1
  
  # How often do you work from home ?
  p.freqWFH <- OftHome_B01ID
  
  # Working status of individual
  p.occupationStatus<-case_when(EcoStat_B02ID<1 ~ "NA",
                                  EcoStat_B02ID==1 ~ "full-time",
                                  EcoStat_B02ID==2 ~ "part-time",
                                  EcoStat_B02ID==3 ~ "unemployed",
                                  EcoStat_B02ID==4 ~ "retired, sick, disabled",
                                  EcoStat_B02ID==5 ~ "student",
                                  EcoStat_B02ID==6 ~ "other")
                                  
  
  # Owns or can access a bicycle?
  p.ownBicycle <- na_if(OwnCycle_B01ID,-8) <= 3
  
  # Season ticket (Q: what about OAPs?)
  p.seasonTicket <- na_if(TicketHolding_B01ID,-8) <= 2
  
  # Usual work commute mode (as in census)
  p.workMode <- case_when(WkMode_B01ID <= 0 ~ "NA",
                          WkMode_B01ID == 1 ~ "carD",
                          WkMode_B01ID == 2 ~ "carP",
                          WkMode_B01ID <= 5 ~ "car",
                          WkMode_B01ID == 6 ~ "cycle",
                          WkMode_B01ID == 7 ~ "bus",
                          WkMode_B01ID <= 10 ~ "train",
                          WkMode_B01ID == 11 ~ "walk",
                          WkMode_B01ID <= 13 ~ "other")
  
  # Frequency of use of different modes (for mode restriction)
  p.freq_car <- mode_restriction(PrivCar_B01ID)
  p.freq_bus <- mode_restriction(OrdBusFreq_B01ID)
  p.freq_coach <- mode_restriction(CoachFreq_B01ID)
  p.freq_train <- mode_restriction(TrainFreq_B01ID)
  p.freq_pt <- pmin(p.freq_bus,p.freq_coach,p.freq_train,na.rm = TRUE)
  p.freq_bicycle <- mode_restriction(BicycleFreq_B01ID)
  p.freq_walk <- mode_restriction(WalkFreq_B01ID)
})


######################## TRIP VARIABLES ########################
trips[["t.recordType"]] <- trips$HowComp_B01ID

# Short Walk Trip
trips[["t.shortWalk"]] <- with(trips, ShortWalkTrip_B01ID == 1)

# First and last trip
trips[["t.firstTrip"]] <- with(trips, (hh.id != lag(hh.id, default = 0)) | (hh.id == lag(hh.id, default = 0) & (p.id != lag(p.id, default = 0))))
trips[["t.lastTrip"]] <- with(trips, (hh.id != lead(hh.id, default = 0)) | (hh.id == lead(hh.id, default = 0) & (p.id != lead(p.id, default = 0))))

# Trip Mode
trips[["t.mode"]] <- with(trips, case_when(MainMode_B03ID <= 2 ~ "walk",
                                           MainMode_B03ID == 3 ~ "cycle",
                                           MainMode_B03ID == 4 ~ "other",
                                           MainMode_B03ID <= 6 ~ "carD",
                                           MainMode_B03ID <= 8 ~ "carP",
                                           MainMode_B03ID <= 10 ~ "carD",
                                           MainMode_B03ID <= 12 ~ "carP",
                                           MainMode_B03ID <= 17 ~ "other",
                                           MainMode_B03ID <= 21 ~ "PT",
                                           MainMode_B03ID <= 24 ~ "PT",
                                           TRUE ~ "other"))

trips[["t.mode2"]] <- with(trips, case_when(MainMode_B03ID <= 2 ~ "walk",
                                            MainMode_B03ID <= 3 ~ "cycle",
                                            MainMode_B03ID <= 4 ~ "other",
                                            MainMode_B03ID <= 8 ~ "car",
                                            MainMode_B03ID <= 12 ~ "motorcycle",
                                            MainMode_B03ID <= 17 ~ "other",
                                            MainMode_B03ID <= 24 ~ "PT",
                                            TRUE ~ "other"))

# Classify origins & destinations
getLocationType <- function(rawLocation) {
  case_when(rawLocation == -10  ~ "unknown",
            rawLocation == -8  ~ "unknown",
            rawLocation == 1  ~ "W",
            rawLocation == 2  ~ "B", # business trips
            rawLocation == 3  ~ "E",
            rawLocation <= 5  ~ "S",
            rawLocation <= 8  ~ "O",
            rawLocation <= 13 ~ "R",
            rawLocation == 14 ~ "2", # holiday: base
            rawLocation == 15 ~ "J", # day trip / just walk
            rawLocation == 16 ~ "O",
            rawLocation <= 22 ~ "A",
            rawLocation == 23 ~ "H")
}

# Trip Origins, Destinations, Purpose
trips[["t.origin"]] <- with(trips, getLocationType(TripPurpFrom_B01ID))

trips[["t.destination"]] <- with(trips, getLocationType(TripPurpTo_B01ID))

# Trip Purpose
trips[["t.purpose"]] <- with(trips, case_when((t.destination %in% c("unknown", "B")) ~  "NA", # exclude unknown and business trip purposes
                                              (t.destination == "J" & !(t.mode2 %in% c("cycle", "walk"))) ~  "NA", # exclude unknown and business trip purposes
                                              (t.destination == "J") ~  "RRT", # replaced J by RRT # !(t.origin %in% c("unknown", "B")) &
                                              (t.origin == "H" & t.destination == "2") ~ "HB2",
                                              (t.origin == "H" & t.destination == "W") ~ "HBW",
                                              (t.origin == "H" & t.destination == "E") ~ "HBE",
                                              (t.origin == "H" & t.destination == "S") ~ "HBS",
                                              (t.origin == "H" & t.destination == "A") ~ "HBA",
                                              (t.origin == "H" & t.destination == "R") ~ "HBR",
                                              (t.origin == "H" & t.destination == "O") ~ "HBO",
                                              (t.destination == "H") ~ "NA", # exclude return home-based trips
                                              (t.origin == "J") ~ "NA", # exclude return RRT trips
                                              (t.origin == "W" | t.destination == "W") ~ "NHBW",
                                              TRUE ~ "NHBO"))


trips[["t.full_purpose"]] <- with(trips, case_when((t.origin == "J" | t.destination == "J") ~ "RRT", # replaced J by RRT
                                                   ((t.origin == "H" & t.destination == "2") | (t.origin == "2" & t.destination == "H")) ~ "HB2",
                                                   ((t.origin == "H" & t.destination == "W") | (t.origin == "W" & t.destination == "H")) ~ "HBW",
                                                   ((t.origin == "H" & t.destination == "E") | (t.origin == "E" & t.destination == "H")) ~ "HBE",
                                                   ((t.origin == "H" & t.destination == "S") | (t.origin == "S" & t.destination == "H")) ~ "HBS",
                                                   ((t.origin == "H" & t.destination == "A") | (t.origin == "A" & t.destination == "H")) ~ "HBA",
                                                   ((t.origin == "H" & t.destination == "R") | (t.origin == "R" & t.destination == "H")) ~ "HBR",
                                                   ((t.origin == "H" & t.destination == "O") | (t.origin == "O" & t.destination == "H")) ~ "HBO",
                                                   (t.destination == "H") ~ "NA",
                                                   (t.origin == "W" | t.destination == "W") ~ "NHBW",
                                                   TRUE ~ "NHBO"))

trips[["t.is_RRT"]] <- with(trips, case_when((t.origin == t.destination) ~ TRUE,
                                             TRUE ~ FALSE))

trips[["t.is_RRTH"]] <- with(trips, case_when(((t.origin == t.destination) & (t.origin == "H")) ~ TRUE,
                                             TRUE ~ FALSE))



# Trip Distance
trips[["t.distance"]] <- with(trips, TripDisIncSW * 1.60934)
# trips[["t.distance"]] <- with(trips, JD * 1.60934)miles

# Departure time
trips[["t.departureTime"]] <- trips$TripStart
trips[["t.arrivalTime"]] <- trips$TripEnd

# Travel Time
trips[["t.travelTime.wtd"]] <- with(trips, JTTXSC)
trips[["t.tripTime.wtd"]] <- with(trips, JOTXSC)

# Trip Speed
trips[["t.speed"]] <- with(trips, TripDisIncSW * 1.60934 * 60 / TripTotalTime)

# Unrealistic speed
trips[["t.badSpeed"]] <- with(trips, (MainMode_B03ID <= 2 & t.speed > 10) |
                                     (MainMode_B03ID == 3 & t.speed < 2) | 
                                     (MainMode_B03ID == 3 & t.speed > 25))
# Trip day of week
trips[["t.dow"]] <- trips$TravDay

######################## TRIP ACCESS & EGRESS ########################
stages <- stages %>% within({
  s.mode = case_when(StageMode_B03ID <= 2 ~ "walk",
                     StageMode_B03ID == 3 ~ "cycle",
                     StageMode_B03ID == 4 ~ "other",
                     StageMode_B03ID <= 6 ~ "carD",
                     StageMode_B03ID <= 8 ~ "carP",
                     StageMode_B03ID <= 10 ~ "carD",
                     StageMode_B03ID <= 12 ~ "carP",
                     StageMode_B03ID <= 17 ~ "other",
                     StageMode_B03ID <= 21 ~ "bus",
                     StageMode_B03ID <= 24 ~ "train",
                     TRUE ~ "other")
  
  s.distance = StageDistance * 1.60934
  s.time = StageTime * 60
})

######################## REMOVE ALL OLD VARIABLES ########################
NTS <- list()
NTS$rawData <- rawData
NTS$households <- dplyr::select(households, starts_with("hh."))
NTS$persons <- dplyr::select(persons, starts_with(c("hh.","p.")))
NTS$trips <- dplyr::select(trips, starts_with(c("hh.","p.","t.")))
NTS$stages <- dplyr::select(stages, starts_with(c("hh.","p.","t.","s.")))

# Clean up
rm(rawData, households, persons, trips, stages)

######################## WRITE FILES ########################

write.csv(NTS$trips, file = "datasets/trips.csv")
write.csv(NTS$persons, file = "datasets/persons.csv")
write.csv(NTS$households, file = "datasets/households.csv")


