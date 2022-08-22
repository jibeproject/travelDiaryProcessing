####### SETUP #######
library(tidyverse)
INPUT_PATH <- "data/Manchester/"
OUTPUT_PATH <- "data/Manchester/processed/"

####### READ IN DATA #######
years <- foreign::read.spss(paste0(INPUT_PATH,"Yrs 6,7,8 HouseholdPerson YearNo.sav"), to.data.frame = T) %>% distinct()

indiv <- foreign::read.spss(paste0(INPUT_PATH,"Yrs 6,7,8 HouseholdPerson Academic.sav"), to.data.frame = T) %>% 
  left_join(years) %>% 
  rename(IDNumber = Household_IDNumber) %>%
  group_by(IDNumber,PersonNumber) %>%
  mutate(duplicate = n() > 1) %>%
  ungroup() %>%
  filter(!duplicate) %>%
  select(-duplicate)

trips <- foreign::read.spss(paste0(INPUT_PATH,"Yrs 6,7,8 HouseholdPersonTrip Academic.sav"), to.data.frame = T) %>% 
  arrange(IDNumber,PersonNumber,TripNumber) %>% semi_join(select(indiv,IDNumber,PersonNumber))

trips_newOAs <- foreign::read.spss(paste0(INPUT_PATH,"Yrs 6,7,8 HouseholdPersonTrip (corrected OA).sav"), to.data.frame = T) %>%
  distinct(IDNumber,PersonNumber,TripNumber, .keep_all = TRUE) %>%
  mutate(across(c("PersonNumber", "TripNumber"), as.numeric))
trips <- trips %>% select(-StartOutputArea,-EndOutputArea) %>% left_join(trips_newOAs)
rm(trips_newOAs)

locations <- readr::read_csv(paste0(INPUT_PATH,"OA_lookup.csv"), col_select = c("OA11CD","LSOA11CD","MSOA11CD")) %>%
  transmute(hh.OA = OA11CD, hh.LSOA = LSOA11CD, hh.MSOA = MSOA11CD)

# Remove individual/household columns from trips dataset (can be added back later through left_join)
indiv_cols = names(indiv)[-c(1,2,13)]
trips <- trips %>% select(-all_of(indiv_cols[indiv_cols %in% names(trips)]))
rm(indiv_cols)

###### CREATE HOUSEHOLDS DATASET ######
# Find attributes where individual-level vars are always the same for each household
household_cols <- indiv %>% 
  group_by(IDNumber) %>% 
  summarise(across(everything(),n_distinct)) %>% 
  summarise(across(everything(),function(x) all(x == 1))) %>%
  tidyr::pivot_longer(cols = everything()) %>% 
  filter(value) %>% 
  pull(name)

# Create households dataset from only these attributes
households <- indiv %>% filter(PersonNumber == 1) %>% select(IDNumber,all_of(household_cols))
indiv <- indiv %>% select(-all_of(household_cols))

# Clean up
rm(household_cols)

###### DEFINE PERSON/TRIP/HOUSEHOLD IDS ######
households <- households %>% 
  transmute(hh.id = IDNumber,
            hh.year = Household_YearNo,
            hh.day = TravelDay,
            hh.cars = NumberCarsOwned + NumberMotorcycles,
            hh.bikes = NumberBicycles,
            hh.income = recode(Income, `997` = "unknown", `998` = "unknown"),
            hh.structure = SocialEconomicGroup,
            hh.structure2 = HouseholdStructure,
            hh.structure3 = HouseholdCategoryDescription,
            hh.expansionFactor = ExpansionFactor,
            hh.OA = OutputArea) %>%
  left_join(locations)

indiv <- indiv %>%
  transmute(hh.id = IDNumber,
            p.id = PersonNumber,
            p.ID = Person_ID,
            p.female = na_if(Gender,99) == "Female",
            p.age_group = Age,
            p.ethnicity = Ethnicity,
            p.ws_workOver30h = WorkStatus2 != 0,
            p.ws_work16to30h = WorkStatus3 != 0,
            p.ws_workUnder16h = WorkStatus4 != 0,
            p.ws_retired = WorkStatus5 != 0,
            p.ws_volunteer = WorkStatus6 != 0,
            p.ws_studyFullTime = WorkStatus7 != 0,
            p.ws_studyPartTime = WorkStatus8 != 0,
            p.ws_homeMaker = WorkStatus9 != 0,
            p.ws_unemployed = WorkStatus10 != 0,
            p.ws_longTermDisabled = WorkStatus11 != 0,
            p.ws_other = WorkStatus12 != 0,
            p.workLocationType = WorkLocation,
            p.workOA = na_if(WorkOutputArea,"         "),
            p.studyOA = na_if(EdOutputArea,"         "),
            p.travelIntegralToJob = na_if(TravelWithWork,98) == 1,
            p.mainWorkCommuteMode = WorkTravelMainMode,
            p.mainStudyCommuteMode = recode(EducationTravelMainMode,`98` = "NR"),
            p.disability = recode(DisabilityLimited,`Limited a lot` = "a lot", `Limited a little` = "a little", `No` = "none"),
            p.mobile = LeaveHome,
            p.trips = NumberTrips)

trips <- trips %>% 
  transmute(hh.id = IDNumber,
            p.id = PersonNumber,
            t.id = TripNumber,
            t.startPurpose = StartPurpose,
            t.endPurpose = EndPurpose,
            t.startOA = StartOutputArea,
            t.endOA = EndOutputArea,
            t.departureTime = round(StartTime),
            t.arrivalTime = round(EndTime),
            t.travelTime = round(TravelTime),
            t.m_walk = Mode1 != 0,
            t.m_cycle = Mode2 != 0,
            t.m_motorcycle = Mode3 != 0,
            t.m_carDriver = Mode4 != 0,
            t.m_carPassenger = Mode5 != 0,
            t.m_train = Mode6 != 0 | Mode11 != 0,
            t.m_metrolink = Mode7 != 0 | Mode12 != 0,
            t.m_bus = Mode8 != 0 | Mode13 != 0,
            t.m_taxi = Mode9 != 0,
            t.m_other = Mode10 != 0,
            t.m_main = Mainmode2,
            t.parkLocation = ParkLocation,
            t.carAvailable = CarAvailable,
            t.busTicket1 = BusTicketType1,
            t.busTicket2 = BusTicketType2,
            t.busTicket3 = BusTicketType3,
            t.metroTicket1 = MetroTicketType1,
            t.metroTicket2 = MetroTicketType2,
            t.metroTicket3 = MetroTicketType3,
            t.trainTicket1 = TrainTicketType1,
            t.trainTicket2 = TrainTicketType2,
            t.trainTicket3 = TrainTicketType3,
            t.busTicket1_other = BusTicketType1Other,
            t.busTicket2_other = BusTicketType2Other,
            t.busTicket3_other = BusTicketType3Other,
            t.metroTicket1_other = MetroTicketType1Other,
            t.metroTicket2_other = MetroTicketType2Other,
            t.metroTicket3_other = MetroTicketType3Other,
            t.trainTicket1_other = TrainTicketType1Other,
            t.trainTicket2_other = TrainTicketType2Other,
            t.trainTicket3_other = TrainTicketType3Other)

###### ASSIGN ORIGIN/DESTINATION ACTIVITY TYPES ######
categorise_activity <- function(purpose) {
  recode_factor(purpose,
                `Home` = "H",
                `Usual place of work` = "W",
                `Work - Business, other` = "B",
                `Moving people or goods in connection with employment` = "B",
                `Education as pupil, student` = "E",
                `Shopping Food` = "S",
                `Shopping Non food` = "S",
                `Social - Entertainment, recreation, Participate in sport, pub, restaurant` = "R",
                `Tourism, sightseeing` = "R",
                `Escorting to place of work, pick-up, drop-off` = "A",
                `Escorting to place of education, pick-up, drop-off` = "A",
                `Childcare  taking or collecting child to or from babysitter, nursery etc` = "A",
                `Accompanying or giving lift to other person, not school, or work` = "A",
                `Visit friends or relatives` = "O",
                `Use Services, Personal Business, bank, hairdresser, library etc` = "O",
                `Health or medical visit` = "O",
                `Worship or religious observance` = "O",
                `Unpaid, voluntary work` = "O",
                `Staying at hotel or other temporary accommodation` = "O",
                `Other` = "O",
                `Round trip walk, cycle, drive for enjoyment` = "RRT",
                `NR` = "unknown")
}

trips$t.origin <- categorise_activity(trips$t.startPurpose)
trips$t.destination <- categorise_activity(trips$t.endPurpose)

###### SAVE FULL VERSION ######
TRADS <- list()
TRADS$households = households
TRADS$indiv = indiv
TRADS$trips = trips
saveRDS(TRADS,paste0(OUTPUT_PATH,"TRADS.rds"))

###### SAVE SAFE VERSION ######
SAFE <- TRADS

# Remove location-specific data from persons/trips dataset
SAFE$trips <- SAFE$trips %>% select(-t.startOA,-t.endOA)
SAFE$indiv <- SAFE$indiv %>% select(-p.workOA,-p.studyOA)

# Replace household locations with group IDs
SAFE$households <- SAFE$households %>% 
  group_by(hh.OA)   %>% mutate(hh.OA_id = cur_group_id()) %>%
  group_by(hh.LSOA) %>% mutate(hh.LSOA_id = cur_group_id()) %>%
  group_by(hh.MSOA) %>% mutate(hh.MSOA_id = cur_group_id()) %>%
  ungroup() %>%
  select(-hh.OA,-hh.LSOA,-hh.MSOA)

# Save
saveRDS(SAFE,paste0(OUTPUT_PATH,"TRADS_safe.rds"))

###### CLEAN UP ######
rm(households,indiv,trips,years,locations,SAFE,INPUT_PATH,OUTPUT_PATH,categorise_activity)