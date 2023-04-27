## Check ABM-expanded trips data

# Check activity & trip durations add to 86400 (i.e. a whole day)
test <- trips %>% group_by(hh.id,p.id) %>% summarise(a = sum(t.travelTime), b = sum(act.duration), c = a + b)
summary(test$c)

# Check false/true distinct
with(trips, table(tour.purpose, act.type))
with(trips, table(tour.purpose, t.destination))
with(trips, table(subtour, is.na(subtour.id)))
with(trips, table(tour.incomplete,tour.id == 0))
with(trips, table(tour.w, tour.purpose))
with(trips, table(tour.e, tour.purpose))
with(trips, table(tour.d, tour.purpose))
with(trips, table(tour.incomplete,tour.purpose))
with(trips, table(tour.incomplete,act.type))


# Check number of main activities (minus subtours if work/education) matches total tours
discTours <- trips %>% filter(!tour.incomplete, tour.d)
nrow(count(discTours,hh.id,p.id,tour.id))
sum(discTours$act.type == "main")

mandTours <- trips %>% filter(!tour.incomplete, tour.w | tour.e)
nrow(count(mandTours,hh.id,p.id,tour.id))
mandTours %>% filter(subtour) %>% count(hh.id,p.id,tour.id,subtour.id) %>% nrow()
sum(mandTours$act.type == "main")
