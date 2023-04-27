## Filter bike results ##

csv <- readr::read_csv("500/tradsMcOutputBike.csv")
gpkg <- sf::read_sf("500/tradsMcOutputBike.gpkg")


filtered_csv <- dplyr::filter(csv,
                              (IDNumber == "MA36068N" & PersonNumber == 4 & TripNumber == 1) |
                                (IDNumber == "MA44388D" & PersonNumber == 1 & TripNumber == 1) |
                                (IDNumber == "MA36169K" & PersonNumber == 3 & TripNumber == 1) |
                                (IDNumber == "OL42304M" & PersonNumber == 1 & TripNumber <= 2) | 
                                (IDNumber == "MA35690Q" & PersonNumber == 1 & TripNumber == 2))

filtered_gpkg <- dplyr::filter(gpkg,
                      (IDNumber == "MA36068N" & PersonNumber == 4 & TripNumber == 1) |
                        (IDNumber == "MA44388D" & PersonNumber == 1 & TripNumber == 1) |
                        (IDNumber == "MA36169K" & PersonNumber == 3 & TripNumber == 1) |
                        (IDNumber == "OL42304M" & PersonNumber == 1 & TripNumber <= 2) | 
                        (IDNumber == "MA35690Q" & PersonNumber == 1 & TripNumber == 2))


readr::write_csv(filtered_csv,"10/tradsMcOutputBike_sampled.csv")
sf::write_sf(filtered_gpkg,"10/tradsMcOutputBike_sampled.gpkg")
