# Micro Destination Attraction Analysis


# Micro Destination Attraction Analysis

This document processes point-of-interest (POI) data for Melbourne to
calculate destination attraction coefficients for transport modeling.

## Setup

``` r
library(tidyverse)
```

## Load Spatial Zones and Attraction Coefficients

``` r
# Read output areas and attraction coefficients
zones <- sf::st_read(
    "../../../../melbourne/input/zonesShapefile/SA1_2016_AUST_MEL.shp"
  ) %>%
  transmute(zone = SA1_MAIN16) %>%
  sf::st_transform(crs = 28355)
```

    Reading layer `SA1_2016_AUST_MEL' from data source 
      `D:\projects\jibe\melbourne\input\zonesShapefile\SA1_2016_AUST_MEL.shp' 
      using driver `ESRI Shapefile'
    Simple feature collection with 10289 features and 14 fields
    Geometry type: MULTIPOLYGON
    Dimension:     XY
    Bounding box:  xmin: 144.3336 ymin: -38.50299 xmax: 145.8784 ymax: -37.1751
    Geodetic CRS:  GDA94

``` r
attractions <- read_csv("../../../../melbourne/input/mito/tripAttractionsCoefficients.csv")
purposes <- names(attractions)[-1]
```

## Process Destination Data

``` r
# all_destinations comes from destinations.r
all_destinations <- read_csv("../../../../melbourne/preprocessing/poi/final/all_destinations.csv")  %>% 
  filter(code != "PT") %>%
  sf::st_as_sf(coords = c("X","Y"),remove = FALSE,crs = 28355) %>%
  sf::st_join(zones,join = sf::st_intersects) %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(zone))  %>%
  mutate(type = recode(type,
    "Community health resources" = "community_health", 
    "Early year access" = "early_year_access",
    "Eating establishments" = "eating_establishments",
    "Education" = "education",
    "Financial" = "financial",
    "Food retail" = "food_retail",
    "Primary health care" = "primary_health_care",
    "Recreational, sports pitches and facilities" = "recreation",
    "Services" = "services",
    "Social and cultural locations" = "social_and_culture",
    "Public open space" = "public_open_space",
  )) %>% 
  rename(WEIGHT = weight) %>%
  group_by(code,id) %>%
  mutate(WT = WT / n()) %>%
  ungroup() %>%
  select(type,X,Y,WEIGHT,Attribute,zone,id,WT,code) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*WT))
```

## Export Results

``` r
write_csv(all_destinations,"../../../../melbourne/input/mito/microDestinationAttraction.csv")
```

## Data Quality Tests

### Check Uniqueness of Reference Numbers

``` r
# Check uniqueness of reference numbers
all_destinations %>%
  group_by(type) %>%
  summarise(count = n(),
            dist_ID = n_distinct(id))
```

    # A tibble: 11 × 3
       type                  count dist_ID
       <chr>                 <int>   <int>
     1 community_health        717     717
     2 early_year_access      3616    3616
     3 eating_establishments  6192    6192
     4 education              1800    1800
     5 financial              1899    1899
     6 food_retail            1406    1406
     7 primary_health_care     784     784
     8 public_open_space     11556   11556
     9 recreation             2691    2691
    10 services               8112    8112
    11 social_and_culture     2238    2238

### Verify ID Uniqueness

``` r
# These should match. If not, need to change method for getting reference number
with(all_destinations, n_distinct(code,id))
```

    [1] 41011

### Weight Distribution by Zone and Code

``` r
test <- all_destinations %>%
  group_by(zone,code) %>%
  summarise(sum_wt = sum(WT))
```

## Visualization

### Weight Distribution Over Study Area

``` r
plot_data <- all_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WEIGHT)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p, y = TRUE, fill = code)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Weight Distribution by POI Code",
       x = "Proportion",
       y = "",
       fill = "POI Code") +
  theme_minimal()
```

![Distribution of weights by POI code across the study
area](microDestinationAttraction_files/figure-commonmark/plot-weight-distribution-1.png)

### Count by Zone and Code

``` r
test <- count(all_destinations,zone,code)
```

### Attraction Results by Purpose

``` r
# Plot attraction results
plot_data <- all_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  group_by(purpose,code) %>%
  summarise(sum_wt = sum(value)) %>%
  mutate(p = sum_wt / sum(sum_wt))

ggplot(plot_data, aes(x = p,y = purpose, fill = code)) + 
  geom_bar(position = "fill",stat = "identity") + 
  labs(title = "Attraction Share by Purpose and POI Code",
       x = "Share",
       y = "Purpose",
       fill = "POI Code") +
  theme_minimal()
```

![Share of attraction by purpose and POI
code](microDestinationAttraction_files/figure-commonmark/plot-attraction-results-1.png)

### Distribution of Attraction Values

``` r
test <- all_destinations %>%
  pivot_longer(cols = all_of(purposes),names_to = "purpose") %>%
  filter(value > 0) %>%
  ggplot(aes(x = value)) + 
  geom_density() + 
  facet_wrap(~purpose) + 
  xlim(0,0.2) +
  labs(title = "Distribution of Attraction Values by Purpose",
       x = "Attraction Value",
       y = "Density") +
  theme_minimal()

test
```

![Density distribution of attraction values by
purpose](microDestinationAttraction_files/figure-commonmark/plot-value-distribution-1.png)

## Validation

### Test Aggregation

``` r
# Test aggregation before estimating total attraction
# (should match plot data, which estimates attraction before aggregating)
test <- all_destinations %>%
  group_by(code) %>%
  summarise(sum_wt = sum(WT)) %>%
  left_join(attractions, by = c("code" = "poi")) %>%
  mutate(across(all_of(purposes), ~.*sum_wt))

test
```

    # A tibble: 11 × 10
       code  sum_wt    HBW   HBE   HBA   HBO   HBR   HBS   NHBO    NHBW
       <chr>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
     1 CHR     370.     0     0   841.    0     0     0   168.    92.6 
     2 EDU     236.   793.  564.  407.    0     0     0   110.   423.  
     3 EE     2071.  8896.    0     0     0  3546. 5643. 2592.  5743.  
     4 EYA     882.     0  2383. 4190.    0     0     0   957.   485.  
     5 FIN     539.     0     0     0   131.    0     0    28.9    9.20
     6 FR      996.     0     0   320.    0     0  4672. 1652.   829.  
     7 PHC     358.  4758.    0  1347. 1174.    0     0   528.  2466.  
     8 POS    8042.   520.    0     0     0   200.    0    38.0  278.  
     9 RSPF    997.     0     0     0     0  3979.    0   756.   676.  
    10 SCL     786. 18725.    0     0   490.    0     0   108.  8835.  
    11 SER    4291.     0     0     0     0     0     0     0      0   

## Summary

This analysis processes Melbourne POI data to create micro-level
destination attraction coefficients for transport modeling. The data is
spatially joined with statistical areas, standardised with JIBE naming
conventions, and weighted according to attraction coefficients for
different trip purposes.
