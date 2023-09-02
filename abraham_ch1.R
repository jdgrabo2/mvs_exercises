# Exercises for Ch 1 from Abraham text ------------------------------------

# Julia Dawn Grabow -------------------------------------------------------


# Packages to load --------------------------------------------------------

library(tidyverse)
library(ggExtra)
library(here)

# Read csv's for exercises ------------------------------------------------
## change working directory to bring in csv's from other project:

prism_ppt_cleaned <- 
  read_csv(
  here(
    "prism_ppt_cleaned.csv"
  )
  )

isco_levels_cleaned <-
  read_csv(
    here(
      "isco_levels_cleaned.csv"
    )
  )

bogan_water_flow_cleaned <- 
  read_csv(
    here(
      "bogan_water_flow_cleaned.csv"
    )
  )
  
ions_cleaned <-
  read_csv(
    here(
      "ions_cleaned.csv"
    )
  )

## I will reduce df each to one measurement per day, using max, mean, median,
## and sum/cumulative (maybe only the last for ppt)
### Create date column----
## Make adjustments on whole data frames to increase sample size----
### filter as needed and at a date column:

doc <-
  ions_cleaned %>%
  filter(
    Measurement_type == "DOC_mg/L"
  ) %>%
  mutate(
    Event_date = as.Date(
      Event_date_time
    )
  )

water_levels <-
  isco_levels_cleaned %>%
  mutate(
    Event_date = as.Date(
      Event_date_time
    )
  )

## Add summary statistics, place in summary_stat column----
### Note: prism ppt is good as is (I think)

doc <-
  doc %>%
  group_by(
    Measurement_type,
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_max = max(Mean_measurement)
  )

doc <-
  doc %>%
  group_by(
    Measurement_type,
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_mean = mean(Mean_measurement)
  )

doc <-
  doc %>%
  group_by(
    Measurement_type,
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_median = median(Mean_measurement)
  )

### Remove duplicated values----

doc %>%
  count(
    Measurement_type,
    Site_abbreviation,
    Event_date,
    Measurement_max
  )

doc %>%
  count(
    Measurement_type,
    Site_abbreviation,
    Event_date,
    Measurement_mean
  )

doc %>%
  count(
    Measurement_type,
    Site_abbreviation,
    Event_date,
    Measurement_median
  )

### The above shows max, mean, and median have 80 unique values

doc <-
  doc %>%
  group_by(
    Measurement_type,
    Site_abbreviation,
    Event_date,
    Measurement_max,
    Measurement_mean,
    Measurement_median
  ) %>%
  slice_head()

### DF has 80 rows, as expected. Now remove Mean_measurement, n, event_date_time
### columns:

doc <-
  doc[ ,
       -c(5:7)
  ]

## Water levels----

water_levels <-
  water_levels %>%
  group_by(
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_max = max(Measurement)
  )

water_levels <-
  water_levels %>%
  group_by(
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_mean = mean(Measurement)
  )

water_levels <-
  water_levels %>%
  group_by(
    Site_abbreviation,
    Event_date
  ) %>%
  mutate(
    Measurement_median = median(Measurement)
  )

### Remove duplicated values

water_levels %>%
  count(
    Site_abbreviation,
    Event_date,
    Measurement_max
  )

water_levels %>%
  count(
    Site_abbreviation,
    Event_date,
    Measurement_mean
  )

water_levels %>%
  count(
    Site_abbreviation,
    Event_date,
    Measurement_mean
  )

### The above shows max, mean and median with 242 values

water_levels <-
  water_levels %>%
  group_by(
    Measurement_type,
    Site_abbreviation,
    Event_date
  ) %>%
  slice_head()

### DF has 242 rows, as expected. Now remove unneeded columns:

water_levels <-
  water_levels[ ,
                -c(1, 3, 7)
                ]

# Compare [DOC] with ppt and water level ----------------------------------
## I have 242 points in water_levels, only 80 in doc. I need to pare down
## water levels. First to see where discrepancies are----

unique(doc$Event_date)
unique(water_levels$Event_date)

### Only unique 34 dates in doc df, while water_levels has 109

unique(doc$Site_abbreviation)
unique(water_levels$Site_abbreviation)

### water_levels has on CC, IS, MW, RC, and SS measurements. doc has BMW, CC,
### IS, MW, NS, RC, RV, SS, UPSS

unique(bogan_water_flow_cleaned$Site)

### bogan df has "Beeline_trib", "Below_neon", "Camp_creek" (CC), "EF_201A", 
### "EF_25", "First_bridge", "Indian_springs" (IS), "Kitty_joe", "Mesquite_wash"  
### (MW), "Otero_wash", "Picadilla_creek", "Pine_creek", "Rock_creek_main" (RC),
### "Rock_creek_trib", "Sunflower", "Syca_beeline", "WF_thicket", "WFabove25" 

# I need make both water level and doc have same info -------------
## Isolate dates from doc df:

filtered_levels <-
  water_levels %>%
  dplyr::filter(
    paste0(
      Event_date, "_", Site_abbreviation
      ) %in% 
      paste0(
        doc$Event_date, "_", doc$Site_abbreviation
        )
    )

##Only 31 obs in filtered water_levels, but doc has 80... something seems
##muy wrong.

# Plotting the data -------------------------------------------------------

ggplot() +
  geom_point(
    data = water_levels,
    mapping = aes(
      x = Event_date,
      y = Measurement_max
    ),
    alpha = 0.75
  ) +
  geom_point(
    data = doc,
    mapping = aes(
      x = Event_date,
      y = Measurement_max
    ),
    colour = "purple",
    alpha = 0.75
  ) +
  facet_grid(
    vars(
      Measurement_type
    ),
    scales = "free_y"
  ) +
  ggtitle(
    aes(
      title = "Comparison of [DOC] and water levels in Sycamore Creek"
    )
    ) +
  xlab("Event date") +
  ylab("Maximum measurement of day")

ggplot() +
  geom_point(
    data = water_levels,
    mapping = aes(
      x = Event_date,
      y = Measurement_max
    ),
    alpha = 0.75
  ) +
  geom_point(
    data = doc,
    mapping = aes(
      x = Event_date,
      y = Measurement_max
    ),
    colour = "purple",
    alpha = 0.75
  ) +
  ggtitle(
    aes(
      title = "Comparison of [DOC] and water levels in Sycamore Creek"
    )
  ) +
  xlab("Event date") +
  ylab("Maximum measurement of day") +
  facet_wrap(
    vars(
      Site_abbreviation
    ),
    scales = "free_y"
  )

# Playing with ppt and [DOC} ----------------------------------------------

prism_ppt_cleaned <-
  prism_ppt_cleaned %>%
  mutate(
    Site_abbreviation = if_else(
      Site_abbreviation == "Above_SS",
      "UPSS",
      Site_abbreviation
    )
  )

prism_ppt_cleaned <-
  prism_ppt_cleaned %>%
  mutate(
    Site_abbreviation = if_else(
      Site_abbreviation == "Below_MW",
      "BMW",
      Site_abbreviation
    )
  )

date_site <-
  doc %>%
  select(
    Event_date,
    Site_abbreviation
  )

filter_ppt <-
  prism_ppt_cleaned %>%
  semi_join(
    x = doc,
    by = c(
      "Event_date",
      "Site_abbreviation"
    )
  )
