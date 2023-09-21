# Comparing my r script with TKH for analytes -----------------------------
# Julia Dawn Grabow

# Packages ----------------------------------------------------------------

library(tidyverse)

# Read in csv's for comparison --------------------------------------------

JuliaCSV <-
read_csv(
  file = "/home/julia/Downloads/ions_cleaned.csv"
)

TKH_CSV <-
  read_csv(
    file = "/home/julia/R/mvs_exercises/chems_from_ASU_clean_230617.csv"
  )

JuliaHybridCSV <-
  read.csv(
    file = "/home/julia/R/mvs_exercises/chems_cleaned.csv"
  )

## My original CSV has 2527 rows and one based on TKH script has 2710!
## Diff = 183 obs
## 137 are from NA differences, but still have 46 unaccounted for

## Redo Julia csv to get columns matching hybrid csv----

JuliaCSV <-
  JuliaCSV %>%
  rename(
    SampleID = Sample_ID
  ) %>%
  rename(
    Site = Site_abbreviation
  ) %>%
  rename(
    DateTime = Event_date_time
  ) %>%
  rename(
    final_conc = Mean_measurement
  ) %>%
  rename(
    N = n
  )

# Split Measurement type into two columns----

JuliaCSV <-
  JuliaCSV %>%
  separate_wider_delim(
    cols = Measurement_type,
    delim = "_",
    names = c(
      "Analyte",
      "Concentration_units"
    )
  )

# Fix notation of conc units (remove /)----
## I fucked up the units when working on the df