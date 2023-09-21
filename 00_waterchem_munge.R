### Summarize & plot SYC burn water chem ###
## Author: T.K. Harms

## Input: 
  # 2 Excel files on google drive
  # multiple Excel files containing cations from instrument
  # .csv containing compiled anions

## Outputs:
  # merged data file of chems + metadata
  # plots of chemistry summarized by storm
  # plots of chems vs. time at events scale

library(here)
library(tidyverse)
library(googledrive)
library(readxl)
library(googlesheets4)
library(viridis)
library(gridExtra)
library(grid)
library(gtable)
library(data.table)
library(lubridate)

##################
### Data input ###
##################
## Get chems from Drive

# Retrieve Rapid ions sample list_in progress -----------------------------

chem_url <- 
  "https://docs.google.com/spreadsheets/d/1Qalsj8EHJ2Zs1wTitPUkSJQWEAaOofpL/edit#gid=1084356142"

chem_drive <- 
  drive_get(
    as_id(
      chem_url
      )
    )

# Create file with walk function

walk(chem_drive$id, ~ drive_download(as_id(.x), overwrite = TRUE))

# Read in file ------------------------------------------------------------
## Skip the following code chunk and go to revised chunk

chems <- 
  read_excel(
    "RAPID ion sample list_in progress.xlsx", 
    col_types = c(
      "numeric",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "text",
      "text",
      "date",
      "date",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text"
      )
    )

## The above had warnings related to N.D. occuring in a numeric column
## Is this problematic? I wonder if it is better to read in as text, then
## convert to numeric later? It appears the N.D. was converted to NA
## How many NA?

sum(
  is.na(
    chems$Peak_Concentration
  )
)

## 502. How many N.D. in the original? Only 9 warnings, and checks out with 
## spreadsheet on google drive
## Warning messages:                                                                                              
# 1: Expecting numeric in G1098 / R1098C7: got 'N.D.' 
# 2: Expecting numeric in G1128 / R1128C7: got 'N.D.' 
# 3: Expecting numeric in G1130 / R1130C7: got 'N.D.' 
# 4: Expecting numeric in G1774 / R1774C7: got 'N.D.' 
# 5: Expecting numeric in G1786 / R1786C7: got 'N.D.' 
# 6: Expecting numeric in G1789 / R1789C7: got 'N.D.' 
# 7: Expecting numeric in G1792 / R1792C7: got 'N.D.' 
# 8: Expecting numeric in G1797 / R1797C7: got 'N.D.' 
# 9: Expecting numeric in G1798 / R1798C7: got 'N.D.' 

## I am going to play above with the read-in here:
## Read in column seven as character

chems <-
  read_excel(
    "RAPID ion sample list_in progress.xlsx", 
    col_types = c(
      "numeric",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "text",
      "text",
      "text",
      "date",
      "date",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text"
    )
  )

# Read in UAF_Samples spreadsheet -----------------------------------------
## Obtain url for spreadsheet

IC_url <- 
  "https://docs.google.com/spreadsheets/d/15h0wpigQOXxe2a3i6_RpQ0ujeMAvRCwn/edit#gid=659765141"

IC_drive <- 
  drive_get(
    as_id(
      IC_url
      )
    )

## Save the file locally methinks

walk(
  IC_drive$id,
  ~ drive_download(
    as_id(.x), 
    overwrite = TRUE
    )
  )

## Read in spreadsheet, fill in correct column types

ICmeta <- 
  read_excel(
    "UAF_Samples.xlsx", 
    col_types = c(
      "numeric",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "text",
      "text",
      "date",
      "date",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text"
      )
    )

## Rename columns as appropriate

ICmeta <- 
  ICmeta %>% 
  rename(
    Analyte = Annalyte
    ) %>%
  mutate(
    Sample_ID = as.character(
      Sample_ID
      )
    )

# Working on chems df -----------------------------------------------------
# Basic changes----
## Change sample_id column to character column from numeric
  
chems <- 
  chems %>% 
  mutate(
    Sample_ID = as.character(
      Sample_ID
      )
    )

## before joining metadata with chems data, need to change N.D. in peak conc
## so that I can make it numeric and allow the join

chems <-
  chems %>%
  mutate(
    Peak_Concentration = ifelse(
      Peak_Concentration == "N.D.",
      "0.001",
      Peak_Concentration
    )
  ) %>%
  mutate(
    Peak_Concentration = as.numeric(
      Peak_Concentration
    )
  )

## Join metadata df with chems df to create cations_now column

chems <- 
  bind_rows(
    chems, 
    ICmeta
    )

## Format data----
# N.D. in data = not detected
# Setting dilution factor = NA to 1

chems <-
  chems %>% # rename columns
  rename(
    dilution = "Auto_Dilution Factor"
    ) %>%
  rename(
    SampleID = Sample_ID
    ) %>% # change CL to chloride in analyte column
  mutate(
    Analyte = ifelse(
      Analyte == "CL",
      "Chloride",
      Analyte
      )
    ) %>% # Set NA dilution to 1 in dilution column
  mutate(
    dilution = ifelse(
      is.na(dilution),
      1, 
      dilution
      )
    )

## Ignore following code chunk; revised version already used above to replace
## N.D. with 0.001 in Peak Conc column

practice <-
  chems %>%
mutate_at(
  vars(Peak_Concentration), 
  ~replace(.,
           . == "N.D.",
           "0.001"
  )
)            

# Create csv of chems df --------------------------------------------------
## Why is the csv made now?

write.csv(
  x = chems, 
  file = here(
    "chems.csv"
  )
)

# Fix date time columns ---------------------------------------------------
## Fix collection times

chems <- 
  data.frame(
    chems
    ) # Is this code necessary? What is its purpose?

chems$Date <- 
  as.Date(
    chems$Date, 
    format = "%Y-%m-%d"
    )

# Replacing all time = NA with 12:00 for now. 
## Check with Leah on missing times

chems <- 
  chems %>% # Convert time column to character using strftime()
  mutate(
    time = strftime(
      time, 
      format = "%H:%M:%S", 
      tz = "UTC"
      )
    ) %>% # Replace NAs in time with 12:00:00
  mutate(
    time = replace_na(
      time, 
      "12:00:00"
      )
    ) %>% # Create datetime column with paste(), ensure it is datetime class
  mutate(
    DateTime = as.POSIXct(
      paste( 
        Date, 
        time
        ), 
      format = "%Y-%m-%d %H:%M:%S",
      tz="America/Phoenix"
      )
    )

# Harmonize analyte names and units ---------------------------------------

chems <- 
  chems %>% # Create sampleID column, ensure it is character class (why?)
  mutate(
    sampleID = as.character(
      SampleID
      )
    ) %>% # Use nested ifelse() to standardize concentration units
  mutate(
    Concentration_Units = ifelse(
      Analyte == "ions", "uM", ifelse(
        Analyte == "Chloride","mgL", ifelse(
          Analyte == "SRP", "ugL", ifelse(
            Analyte == "TDP", "mgL", ifelse(
              Analyte == "TC", "mgL", ifelse(
                Analyte == "DOC", "mgL", ifelse(
                  Analyte == "TOC", "mgL", ifelse(
                    Analyte == "TN", "mgL", "mgNL"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

# Apply dilution factor

chems <- 
  chems %>% 
  mutate(
    final_conc = as.numeric(
      Peak_Concentration
      )*dilution
    )

# Duplicate analyses ------------------------------------------------------
## If duplicates of sampleID & analyte but different dilution, keep the row with 
## greater dilution; if not duplicated or if dilution is same, keep all

### following code unnecessary as this was already performed. No NAs in col

chems <- 
  chems %>% 
  mutate(
    dilution = ifelse(
      is.na(dilution), 
      1, 
      dilution
      )
    )

## Keep highest dilution factor----
### skip following code in favor of my changes :)

chems <- 
  chems %>% # group by diff cols; this occurs in ascending order
  group_by(
    Analyte, 
    Site, 
    DateTime, 
    Type, 
    SampleID
    ) %>% # top_n should pick lowest, correct? and why abs(dilution)?
  top_n(
    1, 
    abs(dilution)
    )

# My code attempt:

chems <-
practice1 %>% # group by diff cols; this occurs in ascending order
  slice_max(
    dilution,
    by = c(
    Analyte, 
    Site, 
    DateTime, 
    Type, 
    SampleID
    ),
    n = 1
  )

## Difference between chems and updated df?

diff_df <-
  setdiff(
    chems,
    practice1
  )

## Looks correct, as does TKH script from above. Now apply to actual chems df:

chems <-
chems %>%
  slice_max(
    dilution,
    by = c(
      Analyte, 
      Site, 
      DateTime, 
      Type, 
      SampleID
    ),
    n = 1
  )

# If Analyte & sampleID match, take mean----
# Add placeholder for "ions": these are samples run on IC at UAF

chems_mn <- 
  chems %>% # Choose columns to focus on
  group_by(
    SampleID, 
    DateTime, 
    Site, 
    Analyte, 
    Concentration_Units, 
    Type, 
    Bottle_number, 
    Event_Date, 
    Stage) %>% # Create column N that denotes how many samples calculated mean
  mutate(
    N = n()
    ) %>% # again, select cols to focus on, this time include N
  group_by(
    SampleID, 
    DateTime, 
    Site, 
    Analyte, 
    Concentration_Units, 
    Type, 
    Bottle_number, 
    Event_Date, 
    Stage, 
    N
    ) %>% # Condense final_conc to one value, remove NAs
  summarize_at(
    vars(final_conc), 
    mean, 
    na.rm = TRUE
    ) %>% # Change NaN to NA, but I don't see any in df. Unnecessary(?)
  mutate(
    final_conc = ifelse(
      final_conc == "NaN", 
      NA, 
      final_conc
      )
    ) %>% # All ion analytes have NA, replace with 1
  mutate(
    final_conc = ifelse(
      Analyte == "ions", 
      1, 
      final_conc
      )
    )

## The above removes NAs from final conc column, then adds NAs. Why?
## Looks to be that correct changes were made when creating chems_mn

# Reorganize data for plotting --------------------------------------------
## Creates wider table for mean chems df. Why wider?

chems_wide <- 
  chems_mn %>% # What is the purpose of the distinct function?
  distinct () %>%
  pivot_wider(
    ., # Unsure of this meaning? Implying chems_mn?
    names_from = c(
      Analyte, 
      Concentration_Units
      ), # Create column for each Analyte with Concentration unit
    values_from = c(
      final_conc, 
      N
      ) # Create column for each Analyte and N
    )

# I prefer the df as it is right now, will use chems_mn
## Remove final_conc_ at the beginning of each analyte

names(
  chems_wide
  ) = gsub(
    pattern = "final_conc_", 
    replacement = "", 
    x = names(
      chems_wide
      )
    )

## LOQ correct
### Would rather have LOQ here. Currently replacing anything below the low 
### standard with the MDL.
## Note: the low DOC standard is greater than most samples. Setting 1 mg/L as 
##the low.

chems_corr <-
  chems_wide %>% 
  mutate(
    DOC_mgL = replace(
      DOC_mgL, 
      DOC_mgL < 1, 
      0.144
      )
    ) %>% # I don't see this next analyte listed in table
  mutate(
    NH4N_color_mgL = replace(
      NH4_mgNL, 
      NH4_mgNL < 0.01, 
      0.003
      )
    ) %>% 
  mutate(
    SRP_ugL = replace(
      SRP_ugL, 
      SRP_ugL < 0.005, 
      0.000139
      )
    ) %>%
  mutate(
    TC_mgL = replace(
      TC_mgL, 
      TC_mgL < 12.5, 
      0.375)) %>%
  mutate(
    TDP_mgL = replace(
      TDP_mgL, 
      TDP_mgL < 0.1, 
      0.00465
      )
    ) %>%
  mutate(
    TN_mgL = replace(
      TN_mgL, 
      TN_mgL < 0.5, 
      0.004
      )
    ) %>%
  mutate(
    Cl_color_mgL = replace(
      Chloride_mgL, 
      Chloride_mgL < 2.5, 
      0.19
      )
    ) %>%
  mutate(
    NO3N_color_mgL = replace(
      NO3_mgNL, 
      NO3_mgNL < 0.005, 
      0.00085
      )
    ) 
## Do the above for the long format chems_mn

chems_long <-
  chems_mn %>%
  mutate(
    Analyte = ifelse(
      "DOC" < 1, 
      0.144,
      "DOC"
    )
  ) %>%
  mutate(
    Analyte = ifelse(
      "NH4" < 0.01, 
      0.003,
      "NH4"
    )
  ) %>% 
  mutate(
    Analyte = ifelse(
      "SRP" < 0.005, 
      0.000139,
      "SRP"
    )
  ) %>%
  mutate(
    Analyte = ifelse(
      "TC" < 12.5, 
      0.375,
      "TC"
      )
    ) %>%
  mutate(
    Analyte = ifelse(
      "TDP" < 0.1, 
      0.00465,
      "TDP"
    )
  ) %>%
  mutate(
    Analyte = ifelse(
      "TN" < 0.5, 
      0.004,
      "TN"
    )
  ) %>%
  mutate(
    Analyte = ifelse(
      "Chloride" < 2.5, 
      0.19,
      "Chloride"
    )
  ) %>%
  mutate(
    Analyte = ifelse(
      "NO3" < 0.005, 
      0.00085,
      "NO3"
    )
  ) 

## Remove ammonium, chloride, and nitrate...? ?Porque?

chems_corr <- 
  chems_corr %>% 
  select(
    -c(
      NH4_mgNL, 
      Chloride_mgL, 
      NO3_mgNL
      )
    )
  
write.csv(
  chems_corr, 
  here(
    "chems_from_ASU_clean_230617.csv"
    ), 
  row.names = FALSE
  )

write.csv(
  chems_long,
  here(
    "chems_cleaned.csv"
  )
)

### Anions ###
#2022
sheets1 <- excel_sheets(here("data", "Anions", "220722 anions.xlsx"))
anions2022 <- map_df(sheets1, ~mutate(read_excel(here("data", "Anions", "220722 anions.xlsx"), col_types = "text" , sheet = .x, skip = 3), anion_rundate = .x))

sheets2 <- excel_sheets(here("data", "Anions", "2023_anions.xlsx"))
anions2023 <- map_df(sheets2, ~mutate(read_excel(here("data", "Anions", "2023_anions.xlsx"), col_types = "text" , sheet = .x, skip = 3), anion_rundate = .x))

anions <- bind_rows(anions2022, anions2023)

names(anions) <- c("runorder", "sampleID", "type", "Site", "year", "dilution",
                       "rettime_Fl", "Fluoride_mgL", "relareaFl", "areaFl", "heightFl",
                       "rettime_Cl", "Chloride_mgL", "relareaCl", "areaCl", "heightCl",
                       "rettime_SO4", "Sulfate_mgL", "relareaSO4", "areaSO4", "heightSO4",
                       "rettime_Br", "Bromide_mgL", "relareaBr", "areaBr", "heightBr",
                       "rettime_NO3", "NitrateN_mgL", "relareaNO3", "areaNO3", "heightNO3", "anion_rundate",
                   "junk1", "junk2", "junk3")

# Variable name matching
anions$runorder <- as.numeric(anions$runorder)
anions$anion_rundate <- as.numeric(anions$anion_rundate)
anions$year <- as.numeric(anions$year)
anions$type <- as.character(anions$type)
anions$dilution <- as.numeric(anions$dilution)
anions$NitrateN_mgL <- as.numeric(anions$NitrateN_mgL)
anions$Site <- as.character(anions$Site)

Syc_anions <- anions[,-c(33:35)] %>% filter(Site %in% c("Sycamore", "Syc", "SYC"),
                                              !sampleID %in% c("Fl check", "EX QC 0.1", "blank", "ExQC", "CCV", "0.02"), !grepl("NO3", sampleID))

## remove rows where NA for all anions
Syc_anions <- Syc_anions %>% filter(!is.na(NitrateN_mgL))

Syc_anions <- Syc_anions %>% 
  mutate(across(ends_with("mgL"), ~as.numeric(.x))) %>%
  mutate(across(ends_with("mgL"), ~replace_na(.x, 0.0049))) %>%
  mutate(Fluoride_mgL = replace(Fluoride_mgL, Fluoride_mgL < 0.005, 0.0012)) %>%
  mutate(Chloride_mgL = replace(Chloride_mgL, Chloride_mgL < 0.005, 0.0034)) %>%
  mutate(Sulfate_mgL = replace(Sulfate_mgL, Sulfate_mgL < 0.005, 0.0012)) %>%
  mutate(Bromide_mgL = replace(Bromide_mgL, Bromide_mgL < 0.005, 0.0023)) %>%
  mutate(NitrateN_mgL = replace(NitrateN_mgL, NitrateN_mgL < 0.005, 0.0004)) 

### No dilution factors here... were anions all run undiluted in 2022?
Syc_anions <- Syc_anions %>% select("runorder", "sampleID", "type", "Site", "year", "dilution",
                                              "Fluoride_mgL","Chloride_mgL","Sulfate_mgL",  "Bromide_mgL","NitrateN_mgL", "anion_rundate") %>%
                                       filter(!sampleID %in% "253" | anion_rundate == "220922") %>%
                                       filter(!is.na(sampleID))

## Were any samples analyzed more than once? 
# Find them, take means or reject values
n_occur <- data.frame(table(Syc_anions$sampleID))
n_occur[n_occur$Freq > 1,]
dups <- Syc_anions[Syc_anions$sampleID %in% n_occur$Var1[n_occur$Freq > 1],]

# If SampleIDs match, take mean
Syc_anions_QAQC <- Syc_anions %>% group_by(sampleID) %>%
                                           mutate(N_anions = n()) %>%
                                           group_by(sampleID, N_anions) %>%
                                           summarize(across(ends_with("mgL"), ~mean(.x, na.rm = TRUE)), across(anion_rundate, first)) 

write.csv(Syc_anions_QAQC, here("data", "SYC_anions_all.csv"), row.names = FALSE)

### Cations ###

#2021
sheets <- excel_sheets(here("data", "Cations", "SYC_burn_cations_2021.xlsx"))

sheets_SYC <- sheets[1:2]

cations_1 <- map_df(sheets_SYC, ~mutate(read_excel(here("data", "Cations", "SYC_burn_cations_2021.xlsx"), sheet = .x, skip = 3), rundate = .x))

names(cations_1) <- c("runorder", "SampleID", "type", "Site", "year", "dilution",
                      "rettime_Na", "Sodium_uM", "relareaNa", "areaNa", "heightNa",
                      "rettime_NH4", "Ammonium_uM", "relareaNH4", "areaNH4", "heightNH4",
                      "rettime_K", "Potassium_uM", "relareaK", "areaK", "heightK",
                      "rettime_Mg", "Magnesium_uM", "relareaMg", "areaMg", "heightMg",
                      "rettime_Ca", "Calcium_uM", "relareaCa", "areaCa", "heightCa", "cation_rundate")

cations_1[cations_1 == "n.a."] <- NA

cations_1 <- cations_1 %>% mutate(across(c("Sodium_uM", "Ammonium_uM", "Potassium_uM", "Magnesium_uM", "Calcium_uM"), ~as.numeric(.)))

#2022/2023
filelist = list.files(path = "data/Cations", pattern = "*23.*._cations.*.xlsx", full.names = TRUE, recursive = TRUE )
df.list <- lapply(filelist, read_excel)
names(df.list) <- basename( filelist ) # set file names
cations_2 <- rbindlist(df.list, idcol = TRUE, fill = TRUE) %>% separate(.id, into = c("cation_rundate", NA), sep = '[_]')

names(cations_2) <- c("cation_rundate", "runorder", "SampleID", "type", "Site", "year", "dilution",
                         "rettime_Na", "Sodium_uM", "relareaNa", "areaNa", "heightNa",
                         "rettime_NH4", "Ammonium_uM", "relareaNH4", "areaNH4", "heightNH4",
                         "rettime_K", "Potassium_uM", "relareaK", "areaK", "heightK",
                         "rettime_Mg", "Magnesium_uM", "relareaMg", "areaMg", "heightMg",
                         "rettime_Ca", "Calcium_uM", "relareaCa", "areaCa", "heightCa")

cations_2[cations_2 == "n.a."] <- NA

cations_2 <- cations_2 %>% filter(Site %in% c("Syc", "SYC")) %>% mutate(across(c("Sodium_uM", "Ammonium_uM", "Potassium_uM", "Magnesium_uM", "Calcium_uM", "year", "dilution"), ~as.numeric(.)))

# Merge cations
cats_all <- full_join(cations_1, cations_2)

# LOQ correct
cats_all <- cats_all %>% filter(Site %in% c("Syc", "SYC") | is.na(Site)) %>% 
  filter(as.numeric(runorder) > 12, !SampleID %in% c("blank", "ExQC", "CCV 10/5", "LOQ 2.4/1.2"), !grepl("uM Ca", SampleID), !grepl("EX", SampleID)) %>%
  select(cation_rundate, runorder, SampleID, year, dilution, Sodium_uM, Ammonium_uM, Potassium_uM, Magnesium_uM, Calcium_uM) %>%
  mutate(Sodium_uM = replace(Sodium_uM, Sodium_uM < 1.2, 0.113)) %>%
  mutate(Ammonium_uM = replace(Ammonium_uM, Ammonium_uM < 0.6, 0.306)) %>%
  mutate(Ammonium_uM = replace(Ammonium_uM, is.na(Ammonium_uM), 0.306)) %>%
  mutate(Potassium_uM = replace(Potassium_uM, Potassium_uM < 0.6, 0.074)) %>%
  mutate(Potassium_uM = replace(Potassium_uM, is.na(Potassium_uM), 0.074)) %>%
  mutate(Magnesium_uM = replace(Magnesium_uM, Magnesium_uM < 0.6, 0.049)) %>%
  mutate(Calcium_uM = replace(Calcium_uM, Calcium_uM < 1.2, 0.236)) 

# Dilutions
cats_all <- cats_all %>% mutate(dilution = ifelse(is.na(dilution), 1, dilution), across(contains("uM"), ~ .x*dilution)) %>% select(-"dilution")

# Remove summary stats from end of runs
cats_all <- cats_all[!is.na(as.numeric(as.character(cats_all$runorder))),]

write.csv(cats_all, here("data", "SYC_cations_all.csv"), row.names = FALSE)

## Duplicate analyses ##
## Were any samples analyzed more than once? 
# Find them, take means or reject values
n_occur <- data.frame(table(cats_all$SampleID))
n_occur[n_occur$Freq > 1,]
dups <- cats_all[cats_all$SampleID %in% n_occur$Var1[n_occur$Freq > 1],]

# If SampleID match, take mean
### TKH: there are some duplicates that do not appear to be replicates. Duplicated IDs or rerun due to instrument conditions?
SYC_cats_QAQC <- cats_all %>% group_by(SampleID) %>%
  mutate(N_cations = n()) %>%
  group_by(SampleID, N_cations) %>%
  summarize(across(ends_with("uM"), ~mean(.x, na.rm = TRUE)), across(cation_rundate, first)) 

# Data output
write.csv(SYC_cats_QAQC, here("data", "SYC_cations_QAQC.csv"), row.names = FALSE)

######################
### Merge analytes ###
######################
# add year to chems_corr
chems_corr$year <- as.numeric(format(as.Date(chems_corr$DateTime, format = "%Y-%m-%d %H:%M:%S"), "%Y"))

# Match column names
names(Syc_anions_QAQC)[names(Syc_anions_QAQC) == 'sampleID'] <- 'SampleID'

# Metadata for samples designated 
ions <- chems_corr %>% filter(ions_uM == 1)

## Join anions & cations ##
IC <- full_join(Syc_anions_QAQC, SYC_cats_QAQC, by = "SampleID")

## Join chems & anions/cations ##
# Samples identified for analysis of anions/cations in RAPID ion sample list_in progress.xlsx
ions$SampleID <- as.character(ions$SampleID)
chems_IC <- left_join(ions, IC, by = "SampleID")

# Many samples with anion/cation data not on the list. Pull samples from RAPID sample list not designated for ions and data from IC not previously matched via ions.
# ***Taking a guess that 2020 samples were not sent for anions & cations
chemsnoions <- chems_corr %>% filter(is.na(ions_uM)) %>%
                              filter(year != 2020)
chemsnoions$SampleID <- as.character(chemsnoions$SampleID)
ICnoions <- IC %>% filter(!SampleID %in% chems_IC$SampleID)
  
chem_an_cat <- full_join(chemsnoions, ICnoions, by = "SampleID")

chems2020 <- chems_corr %>% filter(year == 2020)
  
chems_most <- bind_rows(chem_an_cat, chems_IC)

chems2020$SampleID <- as.character(chems2020$SampleID)
chems_all <- bind_rows(chems_most, chems2020)

#####**** TKH: merge color & IC into new columns for Cl, NH4, NO3#####
#####* Are there any samples with both methods? #####* 

## combine colorimetric & IC nitrate
chems_all <- chems_all %>% mutate(NO3N_mgL = ifelse(is.na(NitrateN_mgL), NO3N_color_mgL, NitrateN_mgL)) %>%
                           mutate(Cl_mgL = ifelse(is.na(Chloride_mgL), Cl_color_mgL, Chloride_mgL))

write.csv(chems_all, here("data", "chems_all_230901.csv"), row.names = FALSE)

### Calculate replicate means
chems_repmean <- chems_all %>% group_by(Site, DateTime, Type, Event_Date, Stage) %>% 
                               summarize(across(where(is.numeric), list(mn = ~mean(., na.rm = TRUE), 
                                                                        SD = ~sd(., na.rm = TRUE), 
                                                                        N = ~sum(!is.na(.)))))


################
### Metadata ###
################
chems_repmean <- chems_repmean %>% mutate(burn = ifelse(Site == "IS", "unburned",
                                                 ifelse(Site == "NS", "unburned", "burned"))) %>%
                                   mutate(trib_main = ifelse(Site == "IS"| Site == "CC" | Site == "RC" | Site == "MW", "tributary", "mainstem"))

write.csv(chems_repmean, here("data", "chems_all_repmeans2023.csv"), row.names = FALSE)

### to do: *** Merge w/ NEON grabs.... ###

###################
### Check plots ###
###################

ggplot(data = chems_all, aes(x = DateTime, y = NitrateN_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = NO3N_color_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = NO3N_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = NH4N_color_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = TC_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Chloride_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Cl_color_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = SRP_ugL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = DOC_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = TN_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = TDP_mgL)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Ammonium_uM)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Calcium_uM)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Sodium_uM)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(data = chems_all, aes(x = DateTime, y = Potassium_uM)) +
  geom_point() +
  facet_wrap(~Site)

### Below no longer needed ###
## Metadata are now entered with chems data from ASU

################
### Metadata ###
################

# Sites = Site IDs, burn/unburned, trib/MS
sites_url <- "https://docs.google.com/spreadsheets/d/1Nijp4v8hVLf5fnDKDkwMV1d_o4aqGzy9kM4U7vazofs/edit#gid=0"
sites <- read_sheet(sites_url)
names(sites)[names(sites) == 'SiteID'] <- 'Site' 

# Sample dates = beginning sample date for storm sampling, baseflow sample dates, stage (baseflow, flood)
sample_dates_url <- "https://drive.google.com/file/d/1KkaLW_r8kIY9mj17TrKXitizNXyweY_k/view?usp=sharing"
sample_drive <- drive_get(as_id(sample_dates_url))
walk(sample_drive$id, ~ drive_download(as_id(.x), overwrite = TRUE))
samp_dates <- read.csv("SYC_sample_dates.csv", header = TRUE)

## Fix dates & times
samp_dates$Sample_date <- as.Date(samp_dates$Sample_date, format = "%m/%d/%y", tz = "America/Phoenix")
samp_dates$Event_Date <- as.Date(samp_dates$Event_date, format = "%m/%d/%y", tz = "America/Phoenix")

# Floods = estimated peak flow and timing from gauge for each sampled flood date
#floods_url <- "https://drive.google.com/file/d/1hTaws2lmFgiiBUKGCc6KOQXYn8itL4h0/view?usp=sharing"
#floods_drive <- drive_get(as_id(floods_url))
#walk(floods_drive$id, ~ drive_download(as_id(.x), overwrite = TRUE))
#floods <- read.csv("Flood Events Meta Data.csv", header = TRUE)

#floods$Event_Date <- as.Date(floods$Event_date, format = "%m/%d/%Y", tz = "America/Phoenix")

# Join floods to samp_dates
#floods_meta <- left_join(samp_dates, floods, by = "Event_Date")

# Remove NA rows
#floods_meta <- floods_meta %>% filter(!is.na(Event_Date))

# Replace missing times with noon. **Check with Leah on missing times**
#floods_meta <- floods_meta %>% mutate(Time = replace_na(Time, "12:00")) %>%
                               mutate(Time = ifelse(Time == "", "12:00", Time))

# combine date & time
#floods_meta$sampDateTime <- as.POSIXct(paste(floods_meta$Sample_date, floods_meta$Time), format="%Y-%m-%d %H:%M", tz = "America/Phoenix")

#floods_meta$peakDateTime <- as.POSIXct(paste(floods_meta$Event_Date, floods_meta$time_at_peak), format="%Y-%m-%d %H:%M", tz = "America/Phoenix")

#floods_meta$Date <- as.POSIXct(paste(floods_meta$Event_Date, floods_meta$time_at_peak), format="%Y-%m-%d %H:%M", tz = "America/Phoenix")


## Join metadata to chems
chems_merge$Sample_date <- as.Date(format(as.POSIXct(chems_merge$DateTime, format="%Y-%m-%d", tz = "America/Phoenix")))
chems_merge$sampDateTime <- chems_merge$DateTime

# Sites
chems_meta <- left_join(chems_merge, sites, by = "Site")

# Event dates & stats
#chems_meta <- left_join(chems_meta, floods_meta, by = c("Site", "Sample_date"))
  
dir.create(here("data"))
write.csv(chems_meta, here("data", "chems_meta_230511.csv"), row.names = FALSE)

### Summarize analytical replicates by date ###
#chems_meta <- chems_meta %>% relocate(Temp_C, .after = NO3_mgNL)
#chems_meta <- chems_meta %>% relocate(SPC, .after = Temp_C)
#chems_meta$DateTime <- as.POSIXct(chems_meta$sampDateTime, format="%Y-%m-%d %H:%M", tz = "America/Phoenix")

#chems.summ <- chems_meta %>% group_by(DateTime, Site, Event_Date, Stage, SiteName, burn, trib_main, eventID, Gageheight_ft, Peak_cfs, sampDateTime.y, year, Type) %>% 
#  summarize(across(DOC_mgL:Calcium_uM_mean, list(mn = mean, SD = sd, N = ~sum(!is.na(.x))), na.rm = TRUE))


  
write.csv(chems.summ, here("data", "chems_repmeans_220511.csv"), row.names = FALSE)


## Plots ##
NO3.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = NO3_mgNL_mn, color = Stage)) +
            geom_point() +
            facet_wrap(~Site) +
            theme_bw() +
            theme(
              legend.position = c(0.85, 0.2))
            
DOC.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = DOC_mgL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

NH4.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = NH4_mgNL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

SRP.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = SRP_ugL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

TDP.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = TDP_ugL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

TN.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = TN_mgNL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

Cl.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = Chloride_mgL_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

Ca.pl <- ggplot(data = chems.summ, aes(x = DateTime, y = Calcium_uM_mean_mn, color = Stage)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2))

ggsave(NO3.pl, path = here("plots"), file ="SYC_burn_NO3.pdf", width = 11, height = 8, units = "in")

ggsave(DOC.pl, path = here("plots"), file ="SYC_burn_DOC.pdf", width = 11, height = 8, units = "in")

ggsave(TN.pl, path = here("plots"), file ="SYC_burn_TN.pdf", width = 11, height = 8, units = "in")

### Pull in NEON grabs ###


