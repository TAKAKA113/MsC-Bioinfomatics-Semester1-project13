library(dplyr)
library(readxl)
library(stringr)
library(openxlsx)

#1.Merge BLUE values from SPNO, SPL, and TKW (keep NA values)
spno_spl_tkw_merged <- blue_spno %>%
  full_join(blue_spl, by = "Genotype") %>%
  full_join(blue_tkw, by = "Genotype")

head(spno_spl_tkw_merged)
view(spno_spl_tkw_merged)
nrow(spno_spl_tkw_merged) #197 row

#Rename columns for marge with teammate
spno_spl_tkw_merged <- spno_spl_tkw_merged %>%
  rename(
    SPNO = BLUE_SPno,
    SPL  = BLUE_SPL,
    TKW  = BLUE_TKW
  )

print(colnames(spno_spl_tkw_merged))
