# Initial exploration of WHO data

# 20/12/2016

rm(list = ls())

# The data are all in tables contained in an Access .mdb file. 
# To access this I will start by using ImportExport, which uses RODBC

pacman::p_load(
  tidyverse,
  stringr,
  ggplot2,
  lattice, latticeExtra
)

# Let's start by using the Shiny App to see how the package works

# ImportExportApp()
# 
# # One of the packages did not load. Let's do this manually instead. 
# 



# 
# tmp <- access_import(file = "who_bulk/dmdb.mdb")
# 
# # This doesn't work on this machine either: obdc connectaccess only works with 32 bit windows!
# 
# # Let's rethink and go for a different programme
# 
# 
# pacman::p_unload(
#   ImportExport,
#   shiny, shinythemes, compareGroups,
#   shinyBS
# )
# 
# # Let's try RODBC directly 
# 
# pacman::p_load(
#   RODBC
# )
# 
# myconn <- odbcConnectAccess2007("who_bulk/dmdb.mdb")


# After running into a series of issues importing directly, I've exported  each of the 
# tables into csv text files. 


# The table names are 

AgeGroups <- read_csv("who_bulk/exported_text/AgeGroups.txt")
AgeGroupTypes <- read_csv("who_bulk/exported_text/AgeGroupTypes.txt")
AStan <- read_csv("who_bulk/exported_text/AStan.txt")
CodingChapters <- read_csv("who_bulk/exported_text/CodingChapters.txt")
CodingTypes <- read_csv("who_bulk/exported_text/CodingTypes.txt")
Countries <- read_csv("who_bulk/exported_text/Countries.txt")
DiagConversions <- read_csv("who_bulk/exported_text/DiagConversions.txt")
Diagnosis <- read_csv("who_bulk/exported_text/Diagnosis.txt")
Gender <- read_csv("who_bulk/exported_text/Gender.txt")
MDD <- read_csv("who_bulk/exported_text/MDD.txt")
Population <- read_csv("who_bulk/exported_text/Population.txt")
Years <- read_csv("who_bulk/exported_text/Years.txt")


# All loaded... 


# First, link AgeGroups to AgeGroupTypes 

AgeGroups
AgeGroupTypes
AgeGroups_AgeGroupTypes <- left_join(AgeGroups, AgeGroupTypes) %>% 
  rename(agegroup_desc = Dsc)

# Unsure what to do with AStan right now 

# Join CodingTypes to CodingChapter
CodingChapters_CodingTypes <- left_join(CodingChapters, CodingTypes, by = c("Coding"="CodingId")) %>% 
  rename(coding_desc = Dsc.x, icd_class_desc = Dsc.y)


Countries


# The main file to work on is mdd

MDD %>% 
  gather(key = "age_group", value = "death_count", d0:d21) %>% 
  mutate(age_group = str_replace_all(age_group, "d", "") %>% as.integer) -> mdd_long

#mdd long should be combined with AgeGroups_AgeGroupTypes

mdd_long %>% 
  left_join(AgeGroups_AgeGroupTypes, by = c("AgeGroup" = "agType", "age_group" = "agItem")) %>% 
  select(
    country = Country, year = yId, icd_schema = Coding, 
    mort_code = Code, sex = Gender, age = Title, 
    age_range = agegroup_desc, death_count
  ) %>% 
  left_join(Gender, by = c("sex" ="GenderId")) %>% 
  mutate(sex = Title) %>% select(-Title) -> mdd_tidy

# Now to produce age groupings 
unique(mdd_tidy$age) %>% as_tibble -> age_labels

age_fst <- c(
  0,
  0,
  1,
  seq(5, 85, by = 5),
  85, 
  90,
  NA,
  95
)

age_lst <- c(
  110,
  0,
  seq(4, 89, by = 5), 
  110,
  94,
  NA,
  110
)

age_labels %>% 
  mutate(age_fst = age_fst, age_lst = age_lst) -> age_labels

rm(age_fst, age_lst)

mdd_tidy %>% 
  left_join(age_labels, by = c("age" = "value")) -> mdd_tidy

write_csv(mdd_tidy, "tidy_data/tidied_data.csv")


rm(age_labels, AgeGroups, AgeGroups_AgeGroupTypes, MDD) 
gc()
# Let's see if I can produce some kind of Lexis plot fairly quickly




