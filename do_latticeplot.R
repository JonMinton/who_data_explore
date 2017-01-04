

rm(list = ls())

pacman::p_load(
  tidyverse,
  readxl,
  stringr,
  ggplot2,
  RColorBrewer,
  lattice, latticeExtra
)

# Data are here 
all_tidy <- read_csv("tidy_data/tidied_data.csv")
diagnosis <-  read_csv("who_bulk/exported_text/Diagnosis.txt",col_types = "cccc") # to convert from code to description 
# and chapter 
icd_chap <- read_excel(path = "tidy_data/icd_details.xlsx", sheet = "ICD_Chapters") %>% .[,c(1:4)]


# All cause mortality in Russia -------------------------------------------

all_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  filter(country == "RU") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  filter(mort_code == "TOT") %>% 
  levelplot(
    death_rate ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Russia, Total Mortality Rates",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    cuts = 40
  )

# Log scale
all_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  filter(country == "RU") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  filter(mort_code == "TOT") %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Russia, Total Mortality Rates (Log10 scale)",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    cuts = 40
  )

# Let's now compare mortality and log mortality for each of the countries
# Bulgaria, Georgia, Russia

all_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  filter(country %in% c("RU", "BG", "GE")) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  filter(mort_code == "TOT") %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | sex + country, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Russia, Bulgaria, and Georgia. Total Mortality Rates (Log10 scale)",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  ) -> p

png("figures/three_country_comparisons/total_log10.png", height = 40, width = 40, units = "cm", res = 300)
print(p)
dev.off()


# Russia - Labelled ---------------------------------------------


# The information in Diagnosis can be used to provide descriptions 
# for each of the disease types 
# It appears the coding involve both a combinatino of chapters and sub-chapters 

tmp1 <- diagnosis %>% 
  filter(Coding == "101") %>% 
  select(code = Code, title_who = Title)

tmp2 <- icd_chap %>% 
  filter(ICD == 10) %>% 
  select(chapter = Chapter, title_icd = Title)

tmp3 <- tmp1 %>% 
  inner_join(tmp2, by = c("title_who" = "title_icd"))



# For Russia 
all_tidy %>% 
  filter(country == "RU") %>% 
  right_join(tmp3, by = c("mort_code" = "code")) %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  select(year, sex, age, death_count, population_count, chapter, title_who) -> dta_ru

rm(tmp1, tmp2, tmp3)

# Proof of concept: 
dta_ru %>% 
  mutate(death_rate = death_count / population_count) %>% 
  filter(chapter == 1) %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | sex , 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Russia, Chapter 1, log10 mortality",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  )

png("figures/three_country_comparisons/Russia/log10_chapter.png", height = 30, width = 80, units = "cm", res = 300)
dta_ru %>% 
  mutate(death_rate = death_count / population_count) %>% 
  mutate(chapter = factor(chapter)) %>% 
  filter(!chapter %in% c(7, 8, 16)) %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | chapter + sex , 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Log10 Mortality by ICD-10 Chapter, Russia",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  )
dev.off()

png("figures/three_country_comparisons/Russia/ident_chapter.png", height = 30, width = 80, units = "cm", res = 300)
dta_ru %>% 
  mutate(death_rate = death_count / population_count) %>% 
  mutate(chapter = factor(chapter)) %>% 
  filter(!chapter %in% c(7, 8, 16)) %>% 
  levelplot(
    death_rate ~ year * age | chapter + sex , 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Mortality by ICD-10 Chapter, Russia",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  )
dev.off()




# Now to automate the comparison of these three countries by cause of death 




automate_threecountries_ident <- function(CODE, DTA){
  label <- paste0(
    "Russia, Bulgaria, and Georgia. Death code: ",
    CODE
  )
  
  flnm <- paste0("figures/three_country_comparisons/ident/RU_BU_GE_", CODE, ".png")
  
  code_last <- CODE %>% str_replace("\\d{2,3}_", "")
  
  DTA %>% 
    filter(mort_code == code_last) %>% 
    levelplot(
      death_rate ~ year * age | sex + country,
      data=.,
      region=T,
      strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
      main=label,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=0.9),
        y=list(cex=0.9),
        alternating=3
      ),
      cuts = 40
    ) -> p

  png(
    flnm ,
    height = 40, width = 40, units = "cm", res = 300
  )
  print(p)
  dev.off()
  TRUE
}



all_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  filter(country %in% c("RU", "BG", "GE")) %>% 
  mutate(code = paste(icd_schema, mort_code, sep = "_")) %>% 
  filter(mort_code != "TOT") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(code) %>% 
  nest() -> nested_3countries

nested_3countries %>% 
  mutate(tmp = map2(code, data, try(automate_threecountries_ident))) 




# Same again, but all countries (THIS WILL TAKE AGES!)




automate_allcountries_ident <- function(CODE, DTA){
  label <- paste0(
    "All countries. Death code: ",
    CODE
  )
  
  flnm <- paste0("figures/all_country_comparisons/ident/all_", CODE, ".png")
  
  code_last <- CODE %>% str_replace("\\d{2,3}_", "")
  
  DTA %>% 
    filter(mort_code == code_last) %>% 
    levelplot(
      death_rate ~ year * age | sex + country,
      data=.,
      region=T,
      strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
      main=label,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=0.9),
        y=list(cex=0.9),
        alternating=3
      ),
      cuts = 40
    ) -> p
  
  png(
    flnm ,
    height = 40, width = 40, units = "cm", res = 300
  )
  print(p)
  dev.off()
  TRUE
}


all_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("all", "85_89","90_94", "95"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("1", "1_4", "5_9", "10_14", "15_19", "20_24", "25_29", "30_34", "35_39", "40_44",
               "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84" )
  )) %>%   
  mutate(code = paste(icd_schema, mort_code, sep = "_")) %>% 
  filter(mort_code != "TOT") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(code) %>% 
  nest() -> nested_allcountries

nested_allcountries %>% 
  mutate(tmp = map2(code, data, try(automate_allcountries_ident))) 




# 
# 
# 
# # Let's now automate the production of figures, for each country and mortality code 
# 
# mdd_tidy %>% 
#   filter(!is.na(age)) %>% 
#   filter(!(age %in% c("All ages", "90 - 94", "95 +"))) %>% 
#   mutate(age = ordered(
#     age, 
#     levels = c("< 1 year", "1  - 4", "5  - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
#                "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89")
#   )) -> mdd_tidy2 
# 
# rm(mdd_tidy)
# gc()
# 
# make_levelplot <- function(LABEL, DTA){
#   DTA %>% 
#     levelplot(
#       death_count ~ year * age | sex, 
#       data=., 
#       region=T, 
#       strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
#       ylab=list(label="Age in years", cex=1.4),
#       xlab=list(label="Year", cex=1.4),
#       cex=1.4,
#       col.regions= rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
#       main=NULL,
#       labels=list(cex=1.2),
#       col="black",
#       scales=list(
#         x=list(cex=1.4), 
#         y=list(cex=1.4),
#         alternating=3
#       ),
#       cuts = 20
#     ) -> p
#   
#   fn <- paste0("figures/",LABEL, ".png")
#   
#   png(filename = fn, width = 20, height = 20, units = "cm", res = 300)
#   
#   print(p)
#   dev.off()
#   NULL
#   
#   
# }
# 
# mdd_tidy2 %>%   
#   mutate(label = paste(country, mort_code, sep = "_")) %>% 
#   group_by(label) %>% 
#   nest() %>% 
#   mutate(tmp = walk2(.x = label, .y = data, .f = make_levelplot))
# 
