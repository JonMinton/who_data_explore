# HMD comparison 

rm(list = ls())

# This document will supplement the WHO database with HMD data. This will allow
# trends in all cause mortality to be compared for more years, and at a higher 
# age resolution (one year by one year). 


pacman::p_load(
  tidyverse,
  stringr,
  forcats,
  
  ggplot2,
  lattice, latticeExtra
)

dta <- read_csv("tidy_data/new_counts.csv")

source("scripts/hmd_country_group_definitions.R")


# Countries of interest are 
# Russia 
# Bulgaria
# Georgia

# Unfortunately Georgia is not in the HMD. However the other two countries are 
dta %>% 
  filter(country_code %in% c("RUS", "BGR")) %>% 
  filter(age <= 85) %>% 
  mutate(death_rate = deaths / exposure) %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | sex + country_code, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Russia & Bulgaria, Total Mortality Rates using HMD (Log10 scale)",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  ) -> p

png("figures/three_country_comparisons/total_log10_hmd.png", height = 40, width = 40, units = "cm", res = 300)
print(p)
dev.off()


# Compared with Europe overall 

dta %>% 
  filter(country_code %in% europe_codes) %>% 
  group_by(year, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), population = sum(population, na.rm = T), exposure = sum(population, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Europe") %>% 
  select(group, year, age, sex, death_rate) -> europe_mort


dta %>% 
  filter(country_code %in% europe_southern) %>% 
  group_by(year, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), population = sum(population, na.rm = T), exposure = sum(population, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Southern Europe") %>% 
  select(group, year, age, sex, death_rate) -> europe_southern_mort

dta %>% 
  filter(country_code %in% europe_northern) %>% 
  group_by(year, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), population = sum(population, na.rm = T), exposure = sum(population, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Northern Europe") %>% 
  select(group, year, age, sex, death_rate) -> europe_northern_mort

dta %>% 
  filter(country_code %in% europe_western) %>% 
  group_by(year, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), population = sum(population, na.rm = T), exposure = sum(population, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Western Europe") %>% 
  select(group, year, age, sex, death_rate) -> europe_western_mort

dta %>% 
  filter(country_code %in% europe_eastern) %>% 
  group_by(year, age, sex) %>% 
  summarise(deaths = sum(deaths, na.rm = T), population = sum(population, na.rm = T), exposure = sum(exposure, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Eastern Europe") %>% 
  select(group, year, age, sex, death_rate) -> europe_eastern_mort


dta %>% 
  filter(country_code == "RUS") %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Russia") %>% 
  select(group, year, age, sex, death_rate) -> russia_mort

dta %>% 
  filter(country_code == "BGR") %>% 
  mutate(death_rate = deaths / exposure) %>% 
  mutate(group = "Bulgaria") %>% 
  select(group, year, age, sex, death_rate) -> bulgaria_mort

europe_regions_mort <- reduce(
  list(europe_mort, europe_eastern_mort, 
       europe_western_mort, europe_northern_mort, europe_southern_mort,
       russia_mort, bulgaria_mort
       ),
  bind_rows  
  ) %>% 
  mutate(group = factor(
    group, 
    levels = c("Eastern Europe", "Southern Europe", "Western Europe", "Northern Europe" ,"Europe", "Russia", "Bulgaria")                    
    )
  )

# Levelplot for whole Europe and specific regions 

europe_regions_mort %>% 
  filter(age <= 85) %>% 
  filter(year >= 1950) %>% 
  levelplot(
    log(death_rate, 10) ~ year * age | sex + group, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main="Total Mortality Rates using HMD for European Regions (Log10 scale)",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    cuts = 40
  ) -> p

png("figures/three_country_comparisons/hmd_region_lvl_log10.png", height = 80, width = 40, units = "cm", res = 300)
print(p)
dev.off()


# And now, a comparison between Russia and each of the European regions 

europe_regions_mort %>% 
  filter(age <= 85) %>% 
  filter(year >= 1950) %>% 
  filter(group != "Bulgaria") %>% 
  mutate(log_mort = log(death_rate, 10)) %>%
  select(-death_rate) %>% 
  spread(group, log_mort) %>% 
  mutate_each(funs(Russia - .), `Eastern Europe`:`Russia`) %>% 
  gather(`Eastern Europe`:`Russia`, key = "group", value = "dif_log_mort") %>% 
  filter(!is.na(dif_log_mort)) %>%
  filter(group != "Russia") %>% 
  mutate(group = factor(group, levels = c("Europe", "Eastern Europe", "Southern Europe", "Western Europe", "Northern Europe"))) %>% 
  levelplot(
    dif_log_mort ~ year * age | group + sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    aspect="iso",
    main="Log10 Mort differences between Russia and particular regions",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    at = seq(from= -1.6, to = 1.6, by=0.2)
  ) -> p

png("figures/three_country_comparisons/russia_clp.png", height = 25, width = 50, units = "cm", res = 300)
print(p)
dev.off()


europe_regions_mort %>% 
  filter(age <= 85) %>% 
  filter(year >= 1950) %>% 
  filter(group != "Russia") %>% 
  mutate(log_mort = log(death_rate, 10)) %>%
  select(-death_rate) %>% 
  spread(group, log_mort) %>% 
  mutate_each(funs(Bulgaria - .), `Eastern Europe`:`Bulgaria`) %>% 
  gather(`Eastern Europe`:`Bulgaria`, key = "group", value = "dif_log_mort") %>% 
  filter(!is.na(dif_log_mort)) %>%
  filter(group != "Bulgaria") %>% 
  mutate(group = factor(group, levels = c("Europe", "Eastern Europe", "Southern Europe", "Western Europe", "Northern Europe"))) %>% 
  levelplot(
    dif_log_mort ~ year * age | group + sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.0), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    aspect="iso",
    main="Log10 Mort differences between Bulgaria and particular regions",
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=0.9), 
      y=list(cex=0.9),
      alternating=3
    ),
    at = seq(from= -1.6, to = 1.6, by=0.2)
  ) -> p

png("figures/three_country_comparisons/bulgaria_clp.png", height = 25, width = 50, units = "cm", res = 300)
print(p)
dev.off()






