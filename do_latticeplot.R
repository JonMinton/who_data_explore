

rm(list = ls())

pacman::p_load(
  tidyverse,
  stringr,
  ggplot2,
  RColorBrewer,
  lattice, latticeExtra
)

# Numerators here 
mdd_tidy <- read_csv("tidy_data/tidied_data.csv")


# Denominators here 
pop_data <- read_csv("tidy_data/population_counts.csv", col_types = "cccid")


mdd_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("All ages", "90 - 94", "95 +"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("< 1 year", "1  - 4", "5  - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
               "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89")
    )) %>% 
  filter(mort_code == "TOT") %>% 
  filter(country == "RU") %>% 
  levelplot(
    death_count ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    cuts = 20
  )


# Let's now automate the production of figures, for each country and mortality code 

mdd_tidy %>% 
  filter(!is.na(age)) %>% 
  filter(!(age %in% c("All ages", "90 - 94", "95 +"))) %>% 
  mutate(age = ordered(
    age, 
    levels = c("< 1 year", "1  - 4", "5  - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
               "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89")
  )) -> mdd_tidy2 

rm(mdd_tidy)
gc()

make_levelplot <- function(LABEL, DTA){
  DTA %>% 
  levelplot(
    death_count ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    cuts = 20
  ) -> p
  
  fn <- paste0("figures/",LABEL, ".png")
  
  png(filename = fn, width = 20, height = 20, units = "cm", res = 300)
  
  print(p)
  dev.off()
  NULL

  
}

mdd_tidy2 %>%   
  mutate(label = paste(country, mort_code, sep = "_")) %>% 
  group_by(label) %>% 
  nest() %>% 
  mutate(tmp = walk2(.x = label, .y = data, .f = make_levelplot))

