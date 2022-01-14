## Statistics for Applied RS and GIS project 
## Stockholm University, 2022 
## Emma Gemal

### Library ----
library(tidyverse)
library(lme4)


## Loading the data
tiles1960 <- read.csv("Data/area_1960_tiles.csv")
tiles2008 <- read.csv("Data/area_2008_tiles.csv")
tiles2018 <- read.csv("Data/area_2018_tiles.csv")

world <- read.csv("Data/other_glaciers.csv")


### Cleaning the data
tiles1960 <- tiles1960 %>% 
                mutate(YEAR = MERGE_SRC) %>% 
                dplyr::select(!MERGE_SRC) %>% 
                mutate(TILE = str_extract(YEAR, "_.._.._..")) %>% 
                mutate(YEAR = str_extract(YEAR, "[\\d]{1,4}")) %>% 
                mutate(SUM_AREAkm = SUM_AREA/1000000)
str(tiles1960)

tiles2008 <- tiles2008 %>% 
                mutate(YEAR = MERGE_SRC) %>% 
                dplyr::select(!MERGE_SRC) %>% 
                mutate(TILE = str_extract(YEAR, "_.._.._..")) %>% 
                mutate(YEAR = str_extract(YEAR, "[\\d]{1,4}")) %>% 
                mutate(SUM_AREAkm = SUM_AREA/1000000) %>% 
                mutate(CLASS_NAME = case_when(CLASS_NAME == "Glaciers" ~ "Glacier",
                                              CLASS_NAME == "Glacier" ~ "Glacier",
                                              CLASS_NAME == "Land" ~ "Land",
                                              CLASS_NAME == "Water" ~ "Water"))
str(tiles2008)

tiles2018 <- tiles2018 %>% 
                mutate(YEAR = MERGE_SRC) %>% 
                dplyr::select(!MERGE_SRC) %>% 
                mutate(TILE = str_extract(YEAR, "_.._.._..")) %>% 
                mutate(YEAR = str_extract(YEAR, "[\\d]{1,4}")) %>% 
                mutate(SUM_AREAkm = SUM_AREA/1000000) %>% 
                mutate(CLASS_NAME = case_when(CLASS_NAME == "Glaciers" ~ "Glacier",
                                              CLASS_NAME == "Glacier" ~ "Glacier",
                                              CLASS_NAME == "Land" ~ "Land",
                                              CLASS_NAME == "Water" ~ "Water"))
str(tiles2018)

# checking area totals and distribution 
sum(tiles1960$SUM_AREAkm)
sum(tiles2008$SUM_AREAkm)
sum(tiles2018$SUM_AREAkm)

ggplot(tiles1960, aes(x = CLASS_NAME, y = SUM_AREAkm)) +
  geom_col()
ggplot(tiles2008, aes(x = CLASS_NAME, y = SUM_AREAkm)) +
  geom_col()
ggplot(tiles2018, aes(x = CLASS_NAME, y = SUM_AREAkm)) +
  geom_col()

# summarizing each year 
sum1960 <- tiles1960 %>% 
              group_by(CLASS_NAME) %>% 
              summarise(AREA = sum(SUM_AREA),
                        AREAkm = sum(SUM_AREAkm))
sum2008 <- tiles2008 %>% 
              group_by(CLASS_NAME) %>% 
              summarise(AREA = sum(SUM_AREA),
                        AREAkm = sum(SUM_AREAkm))
sum2018 <- tiles2018 %>% 
              group_by(CLASS_NAME) %>% 
              summarise(AREA = sum(SUM_AREA),
                        AREAkm = sum(SUM_AREAkm))


### Combining 1960, 2008 and 2018 tile data
tiles60_08 <- full_join(tiles1960, tiles2008)
tiles <- full_join(tiles60_08, tiles2018)

tiles08_18 <- full_join(tiles2008, tiles2018)
tiles08_18 <- tiles08_18 %>% 
                mutate(YEAR = as.numeric(YEAR)) %>%   # making year numeric 
                filter(CLASS_NAME == "Glacier")

tiles60_08 <- tiles60_08 %>% 
                mutate(YEAR = as.numeric(YEAR)) %>%   # making year numeric 
                filter(CLASS_NAME == "Glacier")


### Glacier-data only 
tiles_glaciers <- tiles %>% 
                    filter(CLASS_NAME == "Glacier") %>% 
                    mutate(YEAR = as.numeric(YEAR))

# making a summarized dataset to use for plotting == it's the glacier's data DUH 
tiles_sum <- tiles %>% 
                group_by(YEAR, CLASS_NAME) %>% 
                summarise(AREAm = sum(SUM_AREA),
                          AREAkm = sum(SUM_AREAkm)) %>% 
                ungroup()

sum_glaciers <- tiles_sum %>% 
                  filter(CLASS_NAME == "Glacier") %>% 
                  mutate(YEAR = as.numeric(YEAR))
sum_glaciers

### Figure of results  
(tiles_bar <- ggplot(tiles_sum, aes(x = CLASS_NAME, y = AREAkm)) +
                geom_col(aes(fill = YEAR), position = "dodge") +
                xlab(element_blank()) +
                ylab(expression(paste("Area (", km^{2}, ")"))) +
                theme_bw() +
                theme(panel.grid.minor.y = element_blank(),
                      panel.grid.major.x = element_blank(),
                      axis.title.y = element_text(margin = margin(t = 0, r = 7, l = 0, b = 0)),
                      plot.margin = margin(1, 1, 1, 1, "cm")) +
                scale_y_continuous(expand = c(0,0), limits = c(0, 600),
                                   breaks = c(100, 300, 500)) +
                scale_fill_manual(name = "Year", 
                                  values = c("#061D54", "#02A8DF", "#FDEAB2")))

ggsave(filename = "Figures/barplot_area-time.pdf", plot = tiles_bar, 
       width = 5, height = 4, units = "in")



### Statistical analysis 
## Is there a significant difference between 1960, 2008 and 2018 glacier area? 
boxplot(SUM_AREAkm ~ YEAR, tiles_glaciers)

lm1 <- lmer(SUM_AREAkm ~ YEAR + (1|TILE), tiles_glaciers)
lm2 <- lm(SUM_AREAkm ~ YEAR, tiles_glaciers)

AIC(lm1, lm2) # lm1 is better, but sooo many tiles (not a great random effect)

summary(lm2)

## Change across whole park (sum of all tiles)
lm_sum <- lm(AREAkm ~ YEAR, sum_glaciers)
summary(lm_sum)
# slope with the sums = -2.2811 km2 per year for the whole park 


## Looking at all the classes
tiles_num <- tiles %>% 
                mutate(YEAR = as.numeric(YEAR))

lm_class1 <- lmer(SUM_AREAkm ~ YEAR + (1|CLASS_NAME), tiles_num)
lm_class2 <- lm(SUM_AREAkm ~ YEAR + CLASS_NAME, tiles_num)
lm_class3 <- lm(SUM_AREAkm ~ YEAR + CLASS_NAME, tiles)   # using categorical 'YEAR' 

AIC(lm_class1, lm_class2, lm_class3)  # 'lm_class2' is best 

summary(lm_class2)
# significant difference in area between years for 'Land' (p = <2e-16)
# area of land significantly increases across the years (10.384 km2 per year it seems)
# water also significantly declined (p = 1.16e-12, slope = -6.119)

## Change from 2008-2018 only 
# t-test to get significance
t.test(SUM_AREAkm ~ YEAR, tiles08_18)
# p = 0.04467
# t = 2.0586
# DF = 50.922

# rate of change per year per tile 
lm_modern <- lm(SUM_AREAkm ~ YEAR, tiles08_18)
summary(lm_modern)
# negative change per tile (slope = -0.1925 km2 per year) 

# rate of change per year for the whole park 
sum08_18 <- tiles08_18 %>% 
              group_by(YEAR) %>% 
              summarise(AREAkm = sum(SUM_AREAkm))

lm_mod_sum <- lm(AREAkm ~ YEAR, sum08_18)
summary(lm_mod_sum)
# change = -5.197 km2 per year across the whole park 


## Change from 1960-2008 only 
# t-test to get significance
t.test(SUM_AREAkm ~ YEAR, tiles60_08)
# p = 0.0045
# t = 2.9752
# DF = 49.68

# change per tile per year 
lm_past <- lm(SUM_AREAkm ~ YEAR, tiles60_08)
summary(lm_past)  
# slight negative change (-0.07) and significant (p = 0.0044)

# change per year across whole park
sum60_08 <- tiles60_08 %>% 
                group_by(YEAR) %>% 
                summarise(AREAkm = sum(SUM_AREAkm))

lm_hist_sum <- lm(AREAkm ~ YEAR, sum60_08)
summary(lm_hist_sum)
# -1.891 km2 per year across the whole park 


### Comparison to alpine glaciers ----
ggplot(world, aes(x = time_period, y = rate_perc)) +
  geom_col(aes(fill = place))

## Adding Sarek data 
# 1960-2018 = 0.68% per year
# 1960-2008 = 0.30% per year 
# 2008-2018 = 2.91% per year 
time_period <- c("1960-2008", "2008-2018", "1960-2018")
rate_perc <- c(0.70, 2.91, 0.91)
place <- c("Sweden, Sarek National Park", "Sweden, Sarek National Park", "Sweden, Sarek National Park")

sarek <- tibble(time_period, rate_perc, place)

world_sarek <- full_join(world, sarek)

ggplot(world_sarek, aes(x = time_period, y = rate_perc)) +
  geom_col(aes(fill = place))

# removing the total rates and leaving the smaller time periods 
world_sarek <- world_sarek %>% 
                  filter(time_period != "1960-2018")

## Making an ordered plot (by location)
world_sarek$time_period <- world_sarek$time_period %>% 
                          str_replace_all("-", "- ")

world_sarek_ordered <- world_sarek %>% 
                          arrange(place) %>% 
                          mutate(time_period = factor(time_period, levels = time_period))
str(world_sarek_ordered)    

world_sarek_ordered <- world_sarek_ordered %>% 
                          mutate(place = case_when(place == "Chile, Southern Patagonia Icefield"
                                                    ~ "Chile, \nSouthern Patagonia Icefield",
                                                   place == "Sweden, Sarek National Park"
                                                    ~ "Sweden, \nSarek National Park",
                                                   place == "European Alps" ~ "European Alps",
                                                   place == "Great Caucasus" ~ "Great Caucasus"))

(world_plot2 <- ggplot(world_sarek_ordered, aes(x = time_period, y = rate_perc)) +
                  geom_col(aes(fill = place), position = "dodge") +
                  xlab("Time period") +
                  ylab(expression(paste("Rate of loss (% ", yr^{-1}, ")"))) +
                  theme_bw() +
                  theme(panel.grid.minor.y = element_blank(),
                        panel.grid.major.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 7, l = 0, b = 0)),
                        axis.title.x = element_text(margin = margin(t = 9, r = 0, l = 0, b = 0)),
                        axis.text.x = element_text(size = 8, angle = 30, vjust = 0.5),
                        plot.margin = margin(1, 1, 1, 1, "cm"),
                        legend.text = element_text(size = 8)) +
                  scale_y_continuous(expand = c(0, 0),
                                     limits = c(0, 3.3)) +
                  scale_fill_manual(name = "Location", 
                                    values = c("#02A8DF", "#F49700", "#FDE8B0", "#061D54")))

ggsave(filename = "Figures/barplot_world_rates_ordered.pdf", plot = world_plot2, 
       width = 7, height = 4.5, units = "in")  


tiles_glaciers$SUM_AREAkm<2
