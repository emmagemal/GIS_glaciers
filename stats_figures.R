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

# fixing areas for 1960 (dividing by 2, adding difference between 1960 and 2008/2018 back)
tiles1960 <- tiles1960 %>% 
                mutate(SUM_AREA = SUM_AREA/2) %>% 
                mutate(SUM_AREAkm = SUM_AREAkm/2)

sum(tiles1960$SUM_AREA)
sum(tiles1960$SUM_AREAkm)

sum(tiles2018$SUM_AREAkm) - sum(tiles1960$SUM_AREAkm)  # 17.298 km2 (~17.3)
17.3/59   # 0.293 km2 back per class per tile 

tiles1960 <- tiles1960 %>% 
                mutate(SUM_AREA = SUM_AREA + 293000) %>% 
                mutate(SUM_AREAkm = SUM_AREAkm + 0.293)

sum(tiles1960$SUM_AREA)
sum(tiles1960$SUM_AREAkm)

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

AIC(lm1, lm2) # lm2 is better 

summary(lm1)
summary(lm2)  # using this one
# no significant decline or increase between the years = STABLE, not positive (p = 0.216)
  # although a visual trend between the years (slope = -0.043 km2 per year per tile)


## Looking at all the classes
tiles_num <- tiles %>% 
                mutate(YEAR = as.numeric(YEAR))

lm_class1 <- lmer(SUM_AREAkm ~ YEAR + (1|CLASS_NAME), tiles_num)
lm_class2 <- lm(SUM_AREAkm ~ YEAR + CLASS_NAME, tiles_num)
lm_class3 <- lm(SUM_AREAkm ~ YEAR + CLASS_NAME, tiles)   # using categorical 'YEAR' 

AIC(lm_class1, lm_class2, lm_class3)  # 'lm_class2' is best 

summary(lm_class2)
# significant difference in area between years for 'Land' (p = 8.23e-05)
# area of land significantly increases across the years (11.955 km2 per year it seems)
# no significant difference between glacier or water area across the years 


## Change across whole park (sum of all tiles)
lm_sum <- lm(AREAkm ~ YEAR, sum_glaciers)
summary(lm_sum)
# slope with the sums = -1.155 km2 per year for the whole park 


## Change from 2008-2018 only 
lm_modern <- lm(SUM_AREAkm ~ YEAR, tiles08_18)
summary(lm_modern)
# negative change per tile (slope = -0.1925 km2 per year) 
# and it IS significant! (p = 0.0446)

# using summarized data (change for whole park)
sum08_18 <- tiles08_18 %>% 
              group_by(YEAR) %>% 
              summarise(AREAkm = sum(SUM_AREAkm))

lm_mod_sum <- lm(AREAkm ~ YEAR, sum08_18)
summary(lm_mod_sum)
# change = -5.197 km2 per across the whole park 


## Change from 1960-2008 only 
lm_past <- lm(SUM_AREAkm ~ YEAR, tiles60_08)
summary(lm_past)  
# slight negative change (-0.02278) but NOT significant at all (p = 0.669)



### Comparison to alpine glaciers ----
ggplot(world, aes(x = time_period, y = rate_perc)) +
  geom_col(aes(fill = place))

## Adding Sarek data 
# 1960-2018 = 0.68% per year
# 1960-2008 = 0.30% per year 
# 2008-2018 = 2.91% per year 
time_period <- c("1960-2008", "2008-2018", "1960-2018")
rate_perc <- c(0.30, 2.91, 0.68)
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
                  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) + 
                  scale_fill_manual(name = "Location", 
                                    values = c("#02A8DF", "#F49700", "#FDE8B0", "#061D54")))

ggsave(filename = "Figures/barplot_world_rates_ordered.pdf", plot = world_plot2, 
       width = 7, height = 4.5, units = "in")  



