# This file generates a plot that displays the relationship between damages to GDP for the global
# temperature change and the summed damages to GDP for temperature change per country.

# the output is a plot that will show a linear relationship, for which a coefficient can be 
# calculated by: damage = gdp_w * a_w * (T_w – T_w(0))^2 = \sum a_c * gdp_c * (T_c-T_c(0))^2
# for a_w, the coefficient of the warming function of DICE-2016R is used which is 0.00236
# a_c will be determined in this file.

library(data.table)
library(dplyr)

temp_countries <- fread(file.path('data/cmip6/CMIP6_temp_countries.csv'))
temp_global <- fread(file.path('data/cmip6/CMIP6_temp_global.csv'))

# separate ssp from rcp
temp_countries$ssp = substring(temp_countries$rcp, 1,1)
temp_countries$rcp = substring(temp_countries$rcp,2)
temp_global$ssp = substring(temp_global$rcp, 1,1)
temp_global$rcp = substring(temp_global$rcp,2)
temp_countries <- temp_countries[year < 2101]
temp_global <- temp_global[year < 2101]

# load GDP from 2001 for all SSP scenarios
source('modules/gdpssp.R')
gdp_year <- gdp_yearly
gdp_year$ssp = substring(gdp_year$SSP, 4,4)
gdp_year$SSP <- NULL

# load GDPs from 1900 - 2000 in 2005 PPP USD
# from Geiger, T ; Frieler, K. (2017): Continuous GDP time series for 195 countries: from 1850
# to the Shared Socioeconomic Pathways. GFZ Data Services, http://doi.org/19.5880/pik.2017.003		
gdp_csv = file.path('data','GDP-national-PPP2005_SSP-harmonized_1850-2009.csv')
gdp_19002020 = fread(gdp_csv, header = T)
gdp_1900 <- transpose(gdp_19002020, keep.names = "year")
names(gdp_1900) <- as.matrix(gdp_1900[1, ])
gdp_1900 <- gdp_1900[-1, ]
colnames(gdp_1900)[1] <- "year"
# Load until 2001 to allow for calculating rates until 2001
gdp_1900 <- gdp_1900[year > 1899 & year < 2002]

gdp_1900_table <- data.table(ISO3=character(), year=numeric(), gdp=numeric())
gdp_1900_table$year <- gdp_1900$year
gdp_1900_table$ISO3 <- gdp_19002020$ISO
ISO3_countries <- as.data.frame(colnames(gdp_1900))
# append gdps to datatable in correct format
for (i in (2 : (ncol(gdp_1900))-1)){
  years <- as.numeric(gdp_1900[[1]])
  gdps <- as.numeric(gdp_1900[[i+1]])
  ISO3_country <- as.character(ISO3_countries[[1]][i+1])
  len_list = length(years)
  ISO3_c <- rep(ISO3_country, len_list)
  gdp_datatable <- data.table(ISO3=ISO3_c, year=years, gdp=gdps)
  gdp_1900_table <- rbind(gdp_1900_table,gdp_datatable)
}

# convert USD to billions of USD to match magnitude of the gdp_year
gdp_1900_table <- gdp_1900_table[, gdp := gdp/1e9] 
gdp_1900_table <- gdp_1900_table[!is.na(gdp)]
# remove date from 2001 to bind the list with the other data from 2000
gdp_1900_2000_table <- gdp_1900_table[year < 2001]

# combine gdp_year and gdp_1900 to get full record of GDPs from 1900 - 2100
# use 1900 as starting point
temp_c_1900 <- temp_countries[year < 2001]
temp_c_1900 <- temp_c_1900 %>% left_join(gdp_1900_2000_table)
temp_c_2001 <- temp_countries[year > 2000]
temp_c_2001 <- temp_c_2001 %>% left_join(gdp_year)
tempcountries <- rbind(temp_c_1900, temp_c_2001)
tempcountries <- tempcountries[order(tempcountries$ISO3),]

# use gdp_year and sum gdp's up
gdp_year <- rbind(gdp_1900_2000_table,gdp_year,fill = TRUE)
gdp_year <- gdp_year[order(gdp_year$ISO3),]
w_gdp = gdp_year[, .(gdp=sum(gdp)), by = c("year", "ssp")]

# combine datatables with gdp's from 1900-2000 with 2001-2100
temp_w_1900 <- temp_global[year < 2001]
temp_w_1900 <- temp_w_1900 %>% left_join(w_gdp, by = "year")
temp_w_2001 <- temp_global[year > 2000]
temp_w_2001 <- temp_w_2001 %>% left_join(w_gdp)
tempglobal <- rbind(temp_w_1900, temp_w_2001, by = "ISO3", fill = TRUE)
tempglobal <- tempglobal %>% mutate(ssp = coalesce(ssp,ssp.x)) %>%
  select(V1, model, ssp, rcp, year, temp, gdp)

# basetemps per model and scenario for global dataset
tempgdp_1900 <- tempglobal[year == 1900]
tempgdp_1900 <- select(tempgdp_1900, model, ssp, rcp, temp)
tempgdp_1900 <- tempgdp_1900 %>% rename(basetemp = temp)
tempgdp_1900 <- tempgdp_1900 %>% distinct(model, rcp, ssp, .keep_all = TRUE)
tempandgdp_global <-  tempglobal %>% distinct(model, rcp, ssp, year, 
                                                     .keep_all = TRUE)
tempandgdp_global <- tempandgdp_global %>% right_join(tempgdp_1900) 
tempandgdp_global <- tempandgdp_global[!is.na(tempandgdp_global$temp)]

# basetemps per model and scenario for country dataset
basetemp_countries <- temp_countries[year == 1900]
basetemp_countries <- select(basetemp_countries, model, ssp, rcp, ISO3, temp)
basetemp_countries <- basetemp_countries %>% rename(basetemp = temp)
basetemp_countries <- basetemp_countries %>% distinct(model, rcp, ssp, ISO3, .keep_all = TRUE)
tempandgdp_countries <-  tempcountries %>% distinct(model, rcp, ssp, year, ISO3, 
                                                           .keep_all = TRUE)
tempandgdp_countries <- tempandgdp_countries %>% right_join(basetemp_countries)
tempandgdp_countries <- tempandgdp_countries[!is.na(tempandgdp_countries$temp)]

# warming effect: damages = gdp * damcoeff. damcoeff = a * (temp - basetemp)**2
# coefficient, a, is being determined later
country_damages <- tempandgdp_countries[, damages := gdp * (temp-basetemp)^2]

# global warming effect: damages = gdp * damcoeff. damcoeff = 0.00236*(temp - basetemp)**2
# a_w = 0.00236, based on the DICE-2016R
global_damages <- tempandgdp_global[, global_damages := gdp * 0.00236 * (temp-basetemp)^2]
global_damages <- global_damages %>% select(year, rcp, model, ssp, global_damages)

# Damage = gdp_w * a_w * (T_w – T_w(0))^2 = \sum a_c * gdp_c * (T_c-T_c(0))^2
# sum country damages for each model and rcp in each year
summed_damages <- country_damages %>% group_by(year, rcp, model, ssp) %>% 
  summarize(country_damages = sum(damages, na.rm=T), gdp = sum(gdp, na.rm = T))

damage_table <- summed_damages %>% left_join(global_damages, by = c("year", "rcp", "ssp", "model"))

plot(damage_table$country_damages, damage_table$global_damages, 
     xlab = "Damages to country GDP without coefficient (billions of 2005 USD)", 
     ylab = "Damages to global GDP (billions of 2005 USD)", 
     main = "Correlation of damages: Global GDP to country GDP")
abline(lm(damage_table$global_damages ~ damage_table$country_damages, data = damage_table), 
       col = "blue")


linear_model = lm(damage_table$global_damages ~ damage_table$country_damages, data = damage_table)
anova(linear_model)

summary(linear_model)
# beta coefficient: 0.001796, intercept: -7.159

# so the relationship between the damage to GDP of the world to countries is:
# damage = gdp_w * 0.00236 * (T_w – T_w(0))^2 = \sum 0.001796 * gdp_c * (T_c-T_c(0))^2
