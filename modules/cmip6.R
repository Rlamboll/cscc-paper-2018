library(data.table)

temp_countries <- fread(file.path('data/cmip6/CMIP6_temp_countries.csv'))
# separate ssp from rcp
temp_countries$ssp = substring(temp_countries$rcp, 1,1)
temp_countries$rcp = substring(temp_countries$rcp,2)
temp_countries <- temp_countries[year < 2101]

# basetemps per model and scenario for country dataset
basetemp_countries <- temp_countries[year == 1900]
basetemp_countries <- select(basetemp_countries, model, ssp, rcp, ISO3, temp)
basetemp_countries <- basetemp_countries %>% rename(basetemp = temp)
basetemp_countries <- basetemp_countries %>% distinct(model, rcp, ssp, ISO3, .keep_all = TRUE)

ctemp <-  temp_countries %>% distinct(model, rcp, ssp, year, ISO3, 
         .keep_all = TRUE)

ctemp$ssp <- sub("^","SSP", ctemp$ssp)
ctemp$rcp <- sub("^","rcp", ctemp$rcp)
names(ctemp)[names(ctemp) == 'ssp'] <- 'SSP'

basetemp_countries$ssp <- sub("^","SSP", basetemp_countries$ssp)
basetemp_countries$rcp <- sub("^","rcp", basetemp_countries$rcp)
names(basetemp_countries)[names(basetemp_countries) == 'ssp'] <- 'SSP'
