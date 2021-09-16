# This takes a while to run (and sometimes freezes on loading files), so after the 
# first run we save the results and load them if they exist. 

# Also, if this file fails during the first run, it is possible to recover 
# workings results instead of re-initialising all_ctemp

saveloc = "./data/cmip6/all_ctemp_calced_single.Rdata"
if(file.exists(saveloc)){
  load(saveloc)
} else{
  library(arrow)
  library(data.table)
  library(dplyr)
  library(countrycode)
  library(foreach)
  library(stringr)
  
  # Alter the start date to get earlier info
  startdate = 1980
  
  # Load popweighted country temperature increase
  files = Sys.glob(file.path("data","cmip6","20210416_*.feather"))
  
  # Either initialise the list of data or load the previous results and start 
  # from a higher fileblocki by uncommenting one of the lines below. 
  all_ctemp = list()
  #load("./data/cmip6/temp_processing_cmip6.Rdata")
  
  scenario_counter = data.frame(matrix(ncol = 2, nrow = 0))
  colnames(scenario_counter) = c("scenario", "count")
  fileblocklength = 8
  for (fileblocki in c(1:ceiling(length(files) / fileblocklength))){
    for (f in files[c((1+(fileblocki - 1) * fileblocklength) : (fileblocki*fileblocklength))]) {
      if (is.na(f)) {break}
      print(paste0("reading file ", f))
      # Load sample temp from one model [temperatures have to be adjusted to baseline]
      temp = read_feather(f, as_data_frame = TRUE)
      scens = unique(temp$scenario)
      for (scen in as.vector(scens)) {
        if (scen %in% scenario_counter$scenario) {
          scenario_counter$count[scenario_counter$scenario == scen] = 1 + as.numeric(
            scenario_counter$count[scenario_counter$scenario == scen])
        } else {
          scenario_counter[nrow(scenario_counter)+1,] = c(scen, 1)
        }
      }
      print("File catalogued")
      countrypref = 'Popn weighted World\\|Natural Earth 50m\\|'
      desiredCols = c(colnames(temp)[1:11], sapply(c(startdate:2100), toString))
      temp2 = as.data.table(temp[, desiredCols])
      rm(temp)
      ctemp = melt(temp2, id.vars=c("region", "scenario", "climate_model"),
                   measure.vars=paste(startdate:2100),
                   variable.name="year",value.name="temperature",
                   variable.factor = F)
      ctemp = aggregate(temperature~region+scenario+climate_model+year, ctemp, mean)
      print("File loaded")
      ctemp = dplyr::filter(ctemp, grepl(countrypref, region))
      ctemp$region = str_replace(ctemp$region, countrypref, "")
      
      forbidden_countries = c("Ashmore and Cartier Is.", "Fr. Polynesia", "Kosovo", "Micronesia", 
                              "S. Sudan", "Siachen Glacier", "Somaliland", "St. Vin. and Gren.", 
                              "U.S. Virgin Is.")
      ctemp = as.data.table(ctemp)
      ctemp = ctemp[!region %in% forbidden_countries,]
      ctemp = rename(ctemp, model=climate_model, Country=region)
      ctemp[, year := as.numeric(year)]
      ctemp[, rcp := str_sub(scenario, 4,-1)]
      
      record_countries = c(c("W. Sahara", "Western Sahara"))
      for (rc in record_countries){
        ctemp[Country==rc[1]] = rc[2]
      }
        
      # Check Afganistan [only for ACCESS-1]
      if(nrow(ctemp[Country=="Afghanistan"])==0){
        .temp = ctemp[Country=="Pakistan"]
        .temp$Country="Afghanistan"
        ctemp = rbind(ctemp,.temp)
      }
      
      # Check Sudan
      if(nrow(ctemp[Country=="Sudan"])==0){
        .temp = ctemp[Country=="Chad"]
        .temp$Country="Sudan"
        ctemp = rbind(ctemp,.temp)
      }
    
      # Check Serbia
      if(nrow(ctemp[Country=="Serbia"])==0){
        .temp = ctemp[Country=="Bosnia and Herzegovina"]
        .temp$Country="Serbia"
        ctemp = rbind(ctemp,.temp)
      }
      
      # Check Western Sahara
      if(nrow(ctemp[Country=="Western Sahara"])==0){
        .temp = ctemp[Country=="Mauritania"]
        .temp$Country="Western Sahara"
        ctemp = rbind(ctemp,.temp)
      }
      
      all_ctemp = c(all_ctemp,list(ctemp))
      rm(ctemp)
    }
    save.image("./data/cmip6/temp_processing_workings_cmip6.Rdata")
    print(paste0("~~~~~~~~~~~~~ Completed file block #", fileblocki, " ~~~~~~~~~~~~~~~"))
  }
  all_ctemp = rbindlist(all_ctemp)
  rm(temp,ctemp,temp2)
  # Clean the data and use Celsius
  all_ctemp = all_ctemp[!is.na(Country)]
  all_ctemp$temperature = all_ctemp$temperature - 273.15
  all_ctemp$Country = countrycode(all_ctemp$Country, "country.name", "country.name")
  all_countries <- unique(all_ctemp$Country)
  all_iso3 <- countrycode(all_countries, "country.name", "iso3c")
  
  ctemp <- merge(all_ctemp,data.table(Country=all_countries, ISO3=all_iso3),by=c("Country"))
  rm(all_ctemp)
  
  ctemp = ctemp[,list(model,rcp,ISO3,year,temp=temperature)]
  
  setkey(ctemp,rcp,ISO3,year)
  
  # Expected temperature
  etemp = ctemp[,.(temp=mean(temp)),by=c("rcp","ISO3","year")]
  
  
  # BASELINE TEMPERATURE (annual average popweighted temperature observed 1980-2010)
  
  basetemp = ctemp[year>1979 & year<2011,.(temp=mean(temp)),by=c("ISO3")]
  basetemp = basetemp[,list(ISO3,basetemp=temp)]
  save.image(saveloc)
}
