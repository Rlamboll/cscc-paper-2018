library(ggplot2)
library(stringr)
library(docopt)
library(data.table)
library(tidyr)
library(dplyr)


# This file plots results from the generate_cscc.R file
# The outputs can be either "eri_eq_statscc_2020d" (GSCC relative to the income of an Eritrean),
# or "poor_pref_10dollars" (income at which donation is preferable to SCC) 

# Input values in options should correspond with files in the results folder for -e, -r and -f.

' Input values to be plotted. Use values also used to generate files in the generate_cscc.r results folder

usage: plot_equality_data.R [-e <eta> -v <ver> -t <type> -r <rcp> -s <ssp> -p <pro> -c <true> -f <dmg> -m <cmip>]

options:
 -e Eta value used in the model (1, 2 or 1:2 (default))
 -v Version number to name the plotted figure
 -t Type of string and plot (either poor_pref_10dollars (default) or eri_eq_statscc_2020d)
 -r Plot rcp scenario (4.5, 6.0, 8.5 or all (default))
 -s SSP baseline (all(default), 1, 2,..., 5. ))
 -p Projection type. (constant (default), horizon2100, all)
 -c Check in order to do a test
 -f Damage functions (default=bhm (Burke et al.), djo (Dell et al.), dice, nice 
    (if additional data has been imported). Separate these options by commas)
 -m cmip version. (cmip5 (default), cmip6, cmip6_all, both)' -> my_doc

#my_opts <- docopt(my_doc, "-e 1 -v v4 -t poor_pref_10dollars -r 6.0,4.5,8.5 -f bhm") # Default case
#my_opts <- docopt(my_doc, "-e 1,2 -t eri_eq_statscc_2020d -v v7 -r all -s all -f djo,bhm,nice -m cmip6_all") 
my_opts <- docopt(my_doc, "-e 1 -t neg_growth_ -v v8 -r all -s all -f bhm -m cmip6") 
#my_opts <- docopt(my_doc, "-e 1 -t poor_pref_10dollars -r 8.5 -s 3 -f dice,bhm,dice,nice") 
#my_opts <- docopt(my_doc, "-e 1,2 -t poor_pref_10dollars -r all -s all -f bhm,djo,dice") 
#my_opts <- docopt(my_doc, "-e 1,2 -t eri_eq_statscc_2020d -r all -s all -f bhm,djo,dice")
#my_opts <- docopt(my_doc)

# unpack variables from the options
if (is.null(my_opts[["e"]]) | my_opts[["e"]] == "1,2"){ # default RRA is 1 and 2
  variable_risk = c(1,2)
} else if (my_opts[["e"]] == 1) {
  variable_risk = c(1)
} else if (my_opts[["e"]]==2){
  variable_risk = c(2)
} else if (my_opts[["e"]]=="0,1,2"){
  variable_risk = c(0, 1, 2)
} else if (my_opts[["e"]]=="0"){
  variable_risk = c(0)
}

if (is.null(my_opts[["v"]])) {
  version_string = "" # no version number if there is no input for -v
} else{ version_string = as.character(my_opts[["v"]])
}
all_cmip6_scen = FALSE
if (is.null(my_opts[["m"]])) {
  cmips=c("")
} else if (my_opts[["m"]] == "cmip6_all") {
  cmips = c("_cmip6")
  all_cmip6_scen = TRUE
} else if (my_opts[["m"]] == "cmip6") {
  cmips=c("_cmip6")
} else if (my_opts[["m"]] == "both") {
  cmips=c("", "_cmip6")
} else {
  stop("Invalid cmip choice")
}

if (is.null(my_opts[["t"]])){
  type_str = "poor_pref_10dollars" #default
} else{type_str = as.character(my_opts[["t"]])}  

variable_rcp = c()
if (grepl(4.5, my_opts[["r"]])){
  variable_rcp <- append(variable_rcp, 45)
} 
if (grepl(6.0, my_opts[["r"]])){
  variable_rcp <- append(variable_rcp, 60)
} 
if (grepl(8.5, my_opts[["r"]])){
  variable_rcp <- append(variable_rcp, 85)
} 
# Currently  we default to not includine 2.6 or 7.0
if (length(variable_rcp) == 0){variable_rcp = c(19, 34, 45, 60, 85)} 

if (my_opts[["s"]] == "all" || is.null(my_opts[["s"]])) {
  ssp_plot = c(1:5) # SSP{1,2,3,4,5
} else {ssp_plot = c(as.character(my_opts[["s"]]))}

if ((is.null(my_opts[["p"]]) || my_opts[["p"]]== "all")){
  variable_timeframe = c("constant", "horizon2100")
} else if (my_opts[["p"]]== "horizon2100"){
  variable_timeframe = c("horizon2100")
} else {variable_timeframe = c("constant")}  
  
if (!is.null(my_opts[["c"]])){
  test = TRUE
  results_dir = "/results_test"
} else {
  test = FALSE
  results_dir = "/results"
  }

if (is.null(my_opts[["f"]])) {
  dmg_f = "bhm"     # bhm is default damage function
} else {
  dmg_f = as.character(my_opts["f"])
}

# Conversion factor
dollar_val_2020 = 1.35
  
subfolder = if (all_cmip6_scen)""
  
namefun <- function(x, y, z, timeframe, cmip, subfolder){
  paste0(results_dir, "/res_statbhm_30C/", subfolder, type_str, "SSP", x, "_rcp", y, 
         "_", timeframe, "_estimates_climensemble_", "eta_", z, cmip, ".RData") 
}
namefun_altdamage <- function(x, y, z, cmip, subfolder){
    paste0(results_dir, "/res_statdjo_richpoor/", subfolder, type_str, "SSP", x, "_rcp", y,
         "_constant_estimates_climensemble_djo_", "eta_", z, cmip, ".RData")
}
namefun_nocut <- function(x, y, z, timeframe, cmip, subfolder){
  paste0(results_dir, "/res_statbhm/", subfolder, type_str, "SSP", x, "_rcp", y, 
         "_", timeframe, "_estimates_climensemble_", "eta_", z, cmip, ".RData") 
}
namefun_dice <- function(x, y, z, cmip, subfolder){
  paste0(results_dir, "/res_statdice/", subfolder, type_str, "SSP", x, "_rcp", y, 
        "_constant_estimates_climensemble_dice_", "eta_", z, cmip, ".RData") 
}
filelist=c()

if (all_cmip6_scen){
  subfolder="allcombs_cmip6/"
} else {
  subfolder=""
}
for (cmip in cmips){
  if (grepl("djo", dmg_f)){
    for (y in variable_rcp){
      for (z in variable_risk){
        filelist2 = lapply(ssp_plot, namefun_altdamage, y=y, z=z, cmip=cmip, subfolder=subfolder)
        filelist=c(filelist, filelist2)
      }
    }
  }
  
  if (grepl("bhm", dmg_f)){
    dir_30C <- str_sub(results_dir, 2, str_length(results_dir))
    if (dir.exists(paste0(dir_30C, "/res_statbhm_30C"))){
      for (y in variable_rcp){
        for (z in variable_risk){
          for (timeframe in variable_timeframe){
            filelist2 = lapply(ssp_plot, namefun, y=y, z=z, timeframe=timeframe, cmip=cmip, subfolder=subfolder) 
            filelist=c(filelist, filelist2)
          }
        }
      }
    } 
    for (y in variable_rcp){
      for (z in variable_risk){
        for (timeframe in variable_timeframe){
          filelist2 = lapply(ssp_plot, namefun_nocut, y=y, z=z, timeframe=timeframe, cmip=cmip, subfolder=subfolder) 
          filelist=c(filelist, filelist2)
        }
      }
    }
  }
  
  if (grepl("dice", dmg_f)){
    for (y in variable_rcp){
      for (z in variable_risk){
        filelist2 = lapply(ssp_plot, namefun_dice, y=y, z=z, cmip=cmip, subfolder=subfolder)
        filelist=c(filelist, filelist2)
      }
    }
  }  
}

results_table <- data.table(cmip=character(), ssp=integer(), rcp=numeric(), eta=numeric(), PRTP=numeric(), 
                            damages=character(), indicator=character(), value=numeric())

origin = getwd()
compare_results = results_table
for (file in filelist) {
  possible_fail <- tryCatch(load(paste0(origin, file)), silent=TRUE, error=function(e){return("File not found")})
  if (possible_fail=="File not found") next
  
  sspnum = as.numeric(substr(strsplit(file, split = "SSP")[[1]][2], 1, 1))
  rcpnum = substr(strsplit(file, split = "rcp")[[1]][2], 1, 2)
  rcpnum = sub("(.{1})(.*)", "\\1.\\2", rcpnum)
  damages = if(grepl("djo", file)) "Dell" else if(grepl("dice", file)) "Dice" else (
    if (grepl("horizon2100", file)) "Burke 2100" else "Burke 2200")
  if (grepl("_30C", file)){
    damages = str_c(damages, " 30C")
  }
  cmip = if(grepl("cmip6", file)) "cmip6" else "cmip5"
  if (type_str == "neg_growth_") {
    use_table =  rename(neg_growth, indicator=ISO3, value=neg_prop)
    use_table["cmip"] = cmip
    use_table["ssp"] = sspnum
    use_table["rcp"] = rcpnum
    use_table["eta"] = NA
    use_table["PRTP"] = NA
    use_table["damages"] = damages
    use_table <- use_table[, colnames(results_table) ]
    results_table <- rbind(results_table, use_table)
  } else {
    columns_to_save = c("mean")
    for (column in columns_to_save){
      if(type_str == "eri_eq_statscc_2020d"){
        use_table = eri_eq_stat_wscc
        # We can also compare the impact of inequality aversion
        compare_file = str_replace(file, "eri_eq_statscc_2020d", "statscc_")
        load(paste0(origin, compare_file))
        compare_table = stat_scc
      } else if (type_str == "poor_pref_10dollars") {
        use_table = poor_prefer_10
      } else {
        stop("Error: invalid combination of options")
      }
      rows_to_save = lapply(use_table$ID, strsplit, split="_")
      rowind = 0
      for (row in rows_to_save){
        rowind = rowind + 1
        if (row[[1]][1] %in% c("NA", "3") ) next
        PRTPnum = row[[1]][1]
        etanum = row[[1]][2]
        results_table <- rbind(results_table, list(
          cmip, sspnum, rcpnum, etanum, PRTPnum, damages, column, use_table[[rowind, column]])
        )
        if(type_str == "eri_eq_statscc_2020d"){
          compare_results <- rbind(
            compare_results, list(cmip, sspnum, rcpnum, etanum, PRTPnum, damages, column, compare_table[[
              which(compare_table$ID == paste(PRTPnum, etanum, "NA", "WLD", sep="_")), column
            ]])
          )
        }
      }
    }
  }
}
if (type_str == "poor_pref_10dollars"){
  compare_results$value = compare_results$value * dollar_val_2020  
}
if (grepl("nice", dmg_f)){
  files =  c(
    "./data/nice_imports/interpolated_scc_scenarios_v5_2025.csv", 
    "./data/nice_imports/interpolated_scc_scenarios_v5_2025_incomeElast0.csv"
  )
  nicenames = c("NICE linear", "NICE constant")
  for (fileind in c(1:2)  ) {
    nice_data <- read.csv(files[fileind])
    nice_data$ssp = substr(nice_data$Scenario, 4, 4)
    nice_data$rcp = sapply(nice_data$Scenario, function(x) strsplit(x, "-")[[1]][2])
    nice_data$rcp[nice_data$rcp=="Baseline"] = "85"
    # Put decimal point in
    nice_data$rcp = sapply(nice_data$rcp, function(x) {paste0(substr(x, 1, 1), ".", substr(x, 2,3))})
    nice_data_poor = nice_data[nice_data$whose=="poorest", ]
    len_nice = nrow(nice_data_poor)
    # Mute warnings for constructing a table mostly of NAs. 
    options(warn = -1)
    nice_results_table <- data.table(
      cmip=character(), ssp=integer(), rcp=numeric(), eta=numeric(), PRTP=numeric(), damages=rep(nicenames[fileind], len_nice), 
      indicator=rep("mean", len_nice), value=numeric())
    options(warn=0)
    nice_results_table$cmip = "RICE"
    nice_results_table$ssp = nice_data_poor$ssp
    nice_results_table$rcp = nice_data_poor$rcp
    nice_results_table$PRTP = as.integer(nice_data_poor$prtp * 100)
    nice_results_table$eta = nice_data_poor$eta
    # We will start with the scc and transform it into the value we want
    nice_results_table$value = nice_data_poor$scc
    if (type_str == "poor_pref_10dollars") {
      # The post-abatement cost, post-damages income of the poorest group in the NICE model in 2025.
      poorest_nice = 0.34 * 1e3 
      nice_results_table$value = poorest_nice * (nice_results_table$value / 10)**(-1/nice_results_table$eta)
      results_table = rbind(results_table, nice_results_table)
    } else {
      results_table = rbind(results_table, nice_results_table)
    }
  }
}

if (type_str == "neg_growth_"){
  results_sum = aggregate(results_table[, "value"], list(results_table$indicator), mean)
  scen_len = length(results_table[rcp==1.9])
  for (rcp in unique(results_table[,"rcp"])){
    stopifnot(length(results_table[rcp==rcp]) == scen_len)
  }
  library("rnaturalearth")
  library("rnaturalearthdata")
  world <- ne_countries(scale = "large", returnclass = "sf")
  world <- merge(world, results_sum, by.x="adm0_a3", by.y="Group.1", all=TRUE)
  target_crs <- '+proj=eqearth +wktext'
  theme_set(theme_bw())
  ggplot(data = world) + geom_sf(aes(fill=value)) + coord_sf(crs = target_crs) +
    scale_fill_viridis_c(option = "inferno",  name = "Negative growth fraction") + 
    theme(legend.title = element_text(size = 12, angle = 90), legend.title.align = 0.5)
  
  savediffig = paste0(type_str, damages, version_string, cmip, ".png")
  ggsave(path="plots", filename=savediffig) 
  
  
} else {
  
  if(type_str == "eri_eq_statscc_2020d"){
    plot_labs = labs(
      title="World SCC relative to the average Eritrean ($)",
      fill="Inequality aversion"
    ) 
    plot_ylab = ylab("World SCC relative to the average Eritrean ($)") 
  } else if(type_str == "poor_pref_10dollars"){
    plot_labs = labs(
      title="Income at which $10 cash is preferable to a ton CO2",
      fill="Inequality aversion"
    )
    plot_ylab = ylab("Yearly income ($)") 
  }
  
  set.seed(10)
  explain_eta = function(et){paste0("RRA: ", et)}
  explain_prtp = function(pr){paste0("PRTP: ", pr)}
  label_axes = labeller(
    eta=explain_eta, PRTP=explain_prtp
  )
  results_table$SSP <- factor(results_table$ssp)
  plot = ggplot(results_table, aes(x=rcp, y=value, color=SSP))+geom_point(
    alpha=0.9, size=2
  ) + plot_labs + plot_ylab + xlab("RCP pathway") + facet_grid(PRTP+eta~damages , labeller = label_axes
  )
  if (type_str != "eri_eq_statscc_2020d"){
    plot = plot + geom_hline(yintercept=1.9*365)
    plot = plot + geom_hline(yintercept=500, color="red")
  } else if (type_str == "poor_pref_10dollars") {
    plot = plot + geom_hline(yintercept=10)
  }
  plot = plot + scale_y_continuous(trans = "log2")
  
  plot
  
  subDir = "plots"
  if (!dir.exists(file.path(subDir))){
    dir.create(file.path(subDir))
  }
  
  savefig = paste0(type_str, version_string, ".png")
  ggsave(path="plots", filename=savefig)
  print(paste0("Saved ", savefig))
  
  if (type_str == "eri_eq_statscc_2020d" & !(grepl("nice", dmg_f))){
    combined_data = results_table
    combined_data$ratio = compare_results$value / results_table$value
    combined_data$no_ineq = compare_results$value
    plotdif = ggplot(combined_data, aes(x=value, y=no_ineq, color=SSP, shape=rcp)) + geom_point(
      alpha=0.9, size=3
    ) + facet_grid(PRTP+eta~damages, labeller = label_axes) + 
      scale_y_continuous(trans = "log2") + scale_x_continuous(trans = "log2") + ylab(
        "Summed country cost of carbon ($)") + xlab("World SCC to average Eritrean ($)")
    plotdif
    savediffig = paste0(type_str, "InequalityAlteration", version_string, cmip, ".png")
    ggsave(path="plots", filename=savediffig)
    print(paste0("Saved ", savediffig))
  }
  
  if (type_str == "eri_eq_statscc_2020d" & (length(cmips) == 2)) {
    double_data = aggregate(results_table, by=list(results_table$ssp, results_table$rcp, 
      results_table$eta, results_table$PRTP, results_table$damages, results_table$indicator, 
      results_table$SSP), FUN=length)
    double_data = double_data[double_data$cmip==2,]
    doublecmipdata = results_table[FALSE,]
    for (row_i in c(1:nrow(double_data))){
      doublecmipdata = rbind(doublecmipdata, results_table[
        results_table$ssp == double_data$Group.1[row_i] & 
        results_table$rcp == double_data$Group.2[row_i] &
        results_table$eta == double_data$Group.3[row_i] & 
        results_table$PRTP == double_data$Group.4[row_i] &
        results_table$damages == double_data$Group.5[row_i] & 
        results_table$indicator == double_data$Group.6[row_i] &
        results_table$SSP == double_data$Group.7[row_i],])
    }
    doublecmipdataw = pivot_wider(doublecmipdata, names_from=cmip, values_from=value)
    plotdif = ggplot(doublecmipdataw, aes(x=cmip5, y=cmip6)) + geom_point() + 
      geom_abline(slope=1) + xlab("CMIP5 World SCC relative to the average Eritrean (2020 USD") + scale_x_continuous(trans = "log2") +
      ylab("CMIP6 World SCC relative to the average Eritrean (2020 USD") + scale_y_continuous(trans = "log2") + 
      theme(text = element_text(size=15))
    plotdif
    savediffig = paste0(type_str, "Plots", version_string, cmip, ".png")
    ggsave(path="plots", filename=savediffig)
  }
}
