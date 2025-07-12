##among other things , here, Fulda_daily_temp_joh_long is produced. This file thus "only" shows the production of that file, but is not part of the model itself

library(ggplot2)
library(dplyr)
library(DBI)#dbConnect
library(openxlsx)#read.xlsx
library(DescTools)#year
library(patchwork) #for sticking together different plots 


rm(list = ls())

#define your own local path where to save plots to
  gw_FuldaEcosystemServices_plots_path <-""

  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/produkt_tu_termin_18850101_20231231_01526.txt"
  Fulda_daily_temp <- read.table(urlfiletext,sep = ";", header = TRUE)  
  
  Fulda_daily_temp$dateRi  <- as.Date(as.character(substr(Fulda_daily_temp$MESS_DATUM, 1, 8)), format = c("%Y%m%d") )
  
  Fulda_daily_temp_ <- Fulda_daily_temp %>%
    dplyr::filter(TT_TER > -999)%>%
    # three values per day are given, morning mid day evening;  take the average here because for groundwater temperature, the mean is more decisive than minima and maxima
    dplyr::mutate(dateRi = substr(MESS_DATUM, 1, 8)) %>%
    dplyr::group_by(dateRi)%>%
    dplyr::summarise(TT_TER = mean(TT_TER, na.rm = TRUE))
  
  Fulda_daily_temp_$dateRi <- as.Date(Fulda_daily_temp_$dateRi, format = "%Y%m%d")
  
  
# urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_concrete_rates_with_data_from_Marxsen_et_al_2021_Fulda_fp.R"
# source(urlfiletext)  ##fulda_variables, including tempreature and preciptaiotn 


# urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_concrete_rates_with_data_from_Marxsen_et_al_2021_parameter_variables_fp.R"
#   source(urlfiletext)
  
########
  #functions reactions

#   urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_functions_fp.R"
# source(urlfiletext)

# urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/parameters_variables_fp.xlsx"
#  parvar <- read.xlsx(urlfiletext, startRow = 3, sheet = 1)
  
 ##########
#reading in Fulda data from file
##########


#temperature_scenario <- parvar$temperature_scenario[run]


# # ##########
# # #which run in the excel file
# # ##########
# # #needs to be read in here already, for the two variables regarding waht scenario - otherwise, read in in model_concrete_rates_with_data_from_Marxsen_et_al_2021_parameter_variables.R. But the parvar run .. Faden
# 
# run <- 1 # use one of the run here where no temperature increase, jus tto be able to use fulda_variables()
# 
# ##########
# #reading in Fulda data  from file
# ##########
# 
# scenario_with_1_or_without_0_MO <-  parvar$scenario_with_1_or_without_0_MO[run]
# scenario_with_1_or_without_0_fauna <- parvar$scenario_with_1_or_without_0_fauna[run]# use one of the run here where no temperature increase, jus tto be able to use fulda_variables()
# #this depends on the scenario variables
# 
# 
# factor_CC_MO <-  parvar$factor_CC_MO[run]
# factor_CC_fauna <-  parvar$factor_CC_fauna[run]
# # use one of the run here where no temperature increase, jus tto be able to use fulda_variables()
# #this depends on the scenario variables
# 
# fulda_variables_read_in <- fulda_variables(run, factor_CC_MO, factor_CC_fauna)
# names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_w_dat_ordered_per_date_1978_1981", "chem_w_dat_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "COD_gr1_t0", "COD_gr2_t0", "COD_gr3_t0", "COD_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0", "fauna_deep_PerSamplPerTaxon_bm_sum_per_P", "CC_table_MO", "CC_table_fauna") 
# 
# list2env(fulda_variables_read_in, globalenv())

  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/kmeans_chem4centers_exch_8dec_seed7_win_on_mac_2412.txt"
  
  kmeans_chem4_exchgroups <-read.table(urlfiletext, sep = " ", header = TRUE)#
  
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/chem_w_date.txt"
  chem_w_date <- read.table(urlfiletext, sep = " ", header = TRUE)
  
  chem_w_dat_ordered_per_date <- chem_w_date[order(chem_w_date$Date) , ]
  chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date[DescTools::Year(chem_w_dat_ordered_per_date$Date)>1977 & DescTools::Year(chem_w_dat_ordered_per_date$Date)<1982,]
  chem_w_dat_ordered_per_date_1978_1981$Date <- as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)
  
  chem_w_dat_ordered_per_date_1978_1981$kmeans4gr <- kmeans_chem4_exchgroups$V2[match(chem_w_dat_ordered_per_date_1978_1981$P, kmeans_chem4_exchgroups$V1)]
  
  chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(kmeans4gr))
  
chem_w_dat_ordered_per_date_1978_1981$group <- chem_w_dat_ordered_per_date_1978_1981$kmeans4gr

chem_w_dat_ordered_per_date_1978_1981$group_letter <- ifelse(chem_w_dat_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_w_dat_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 1, "M", NA))))   #

chem_w_dat_ordered_per_date_1978_1981$group_letter <- factor(chem_w_dat_ordered_per_date_1978_1981$group_letter , levels = c("R", "M", "P", "A"))




##########
#parameters
##########

#read parameters and variables  from file

# parameter_variables_read_in <- parameter_variables( run)
# names(parameter_variables_read_in) <- c("dt", "aquifer_depth", "import_MO_het", "scenario_with_1_or_without_0_fauna", "scenario_with_1_or_without_0_MO", "carboxylic_acids_fraction", "acetic_acids_fraction", "mortalityRate", "import_fauna", "yield_ac", "yield_MO", "K_MO_at_temp", "rMO_COD_uptake_per_day_at_lab_temperature", "rFauna_MO_uptake_per_day_at_TEMP", "k1", "excretionRate", "TOC_COD_mol_m2_yr_precipitation", "RECHARGE_COD_mol_per_m3_per_day_df", "lab_temp", "K_ac", "growth_model_MO_type", "growth_model_fauna_type", "mortalityRate_per_degree") 
# 
# list2env(parameter_variables_read_in, globalenv())


##########
#creating temperature scenarios
##########

#temperature_scenario <- parvar$temperature_scenario[run]

Fulda_daily_temp_$dateRi <- as.Date(Fulda_daily_temp_$dateRi, format = c("%Y%m%d"))
Fulda_daily_temp_joh <- Fulda_daily_temp_ %>%
  dplyr::filter(lubridate::year(dateRi) > 1977#, lubridate::year(dateRi) < 1982
  )

#11.5.25 because of the mutates and group ?? I need to reformat date:
Fulda_daily_temp_joh$dateRi <- as.Date(Fulda_daily_temp_joh$dateRi, format = c("%Y%m%d"))

#Fulda_daily_temp_joh$TT_TER <- Fulda_daily_temp_joh$TT_TER + temperature_scenario
Fulda_daily_temp_johave <- mean(Fulda_daily_temp_joh$TT_TER, na.rm =TRUE)
Fulda_daily_temp_johmin <-  min(Fulda_daily_temp_joh$TT_TER, na.rm =TRUE)
Fulda_daily_temp_johmax <- max(Fulda_daily_temp_joh$TT_TER, na.rm =TRUE)


Fulda_temp_amplitude <- Fulda_daily_temp_johmax - Fulda_daily_temp_johmin

#estimating groundwater temperature from mean, max and min in gw
groupA_gw_temp <- chem_w_dat_ordered_per_date_1978_1981$Temp[chem_w_dat_ordered_per_date_1978_1981$group_letter =="A"]
groupA_gw_temp_ave <- mean(groupA_gw_temp, na.rm =TRUE)
groupA_gw_temp_min <-  min(groupA_gw_temp, na.rm =TRUE)
groupA_gw_temp_max <- max(groupA_gw_temp, na.rm =TRUE)
groupA_gw_temp_amplitude <- groupA_gw_temp_max - groupA_gw_temp_min
groupA_gw_temp_amplitude_factor_to_Kassel <- groupA_gw_temp_amplitude/Fulda_temp_amplitude


Fulda_daily_temp_joh$gw_temp_groupA <- groupA_gw_temp_ave + (Fulda_daily_temp_joh$TT_TER - Fulda_daily_temp_johave)*groupA_gw_temp_amplitude_factor_to_Kassel


groupR_gw_temp <- chem_w_dat_ordered_per_date_1978_1981$Temp[chem_w_dat_ordered_per_date_1978_1981$group_letter =="R"]
groupR_gw_temp_ave <- mean(groupR_gw_temp, na.rm =TRUE)
groupR_gw_temp_min <-  min(groupR_gw_temp, na.rm =TRUE)
groupR_gw_temp_max <- max(groupR_gw_temp, na.rm =TRUE)
groupR_gw_temp_amplitude <- groupR_gw_temp_max - groupR_gw_temp_min
groupR_gw_temp_amplitude_factor_to_Kassel <- groupR_gw_temp_amplitude/Fulda_temp_amplitude
Fulda_daily_temp_joh$gw_temp_groupR <- groupR_gw_temp_ave + (Fulda_daily_temp_joh$TT_TER - Fulda_daily_temp_johave)*groupR_gw_temp_amplitude_factor_to_Kassel

groupP_gw_temp <- chem_w_dat_ordered_per_date_1978_1981$Temp[chem_w_dat_ordered_per_date_1978_1981$group_letter =="P"]
groupP_gw_temp_ave <- mean(groupP_gw_temp, na.rm =TRUE)
groupP_gw_temp_min <-  min(groupP_gw_temp, na.rm =TRUE)
groupP_gw_temp_max <- max(groupP_gw_temp, na.rm =TRUE)
groupP_gw_temp_amplitude <- groupP_gw_temp_max - groupP_gw_temp_min
groupP_gw_temp_amplitude_factor_to_Kassel <- groupP_gw_temp_amplitude/Fulda_temp_amplitude
Fulda_daily_temp_joh$gw_temp_groupP <- groupP_gw_temp_ave + (Fulda_daily_temp_joh$TT_TER - Fulda_daily_temp_johave)*groupP_gw_temp_amplitude_factor_to_Kassel

groupM_gw_temp <- chem_w_dat_ordered_per_date_1978_1981$Temp[chem_w_dat_ordered_per_date_1978_1981$group_letter =="M"]
groupM_gw_temp_ave <- mean(groupM_gw_temp, na.rm =TRUE)
groupM_gw_temp_min <-  min(groupM_gw_temp, na.rm =TRUE)
groupM_gw_temp_max <- max(groupM_gw_temp, na.rm =TRUE)
groupM_gw_temp_amplitude <- groupM_gw_temp_max - groupM_gw_temp_min
groupM_gw_temp_amplitude_factor_to_Kassel <- groupM_gw_temp_amplitude/Fulda_temp_amplitude
Fulda_daily_temp_joh$gw_temp_groupM <- groupM_gw_temp_ave + (Fulda_daily_temp_joh$TT_TER - Fulda_daily_temp_johave)*groupM_gw_temp_amplitude_factor_to_Kassel


Fulda_daily_temp_joh_long <- Fulda_daily_temp_joh %>%
  tidyr::pivot_longer(cols = c( gw_temp_groupA, gw_temp_groupP, gw_temp_groupR, gw_temp_groupM), names_to = "group_letter")

Fulda_daily_temp_joh_long$group_letter <- sub("gw_temp_group", "", Fulda_daily_temp_joh_long$group_letter)

chem_w_dat_ordered_per_date_1978_1981$group_letter <- ifelse(chem_w_dat_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_w_dat_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 1, "M", NA))))

chem_w_dat_ordered_per_date_1978_1981$group_letter <- factor(chem_w_dat_ordered_per_date_1978_1981$group_letter, levels = c("R", "M", "P", "A"))
Fulda_daily_temp_joh_long$group_letter <- factor(Fulda_daily_temp_joh_long$group_letter, levels = c("R", "M", "P", "A"))

chem_w_dat_ordered_per_date_1978_1981$size <- 1
#all the same sizes for the four groups, in order to plot a legend
Fulda_daily_temp_joh_long$size <- as.numeric( ifelse(Fulda_daily_temp_joh_long$group_letter ==  "P", 1.1, ifelse(Fulda_daily_temp_joh_long$group_letter == "R" , 1.1, ifelse (Fulda_daily_temp_joh_long$group_letter == "A", 1.1, ifelse (Fulda_daily_temp_joh_long$group_letter == "M", 1.1, NA)))))

#the plot only makes sense if the max date is as outcommented
(plot_Temperature_air_and_groundwater_estimated_and_measured_groups <- ggplot()+
    geom_line( data = Fulda_daily_temp_joh_long, aes(x = dateRi , y = TT_TER, group = group_letter, fill = "label1", col  = "black")    )+
    geom_line( data = Fulda_daily_temp_joh_long, aes(x = dateRi , y = value, group = group_letter, fill = "label2", col  = "red")
    )+
    geom_point(data = chem_w_dat_ordered_per_date_1978_1981, aes(x = Date, y = Temp, group = group_letter, fill = "in situ", 
 col = factor(size)), col = "blue"
    )+
    scale_x_date(limits = c(as.Date("1978-01-01") , as.Date("1982-01-01")))+ 
    #https://stackoverflow.com/questions/47865121/create-ggplot2-legend-for-multiple-datasets
    scale_color_discrete(name = "daily" , type = c("black", "red"), label = c("air", "extrapolated groundwater")) +
   scale_fill_manual(name = "Temperature °C, \nin situ", values = c("in situ" = "grey")) +
labs(x = "Date", y = "Daily average temperature [°C]", title = "Temperature in situ and extrapolation")+
  theme(panel.background = element_rect(fill = "white",  colour = "black", #size = 0.5, 
                                         linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(), 
        legend.background=element_blank(),
        
        axis.text.x = element_text(angle = 45, vjust = 0.4))+
  facet_grid( . ~ group_letter)
)
  
setwd(gw_FuldaEcosystemServices_plots_path )
ggsave ("Temperature_air_and_groundwater_estimated_and_measured_groups_with_Fulda_not_Kassel.png", width = 8, height = 3#, units = "px", pointsize = 12, res = 200
)

#for further work it is advantageous to have the numeric groups as well
Fulda_daily_temp_joh_long$group <-  as.numeric( ifelse(Fulda_daily_temp_joh_long$group_letter ==  "P", 2, ifelse(Fulda_daily_temp_joh_long$group_letter == "R" , 3, ifelse (Fulda_daily_temp_joh_long$group_letter == "A", 4, ifelse (Fulda_daily_temp_joh_long$group_letter == "M", 1, NA)))))


setwd(gw_FuldaEcosystemServices_plots_path)
write.table(Fulda_daily_temp_joh_long, "Fulda_daily_temp_joh_long.txt", row.names = FALSE)
