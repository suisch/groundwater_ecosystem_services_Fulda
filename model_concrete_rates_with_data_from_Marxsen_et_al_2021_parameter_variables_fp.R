
# function which returns ca. 30 variables 

parameter_variables<- function(run){
  
  library(dplyr)
  library(tidyr)
  library(seas)#mkseas
  library(DBI)#dbConnect
  library(openxlsx)#read.xlsx
  library(DescTools) ##year
  library(patchwork) #for sticking together different plots 
  
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/parameters_variables.xlsx"
  parvar <- read.xlsx(urlfiletext, startRow = 3)
  
  delta_t          = parvar$dt[run] #days
  
  max_t          = as.Date(parvar$max_t[run], origin = "1899-12-30") #date - depends on operating system!! 
  
  #depth of gw aquifer
  aquifer_depth <- parvar$aquifer_depth[run] #m
  
  #dummy; for further developments
  import_MO_het <- parvar$import_MO_het[run] #
  
  scenario_with_1_or_without_0_fauna <- parvar$scenario_with_1_or_without_0_fauna[run]  #1 for with fauna, or 0 for without fauna 
  
  #dummy; for further developments
  carboxylic_acids_fraction <- parvar$carboxylic_acids_fraction[run]  # = .08 # Thurman 1985 Fig. 4.1
  
  #dummy; for further developments
  acetic_acids_fraction <- parvar$acetic_acids_fraction[run]  # 
  
  mortalityRate <- parvar$mortalityRate[run]  # 
  
  import_fauna <- parvar$import_fauna[run] 
  
  # taking the most competitive acetate degrader as of Schmidt et al. 2018
  #maximal specific growth rate 0.43 h-1, half- saturation constant of 4.3 microM [19]
  yield_ac <- parvar$yield_ac[run] # gdrymass_MO  gacetate^-1 Gerritse et al. (1992)
  
  yield_MO <- parvar$yield_MO[run] 
  
  k_temp <- parvar$k_temp[run]   #k_temp <- 30 # temperature in Gerritse et al. (1992)
  
  K_ac <- parvar$K_ac[run]
  
  K_MO_at_temp <- parvar$K_MO_at_temp[run]
  
  #microbe maximum growth rate 	Rate constant for bacterial uptake and oxidation of acetate	Gerritse et al. (1992), ASSUMING COD = BOC CAN BE DEGRADED LIKE ACETATE, which is wrong !!
  rMO_BOC_uptake_h_at_lab_temperature <- parvar$rMO_BOC_uptake_h_at_lab_temperature[run]
  
  # per day # #micro ac 	Rate constant for bacterial uptake and oxidation of acetate	4.7E-05	s-1	Gerritse et al. (1992) ****
  #that's at 30 degrees;  in Schmidt et al. (2018): Comamonas testosterone  specific growth rate back calculated to  10degC (4.7 * 10-5 s-1)
  rMO_BOC_uptake_per_day_at_lab_temperature <- parvar$rMO_COD_uptake_per_day_at_lab_temperature[run]
  
  #4.1.25 groundwater fauna does not survive at 30 degrees - see paper and SI. Therefore, take the maximum growth rate rate given and assume that it is valid within the narrow band of temperatures in gw
  rFauna_MO_uptake_per_day_at_TEMP <- parvar$rFauna_MO_uptake_per_day_at_TEMP[run]
  
  
  ##########
  #fractions
  ##########
  #fraction of DETRITUS  becoming COD  = BOC -  assumption: independent of temperature
  
  k1 <- parvar$k1[run]
  
  #excretionRate = 0.01  # /day # from Soetaert & Herman 2008 
  excretionRate <- parvar$excretionRate[run]
  
  
  ##########
  #rates
  ##########
  
  TOC_COD_mol_m2_yr_precipitation <- parvar$TOC_COD_mol_m2_yr_precipitation[run] #
  
  average_precipitation_mm_yr <- parvar$average_precipitation_mm_yr[run]
  average_precipitation_mm_yr <- as.numeric(average_precipitation_mm_yr)
  
  average_precipitation_m_yr<- average_precipitation_mm_yr/1000
  
  TOC_COD_mol_m3_precipitation <- parvar$TOC_COD_mol_m3_precipitation[run]
  
  
  #DETRITUS recharge in dependence on precipitation
  #  x times as much detritus mobilized as TOC from preciptation
  factor_how_many_times_Detritus_compared_to_TOC <- parvar$factor_how_many_times_Detritus_compared_to_TOC[run]
  
  Detritus_COD_mol_m3_precipitation = parvar$Detritus_COD_mol_m3_precipitation[run]
  
  Detritus_COD_mol_L_precipitation = parvar$Detritus_COD_mol_L_precipitation[run]
  
  RECHARGE_COD_mol_per_m2_per_day        = ifelse(Fulda_daily_prec$RS >0, (TOC_COD_mol_m3_precipitation + Detritus_COD_mol_m3_precipitation)/365 * Fulda_daily_prec$RS/1000 , 0) # der hier ist wirklich mol per m2
  
  RECHARGE_COD_mol_per_m3_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth 
  
  RECHARGE_COD_mol_per_m3_per_day_df <- data.frame(cbind("dateRi" = as.Date(Fulda_daily_prec$dateRi), RECHARGE_COD_mol_per_m3_per_day))
  
  RECHARGE_COD_mol_per_m3_per_day_df$dateRi <-Fulda_daily_prec$dateRi
  
  growth_model_MO_type <- parvar$growth_model_MO[run]
  growth_model_fauna_type <- parvar$growth_model_fauna[run]
  
  mortalityFraction_per_degree <- parvar$mortalityFraction_per_degree[run]
  
  microbe_loss_factor_when_no_fauna <- parvar$microbe_loss_factor_when_no_fauna[run]
  
  return(list(delta_t, max_t, aquifer_depth, import_MO_het, scenario_with_1_or_without_0_fauna, scenario_with_1_or_without_0_MO, carboxylic_acids_fraction, acetic_acids_fraction, mortalityRate, import_fauna, yield_ac, yield_MO, K_MO_at_temp, rMO_BOC_uptake_per_day_at_lab_temperature, rFauna_MO_uptake_per_day_at_TEMP, k1, excretionRate, TOC_COD_mol_m2_yr_precipitation,      RECHARGE_COD_mol_per_m3_per_day_df, k_temp, K_ac, growth_model_MO_type, growth_model_fauna_type, mortalityFraction_per_degree, microbe_loss_factor_when_no_fauna))
}

