library(ggplot2)
library(dplyr)
library(DBI)#dbConnect
library(openxlsx)#read.xlsx
library(DescTools)#year()
library(patchwork) 
library(ggpubr)#ggarrange. 
library(ppgmisc) # 
library(colorblindr)

rm(list = ls())

##########
#the runs with the different parameters are listed in the excel file
##########
run <- 1

##########
#preparing read-in of data
##########


#define these paths for saving your result text files locally
gw_FuldaEcosystemServices_plots_path <-""
  gw_FuldaEcosystemServices_results_txt_path <-""
 
12.7.25 den hier oder gehe ich durch variables ?
urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_daily_temp_joh_long.txt"
Fulda_daily_temp_joh_long <- read.table(urlfiletext,sep = " ", header = TRUE)  

TODO revert back to model_concrete_rates_with_Fulda_a_la_Soetaert_2025_01_04_Fulda_fp
urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_concrete_rates_with_Fulda_a_la_Soetaert_2025_01_04_Fulda_fp_.R"
source(urlfiletext)  ##fulda_variables

urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_concrete_rates_with_Fulda_a_la_Soetaert_2025_01_04_parameter_variables.R"
6.7.25 ist noch nicht hoch geladen.
sollte sowieso erst mal datienamen aendern
  source(urlfiletext)
           
  ########
  #functions reactions
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/model_functions_fp.R"
source(urlfiletext)
  
 urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/parameters_variables_fp.xlsx"
noch nicht hoch geladen
 parvar <- read.xlsx(urlfiletext, startRow = 3, sheet = 1)
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_prec_plot.R"
source(urlfiletext)



##########
#reading in Fulda data from file
##########

scenario_with_1_or_without_0_MO <-  parvar$scenario_with_1_or_without_0_MO[run]  
scenario_with_1_or_without_0_fauna <- parvar$scenario_with_1_or_without_0_fauna[run]  

factor_CC_MO <-  parvar$factor_CC_MO[run]
factor_CC_fauna <-  parvar$factor_CC_fauna[run]

#this depends on the two scenario variables
fulda_variables_read_in <- fulda_variables(run, factor_CC_MO, factor_CC_fauna)
names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_w_dat_ordered_per_date_1978_1981", "chem_w_dat_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0", "fauna_deep_PerSamplPerTaxon_bm_sum_per_P", "CC_table_MO", "CC_table_fauna") 

list2env(fulda_variables_read_in, globalenv())


chem_w_dat_ordered_per_date_1978_1981$group <- chem_w_dat_ordered_per_date_1978_1981$kmeans4gr
chem_w_dat_ordered_per_date_1978_1981$group_letter <- ifelse(chem_w_dat_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_w_dat_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_w_dat_ordered_per_date_1978_1981$group == 1, "M", NA))))   #
 

chem_w_dat_ordered_per_date_1978_1981$group_letter <- factor(chem_w_dat_ordered_per_date_1978_1981$group_letter , levels = c("R", "M", "P", "A"))


##########
#parameters
##########


#read parameters variables  from file
parameter_variables_read_in <- parameter_variables( run)
names(parameter_variables_read_in) <- c("delta_t", "max_t", "aquifer_depth", "import_MO_het", "scenario_with_1_or_without_0_fauna", "scenario_with_1_or_without_0_MO", "carboxylic_acids_fraction", "acetic_acids_fraction", "mortalityRate", "import_fauna", "yield_ac", "yield_MO", "K_MO_at_temp", "rMO_BOC_uptake_per_day_at_lab_temperature", "rFauna_MO_uptake_per_day_at_TEMP", "k1", "excretionRate", "TOC_COD_mol_m2_yr_precipitation", "RECHARGE_COD_mol_per_m3_per_day_df", "lab_temp", "K_ac", "growth_model_MO_type", "growth_model_fauna_type", "mortalityFraction_per_degree", "microbe_loss_factor_when_no_fauna") 

list2env(parameter_variables_read_in, globalenv())

##########
#creating temperature scenarios
##########

temperature_scenario <- parvar$temperature_scenario[run]

urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_daily_temp_joh_long.txt"
Fulda_daily_temp_joh_long <- read.table(urlfiletext,sep = " ", header = TRUE)  

Fulda_daily_temp_joh_long$dateRi <- as.Date(Fulda_daily_temp_joh_long$dateRi)
Fulda_daily_temp_joh_long$TT_TER <- Fulda_daily_temp_joh_long$TT_TER + temperature_scenario #air temperature
Fulda_daily_temp_joh_long$value <- Fulda_daily_temp_joh_long$value + temperature_scenario #daily groundwater temperature extrapolated from air temperature

##########
#container for results
##########

results <- NULL

if(is.na(max_t)){
  t_max = t_max
}else{
  t_max = max_t #set another end date than the one in the Fulda study; max_t is read in from parameter_variables
}

##########
#reading in Fulda precitiation plot - does not depend on model variables, but depends on Fulda_daily_prec in fulda_variables_read_in, and thus, cannot be read in earlier than this
##########

urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_prec_plot.R"
source(urlfiletext)
Fulda_prec_plotted <- Fulda_prec_plot(enddate = t_max)

##########
#over time
##########

# group 1
results1 <- NULL
results1$dateRi <- seq(t_0,  t_max, by = delta_t)
results1 <- as.data.frame(results1)

results1$group <- 1
results1$DETRITUS[1] <- DETRITUS_gr1_t0 #mol COD / L, was OS
results1$import_from_detritus[1] <- 0
results1$BOC[1] <- BOC_gr1_t0

results1$MO_het[1] <- MO_het_gr1_t0

den bruahe ich doch nicht - die zeile drueber muesste doch reichen ?! 
results1$MO_het <- MO_het_gr1_t0 #dry mass mol COD / L
results1$fauna[1] <- fauna_gr1_t0


# group 2
results2 <- NULL
results2$dateRi <- seq(t_0,  t_max, by = delta_t)
results2 <- as.data.frame(results2)

results2$group <- 2
results2$DETRITUS[1] <- DETRITUS_gr2_t0 #mol COD / L
results2$import_from_detritus <- 0
results2$BOC[1] <- BOC_gr2_t0

results2$MO_het[1] <- MO_het_gr2_t0
?
results2$MO_het <- MO_het_gr2_t0 #mol COD / L
results2$fauna <- fauna_gr2_t0


# group 3
results3 <- NULL
results3$dateRi <- seq(t_0,  t_max, by = delta_t)
results3 <- as.data.frame(results3)

results3$group <- 3
results3$DETRITUS[1] <- DETRITUS_gr3_t0 #mol COD / L
results3$import_from_detritus <- 0
results3$BOC[1] <- BOC_gr3_t0

results3$MO_het[1] <- MO_het_gr3_t0

?
results3$MO_het <- MO_het_gr3_t0 #mol COD / L
results3$fauna <- fauna_gr3_t0


# group 4
results4 <- NULL
results4$dateRi <- seq(t_0,  t_max, by = delta_t)
results4 <- as.data.frame(results4)

results4$group <- 4
results4$DETRITUS[1] <- DETRITUS_gr4_t0 #mol COD / L
results4$import_from_detritus <- 0
results4$BOC[1] <- BOC_gr4_t0

results4$MO_het[1] <- MO_het_gr4_t0
?
results4$MO_het <- MO_het_gr4_t0 #mol COD / L
results4$growthrate <- 0
results4$fauna <- fauna_gr4_t0


results <- rbind(results1, results2, results3, results4)

results$group_letter <- ifelse(results$group ==  2, "P", ifelse(results$group == 3, "R" , ifelse (results$group == 4, "A", ifelse (results$group == 1, "M", NA))))

results$group_letter <- factor(results$group_letter , levels = c("R", "M", "P", "A"))

uniquedatevector <- unique(results$dateRi)
uniquegroupvector <- unique(results$group)
uniquegrouplettervector <- unique(results$group_letter)


i = 2; g = 1
for (g in 1:length(unique(results$group))){
  CC_group_MO_g <- CC_table_MO$CC[CC_table_MO$group == uniquegroupvector[g]]
  CC_group_fauna_g <- CC_table_fauna$CC[CC_table_fauna$group == uniquegroupvector[g]]
  group_letter_g <- unique(results$group_letter[results$group == uniquegroupvector[g]])
    
  Fulda_daily_temp_joh_long_g <- Fulda_daily_temp_joh_long %>%
       dplyr::filter(group_letter == group_letter_g)
  
  for (i in 2:(length(uniquedatevector))) {
    
    #air temp.
    AIRTEMP_ti <- Fulda_daily_temp_joh_long_g$TMK [Fulda_daily_temp_joh_long_g$dateRi == results$dateRi[i]]
    #groundwater temp.
    GWTEMP_ti <- Fulda_daily_temp_joh_long_g$TT_TER [Fulda_daily_temp_joh_long_g$dateRi == results$dateRi[i]]
    
    RECHARGE_COD_mol_per_m3_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day [RECHARGE_COD_mol_per_m3_per_day_df$dateRi == uniquedatevector[i]]
    
    RECHARGE_COD_mol_per_L_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df_ti /1000
    
    #take the respective field of the  Date == Date[i] aund group == group[g] 
    DETRITUS_ti_minus_1 <- results$DETRITUS[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]     
    BOC_ti_minus_1 <- results$BOC[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]

    MO_het_ti_minus_1 <- results$MO_het[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    
    Fauna_ti_minus_1 <- results$fauna[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    
    
    BOC_import_from_detritus_ti <- dCOD_from_detritus_dt(k1, DETRITUS_ti_minus_1) #mol COD / L
    
    
    if (scenario_with_1_or_without_0_fauna == 1) {
      
      Excretion <- dExcretion_dt(excretionRate, Fauna_ti_minus_1 )
      
      Mortality <- dMortality_dt(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1, GWTEMP_ti  ) 
      
      f_S_fauna <- dMO_fauna_degradation_factor_dt(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP , K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g)

      Fauna_ti_list <- dMO_fauna_uptake_dt(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP , K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g, yield_MO, Excretion , Mortality)

      Fauna_growth <- Fauna_ti_list[[1]]
      Fauna_ti <- Fauna_ti_list[[2]]
       
    }else{
      f_S_fauna <- 0

      Fauna_ti <- 0
      Fauna_growth <- 0
      Excretion <- 0
      Mortality <- 0
    }
    
    
    rMO_BOC_uptake_per_day_at_TEMP <- d_COD_MO_het_uptake_dt_per_day_per_temperature( rMO_BOC_uptake_per_day_at_lab_temperature,  GWTEMP_ti, lab_temp) 
 
    K_ac_at_TEMP <- d_K_ac_per_temperature(K_ac, lab_temp, GWTEMP_ti) 
 
    #how much COD = BOC degraded 
    f_S_MO <- dCOD_degradation_factor_dt(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP, K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g)
 
    MO_het_ti_list  <- dCOD_MO_degradation_dt(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, yield_ac, f_S_fauna, delta_t, growth_model_MO_type, CC_group_MO_g) 
    MO_growth <- MO_het_ti_list[[1]]
    MO_het_ti <- MO_het_ti_list[[2]]
    
        BOC_ti_interim <- dCOD_stock_dt(BOC_ti_minus_1,  f_S_MO, BOC_import_from_detritus_ti, Excretion) 
        BOC_ti            <- max(0, BOC_ti_interim)      # to avoid errors when COD = BOC becomes slightly negative.. From Soetaert (2008)
        

    #since this happens in this time step, the new Detritus is not used for further reactions in this time step - the detritus from the time step before is used
    DETRITUS_ti <- dDETRITUS_dt( k1, DETRITUS_ti_minus_1, RECHARGE_COD_mol_per_L_per_day_df_ti, Mortality ) 

    results$MO_het[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- MO_het_ti
    
    results$fauna[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- Fauna_ti
    
    results$BOC[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_ti
    
      results$growthrate[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- rMO_BOC_uptake_per_day_at_TEMP
    
    results$DETRITUS[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- DETRITUS_ti 

    RECHARGE_COD_mol_per_L_per_day_df_ti + Mortality
    
    results$import_from_detritus[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_import_from_detritus_ti 
    
    
  } #end groups
}#end time




# check results 
results_gr_4 <- results %>%
  dplyr::filter(group == 4)
results_gr_4[c(1:5, 800:810),]

results_gr_1 <- results %>%
  dplyr::filter(group == 1)
results_gr_1[c(1:5, 800:810),]

results_gr_2 <- results %>%
  dplyr::filter(group == 2)
results_gr_2[c(1:5, 800:810),]

results_gr_3 <- results %>%
  dplyr::filter(group == 3)
results_gr_3[c(1:5, 800:810),]


#ggplot requires the data to be in data frame
results_df <- as.data.frame(results)

setwd(gw_FuldaEcosystemServices_results_txt_path) 
write.table(results_df, paste0("results_df_2025_06_30_run_",run,".txt"), row.names = FALSE)

#for plotting several variables, make long form of the results data frame
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(BOC, DETRITUS, MO_het, growthrate, totalC, fauna), names_to = "variable") 
  
write.table(results_df, paste0("results_df_long_2025_06_30_run_",run,".txt"), row.names = FALSE)


unified_axes <- 1 # 1 = make all four subplots for the four groups the same axis. 0 = axes reflect the groups minima and maxima

(Fulda_Total_C_plot <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(TOC_estimated_mg_L))%>%
    ggplot() +
    geom_point(data = results_df, aes(x = dateRi, y = totalC, colour = as.factor(group_letter)),  pch = 16, size = 0.5, show.legend = FALSE 
    )+
    geom_point(aes(x = Date, y = TOC_estimated_mg_L, fill = as.factor(group_letter)),  pch = 21 , colour = "black", show.legend = FALSE
    )+
    scale_x_date(limits = c(t_0, t_max) )+
    labs(x = "Date", y = "Total C\n [mol COD/ L]\n measured [o] and\nmodelled [.]")+ 
 
    scale_fill_OkabeIto()+ 
    scale_colour_OkabeIto()+
    theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4)
    )+
    facet_grid(.~group_letter   
    )
 
)


unified_axes <- 1



Fulda_Detritus_partOrganics_plot <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(OS_mol_COD_L))%>%
    ggplot() +
    geom_point(data = results_df, aes(x = dateRi, y = DETRITUS, colour = as.factor(group_letter)),  pch = 16, size = 0.5, show.legend = FALSE 
    )+
    geom_point(aes(x = Date, y = OS_mol_COD_L, fill = as.factor(group_letter)),  pch = 21 , colour = "black", show.legend = FALSE
    )+
    scale_x_date(limits = c(t_0, t_max) )+
    labs(x = "Date", y = "Detritus\n [mol COD/ L]\n measured [o] and\nmodelled [.]")+ 
     scale_fill_OkabeIto()+ 
    scale_colour_OkabeIto()+
    theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4
          )
    )
    
if (unified_axes == 1) {
  (Fulda_Detritus_partOrganics_plot <- Fulda_Detritus_partOrganics_plot+
     facet_grid(.~group_letter   
                )
  )
}else {
  (Fulda_Detritus_partOrganics_plot <- Fulda_Detritus_partOrganics_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}


my.formula <- y ~ x

(Fulda_Detritus_partOrganics_plot_trends <- Fulda_Detritus_partOrganics_plot +
   geom_smooth(method = "lm",
                  data = results_df, aes(x = dateRi, y = DETRITUS),
                  se=TRUE,  formula = my.formula, lwd = 0.3) + 
                  
    stat_smooth(method = "lm",
                  data = results_df, aes(x = dateRi, y = DETRITUS)
                  ,  formula = my.formula, lwd = 0.3)
      )    



Fulda_BOC_plot <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(BOC_mol_L)) %>%
    ggplot() +
  
    geom_point(data = results_df, aes(x = dateRi, y = BOC, colour = as.factor(group_letter)),   pch = 16, size = 0.5, show.legend = FALSE)+
    geom_point(aes(x = Date, y = BOC_mol_L, fill = as.factor(group_letter)),  pch = 21  , colour = "black", show.legend = FALSE)  +
    scale_x_date(limits = c(t_0, t_max) )+
    labs(x = "Date", y = "BOC\n [mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
    scale_fill_OkabeIto()+ 
    scale_colour_OkabeIto()+
  
    theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4
          ),
          
          legend.key = element_blank()
    )


if (unified_axes == 1) {
  (Fulda_BOC_plot <- Fulda_BOC_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_BOC_plot <- Fulda_BOC_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
   )
}


(Fulda_BOC_plot_trends <- Fulda_BOC_plot +
   geom_smooth(method = "lm",
                  data = results_df, aes(x = dateRi, y = BOC),
                  se=TRUE,  formula = my.formula, lwd = 0.3) 
      ) 


Fulda_MO_plot <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(total_Prok_mol_COD_L))%>%
    ggplot(
    ) +
    geom_point(data = results_df, aes(x = dateRi, y = MO_het, colour = as.factor(group_letter)),   pch = 16,  size = 0.5 , show.legend = FALSE
    )+
    geom_point(aes(x = Date, y = total_Prok_mol_COD_L, fill = as.factor(group_letter)),  pch = 21, colour = "black", show.legend = FALSE
    )+
    
    scale_x_date(limits = c(t_0, t_max) )+
  
    labs(x = "Date", y = "Microbial dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
 
    scale_fill_OkabeIto()+ 
    scale_colour_OkabeIto()+
  
    theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4
          )
    )


if (unified_axes == 1) {
  (Fulda_MO_plot <- Fulda_MO_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_MO_plot <- Fulda_MO_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}

(Fulda_MO_plot_trends <- Fulda_MO_plot +
   geom_smooth(method = "lm",
                  data = results_df, aes(x = dateRi, y = MO_het
                  ),
                  se=TRUE,  formula = my.formula, lwd = 0.3) 
      ) 


fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group <- fauna_deep_PerSamplPerTaxon_bm_sum_per_P$kmeans4gr

fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group_letter <- ifelse(fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group ==  2, "P", ifelse(fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group == 3, "R" , ifelse (fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group == 4, "A", ifelse (fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group == 1, "M", NA))))

fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group_letter <- factor(fauna_deep_PerSamplPerTaxon_bm_sum_per_P$group_letter , levels = c("R", "M", "P", "A"))


maxmodelfauna <-max(results_df$fauna)
fauna_deep_PerSamplPerTaxon_bm_sum_per_P_no_high_biomass  <- fauna_deep_PerSamplPerTaxon_bm_sum_per_P %>%
dplyr::filter(bm_mol_COD_perL < 0.00001) #for visualization - explained in text and caption
maxfaunaplot <-max(fauna_deep_PerSamplPerTaxon_bm_sum_per_P_no_high_biomass$bm_mol_COD_perL)



Fulda_fauna_plot <- fauna_deep_PerSamplPerTaxon_bm_sum_per_P %>%
    dplyr::filter(!is.na(bm_mol_COD_perL))%>%
    dplyr::filter(!is.na(kmeans4gr))%>%
    ggplot(
    ) +
    geom_point(data = results_df, aes(x = dateRi, y = fauna, colour = as.factor(group_letter)),   pch = 16,  size = 0.5 , show.legend = FALSE
    )+
    geom_point(aes(x = dateRi, y = bm_mol_COD_perL , fill = as.factor(group_letter)),  pch = 21, colour = "black", show.legend = FALSE
    )+
    lims(y = c(0,maxfaunaplot))+
    scale_x_date(limits = c(t_0, t_max)  )+
    labs(x = "Date", y = "Fauna dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
 
    scale_fill_OkabeIto()+ 
    scale_colour_OkabeIto()+
     theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4
          )
    )

if (unified_axes == 1) {
  (Fulda_fauna_plot <- Fulda_fauna_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_fauna_plot <- Fulda_fauna_plot+
     #  + 
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}


(Fulda_fauna_plot_trends <- Fulda_fauna_plot +
   geom_smooth(method = "lm",
                  data = results_df, aes(x = dateRi, y = fauna
                  ),
                  se=TRUE,  formula = my.formula, lwd = 0.3) 
      ) 

Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot  + Fulda_BOC_plot + Fulda_MO_plot + Fulda_fauna_plot + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")
  

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  ggsave(paste0("model_measured_2025_06_30_run_",run,"_with_MO_one_y_scale.png"), width = 10, height = 9)
}else{
ggsave(paste0("model_measured_2025_06_30_run_",run,"_with_MO_free_y_scale.png"), width = 10, height = 9)
}


(Fulda_trends <- Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot_trends  + Fulda_BOC_plot_trends + Fulda_MO_plot_trends + Fulda_fauna_plot_trends + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a", tag_suffix = ")"))

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  ggsave(paste0("model_measured_trend_2025_06_30_run_",run,"_with_MO_one_y_scale.png"), width = 10, height = 9)
}else{
ggsave(paste0("model_measured_trend_2025_06_30_run_",run,"_with_MO_free_y_scale.png"), width = 10, height = 9)
}


#in order to produce plots which compare the runs, read the four run results from files saved previously
setwd(gw_FuldaEcosystemServices_results_txt_path)

results_df_54 <- read.table( "results_df_2025_06_30_run_54.txt", header = TRUE)
results_df_55 <- read.table( "results_df_2025_06_30_run_55.txt", header = TRUE)
results_df_56 <- read.table( "results_df_2025_06_30_run_56.txt", header = TRUE)
results_df_57 <- read.table( "results_df_2025_06_30_run_57.txt", header = TRUE)
results_df_58 <- read.table( "results_df_2025_06_30_run_58.txt", header = TRUE)
results_df_59 <- read.table( "results_df_2025_06_30_run_59.txt", header = TRUE)

#if using one of the saved ones, prepare the respective data, by replacing xx with the number of the run
results_df <- results_df_xx
results_df$dateRi <- as.Date(results_df$dateRi)
results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
run <- xx

results_df_54$run <- 54
results_df_55$run <- 55
results_df_56$run <- 56
results_df_57$run <- 57
results_df_58$run <- 58
results_df_59$run <- 59


results_df__for_overview_all <- rbind(results_df_54, results_df_55, results_df_56, results_df_57, results_df_58, results_df_59)


setwd(gw_FuldaEcosystemServices_results_txt_path)

write.table(results_df__for_overview_all, "results_df__for_overview_all_for_barplot_54_55_56_57_58_59__2025_06_30.txt", row.names = FALSE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))


results_df__for_overview_all_long <- results_df__for_overview_all %>%
dplyr::group_by(dateRi, group_letter, run) %>%
tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")

#calculate trends 
lm_i_df <- lm_i_list <- NULL
i <- j <- 1 

for(i in c(1:length(unique(results_df__for_overview_all_long$run)))){
  
for(j in c(1:length(unique(results_df__for_overview_all_long$group_letter)))){

for(k in c(1:length(unique(results_df__for_overview_all_long$variable)))){

run_i <- unique(results_df__for_overview_all_long$run)[i]
group_letter_j <- unique(results_df__for_overview_all_long$group_letter)[j]
variable_k <- unique(results_df__for_overview_all_long$variable)[k]

 results_df__for_overview_all_long_i <- results_df__for_overview_all_long %>%
    dplyr::filter(run == run_i
    & 
    group_letter == group_letter_j
    &
    variable == variable_k) 
  
 lm_i <-   lm(results_df__for_overview_all_long_i$value ~  results_df__for_overview_all_long_i$dateRi) 
  lm_i_df$coefficient_intercept <- lm_i$coefficients[1][[1]]
  lm_i_df$coefficient_slope <- lm_i$coefficients[2][[1]]
  
  if(length(anova(lm_i)$F_value[1][[1]] > 0)) {
    lm_i_df$F_value <- anova(lm_i)$F_value[1][[1]]
  }else{
    lm_i_df$F_value <- "NA"
  }
  lm_i_df$p_val <- anova(lm_i)$"Pr(>F)"[1]
  lm_i_df$run <- run_i
  lm_i_df$group_letter <- group_letter_j
  lm_i_df$variable <- variable_k
  
  
  lm_i_df$max_ <- max(results_df__for_overview_all_long_i$value, na.rm = TRUE)
  lm_i_df$diff_over_observation_period <- lm_i$fitted.values[length(lm_i$fitted.values)]-lm_i$fitted.values[1]
  lm_i_df$slope_per_year <- lm_i_df$diff_over_observation_period/lm_i_df$time_span
  lm_i_list <- rbind(lm_i_list, as.data.frame(t(unlist(lm_i_df)))) 
}
}
}

setwd(gw_FuldaEcosystemServices_results_txt_path)
write.table(lm_i_list, "results_df__lm_54_55_56_57_58_59__2025_06_30_fp.txt", row.names = FALSE)
# lm_i_list<- read.table( "results_df__lm_54_55_56_57_58_59__2025_06_30_fp.txt", header = TRUE)

#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
results_df__for_overview_all$colour_for_plot <- ifelse(results_df__for_overview_all$run == 54, "#009E73",ifelse(results_df__for_overview_all$run == 56, "#0072B2", ifelse(results_df__for_overview_all$run == 58, "#56B4E9", ifelse(results_df__for_overview_all$run == 55, "#F0E442", ifelse(results_df__for_overview_all$run == 57, "#E69F00", ifelse(results_df__for_overview_all$run == 59, "#D55E00", "black"))))))

results_df__for_overview_all$colour_for_plot <- factor(results_df__for_overview_all$colour_for_plot, level = c("#009E73", "#F0E442", "#0072B2",   "#E69F00", "#56B4E9",   "#D55E00"))

results_df__for_overview_all$line <- ifelse(results_df__for_overview_all$run == 54, 1,ifelse(results_df__for_overview_all$run == 56, 2, ifelse(results_df__for_overview_all$run == 58, 3,  ifelse(results_df__for_overview_all$run == 55, 5, ifelse(results_df__for_overview_all$run == 57, 6, ifelse(results_df__for_overview_all$run == 59, 4, 1))))))

results_df__for_overview_all$line <- factor(results_df__for_overview_all$line, level = c(1, 5, 2,  6, 3,   4))

results_df__for_overview_all$linewidth <- ifelse(results_df__for_overview_all$run == 54, 1.2,ifelse(results_df__for_overview_all$run == 56, .82, ifelse(results_df__for_overview_all$run == 58, .31, ifelse(results_df__for_overview_all$run == 55, 1.1, ifelse(results_df__for_overview_all$run == 57, .81, ifelse(results_df__for_overview_all$run == 59, .3, 1))))))

results_df__for_overview_all$linewidth <- factor(results_df__for_overview_all$linewidth, level = c(1.2, 1.1, .82, .81, .31, .3))


override.col <- c( "#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00")
override.line <- c(1, 5, 2, 6, 3, 4)
override.linewidth <- c(1.2, 1.1, .82, .81,  .31,  .3)

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all)+
   geom_smooth(method = "lm",
                  data = results_df__for_overview_all, aes(x = dateRi, y = BOC, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth) ), se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
        scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+

    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .31,  .3),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+

    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +

    labs(x = "Date", y = "BOC [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank()
    )
)
    
if (unified_axes == 1) {

  (plot_trends_BOC <- plot_trends_BOC +
      facet_wrap(.~group_letter, ncol = 4))

setwd(gw_FuldaEcosystemServices_plots_path)
ggsave(paste0("plot_trends_BOC_54_55_56_57_58_59__2025_06_28_one_y_scale.png"), width = 8, height = 3)

}else{
  (plot_trends_COD <- plot_trends_COD+
      facet_wrap(.~group_letter, scales="free_y", ncol = 4))
ggsave(paste0("plot_trends_BOC_54_55_56_57_58_59__2025_06_30_free_y_scale.png"), width = 8, height = 3)
}



(plot_trends_MO <- ggplot(data = results_df__for_overview_all)+
   
     geom_smooth(method = "lm",
                  data = results_df__for_overview_all, aes(x = dateRi, y = MO_het, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)
                  ),
                  se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
        scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+

    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .31,  .3),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+

    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +

    labs(x = "Date", y = "Micoorganisms [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank()
             )+
      facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_MO_54_55_56_57_58_59__2025_06_30.png", width = 8, height = 2.5)



(plot_trends_fauna <- ggplot(data = results_df__for_overview_all)+
    geom_smooth(method = "lm",
                  data = results_df__for_overview_all, aes(x = dateRi, y = fauna, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)  ), 
                  se=FALSE,  formula = my.formula )+
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
        scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .31,  .3),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+

    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +

    labs(x = "Date", y = "Fauna [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank()
             )+
      facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_fauna_54_55_56_57_58_59__2025_06_30.png", width = 8, height = 2.5)

