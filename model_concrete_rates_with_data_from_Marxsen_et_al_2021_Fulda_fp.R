

fulda_variables<- function(run, factor_CC_MO, factor_CC_fauna){
  
  
  ##########
  # data - weather
  ##########
  #  Fulda_daily_temp is produced from DWD values station 1526
  
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
  
  #from https://www.dwd.de/DE/leistungen/cdc_portal/artikel_nomenklatur_022021.html
  # TMK_MN004	OBS_DEU_P1D_T2M	"Tägliche Stationsmessungen der mittleren Lufttemperatur in 2 m Höhe in °C
  # average air temperature   TMK"
  
  #Fulda_Horas_1526 precipitaiton
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/produkt_nieder_tag_19490101_20171231_01526.txt"
  Fulda_daily_prec <- read.table(urlfiletext, sep = ";", header = TRUE)
  
  Fulda_daily_prec <- Fulda_daily_prec %>%
    dplyr::filter(RS > -999)#exclude obivous place holders
  # according to BESCHREIBUNG_test_obsgermany_climate_daily_more_precip_historical_de.pdf, RS is the "tägliche Niederschlagshöhe mm ", i.e. daily precipitation in mm
  Fulda_daily_prec$dateRi  <- as.Date(as.character(Fulda_daily_prec$MESS_DATUM), format = c("%Y%m%d") )#, origin =
  
  #calculate yearly precipitation
  #Fulda_daily_prec$year <- DescTools::Year(Fulda_daily_prec$dateRi)
  #Fulda_daily_prec_per_year <- Fulda_daily_prec %>%
  #  dplyr::group_by(year) %>%
  #  dplyr::summarise(RS_yearly_sum = sum(RS, na.rm = TRUE))
  #
  #Fulda_daily_prec_per_year_average <- mean(Fulda_daily_prec_per_year$RS_yearly_sum, na.rm = TRUE)
  
  ##########
  # data - read in from Fulda data set
  ##########
  
  
  # divide the data set into the four groups that were developed for the Fulda floodplain in Marxsen et al. (2021)
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/kmeans_chem4centers_exch_8dec_seed7_win_on_mac_2412.txt"
  
  kmeans_chem4_exchgroups <-read.table(urlfiletext, sep = " ", header = TRUE)#
  
  #with group 2 being Red: floodplain zone P = plume, P01, P04, P09
  #with group 3 being Blue: Floodplain zone R = river/hyporheic, P03, P06, P10
  #with group 4 being green: floodplain zone A = agriculturally influenced
  #with group 1 being gray: flood- plain zone M = mixing/transition
  
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/fauna_deep_PerSamplPerTaxonWide_bm_2025_01_03.txt"
  fauna_deep_PerSamplPerTaxonWide_bm <- read.table(urlfiletext, sep = " ", header = TRUE)
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fuldaprokaryotes_deep_PerWellDepth_average.txt"
  Fuldaprokaryotes_deep_PerWellDepth_average  <- read.table(urlfiletext, sep = " ", header = TRUE)
  
  
  urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/chem_w_date.txt"
  chem_w_date <- read.table(urlfiletext, sep = " ", header = TRUE)
  
  chem_w_dat_ordered_per_date <- chem_w_date[order(chem_w_date$Date) , ]
  chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date[DescTools::Year(chem_w_dat_ordered_per_date$Date)>1977 & DescTools::Year(chem_w_dat_ordered_per_date$Date)<1982,]
  chem_w_dat_ordered_per_date_1978_1981$Date <- as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)
  
  chem_w_dat_ordered_per_date_1978_1981$drymass_g_per_L <- Fuldaprokaryotes_deep_PerWellDepth_average$drymass_mug_per_L[match(chem_w_dat_ordered_per_date_1978_1981$sampl, Fuldaprokaryotes_deep_PerWellDepth_average$sampl)]/1000000  
  
  #prokaryote biomass is given as dry mass already. Now calculate mol with dry mass 24. g / mol
  chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_L <- chem_w_dat_ordered_per_date_1978_1981$drymass_g_per_L / 24.6 
  
  #multiplying dry mass with dry masses' COD of 1.05 (see derivation in SI), which is the reaction of N being reduced to NH3, instead of NO3 - N being reduced to NH3 is more realistic in groundwater where dissolved oxygen is often (temporarily) limiting
  
  #COD of respiring dry mass is 1.05 g O2, see SI
  chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_COD_L <- chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_L *1.05 
  
  chem_w_dat_ordered_per_date_1978_1981$OS_mol_L <- chem_w_dat_ordered_per_date_1978_1981$OS /227.172 / 1000 #OS = organic substance was given in mg / L. calculate mol l-1 assuming Humic acid with the Molecular formula:	C9H9NO6; Average mass:	227.172; ChemSpider ID:	32820151; https://www.chemspider.com/Chemical-Structure.32820151.html. comes in mg / L . umreche  nach mol l-1 assuming Humic acid; Molecular formula:	C9H9NO6; Average mass:	227.172; ChemSpider ID:	32820151; https://www.chemspider.com/Chemical-Structure.32820151.html
  
  #COD of humic acid is 7.5; check SI
  chem_w_dat_ordered_per_date_1978_1981$OS_mol_COD_L <- chem_w_dat_ordered_per_date_1978_1981$OS_mol_L *  7.5 #
  
  chem_w_dat_ordered_per_date_1978_1981$COD_mol_L <- chem_w_dat_ordered_per_date_1978_1981$COD / 32 / 1000 #calculating mg O2 into mol using molar mass of O2, because of chemical oxygen demand: 32 g / mol
  #in the Marxen 2021 paper, this was called COD, in contrast to OS which is a different method but likely encompasses more than just COD. Here, we boldly assume that COD is BOD, i.e. biologically degradable carbon (BOD). The code is here slightly misleading, therefore we relabel:
  names(chem_w_dat_ordered_per_date_1978_1981) <-  sub("COD_mol_L", "BOC_mol_COD_L" , names(chem_w_dat_ordered_per_date_1978_1981))
  
  chem_w_dat_ordered_per_date_1978_1981$kmeans4gr <- kmeans_chem4_exchgroups$V2[match(chem_w_dat_ordered_per_date_1978_1981$P, kmeans_chem4_exchgroups$V1)]
  
  chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na(kmeans4gr))
  
  # calculating group-wise averages for start values of the simulation
  chem_w_dat_ordered_per_date_1978_1981_mean_per_group <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::group_by(kmeans4gr) %>%
    summarize(
      BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE), 
      OS_mol_COD_L = mean(OS_mol_COD_L, na.rm =TRUE), 
      total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm =TRUE) )
  
  #calculate max per group for deriving carrying capacity 
  chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max <- chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::group_by(kmeans4gr) %>%
    summarize(
      BOC_mol_COD_L_max = max(BOC_mol_COD_L, na.rm = TRUE),  
      OS_mol_COD_L_max =  max(OS_mol_COD_L, na.rm =TRUE), 
      OS_mol_COD_L_max =  max(OS_mol_COD_L, na.rm =TRUE),  
      total_Prok_mol_COD_L_max = max(total_Prok_mol_COD_L, na.rm =TRUE)       
    )
  
  #here , all orgnisms' dry mass is summed per sample
  fauna_deep_PerSamplPerTaxonWide_bm_sum <- fauna_deep_PerSamplPerTaxonWide_bm %>%
    tidyr::pivot_longer(cols = names(fauna_deep_PerSamplPerTaxonWide_bm)[2:dim(fauna_deep_PerSamplPerTaxonWide_bm)[2]]) %>%
    dplyr::group_by(sampl) %>%
    dplyr::summarise(bm_perL = sum(value, na.rm = TRUE))
  
  fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_mol_perL <- fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_perL / 24.6 / 1000000 #  divide by 1000 000 to transfrom from micro g to g, and  use the 24.6 g/ mol molar mass of dry mass
  
  # 1.05 mol COD / mol dry mass - the same as for BDC, biodegradable organic carbon, which is respired to CO2 and NH4.  #COD of respiring dry mass is 1.05 g O2, see SI
  fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_mol_COD_perL <- fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_mol_perL *1.05 
  
  #we want to average per sample, and then per group
  fauna_deep_PerSamplPerTaxonWide_bm_sum$P <- substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 1, 3)
  fauna_deep_PerSamplPerTaxonWide_bm_sum$kmeans4gr <-  kmeans_chem4_exchgroups$V2[match(fauna_deep_PerSamplPerTaxonWide_bm_sum$P, kmeans_chem4_exchgroups$V1)]
  
  fauna_deep_PerSamplPerTaxonWide_bm_sum$dateRi <- as.Date(paste(substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 7, nchar(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl)), substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 5, 6),  "01", sep = "-"))
  
  #used for start values
  fauna_deep_PerSamplPerTaxon_bm_mean_per_group <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
    dplyr::group_by(  kmeans4gr) %>%
    reframe(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))
  
  #for deriving carrying capacity 
  fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
    dplyr::group_by(kmeans4gr) %>%
    reframe( bm_mol_COD_perL_max = max(bm_mol_COD_perL, na.rm = TRUE))
  
  ##########
  #derived parameters 
  ##########
  
  #first time point
  t_0         = min(c(as.Date(chem_w_dat_ordered_per_date_1978_1981$Date), as.Date(fauna_deep_PerSamplPerTaxonWide_bm_sum$dateRi)), na.rm =TRUE)# prokaryote dates are in chem_w_date
  
  t_max       = max(as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)) #
  
  ##########
  #time point 0
  ##########
  #with group 2 being Red: floodplain zone P = plume, P01, P04, P09
  #with group 3 being Blue: Floodplain zone R = river/hyporheic, P03, P06, P10
  #with group 4 being green: floodplain zone A = agriculturally influenced
  #with group 1 being gray: flood- plain zone M = mixing/transition
  
  #assumption: I start the model with the first of the values measured 
  
  DETRITUS_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( OS_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==1) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE))
    DETRITUS_gr1_t0 <- DETRITUS_gr1_t0_all$OS_mol_COD_L[1]
  
  
  BOC_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( BOC_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==1) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE))
    BOC_gr1_t0 <- BOC_gr1_t0_all$BOC_mol_COD_L[1]
  
  if(scenario_with_1_or_without_0_MO == 1) {
    
    MO_het_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( total_Prok_mol_COD_L)) %>%
      dplyr::filter(kmeans4gr ==1) %>%
      dplyr::group_by(Date) %>%
      dplyr::reframe(total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE))
    
    MO_het_gr1_t0 <- MO_het_gr1_t0_all$total_Prok_mol_COD_L[1]
    
    #carrying capacity assumed is the max MO_het abundance measured plus 10% which never are seen in the field due to grazing, "mortality", etc.
    MO_het_CC_gr1 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==1]*factor_CC_MO
  
    }else{
    MO_het_gr1_t0  = 0
    MO_het_CC_gr1 <- 0
  }
  
  if(scenario_with_1_or_without_0_fauna == 1) {
    
    fauna_gr1_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
    dplyr::filter(!is.na( bm_mol_COD_perL)) %>%
      dplyr::filter(kmeans4gr ==1) %>%
      dplyr::group_by(dateRi) %>%
      dplyr::reframe(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))  %>%
      dplyr::filter(! bm_mol_COD_perL == 0) 
    
    fauna_gr1_t0 <- fauna_gr1_t0_all$bm_mol_COD_perL[1]
    
    
    fauna_CC_gr1 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==1]*factor_CC_MO
  }else{
    fauna_gr1_t0   = 0
    fauna_CC_gr1 <- 0
  }
  
  
  
  
  
  DETRITUS_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( OS_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==2) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE))
  DETRITUS_gr2_t0 <- DETRITUS_gr2_t0_all$OS_mol_COD_L[1]
  
  
  
  BOC_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( BOC_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==2) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE))
  
  BOC_gr2_t0 <- BOC_gr2_t0_all$BOC_mol_COD_L[1]
  
  if(scenario_with_1_or_without_0_MO == 1) {
    
    MO_het_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(!is.na( total_Prok_mol_COD_L)) %>%
      dplyr::filter(kmeans4gr ==2) %>%
      dplyr::group_by(Date) %>%
      dplyr::reframe(total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE))
    
    MO_het_gr2_t0 <- MO_het_gr2_t0_all$total_Prok_mol_COD_L[1]
    
    
    #carrying capacity assumed is the max MO_het abundance measured (plus x%, see parameters_variables.xlsx) 
    #which never are seen in the field due to grazing, "mortality", etc.
    
    
    MO_het_CC_gr2 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==2] *factor_CC_MO
    
  }else{
    MO_het_gr2_t0  = 0
    MO_het_CC_gr2 <- 0
  }
  
  if(scenario_with_1_or_without_0_fauna == 1) {
    
    fauna_gr2_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
      dplyr::filter(!is.na( bm_mol_COD_perL)) %>%
      dplyr::filter(kmeans4gr ==2) %>%
      dplyr::group_by(dateRi) %>%
      dplyr::reframe(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))  %>%
      dplyr::filter(! bm_mol_COD_perL == 0) 
    
    fauna_gr2_t0 <- fauna_gr2_t0_all$bm_mol_COD_perL[1]
    
    fauna_CC_gr2 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==2] *factor_CC_MO
  }else{
    fauna_gr2_t0   = 0
    fauna_CC_gr2 <- 0
  }
  
  
  DETRITUS_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( OS_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==3) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE))
  DETRITUS_gr3_t0 <- DETRITUS_gr3_t0_all$OS_mol_COD_L[1]
  
  
  
  BOC_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( BOC_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==3) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE))
  
  BOC_gr3_t0 <- BOC_gr3_t0_all$BOC_mol_COD_L[1]
  
  
  if(scenario_with_1_or_without_0_MO == 1) {
    
    MO_het_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(!is.na( total_Prok_mol_COD_L)) %>%
      dplyr::filter(kmeans4gr ==3) %>%
      dplyr::group_by(Date) %>%
      dplyr::reframe(total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE))
    
    MO_het_gr3_t0 <- MO_het_gr3_t0_all$total_Prok_mol_COD_L[1]
    
    
    MO_het_CC_gr3 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==3] *factor_CC_MO
  }else{
    MO_het_gr3_t0  = 0
    MO_het_CC_gr3 <- 0
  }
  
  if(scenario_with_1_or_without_0_fauna == 1) {
    
    fauna_gr3_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
      dplyr::filter(!is.na( bm_mol_COD_perL)) %>%
      dplyr::filter(kmeans4gr ==3) %>%
      dplyr::group_by(dateRi) %>%
      dplyr::reframe(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))  %>%
      dplyr::filter(! bm_mol_COD_perL == 0) 
    
    fauna_gr3_t0 <- fauna_gr3_t0_all$bm_mol_COD_perL[1]
    
    fauna_CC_gr3 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==3] *factor_CC_MO
  }else{
    fauna_gr3_t0   = 0
    fauna_CC_gr3 <- 0
  }
  
  
  
  DETRITUS_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( OS_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==4) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE))
  DETRITUS_gr4_t0 <- DETRITUS_gr4_t0_all$OS_mol_COD_L[1]
  
  
  
  BOC_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
    dplyr::filter(!is.na( BOC_mol_COD_L)) %>%
    dplyr::filter(kmeans4gr ==4) %>%
    dplyr::group_by(Date) %>%
    dplyr::reframe(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE))
  
  BOC_gr4_t0 <- BOC_gr4_t0_all$BOC_mol_COD_L[1]
  
  
  if(scenario_with_1_or_without_0_MO == 1) {
    
    MO_het_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(!is.na( total_Prok_mol_COD_L)) %>%
      dplyr::filter(kmeans4gr ==4) %>%
      dplyr::group_by(Date) %>%
      dplyr::reframe(total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE))
    
    MO_het_gr4_t0 <- MO_het_gr4_t0_all$total_Prok_mol_COD_L[1]
    
    #carrying capacity assumed is the max MO_het abundance measured plus 10% which never are seen in the field due to grazing, "mortality", etc.
    MO_het_CC_gr4 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==4] *factor_CC_MO
  }else{
    MO_het_gr4_t0  = 0
    MO_het_CC_gr4 <- 0
  }
  
  if(scenario_with_1_or_without_0_fauna == 1) {
    
    fauna_gr4_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
      dplyr::filter(!is.na( bm_mol_COD_perL)) %>%
      dplyr::filter(kmeans4gr ==4) %>%
      dplyr::group_by(dateRi) %>%
      dplyr::reframe(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))  %>%
      dplyr::filter(! bm_mol_COD_perL == 0) 
    
    fauna_gr4_t0 <- fauna_gr4_t0_all$bm_mol_COD_perL[1]
    
    fauna_CC_gr4 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max [fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==4] *factor_CC_MO
  }else{
    fauna_gr4_t0   = 0
    fauna_CC_gr4 <- 0
  }
  
  CC_table_MO <- as.data.frame(cbind(group = c(1:4), CC = c(MO_het_CC_gr1, MO_het_CC_gr2, MO_het_CC_gr3, MO_het_CC_gr4)))
  CC_table_fauna <- as.data.frame(cbind(group = c(1:4), CC = c(fauna_CC_gr1, fauna_CC_gr2, fauna_CC_gr3, fauna_CC_gr4)))
  
  return(list(Fulda_daily_prec, Fulda_daily_temp_, chem_w_dat_ordered_per_date_1978_1981, chem_w_dat_ordered_per_date_1978_1981_mean_per_group, fauna_deep_PerSamplPerTaxonWide_bm_sum, fauna_deep_PerSamplPerTaxon_bm_mean_per_group, t_0, t_max, DETRITUS_gr1_t0, DETRITUS_gr2_t0, DETRITUS_gr3_t0, DETRITUS_gr4_t0, BOC_gr1_t0, BOC_gr2_t0, BOC_gr3_t0, BOC_gr4_t0, MO_het_gr1_t0, MO_het_gr2_t0, MO_het_gr3_t0, MO_het_gr4_t0, fauna_gr1_t0, fauna_gr2_t0, fauna_gr3_t0, fauna_gr4_t0,  CC_table_MO, CC_table_fauna) )

  
}

