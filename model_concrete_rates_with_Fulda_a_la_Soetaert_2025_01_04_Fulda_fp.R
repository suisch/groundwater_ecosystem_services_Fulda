fulda_variables<- function(run, factor_CC_MO, factor_CC_fauna){
  
  ##########
  # data - weather
  ##########
  
  bin durcheinander - ist das die richtige Datei?
    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_daily_temp.txt"
    6.7.25 nein
    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_daily_temp_joh_long.txt"
    Fulda_daily_temp <- read.table(urlfiletext,sep = " ", header = TRUE)  
    
     in joh_long_nicht noetig
    Fulda_daily_temp$dateRi  <- as.Date(as.character(substr(Fulda_daily_temp$MESS_DATUM, 1, 8)), format = c("%Y%m%d") )
    stattdessen. wobei, das kommt unten, oder?
    Fulda_daily_temp$dateRi  <- as.Date(Fulda_daily_temp$dateRi)
    
    Fulda_daily_temp_ <- Fulda_daily_temp %>%
      dplyr::filter(TT_TER > -999)%>%
      # three values per day are given, morning mid day evening;  take the average here becuase for groundwater temperature, the mean is more decisive than minima and maxima
      dplyr::mutate(dateRi = substr(MESS_DATUM, 1, 8)) %>%
      dplyr::group_by(dateRi)%>%
      dplyr::summarise(TT_TER = mean(TT_TER, na.rm = TRUE))
    
    Fulda_daily_temp_$dateRi <- as.Date(Fulda_daily_temp_$dateRi, format = "%Y%m%d")
    
    #from https://www.dwd.de/DE/leistungen/cdc_portal/artikel_nomenklatur_022021.html
    # TMK_MN004	"OBS_DEU_P1D_T2M	Tägliche Stationsmessungen der mittleren Lufttemperatur in 2 m Höhe in °C"
    # average air temperature   TMK
    
    #Fulda_Horas_1526
    
    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_daily_temp_joh_long.txt"
    Fulda_daily_temp_joh_long <- read.table(urlfiletext,sep = " ", header = TRUE)  
 
    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/produkt_nieder_tag_19490101_20171231_01526.txt"
    
    Fulda_daily_prec <- read.table(urlfiletext, sep = ";", header = TRUE)

    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/Fulda_prec_plot.R"
    source(urlfiletext)
    
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
    
    #column total cell numbers is incomplete , therefore, calculate again here
    Fuldaprokaryotes_deep_PerWellDepth_average$total_cells <- Fuldaprokaryotes_deep_PerWellDepth_average$attached_rod_0to1+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_rod_1to2+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_rod_2to3+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_rod_3to4+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_rod_4to+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_cocci_0to0_5+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_cocci_0_5to1+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_cocci_1to2+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_cocci_2to+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$attached_div+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$free_rod_0to1+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$free_rod_1to2+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$free_rod_2to3+
      Fuldaprokaryotes_deep_PerWellDepth_average$free_rod_3to4+
      Fuldaprokaryotes_deep_PerWellDepth_average$free_rod_4to+
      Fuldaprokaryotes_deep_PerWellDepth_average$free_cocci_0to0_5+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$free_cocci_0_5to1+ 
      Fuldaprokaryotes_deep_PerWellDepth_average$free_cocci_1to2+
      Fuldaprokaryotes_deep_PerWellDepth_average$free_cocci_2to
    
    #
    calculate ^1-6 / mL from ?!
      Fuldaprokaryotes_deep_PerWellDepth_average$total_cells <- Fuldaprokaryotes_deep_PerWellDepth_average$total_cells /10^3
    
    
    
    urlfiletext <- "https://raw.github.com/suisch/groundwater_ecosystem_services_Fulda/main/chem_w_date.txt"
    chem_w_date <- read.table(urlfiletext, sep = " ", header = TRUE)
    
    chem_w_dat_ordered_per_date <- chem_w_date[order(chem_w_date$Date) , ]
    chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date[DescTools::Year(chem_w_dat_ordered_per_date$Date)>1977 & DescTools::Year(chem_w_dat_ordered_per_date$Date)<1982,]
    chem_w_dat_ordered_per_date_1978_1981$Date <- as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)
    
    chem_w_dat_ordered_per_date_1978_1981$drymass_g_per_L <- Fuldaprokaryotes_deep_PerWellDepth_average$drymass_mug_per_L[match(chem_w_dat_ordered_per_date_1978_1981$sampl, Fuldaprokaryotes_deep_PerWellDepth_average$sampl)]/1000000 # calculate g / L from microg / L
    
    #prokaryote biomass is wet mass, thus, calculate dry mass 
    multiplying dry mass with dry masses COD of 1.05 (see derivation in SI), which is the reaction of N being reduced to NH3, instead of NO3 - N being reduced to NH3 is more realistic in groundwater where dissolved oxygen is often (temporarily) limiting
    chem_w_dat_ordered_per_date_1978_1981$total_Prok_g_COD_L <- chem_w_dat_ordered_per_date_1978_1981$drymass_g_per_L *1.05  # 
    #derive mol, with molar mass of biomass: 24.6 g / mol
    chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_L <- chem_w_dat_ordered_per_date_1978_1981$drymass_g_per_L / 24.6 # 
    
    #COD of respiring dry mass is 1.05 g O2, see SI
  6.7.25  wieso ist hier noch mal 1.05 ???
    chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_COD_L <- chem_w_dat_ordered_per_date_1978_1981$total_Prok_mol_L *1.05  
    
    chem_w_dat_ordered_per_date_1978_1981$OS_mol_L <- chem_w_dat_ordered_per_date_1978_1981$OS /227.172 / 1000 #OS = organic substance was given in mg / L. calculate mol l-1 assuming Humic acid with the Molecular formula:	C9H9NO6; Average mass:	227.172; ChemSpider ID:	32820151; https://www.chemspider.com/Chemical-Structure.32820151.html
    
    TODO ist das noch aktuell ? #and then would be missing, transforming mol OS to mol COD - which I dont know how to do on moles. Thus, rather:
      
      # as in precipitation
      stimmt das??
      chem_w_dat_ordered_per_date_1978_1981$OS_g_COD_L <- chem_w_dat_ordered_per_date_1978_1981$OS *  7.5 / 1000 #OS was given in mg / L
    chem_w_dat_ordered_per_date_1978_1981$OS_mol_COD_L <- chem_w_dat_ordered_per_date_1978_1981$OS_g_COD_L /32 # 1 mol COD is 32 g / L COD
    #COD of humic acid is 7.5; check SI 
    chem_w_dat_ordered_per_date_1978_1981$OS_mol_COD_L <- chem_w_dat_ordered_per_date_1978_1981$OS_mol_L *  7.5 
    
    chem_w_dat_ordered_per_date_1978_1981$COD_mol_L <- chem_w_dat_ordered_per_date_1978_1981$COD / 32 / 1000 #calculating mg O2 into mol using molar mass of O2, because of chemical oxygen demand: 32 g / mol
    #in the Marxen 2021 paper, this was called COD, in contrast to OS which is a different method but likely encompasses more than just COD. Here, we boldly assume that COD is BOD, i.e. biologically degradable carbon (BOD). The code is here slightly misleading, therefore we relabel:
    names(chem_w_dat_ordered_per_date_1978_1981) <-  sub("COD_mol_L", "BOC_mol_L" , names(chem_w_dat_ordered_per_date_1978_1981))
    ACHTUNG - check ! hab ich von unten rauf geschoben
    
    chem_w_dat_ordered_per_date_1978_1981$kmeans4gr <- kmeans_chem4_exchgroups$V2[match(chem_w_dat_ordered_per_date_1978_1981$P, kmeans_chem4_exchgroups$V1)]
    
    chem_w_dat_ordered_per_date_1978_1981_noNA <- chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(!is.na(kmeans4gr))
    
    #TODO careful ! shortcut
    den sollte ich erst mal aufraeumen?
      chem_w_dat_ordered_per_date_1978_1981 <- chem_w_dat_ordered_per_date_1978_1981_noNA
    
    # calculating group-wise averages for start values of the simulation
    .7.25 fkt nicht    total_Prok_mol_COD_L   not known
    chem_w_dat_ordered_per_date_1978_1981_mean_per_group <- chem_w_dat_ordered_per_date_1978_1981_noNA %>%
      dplyr::group_by(kmeans4gr) %>%
      summarize(
        BOC_mol_L = mean(BOC_mol_L, na.rm = TRUE), 
        OS_mol_COD_L = mean(OS_mol_COD_L, na.rm =TRUE), 
        total_Prok_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm =TRUE) )
    
    
    #here , all orgnisms' biomass is summed per sample
    fauna_deep_PerSamplPerTaxonWide_bm_sum <- fauna_deep_PerSamplPerTaxonWide_bm %>%
      tidyr::pivot_longer(cols = names(fauna_deep_PerSamplPerTaxonWide_bm)[2:dim(fauna_deep_PerSamplPerTaxonWide_bm)[2]]) %>%
      dplyr::group_by(sampl) %>%
      dplyr::summarise(bm_perL = sum(value, na.rm = TRUE))
    
    
    # 1.05 COD - the same as for BDC, biodegradable organic carbon, which is respired to CO2 and NH4
    fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_g_COD_perL <- fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_perL * 1.05 / 1000000 #  divide by 1000 000 to transform from micro g to g
    
    
    #transform into mol COD
    fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_mol_COD_perL <- fauna_deep_PerSamplPerTaxonWide_bm_sum$bm_g_COD_perL / 32 # with M (O2) = 32
    
    #we want to average per sample, and then per group
    fauna_deep_PerSamplPerTaxonWide_bm_sum$P <- substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 1, 3)
    fauna_deep_PerSamplPerTaxonWide_bm_sum$kmeans4gr <-  kmeans_chem4_exchgroups$V2[match(fauna_deep_PerSamplPerTaxonWide_bm_sum$P, kmeans_chem4_exchgroups$V1)]
    
    #2.1.25 exclude one outlier  - that also changes start values
    I did not, in the end - therefore, I can relabel everyting - carefully 
    fauna_deep_PerSamplPerTaxon_bm_sum_per_P <- fauna_deep_PerSamplPerTaxonWide_bm_sum #
    
    fauna_deep_PerSamplPerTaxonWide_bm_sum$dateRi <- as.Date(paste(substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 7, nchar(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl)), substr(fauna_deep_PerSamplPerTaxonWide_bm_sum$sampl, 5, 6),  "01", sep = "-"))
    
    #used for start values
    fauna_deep_PerSamplPerTaxon_bm_mean_per_group <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
      dplyr::group_by(kmeans4gr) %>%
      summarize(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE))
    
    
    ##########
    #derived parameters 
    ##########
    
    #first time point
    t_0         = as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)[1]# day-wise; 20.12.24 I would have preferred doing min(Fuldachem$Date[1]) here, but that does not work
    
    t_max       = max(as.Date(chem_w_dat_ordered_per_date_1978_1981$Date)) #
    
    ##########
    #time point 0
    ##########
    #with group 2 being Red: floodplain zone P = plume, P01, P04, P09
    #with group 3 being Blue: Floodplain zone R = river/hyporheic, P03, P06, P10
    #with group 4 being green: floodplain zone A = agriculturally influenced
    #with group 1 being gray: flood- plain zone M = mixing/transition
    
    #assumption: I start the model with the mean of the values measured 
  .7.25  chem_w_dat_ordered_per_date_1978_1981_mean_per_group not known
    DETRITUS_gr1_t0 = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$OS_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==1] #mol C/ L 
    #2.6.25 unit shoudl read mol COD / L ?!
    
    #changed 12.6.25. 22.6.25 aber warum, das ist doch komplett falsch?? die Zeile mit  DETRITUS_gr1_t0_all[[1]][1]macht nur sinn bei linear model, aber das ist ja hier nicht. 
    #5.7.25 v.a. sollte ich group_mean nehmen, ach nee, der filtert nach group hier - also, ich brauch den hier nicht, oder?
    DETRITUS_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==1) %>%
      dplyr::select(OS_mol_COD_L) %>%
      dplyr::filter(!is.na( OS_mol_COD_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht DETRITUS_gr1_t0 <- DETRITUS_gr1_t0_all[[1]][1]
    # mol COD / L ?!
    
    
    BOC_gr1_t0      =    chem_w_dat_ordered_per_date_1978_1981_mean_per_group$BOC_mol_L [chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==1] #mol COD / L
    
    
    #changed 12.6.25
    BOC_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==1) %>%
      dplyr::select(BOC_mol_L) %>%
      dplyr::filter(!is.na( BOC_mol_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht BOC_gr1_t0 <- BOC_gr1_t0_all[[1]][1]
    # mol COD / L ?!
    
    #TODO 21.1.25 leaving these here for now
    #TODO Achtung! estimated like this: first TOC estimated: 
    #"TOC is the carbon content of an organic compound/substrate, whereas COD is the oxygen equivalent of the same compound/substrate, serving as electron donor during full oxidation. Therefore TOC/COD ratio is a function of the oxidation state of carbon in the compound. For example, this ratio is 5.4 for methane and 2.65 for glucose and acetic acid."
    #angenommen, man kann COD mit DOC einigermassen gleichsetzen - nee, mit TOC. Und zwar ueber die mol.  32 mg / mol O2. groessenordungsmaessig
    # Und annahme, dass 0.3 von part. Org. Substanz , die man mit CHNOPS berechnet, Molmasse 132 g/mol, C sind. Dann ist DOC der COD-part. Org. Substnaz
    #wenn ich das eh ueber COD gemacht hab, dann jetzt noch mal ordenltich:
    # COD mol O2 = COD mg O2/32
    # COD mol C = COD mol O2 mit Annahme, dass JEDES C zu CO2 oxidiert wird
    # DOC mol C = 0.3 COD mol C mit Annahme, dass ein Drittel des Gesamt-oxidierbaren C geloest ist
    
    
    # from paper, per group; transformation of abundance to biomass with Schmidt et al. 2018; with #VX Constant volume of bacterial cells
    #TODO could also calculate average volume / biomass from counts .. at least instead of the 2018 value of volume take the per group average volume from Marxsen
    #2.6.25 thus, calculated as COD - but how ?! now with 1.05 - all good
    if(scenario_with_1_or_without_0_MO == 1) {
      MO_het_gr1_t0   = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$total_Prok_mol_COD_L [chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==1] 
      #carrying capacity assumed is the max MO_het abundance measured plus 10% which never are seen in the field due to grazing, "mortality", etc.
      MO_het_CC_gr1 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==1]*factor_CC_MO
      
      #changed 12.6.25
      MO_het_gr1_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
        dplyr::filter(kmeans4gr ==1) %>%
        dplyr::select(total_Prok_mol_COD_L) %>%
        dplyr::filter(!is.na( total_Prok_mol_COD_L)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht MO_het_gr1_t0 <- MO_het_gr1_t0_all[[1]][1]
      
      
      
    }else{
      MO_het_gr1_t0  = 0
      MO_het_CC_gr1 <- 0
    }
    
    if(scenario_with_1_or_without_0_fauna == 1) {
      fauna_gr1_t0   = fauna_deep_PerSamplPerTaxon_bm_mean_per_group $bm_mol_COD_perL [fauna_deep_PerSamplPerTaxon_bm_mean_per_group$kmeans4gr ==1] 
      
      #changed 12.6.25
      fauna_gr1_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
        dplyr::filter(kmeans4gr ==1) %>%
        dplyr::select(bm_mol_COD_perL) %>%
        dplyr::filter(!is.na( bm_mol_COD_perL)) 
      #22.6.25 das ist ja komplett schwachsinn - was hab ich denn da gemacht ?? ich wollte hier auf lm hinaus, aber hie ist ja kein lm ?! 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht fauna_gr1_t0 <- fauna_gr1_t0_all[[1]][1]
      
      
      fauna_CC_gr1 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==1]*factor_CC_MO
    }else{
      fauna_gr1_t0   = 0
      fauna_CC_gr1 <- 0
    }
    
    
    
    
    DETRITUS_gr2_t0 = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$OS_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==2] #mol COD/ L    
    
    #changed 12.6.25
    DETRITUS_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==2) %>%
      dplyr::select(OS_mol_COD_L) %>%
      dplyr::filter(!is.na( OS_mol_COD_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht DETRITUS_gr2_t0 <- DETRITUS_gr2_t0_all[[1]][1]
    
    
    BOC_gr2_t0      = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$BOC_mol_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==2] #mol COD/ L
    
    #changed 12.6.25
    BOC_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==2) %>%
      dplyr::select(BOC_mol_L) %>%
      dplyr::filter(!is.na( BOC_mol_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht BOC_gr2_t0 <- BOC_gr2_t0_all[[1]][1]
    
    
    if(scenario_with_1_or_without_0_MO == 1) {
      MO_het_gr2_t0   = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$total_Prok_mol_COD_L [chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==2] 
      
      #changed 12.6.25
      MO_het_gr2_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
        dplyr::filter(kmeans4gr ==2) %>%
        dplyr::select(total_Prok_mol_COD_L) %>%
        dplyr::filter(!is.na( total_Prok_mol_COD_L)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht MO_het_gr2_t0 <- MO_het_gr2_t0_all[[1]][1]
      
      
      #carrying capacity assumed is the max MO_het abundance measured ()plus 10%) which never are seen in the field due to grazing, "mortality", etc.
      
      
      MO_het_CC_gr2 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==2] *factor_CC_MO
      
    }else{
      MO_het_gr2_t0  = 0
      MO_het_CC_gr2 <- 0
    }
    
    if(scenario_with_1_or_without_0_fauna == 1) {
      fauna_gr2_t0   = fauna_deep_PerSamplPerTaxon_bm_mean_per_group $bm_mol_COD_perL [fauna_deep_PerSamplPerTaxon_bm_mean_per_group$kmeans4gr ==2] 
      
      fauna_gr2_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
        dplyr::filter(kmeans4gr ==2) %>%
        dplyr::select(bm_mol_COD_perL) %>%
        dplyr::filter(!is.na( bm_mol_COD_perL)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht fauna_gr2_t0 <- fauna_gr2_t0_all[[1]][1]
      
      fauna_CC_gr2 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==2] *factor_CC_MO
    }else{
      fauna_gr2_t0   = 0
      fauna_CC_gr2 <- 0
    }
    
    DETRITUS_gr3_t0 = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$OS_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==3] #mol COD/ L    
    
    #changed 12.6.25
    DETRITUS_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==3) %>%
      dplyr::select(OS_mol_COD_L) %>%
      dplyr::filter(!is.na( OS_mol_COD_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht DETRITUS_gr3_t0 <- DETRITUS_gr3_t0_all[[1]][1]
    
    
    BOC_gr3_t0      = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$BOC_mol_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==3] #mol COD/ L
    
    #changed 12.6.25
    BOC_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==3) %>% #corrected 21.6.25
      dplyr::select(BOC_mol_L) %>%
      dplyr::filter(!is.na( BOC_mol_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht BOC_gr3_t0 <- BOC_gr3_t0_all[[1]][1]
    
    
    if(scenario_with_1_or_without_0_MO == 1) {
      MO_het_gr3_t0   = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$total_Prok_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==3] 
      
      #changed 12.6.25
      MO_het_gr3_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
        dplyr::filter(kmeans4gr ==3) %>%#corrected 21.6.25
        dplyr::select(total_Prok_mol_COD_L) %>%
        dplyr::filter(!is.na( total_Prok_mol_COD_L)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht MO_het_gr3_t0 <- MO_het_gr3_t0_all[[1]][1]
      
      
      #carrying capacity assumed is the max MO_het abundance measured plus 10% which never are seen in the field due to grazing, "mortality", etc.
      MO_het_CC_gr3 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==3] *factor_CC_MO
    }else{
      MO_het_gr3_t0  = 0
      MO_het_CC_gr3 <- 0
    }
    
    if(scenario_with_1_or_without_0_fauna == 1) {
      fauna_gr3_t0   = fauna_deep_PerSamplPerTaxon_bm_mean_per_group $bm_mol_COD_perL [fauna_deep_PerSamplPerTaxon_bm_mean_per_group$kmeans4gr ==3] 
      
      fauna_gr3_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
        dplyr::filter(kmeans4gr ==3) %>%
        dplyr::select(bm_mol_COD_perL) %>%
        dplyr::filter(!is.na( bm_mol_COD_perL)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht fauna_gr3_t0 <- fauna_gr3_t0_all[[1]][1]
      
      fauna_CC_gr3 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max[fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==3] *factor_CC_MO
    }else{
      fauna_gr3_t0   = 0
      fauna_CC_gr3 <- 0
    }
    
    
    DETRITUS_gr4_t0 = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$OS_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==4] #mol COD/ L   
    
    #changed 12.6.25
    DETRITUS_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==4) %>%#corrected 21.6.25
      dplyr::select(OS_mol_COD_L) %>%
      dplyr::filter(!is.na( OS_mol_COD_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht DETRITUS_gr4_t0 <- DETRITUS_gr4_t0_all[[1]][1]
    
    
    BOC_gr4_t0      = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$BOC_mol_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==4] #mol COD/ L
    
    #changed 12.6.25
    BOC_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
      dplyr::filter(kmeans4gr ==4) %>%#corrected 21.6.25
      dplyr::select(BOC_mol_L) %>%
      dplyr::filter(!is.na( BOC_mol_L)) 
    #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht BOC_gr4_t0 <- BOC_gr4_t0_all[[1]][1]
    
    
    if(scenario_with_1_or_without_0_MO == 1) {
      MO_het_gr4_t0   = chem_w_dat_ordered_per_date_1978_1981_mean_per_group$total_Prok_mol_COD_L[chem_w_dat_ordered_per_date_1978_1981_mean_per_group$kmeans4gr ==4] 
      
      #changed 12.6.25
      MO_het_gr4_t0_all = chem_w_dat_ordered_per_date_1978_1981 %>%
        dplyr::filter(kmeans4gr ==4) %>%#corrected 21.6.25
        dplyr::select(total_Prok_mol_COD_L) %>%
        dplyr::filter(!is.na( total_Prok_mol_COD_L)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht MO_het_gr4_t0 <- MO_het_gr4_t0_all[[1]][1]
      
      #carrying capacity assumed is the max MO_het abundance measured plus 10% which never are seen in the field due to grazing, "mortality", etc.
      MO_het_CC_gr4 <- chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$total_Prok_mol_COD_L_max[chem_w_dat_ordered_per_date_1978_1981_mean_per_group_max$kmeans4gr ==4] *factor_CC_MO
    }else{
      MO_het_gr4_t0  = 0
      MO_het_CC_gr4 <- 0
    }
    
    if(scenario_with_1_or_without_0_fauna == 1) {
      fauna_gr4_t0   = fauna_deep_PerSamplPerTaxon_bm_mean_per_group $bm_mol_COD_perL [fauna_deep_PerSamplPerTaxon_bm_mean_per_group$kmeans4gr ==4] 
      
      fauna_gr4_t0_all = fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
        dplyr::filter(kmeans4gr ==4) %>%
        dplyr::select(bm_mol_COD_perL) %>%
        dplyr::filter(!is.na( bm_mol_COD_perL)) 
      #22.6.25 das war ein FEhler - sowas trifft nur auf linear model zu, aber das ist hier nicht fauna_gr4_t0 <- fauna_gr4_t0_all[[1]][1]
      
      fauna_CC_gr4 <- fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$bm_mol_COD_perL_max [fauna_deep_PerSamplPerTaxon_bm_mean_per_group_max$kmeans4gr ==4] *factor_CC_MO
    }else{
      fauna_gr4_t0   = 0
      fauna_CC_gr4 <- 0
    }
    
    CC_table_MO <- as.data.frame(cbind(group = c(1:4), CC = c(MO_het_CC_gr1, MO_het_CC_gr2, MO_het_CC_gr3, MO_het_CC_gr4)))
    CC_table_fauna <- as.data.frame(cbind(group = c(1:4), CC = c(fauna_CC_gr1, fauna_CC_gr2, fauna_CC_gr3, fauna_CC_gr4)))
    
    return(list(Fulda_daily_prec, Fulda_daily_temp_, chem_w_dat_ordered_per_date_1978_1981, chem_w_dat_ordered_per_date_1978_1981_mean_per_group, fauna_deep_PerSamplPerTaxon_bm_mean_per_group, t_0, t_max, DETRITUS_gr1_t0, DETRITUS_gr2_t0, DETRITUS_gr3_t0, DETRITUS_gr4_t0, BOC_gr1_t0, BOC_gr2_t0, BOC_gr3_t0, BOC_gr4_t0, MO_het_gr1_t0, MO_het_gr2_t0, MO_het_gr3_t0, MO_het_gr4_t0, fauna_gr1_t0, fauna_gr2_t0, fauna_gr3_t0, fauna_gr4_t0, fauna_deep_PerSamplPerTaxonWide_bm_sum, CC_table_MO, CC_table_fauna) )
}

