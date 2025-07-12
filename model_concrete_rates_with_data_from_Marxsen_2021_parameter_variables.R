
# function which returns ca. 30 variables 

parameter_variables<- function(run){
  
  #library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(seas)#mkseas
  library(DBI)#dbConnect
  library(openxlsx)#read.xlsx
  #library(units)#year
  #stattdessen:
  library(DescTools)
  library(patchwork) #for sticking together different plots - should I not rather do ggarrange?
  library(deSolve) #from  Soetaert Hermann. 

   setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain")
  parvar <- read.xlsx("parameters_variables.xlsx", startRow = 3)
  
  
  
  delta_t          = parvar$dt[run] #days
  
  max_t          = as.Date(parvar$max_t[run], origin = "1899-12-30") #date - although I am NOT on windows .. 
  
  #depth of gw aquifer
  #TODO 21.1.25 sould not need this. 24.1.25 yes, need it
  aquifer_depth <- parvar$aquifer_depth[run] #m
  
  #1.1.25
  # maintain_MO_het  <- 0.1 # FACTOR to multiply with existing biomass 1.1.25 p. 43 Soetaert hermann. with 0.6, this includes grazing
  # #TODO 19.1.25 however, in NPZD (see separate file same folder) no maintenance - and also no yield
  # #import_MO_het <-   0.00000001 # mol / L happens regardless of existing biomass
  #TODO I leave this here as a reminder that at some point it might be good to have import
  
  import_MO_het <- parvar$import_MO_het[run] #m
  
  scenario_with_1_or_without_0_fauna <- parvar$scenario_with_1_or_without_0_fauna[run]  #0#1 #choose 1 for with fauna, or 0 for without fauna - for comparing scenarios
  #19.1.25 I do fauna, but no import #18.4.25 this was outcommented, but I need it
  
  
  #scenario_with_1_or_without_0_MO <- parvar$scenario_with_1_or_without_0_MO[run]  
  
  carboxylic_acids_fraction <- parvar$carboxylic_acids_fraction[run]  # = .08 # Thurman 1985 Fig. 4.1
  
  acetic_acids_fraction <- parvar$acetic_acids_fraction[run]  #  #assuming acetate makes up 1 % of carboxylic acids
  
  #2.1.25
  # maintain_fauna  <- 0.7 # FACTOR 2.1.25 same as maintain_MO_het #20.1.25 I leave that here because I might want to come back to that
  #19.1.25 in NPZD (see separate file same folder) split into secretion --> cod = BOC and mortality --> detritus . see below
  #0.05 worked before clena up       #.4 /(molCOD/L)/day   19.1.25 worked in NPDZ
  mortalityRate <- parvar$mortalityRate[run]  # 
  
  #20.1.25 I leave that here because I might want to come back to that
  # if(scenario_with_1_or_without_0_fauna == 1) {
  #   import_fauna <- 0 #changed on 19.1.25 - should get rid of all import 0.0000001 # mol / L happens regardless of existing biomass. 2.1.25 same as maintain_MO_het
  # }else{#Scenario without fauna
  #   import_fauna <-   0 # mol / L happens regardless of existing biomass. 2.1.25 same as maintain_MO_het
  # }
  import_fauna <- parvar$import_fauna[run] 
  
  
  # taking the most competitive acetate degrader as of Schmidt et al. 2018
  #maximal specific growth rate 0.43 h-1, half- saturation constant of 4.3 microM [19]
  yield_ac <- parvar$yield_ac[run] # gdrymass_MO  gacetate^-1 Gerritse et al. (1992)
  #TODO 31.1.25 but should I convert that to mole ?! how ?! and how to convert to g COD = BOC / g COD ?!   0.468909657 see Table_ALL_Parameter_model_7dec17_kmaxstar.xlsx
  
  
  
  
  # gdrymass_fauna gdrymass_M
  #24.1.25 this is a very sensitive factor - with .3, carbon and fauna explode. With .1 and .15, everythign decreases to infisemially low values. with .2 , .18, .17 also too much stuff increasing
  yield_MO <- parvar$yield_MO[run] #.3 #18.1.25 introduec #18.1.25 here setting  , in contrast to above, and tatally without source . this worked in NPZD (see separate file same folder)
  #TODO need source !!
  
  #k_temp <- 30 # temperature in Gerritse et al. (1992)
  k_temp <- parvar$k_temp[run]
  
  # K_ac  = 1.01E-01 #	Michaelis Menten half-saturation coefficient for acetate oxidation	1.01E-01	g acetate * m-3	Gerritse et al. (1992),, i.e. at 30 degrees
  # #23.1.25 thus
  # K_ac  = 1.01E-04 #	Michaelis Menten half-saturation coefficient for acetate oxidation	1.01E-01	g acetate / L at k_temp
  # #23.1.25 but I need mole COD
  # K_ac  = 1.01E-04 /60 /32 #dividing by mole mass of acetate to get mole, then mole COD / L by dividing by mole mass of O2. 
  # #24.1.25 that is wrong - that is mole acetate per L divided by mole mass 32, but that does not give mole COD / L !
  
  #24.1.25
  # K_ac  = 1.01E-04 *1.08 /32 # *1.08 is g COD per g acetate; then mole COD / L by divicing by mole mass of O2
  # #COD of acetate
  # #on p. 1468 in Gerritse: 4.3 microM
  # K_ac  = 4.3*60 /1000000 *1.08 /32 # first calculate microg/L by multiplying with molar mass of acetate, then to g by dividing by 10^6, multiplying wth 1.08 to get g COD / L, then dividing by molar mass of O2 to get mol COD / L. but that's then the same order of mag as before - will not work. Hm, maybe it will just amke the difference ?
  # #experiment K_ac  = .0001
  K_ac <- parvar$K_ac[run]
  
  
  # K_MO = K_ac #1.01E-04 #	Michaelis Menten half-saturation coefficient for MO uptake by fauna should be mole COD / L
  # #TODO GET SOURCE !!!
  # #fauna does not survive at 30 degrees - no adjustment necessary. it is either this value or none : until I have temperature-dependent kinetics
  # K_MO_at_temp = #1.01E-03  #ksGrazing      in NPZD (see separate file same folder)   
  #   .05#0.005208333, #K_ac,   # molCOD/L  #K_MO_at_temp  0.00101 , #19.1.25 this worked at NPZD (see separate file same folder)
  # #need less limiting in Johannesaue
  
  # reculculation from 30 degree from Gerritse to 10 degree by factor 0.392 as in Schmidt 2018 # 17.4.25 this is a rough estimate and therefore is not temperature-adjusted at this stage
  #experiment K_MO_at_temp = .01
  K_MO_at_temp <- parvar$K_MO_at_temp[run]
  
  #microbe maximum growth rate
  # per h # #micro ac 	Rate constant for bacterial uptake and oxidation of acetate	Gerritse et al. (1992) ****, ASSUMING COD = BOC CAN BE DEGRADED LIKE ACETATE, which is wrong !!
  rMO_BOC_uptake_h_at_lab_temperature <- parvar$rMO_BOC_uptake_h_at_lab_temperature[run]
  
  # per day # #micro ac 	Rate constant for bacterial uptake and oxidation of acetate	4.7E-05	s-1	Gerritse et al. (1992) ****
  #that's at 30 degrees, I say in teh 2018 paper   Comamonas testosterone  specific growth rate back calculated to  10degC (4.7 * 10-5 s-1)
  rMO_BOC_uptake_per_day_at_lab_temperature <- parvar$rMO_COD_uptake_per_day_at_lab_temperature[run]
  
  #TODO 23.1.25 at this stage, microbes only somewhat stay the same if rMO_BOC_uptake_per_day_at_lab_temperature is a hundredths of what it should be 
  #no 24.1.25 by accident this had not been implemented and the MO just stay the same rMO_BOC_uptake_per_day_at_lab_temperature = rMO_BOC_uptake_per_day_at_lab_temperature/100
  
  #4.1.25 groundwater fauna does not survive at 30 degrees - see numouers pubs. Therefore, take the maximum growth rate rate given and assume that it is valid within the narrow band of temperatures in gw
  #*.1 #.8#1 does not work,#0.5321782, #18.1.25 factor 3 works, with factor 5 the model crashes #1.709556394,#0.5321782,#1.0,       # /day  #19.1.25 .7 worked in NPZD (see separate file same folder) #but then Johannesaue fauna not doing enoug, thus 0.8
  rFauna_MO_uptake_per_day_at_TEMP <- parvar$rFauna_MO_uptake_per_day_at_TEMP[run]
  #15.6.25 however, using Di Lorenzo and Navel, see SI, we calculate - nee, halt
  
  # #4.1.25 maintenance from fauna must be deduced at the beginning of each run so that - no , must bu deduced AFTER it has munched on microbes
  # maintain_fauna  <- 0.95 #  #20.1.25 I leave that here because I might want to come back to that
  
  
  # #however, ther was also the value 
  # rMO_BOC_uptake_sec_at_30_deg       = 4.7E-05  # BUT THT I AT 10 DEGREES !!!thus not useful here
  # rMO_BOC_uptake_per_day_at_lab_temperature       = rMO_BOC_uptake_sec_at_30_deg*24*60*60
  
  #20.1.25 leaving this here because I might have to think about this some more
  # #Maritn wanted back then the kmax instead of ymax, with the yield 0.353  gdrymass·gacetate−1 Gerritse et al. (1992)
  # #thus kmax,ac Specific reaction rate for bacterial oxidation of acetate  13·10−5  gacetate·gdrymass−1·s−1
  # #kmax_ac = mu_ac_d/yield_ac #specific reaction rate for bacterial oxidation of acetate per day
  # #mu_ac is rMO_DOC_uptake_per_day_at_lab_temperature, correct?  at thsi stage, kmax_ac is not used directly
  # kmax_ac = rMO_BOC_uptake_per_day_at_lab_temperature/yield_ac #specific reaction rate for bacterial oxidation of acetate per day
  #  scaling done below
  
  #schmidt et al. 2018: acetate degradation were derived as follows. The kinetics were measured at 30°C [19] = GErritse 1992. The measured specific growth rate and KS were back calculated to the values at a groundwater temperature of 10degC as described in SI 1, i.e., both parameters drop to 0.392-fold their original values when the temperature drops by 20degC (SI Table 2). We assume that the specific substrate affinity defined by Button [20]  as a0 = mumax * KS i.e. the initial slope of the Monod kinetics, does not change with temperature. The kmax value in Table 1 in the main paper (1.33*10-4 gacetate * gdrymass-1 * s-1) was calculated from the specific growth rate at 10degC (4.7 * 10-5 s-1) and the yield (0.353 gdrymass·gacetate-1) for Comamonas testosterone [19].We assume that the yield does not change with temperature.
  
  
  ##########
  #fractions
  ##########
  #fraction of DETRITUS  becoming COD  = BOC -  assumption: independent of temperature
  
  # with NPZD (see separate file same folder) worked:
  #k1 = .001 # #mineralisationRate =.001#0.1#,       # /day
  #19.1.25 there is too much detritus, thus
  #worked before .1 # #mineralisationRate =.001#0.1#,       # /day
  k1 <- parvar$k1[run]
  #excretionRate = 0.01  # /day # from NPZD (see separate file same folder)
  excretionRate <- parvar$excretionRate[run]
  
  
  
  ##########
  #rates
  ##########
  
  
  #TODO 16.4.25 der hier wurde doch gaerde vorhin noch eingelesen - aber den gibts doch gar nicht im excel file ? neu eingefuegt . 17.4. das ist acetate - aber der Rest geht auf humic acid ?! 17.4.25 jetzt alles anders - jetzt ist der hier auf molmasse von TOC basierend
  TOC_COD_mol_m2_yr_precipitation <- parvar$TOC_COD_mol_m2_yr_precipitation[run] #now mol COD / m2 / year based on acetate -  17.4.25  should not continue to use. DOCH, ejtzt schon, ab run 18
  
  #I need Plesne preciptaiotn here, to deal with Kopacek TOC in precipitation, and that is ca. 1000 mm
  #mean of 1402, 1437 Kopacek et al. 2009 Canopy leaching
  #mean(c(1402, 1437))
  # average_precipitation_mm_yr = mean(c(1402, 1437)) #not relevant here Fulda_daily_prec_per_year_average #mm / yr * m^2  bzw. L /  year
  # average_precipitation_L_per_m2per_yr = average_precipitation_mm_yr #L /  year  # hmm - but I could express that also as L / m2 per year, right?
  # #average_precipitation_m_per_yr = average_precipitation_m_per_m2_per_yr = average_precipitation_mm_yr /1000 # m/year
  #frame 2.2.25 as  
  average_precipitation_mm_yr <- parvar$average_precipitation_mm_yr[run]
  #3.6.25 aus irgedienem Grund wird das nich tals Zahl eingelesen
  average_precipitation_mm_yr <- as.numeric(average_precipitation_mm_yr)
  average_precipitation_m_yr<- average_precipitation_mm_yr/1000
  
  #174.25 ersetzt durch oberes average_precipitation_L_per_m2per_yr <- parvar$average_precipitation_L_per_m2per_yr[run] #L / m2 per year
  
  #  resulting in #mol per L per m^2 
  #done in excel TOC_COD_mol_L_precipitation = TOC_COD_mol_m2_yr_precipitation/average_precipitation_L_per_m2per_yr
  #reframe 2.2.25 as  TOC_COD_mol_L_precipitation = TOC_COD_mol_m2_yr_precipitation/ average_precipitation_mm_yr
  
  # TODO 17.4.25 die drei Zeilen in parameter_variables_: was ist korrekt?
  #   TOC_COD_mol_L_precipitation = TOC_COD_mol_m2_yr_precipitation/ average_precipitation_L_per_m2per_yr
  # #17.4.25 in excel implemented as 
  # TOC_COD_mol_L_precipitation = TOC_COD_mol_L_yr_precipitation/ average_precipitation_L_per_m2per_yr
  #reframe 2.2.25 as  
  #TOC_COD_mol_m3_precipitation = TOC_COD_mol_m2_yr_precipitation/ average_precipitation_m_yr #17.4.25 der hier wird nicht genutzt, nicht in excel _old, aber jetzt doch
  #TOC_COD_mol_m3_precipitation = TOC_COD_mol_m2_yr_precipitation/ average_precipitation_m_yr
  
  #17.4.25 this one is used! not anymore
  # TOC_COD_mol_L_precipitation <- parvar$TOC_COD_mol_L_precipitation[run]#TODO this is HUGE NEW error source !!! this was factor 1000 wrong ! here, for deriving conc. necessary to divice by prciaption s divided instead of multiplied ! added facgro 1000 - or is that compensated below ?! then rename variable
  #noch mal: (mol/m2/yr)  / (mm/m2/yr) = mol / mm   and by multiplying iwth 1000    mol COD / m 
  #24.1.25: mole / m^2 / year / (L/m^2 / year) = mole / m^2 / year / L *m^2 * year = mole  / L 
  #17.4.25 changed to
  TOC_COD_mol_m3_precipitation <- parvar$TOC_COD_mol_m3_precipitation[run]
  #2.6.25 der REst ist in mol / L, also. Wobei, halt, der recharge kommt erst noch - also erst mal alles pro m3 lassen
#  TOC_COD_mol_L_precipitation <- parvar$TOC_COD_mol_L_precipitation[run]
  
  #2.2.25 # see excel file, here just for easier calculation
  #TOC_COD_mol_L_precipitation <- 0.0436948656*1000 #10 times, 100 times  #mole  / L 
  #TODO 16.4.25 das verstehe ich nicht - der Fkator wuerde auch in excel file ghoeren ?! erst mal die Zeile auskommentiert
  #2.6.25 wo kam dieser Faktor her??
  
  
  #DETRITUS recharge in dependence on precipitation
  #fraction of precipitation mobilising detritus into subsurface
  # through soil passage, the DOC from precipitation is degraded, but detritus is mobilised 
  #1.1.25 lets assume x times as much detritus mobilized as TOC from preciptation
  factor_how_many_times_Detritus_compared_to_TOC <- parvar$factor_how_many_times_Detritus_compared_to_TOC[run]
  #outcommmented 17.4.25 to go to m3 Detritus_COD_mol_L_precipitation = factor_how_many_times_Detritus_compared_to_TOC*TOC_COD_mol_L_precipitation #mol per L per m^2 per year. nee, 2.2.25 mole  / L , but we know that it relates to a year
  #17.4.25 I now calculate that in ecxel, for tractabiltiy.
    #2.2.25 reframe
   # Detritus_COD_mol_m3_precipitation = factor_how_many_times_Detritus_compared_to_TOC*TOC_COD_mol_m3_precipitation 
  #done in excel:
  Detritus_COD_mol_m3_precipitation = parvar$Detritus_COD_mol_m3_precipitation[run]
  #4.6.25 replaced by the liter-wise value. however, below the cubic meter scale one is still needed
  Detritus_COD_mol_L_precipitation = parvar$Detritus_COD_mol_L_precipitation[run]
  
  #TODO 17.4.25 however, I should probabl also then derive RECHARGE in excel ?! or document
  
  #Fulda_daily_prec$RS # unit looks like daily mm
  
  #redo 29.12.24 from Kopacek 2008 data
  #RECHARGE_COD_mol_per_L_per_day_and_area        = (Detritus_COD_mol_L_precipitation + TOC_COD_mol_L_precipitation)* Fulda_daily_prec$RS/1000 
  # mol / L * mm/day *1000  = mol / L / day * mm
  
  #22.1.25 alternatively # to get from mol / L / day * mm to moles / L / day ?!
  # RECHARGE_COD_mol_per_m2_per_day        = ifelse(Fulda_daily_prec$RS >0, (TOC_COD_mol_L_precipitation + Detritus_COD_mol_L_precipitation)/365 * Fulda_daily_prec$RS/1000 , 0) # 17.4.25 that was wrong - adding conc per L and getting conc per m2
  # #TODO how do others do that ???
  # #24.1.25   mol / L * L / m^2 / day =   mol / m^2 / day
  # #2.2.25 this is now mol / L  / (365 days ) * mm /1000  = mol / L / day * m  = mol / L / day * m. 2.2.25 heißt noch mol per m2, but mol / L * m is mol / m2 / 1000
  # TODO in  parameter_variables_: was ist korrekt?
    #2.2.25 reframe, with Fulda_daily_prec$RS being in mm, thus, dividing b 1000 to get m
    RECHARGE_COD_mol_per_m2_per_day        = ifelse(Fulda_daily_prec$RS >0, (TOC_COD_mol_m3_precipitation + Detritus_COD_mol_m3_precipitation)/365 * Fulda_daily_prec$RS/1000 , 0) # der hier ist wirklich mol per m2


  
  #dividing that up into aquifer depth of aquifer_depth in m
  # RECHARGE_COD_mol_per_L_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth / 1000 # mol / m^2  / d / m/ 1000 = mol / m^3  / d/ 1000 = mol / L / d
  # #corrected 2.2.25 
  # RECHARGE_COD_mol_per_L_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth 
  # #  mol / L / day  * m / m  = mol / L / d 
    #reframe 2.2.25 from the second line deriving RECHARGE_COD_mol_per_m2_per_day
    RECHARGE_COD_mol_per_m3_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth 
  #  mol / L / day  * m / m  = mol / L / d 
  #to get to the mol / L as for the other concentrations, now
  # RECHARGE_COD_mol_per_L_per_day <- RECHARGE_COD_mol_per_m3_per_day / 1000 #laeuft aufs selbe hinaus - der recharge ist groessenordung 10^-7 per day
  # 
  # RECHARGE_COD_mol_per_L_per_day_df <- data.frame(cbind("dateRi" = as.Date(Fulda_daily_prec$dateRi), RECHARGE_COD_mol_per_L_per_day))
  # RECHARGE_COD_mol_per_L_per_day_df$dateRi <-Fulda_daily_prec$dateRi

    #reframe 17.4.25
  RECHARGE_COD_mol_per_m3_per_day_df <- data.frame(cbind("dateRi" = as.Date(Fulda_daily_prec$dateRi), RECHARGE_COD_mol_per_m3_per_day))
  
  RECHARGE_COD_mol_per_m3_per_day_df$dateRi <-Fulda_daily_prec$dateRi

  growth_model_MO_type <- parvar$growth_model_MO[run]
  growth_model_fauna_type <- parvar$growth_model_fauna[run]
  
  mortalityFraction_per_degree <- parvar$mortalityFraction_per_degree[run]
  
    #TODO temperature dependence - or IS THERE a temperature dependence ?? only with snow
  
  #8.6.25 not used at this stage - works without
  microbe_loss_factor_when_no_fauna <- parvar$microbe_loss_factor_when_no_fauna[run]
  
  return(list(delta_t, max_t, aquifer_depth, import_MO_het, scenario_with_1_or_without_0_fauna, scenario_with_1_or_without_0_MO, carboxylic_acids_fraction, acetic_acids_fraction, mortalityRate, import_fauna, yield_ac, yield_MO, K_MO_at_temp, rMO_BOC_uptake_per_day_at_lab_temperature, rFauna_MO_uptake_per_day_at_TEMP, k1, excretionRate, TOC_COD_mol_m2_yr_precipitation,      RECHARGE_COD_mol_per_m3_per_day_df, k_temp, K_ac, growth_model_MO_type, growth_model_fauna_type, mortalityFraction_per_degree, microbe_loss_factor_when_no_fauna))
}

