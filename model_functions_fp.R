##########
#reactions
##########


# physical degradation of detritus to biologically degradable Carbon (BDC), from detritus 
dCOD_from_detritus_dt    <- function(k1, DETRITUS_ti_minus_1 ) {
  COD_import_from_detritus_ti <- k1*DETRITUS_ti_minus_1   
  return(COD_import_from_detritus_ti)
}


# detritus at time step is the result of Detritus from the preious time step, plus detritus recharged from precipitation minus what is broken down to BDC, plus Import from freshly dead biomass (mortality). 
dDETRITUS_dt    = function(k1, DETRITUS_ti_minus_1 ,RECHARGE_COD_mol_per_L_per_day_df_ti , Mortality) {
  Detritus_broken_down_ti <- dCOD_from_detritus_dt(k1, DETRITUS_ti_minus_1)
  DETRITUS_ti <- DETRITUS_ti_minus_1 - Detritus_broken_down_ti + RECHARGE_COD_mol_per_L_per_day_df_ti + Mortality 
  return(DETRITUS_ti)
}

# rMO_COD_uptake_per_day is temperature dependent, and needs to be rescaled depending on what the temperature was at which the rate was determined originally
d_COD_MO_het_uptake_dt_per_day_per_temperature    = function(rMO_COD_uptake_per_day_at_lab_temperature, GWTEMP_ti, lab_temp) {
  # change the rate by the factor by which it deviates from the rate measured at lab temperature 
  TEMP_related_to_lab_temperature = lab_temp - GWTEMP_ti
  rMO_COD_uptake_per_day_at_TEMP <- rMO_COD_uptake_per_day_at_lab_temperature  / (TEMP_related_to_lab_temperature * 0.9696) #0.9696  is the factor in Schmidt et al. 2018 to calculate respective temperature influence in 1 degree steps - assuming boldly that such relationship is directly transferable
  
  return(rMO_COD_uptake_per_day_at_TEMP)   }


# make K_ac temperature dependent
d_K_ac_per_temperature    = function(K_ac, lab_temp, GWTEMP_ti) {
  
  # change the rate by the factor by which it deviates from the rate measured at lab temperature 
  TEMP_related_to_lab_temperature = lab_temp-GWTEMP_ti
  K_ac_at_TEMP <- K_ac  / (TEMP_related_to_lab_temperature * 0.9696) #0.9696  is the factor in Schmidt et al. 2018 to calcualte respective temperature in 1 degree steps
  
  return(K_ac_at_TEMP)
}




# COD import from precipitation - with preciptaiton-dependent recharge of detritus. 26.12. 24 das stimmt aber nicht mehr
#TODO this shoul dnever be more than 1, becuase COD cannot grow here, it can only decrease
##    make MM a value that is called by DOC uptake an dby microbe growth 
#TODO 20.4.25 THIS is used! not the one below
dCOD_degradation_factor_dt    = function(COD_ti_minus_1,   rMO_COD_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g) {
  
  #  #das folgende muss MM sein ! also, als erste Naeherung ?! oder nehme ich erst mal linear ?!
  #  DOC_degraded_fraction_i <- #30.11. this was in above fct, but does not belong there 
  #    #unit of rMO_DOC_uptake_per_day_at_TEMP is per second , i.e. times DOC divided by biomass
  #   # d_DOC_MO_het_ti <- 
  #    rMO_DOC_uptake_per_day_at_TEMP * DOC_ti_minus_1 / MO_het_ti_minus_1   *dt #
  #   # /d * mol C/ L   d.h. mol C/ L /d, multiplied by time duration, so that just conc. - but it would be more elegant to have just mass here, because next thing I have to deduct this
  # #TODO 22.12.24 how to incorporate MO AND DOC? The fractin should be applied to DOC_ti_minus_1, done below
  #  #rate of DOC per biomass times biomass times time gives DOC. it is not a fraction per se, it is the moles
  #  #25.12.24  mass of DOC per biomass MO per day * mol MO =  mol DOC / time step calculated
  #  #26-12.NOPE . just fraction. dimensionless. will be multiplied with DOC_ti_minus_1 to get the fraction degraded which will be substracted from teh stock
  
  #26.12.24 I should never have done this linearly - it needs to be saturation, i.e. MM. or rather, it needs to be several steps - hab nen Haenger.
  #the fraction becomes all teh larger, the more MO. so I multiply by MO. The fraction does not care about DOC_ti_minus_1. but how to make it dimensionless?? dann hats halt doch unit mol / l
  # DOC_degraded_fraction_i <-
  #   rMO_DOC_uptake_per_day_at_TEMP *  MO_het_ti_minus_1   *dt #
  
  #TODO I need to make the degradation taxon-specific with the most competitive one as per Schmidt et al. 2018.
  #Comamonas testosteroni (maximal specific growth rate 0.43 h-1, half- saturation constant of 4.3 μM [19]).
  
  #Maritn wanted back then the kmax instead of ymax, with the yield 0.353  gdrymass·gacetate−1 Gerritse et al. (1992)
  #thus kmax,ac Specific reaction rate for bacterial oxidation of acetate  13·10−5  gacetate·gdrymass−1·s−1
  #TODO how to get dry mass from mole C? via density 
  #kmax <- rMO_COD_uptake_per_day_at_TEMP / yield_ac
  
  #27.12.24 aber in comSOL habe ich mti um_ac gerechnet - das war MM, oder ? aber wie?
  #29.12.24 ich MUSS hier mikrobio mit drin haben !! wenn ich sie schon nicht in der Rate ausrechne   
  #19.1.25 I add factor 0.1 because I assume that only 10 % of the microbes can use the kinetis
  #TODO the glucose vmax from Juergen concdrns the WHOLE community - impelment!  0.0025 too high. .00125 ok, but .001 is also ok, and does not chnage fauna
  #f_S_MO_Soetaert_2008 <- 0.001 * rMO_COD_uptake_per_day_at_TEMP *  MO_het_ti_minus_1 *(1 - MO_het_ti_minus_1/ K_ac_at_TEMP)*dt #per day times concentration divided by concentration divided by per day means unitless.
  #24.1.25 THAT WAS WRONG !!! how did I derive the bracket term ?! ah, was that still Lotka Volterra ?!  pattern: maxUptake * cod/(cod+ksCOD)*MO
  #24.1.25 not all is acetate, thus factor .01 *  - nema smysl
  if (growth_model_MO_type == "MM") {
    f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP *  COD_ti_minus_1 / (COD_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t
  }
  
  if (growth_model_MO_type == "L") {
    f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP *  (CC_group_MO_g - MO_het_ti_minus_1)/CC_group_MO_g *delta_t## mol / L TODO 19.1.25 where is  maintenance deduced? ther should be -maintain_MO_het*MO_het_ti_minus_1
  }
  
  if (growth_model_MO_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
    f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP * COD_ti_minus_1 / (COD_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t * (CC_group_MO_g - MO_het_ti_minus_1)/CC_group_MO_g *delta_t## mol / L  ??? hier substrat UND MO UND saturation. 
  }
  
  return(f_S_MO_Soetaert_2008) # mol/L ?!
  #TDO ich hantiere die ganze Zeit mit huic acid aber nehme ei Bakterium, das Acetat abbaut - also brauche iech eigneltich auch noch einen SChritt, dass HA in Ac umgebaut wird?! oder einfach fraction?
  
}



#16.4.25 combining that with yield and check that MO not negative
dCOD_MO_degradation_dt    = function(COD_ti_minus_1,   rMO_COD_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, yield_ac, f_S_fauna_Petzoldt_2018, delta_t, growth_model_MO_type, CC_group_MO_g) {
  f_S_MO_Soetaert_2008 <- dCOD_degradation_factor_dt(COD_ti_minus_1,   rMO_COD_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g)
  #  if (growth_model_MO_type == "MM") {
  #   f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP *  COD_ti_minus_1 / (COD_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t
  # }
  #
  # if (growth_model_MO_type == "L") {
  #   f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP *  (CC_group_MO_g - MO_het_ti_minus_1)/ CC_group_MO_g *delta_t## mol / L  #warte, da muss doch substrat UND MO drin sein ?! nee original L hat keine substrat abhängigkeit. dann nuetzt mir das im Ressourcen-getriebenen Modell ncihts - brauche ne Kombi
  # }
  # 
  # if (growth_model_MO_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
  #   f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP * COD_ti_minus_1 / (COD_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t * (CC_group_MO_g - MO_het_ti_minus_1)/CC_group_MO_g *delta_t## mol / L  ??? hier substrat UND MO UND saturation. 
  # }
  
  MO_growth <- yield_ac*f_S_MO_Soetaert_2008 # mol / L  #19.1.25 f_S_fauna_Petzoldt_2018 is equivalent to "grazing" 25.1.25 quatsch, this is C uptake
  
  MO_het_ti <- MO_het_ti_minus_1 + MO_growth - f_S_fauna_Petzoldt_2018 ## mol / L TODO 19.1.25 where is  maintenance deduced? ther should be -maintain_MO_het*MO_het_ti_minus_1
  
  if(MO_het_ti <= 0){
    MO_het_ti <- .00000000001 #MO_het_ti_minus_1 artificially say that MO do NOT vanish - they hide from the grazing presure. 24.1.25 however that feeds the fauna artificially. so take a very small value
  }else{
    MO_het_ti <- MO_het_ti #+ import_MO_het
  }
  
  return(list(MO_growth, MO_het_ti )) # mol/L COD ?!
  #TDO ich hantiere die ganze Zeit mit huic acid aber nehme ei Bakterium, das Acetat abbaut - also brauche iech eigneltich auch noch einen SChritt, dass HA in Ac umgebaut wird?! oder einfach fraction?
  
}

#16.4.25 combining that with yield and check that MO not negative and minimying difference to in situ
diff_insitu_dCOD_MO_degradation_dt    = function(COD_ti_minus_1,   rMO_COD_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, yield_ac, f_S_fauna_Petzoldt_2018, results_gr, delta_t) {
  
  #michaelis Menten type
  f_S_MO_Soetaert_2008 <- rMO_COD_uptake_per_day_at_TEMP *  COD_ti_minus_1 / (COD_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t
  
  MO_growth <- yield_ac*f_S_MO_Soetaert_2008 # mol / L  #19.1.25 f_S_fauna_Petzoldt_2018 is equivalent to "grazing" 25.1.25 quatsch, this is C uptake
  
  MO_het_ti <- MO_het_ti_minus_1 + MO_growth - f_S_fauna_Petzoldt_2018 ## mol / L TODO 19.1.25 where is  maintenance deduced? ther should be -maintain_MO_het
  
  if(MO_het_ti <= 0){
    MO_het_ti <- .00000000001 #MO_het_ti_minus_1 artificially say that MO do NOT vanish - they hide from the grazing presure. 24.1.25 however that feeds the fauna artificially. so take a very small value
  }else{
    MO_het_ti <- MO_het_ti #+ import_MO_het
  }
  
  #diff_insitu_COD <- abs(results_gr$MO_het[i] - 
  
  #das ist ein Widerspruch - ich kann nicht einersetis Messwerte von irgendwo her holen und dann andererseits Raten pro tag benutzen
  #Moeglichkeiten: 1) extrapolate from one time point to the next to daily, aber wo soll dann die Dynamik her kommen?
  #2) take only the in situ values COD and MO that I have, ca. monthly ?! fit rates for monthly
  
  return(MO_het_ti) # mol/L ?!
  #TDO ich hantiere die ganze Zeit mit huic acid aber nehme ei Bakterium, das Acetat abbaut - also brauche iech eigneltich auch noch einen SChritt, dass HA in Ac umgebaut wird?! oder einfach fraction?
  
}

#MM. at this stage. 27.4.25 not anymore only MM
dMO_fauna_degradation_factor_dt    = function(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g) {
  
  if (growth_model_fauna_type == "MM") {
    #  #das folgende muss MM sein ! also, als erste Naeherung ?!
    f_S_fauna_Petzoldelta_t_2018 <- rFauna_MO_uptake_per_day_at_TEMP *  MO_het_ti_minus_1/(MO_het_ti_minus_1 + K_MO_at_temp) *Fauna_ti_minus_1 *delta_t #per day times concentration divided by concentration divided by per day means unitless.
    #yield is multiplied with in  function dMO_fauna_uptake_dt - no tmissing here
  }
  
  if (growth_model_fauna_type == "L") {
    f_S_fauna_Petzoldt_2018 <- rFauna_MO_uptake_per_day_at_TEMP *  (CC_group_fauna_g - MO_het_ti_minus_1)/CC_group_fauna_g *delta_t## mol / L  #warte, da muss doch substrat UND MO drin sein ?! nee original L hat keine substrat abhängigkeit. dann nuetzt mir das im Ressourcen-getriebenen Modell ncihts - brauche ne Kombi
  }
  
  if (growth_model_fauna_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
    f_S_fauna_Petzoldt_2018 <- rFauna_MO_uptake_per_day_at_TEMP * MO_het_ti_minus_1 / (MO_het_ti_minus_1 + K_MO_at_temp)* Fauna_ti_minus_1 *delta_t * (CC_group_fauna_g - Fauna_ti_minus_1)/CC_group_fauna_g *delta_t## mol / L  ??? hier substrat UND MO UND saturation. 
  }
  
  
  return(f_S_fauna_Petzoldt_2018) # unitless
  #TODO ich hantiere die ganze Zeit mit humic acid aber nehme ei Bakterium, das Acetat abbaut - also brauche iech eigneltich auch noch einen SChritt, dass HA in Ac umgebaut wird?! oder einfach fraction?
  
}


# growth of fauna
#2.1.25 just adapted from MO
#4.1. LOtka volterra   18.1.25 can be deleted from here No, but it also is not LV ?!
#21.4.25 not used
dFauna_dt    = function(f_S_fauna_Petzoldt_2018, Fauna_ti_minus_1 , yield_MO, #maintain_fauna,   #19.1.25 maintain_fauna now replaced by excration towards COD and mortality towards detritus
                        Excretion , Mortality) {
  fauna_growth <- f_S_fauna_Petzoldt_2018 * yield_MO # mol / L
  # 29.12.24 MO_growth should also be able to be negative - HOW? decay term. decay_MO_het keep constant for now. could also be called maintenance. In fact, call it maintenance ?!
  #1.1.25 make maintenance a tenth of the biomass
  #TODO 19.1.25 not done yet? added substraction of excretion
  Fauna_ti <- Fauna_ti_minus_1 + fauna_growth - Excretion - Mortality
  
  return(Fauna_ti, )
}


#like for microbes, make one fct from this. used
dMO_fauna_uptake_dt      = function(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g, yield_MO, Excretion , Mortality) {
  
  f_S_fauna_Petzoldt_2018 <- dMO_fauna_degradation_factor_dt (MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g) 
  
  # #  #das folgende muss MM sein ! also, als erste Naeherung ?!
  # if (growth_model_fauna_type == "MM") {
  #   #  #das folgende muss MM sein ! also, als erste Naeherung ?!
  #   f_S_fauna_Petzoldt_2018 <- rFauna_MO_uptake_per_day_at_TEMP *  MO_het_ti_minus_1/(MO_het_ti_minus_1 + K_MO_at_temp) *Fauna_ti_minus_1 *delta_t #per day times concentration divided by concentration divided by per day means unitless.
  #   #TODO 18.1.25 yield missing !!
  #   #* yield_ac
  #   #und uach noch mit yield multiplizieren oder? aber yield muss doch eiglt kleiner 1 sein? nee, beim Abbau brauche ich yield nicht, erst bim Wachstum
  # }
  # 
  # if (growth_model_fauna_type == "L") {
  #   f_S_fauna_Petzoldt_2018 <- rFauna_MO_uptake_per_day_at_TEMP *  (CC_group_fauna_g - MO_het_ti_minus_1)/CC_group_fauna_g *delta_t## mol / L  #warte, da muss doch substrat UND MO drin sein ?! nee original L hat keine substrat abhängigkeit. dann nuetzt mir das im Ressourcen-getriebenen Modell ncihts - brauche ne Kombi
  # }
  # 
  # if (growth_model_fauna_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
  #   f_S_fauna_Petzoldt_2018 <- rFauna_MO_uptake_per_day_at_TEMP * MO_het_ti_minus_1 / (MO_het_ti_minus_1 + K_MO_at_temp)* Fauna_ti_minus_1 *delta_t * (CC_group_fauna_g - Fauna_ti_minus_1)/CC_group_fauna_g *delta_t## mol / L  ??? hier substrat UND MO UND saturation. 
  # }
  
  #Fauna_ti <- dFauna_dt(f_S_fauna_Petzoldt_2018 , Fauna_ti_minus_1 , yield_MO,# maintain_MO_het_per_group
  #                      Excretion , Mortality) 
  #as above, I need these two lines separately
  fauna_growth <- f_S_fauna_Petzoldt_2018 * yield_MO # mol / L  # f_S_fauna_Petzoldt_2018 grazing. 20.4.25 das ist nicht mol / L, das ist unit less, da fraction
  #20.4.25 corrected to 
  #fauna_growth <- f_S_fauna_Petzoldt_2018 * yield_MO *MO_het_ti_minus_1 # JETZT ist es mol / L  # f_S_fauna_Petzoldt_2018 grazing
  # 29.12.24 MO_growth should also be able to be negative - HOW? decay term. decay_MO_het keep constant for now. could also be called maintenance. In fact, call it maintenance ?!
  #1.1.25 make maintenance a tenth of the biomass
  #TODO 19.1.25 not done yet? added substraction of excretion
  Fauna_ti <- Fauna_ti_minus_1 + fauna_growth - Excretion - Mortality
  
  # 
  if(Fauna_ti < 0){ 
    Fauna_ti <- .000000001#Fauna_ti_minus_1  #artificially say that Fauna does NOT vanish - they hide from dying. 24.1.25 however that feeds the fauna artificially. so take a very small value
  }else{
    Fauna_ti <- Fauna_ti #+ import_fauna
  }
  # }else{
  #   Fauna_ti <- 0 #added 18.4.25
  # }
  
  return( list(fauna_growth, Fauna_ti )) # unitless. 21.4.25 qutatsch. das sollte mol COD / L sein
  #TDO ich hantiere die ganze Zeit mit humic acid aber nehme ei Bakterium, das Acetat abbaut - also brauche iech eigneltich auch noch einen SChritt, dass HA in Ac umgebaut wird?! oder einfach fraction?
  
}


# growth of MO 
#1.1.25 ich schmeiss .. nee, oder ? die maintenance haengt von der biomasse ab !!! ist ein faktor s. p. 43 in Soetaert Hermann
#4.1.25 maintenance must happen AFTER it degraded - but I think this is ok here -  wait - the degradation happens with the biomass of the before-last time step anyway, thus, it does not matter when I deduce maintenance - it reduces the biomass which degrades in the next time step anyway - so how do I allow for channeling of C ?
#21.4.25 not used - redundant at this stage
dHETMO_dt    = function(f_S_MO_Soetaert_2008, MO_het_ti_minus_1 , yield_ac, #maintain_MO_het, 
                        f_S_fauna_Petzoldt_2018#, Fauna_ti_minus_1
) {
  MO_growth <- yield_ac*f_S_MO_Soetaert_2008 # mol / L  #19.1.25 f_S_fauna_Petzoldt_2018 is equivalent to "grazing"
  
  # 29.12.24 MO_growth may also be negative -  decay term.  maintenance - and grazing
  MO_het_ti <- MO_het_ti_minus_1 + MO_growth - f_S_fauna_Petzoldt_2018 ## mol / L TODO 19.1.25 where is  maintenance deduced? ther should be -maintain_MO_het*MO_het_ti_minus_1
  
  return(MO_het_ti)
}




# using function physical degradation of detritus to COD , added on what is present
#TODO here it can happen that the degraded is higher than the COD_ti_minus_1 and then COD_stock_ti would become negagive and would then be set to 0 .. 
#dCOD_stock_dt    <- function(COD_ti_minus_1,  COD_uptake_i, COD_import_from_detritus_ti, Excretion) {
#19.1.25 changed to
dCOD_stock_dt    <- function(COD_ti_minus_1,  f_S_MO_Soetaert_2008, COD_import_from_detritus_ti, Excretion) {
  
  COD_stock_ti_diff <-  COD_ti_minus_1 - f_S_MO_Soetaert_2008 
  
  #the import happens regardless of the (potential) degradation
  #until 24.1.25 was  if((COD_import_from_detritus_ti + Excretion) < 0 ) { # no, what was it ? however, changing to the below actualyl des not make any different
  #outcommented the two below and instead managed in the actual file -  COD may become 0 beaue it will be replenished, from detritus, in contrast to fauna dn MO
  #if((COD_stock_ti_diff + COD_import_from_detritus_ti + Excretion) < 0 ) {
  #COD_stock_ti <-                     COD_import_from_detritus_ti + Excretion #19.1.25 added Excretion from fauna
  #}else {
  COD_stock_ti <- COD_stock_ti_diff + COD_import_from_detritus_ti + Excretion #19.1.25 added Excretion from fauna
  #}
  #25.12.24 mol COD / L minus  mol COD / L per time step calculated + mole COD / L
  return(COD_stock_ti) # mol COD  PER LITER 
}




#19.1.25  added based on NPZD (see separate file same folder). this is equivalent to maintenance
dExcretion_dt    = function(excretionRate, Fauna_ti_minus_1 ) {
  Excretion      <- excretionRate * Fauna_ti_minus_1  
  return(Excretion)
}

#19.1.25  added based on NPZD (see separate file same folder). 27.4.25 replaced by below
# dMortality_dt    = function(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1 ,Kassel_daily_temp_ , i) {
#   Mortality      <- mortalityRate * Fauna_ti_minus_1 #* Fauna_ti_minus_1 #sis why is mortality 2nd order? 24.1.25 changed that
#   return(Mortality)
# }



# make  temperature dependent TODO 4.1.25 gross assumption ! I have no data 
#4.1.25 groundwater fauna does not survive at 30 degrees - see numouers pubs. Therefore, take the rate given and assume that it is valid within the narrow band of temperatures in gw
# d_MO_fauna_uptake_dt_per_day_per_temperature    = function(rFauna_MO_uptake_per_day_at_lab_temperature, MO_het_ti_minus_1, Kassel_daily_temp, i) {
#   TEMP_ti <- Kassel_daily_temp$TGK [Kassel_daily_temp$dateRi == results$dateRi[i]]
#   #TODO das hier ist noch weit entfernt von der Temp abhaengigkeit der Rate - aber die Rate an sich ist ja auch noch voellig unrealistisch.
#   #d_DOC_MO_het_ti <-  rMO_DOC_uptake_per_day_at_lab_temperature*MO_het_ti_minus_1  *TEMP_ti/10 #
#   # temperature dependence of the uptake rate
#   
#   # I ned to change the rate by the factor by which it deviates from the 10 degrees - or should I deviate from the 30 degrees
#   #in fact, no, both the same formula - because in the end, the distance is still positive, even if the temperature is negative
#   if (TEMP_ti <0) {
#     TEMP_related_to_30_deg = 30-TEMP_ti
#   }else {
#     TEMP_related_to_30_deg = 30-TEMP_ti
#   }
#   
#   rFauna_MO_uptake_per_day_at_TEMP <- rFauna_MO_uptake_per_day_at_lab_temperature  / (TEMP_related_to_30_deg * 0.9696) #0.9696  is the factor in Schmidt et al. 2018 to calcualte respective temperature in 1 degree steps
#   
#   return(rFauna_MO_uptake_per_day_at_TEMP)   #mass of COD per biomass MO per day at teh current temperautre
# }


#Avramov thesis 2013: "For the amphipod N. inopinatus, the LT50,24h (± standard error) was equal to 27.1°C (±0.5), and for the isopod P.cavaticus the same parameter was equal to 23.0 °C (± 0.1). 
#Relatively soon, after five days of exposure, the values dropped to reach an LT50,5d of 23.3 °C (± 2.9) for N. inopinatus, and 16.6 °C (± 3.8) for P. cavaticus (details are given in Publication II). While these results already indicate a slightly higher susceptibility to thermal stress in the isopods, the difference between the two crustacean species becomes even more distinct when the temperatures tolerated by the entire tested group of each species are compared. The highest temperature that could be tolerated by all isopods for 24 hours was 16 °C, while the amphipods tolerated 20 °C. Moreover, the amphipods endured being exposed to 20 °C for a total of 20 days without a single animal dying. In contrast, in the isopod group, the same temperature led to 20% mortality within 24 hours of exposure. 

#was meint sie mit ganzer Gruppe?
#at what temperature naturally? p. 35: temperature prevailed, to which they had been previously acclimated ( ca. 12 °C)

#The highest temperature treatment that could be tolerated by all isopods and for the longest time was 12 °C for 5 days in our study. The highest temperature that was tolerated by all amphipods for the longest time was 16 °C (for > 50 days)."

#19.4.25 TODO how to calculate the potetial death rate from the LT50 per degree C?
#is a minimum curve - death at low temp and death at high temp

# since it is not easy to extrapolate the parameters from LT50 curves, and since they are often not provided in the literature, very roughly let's assume linear response.
# Thus: 100% at 12 degrees, between 50 and 100% at 16 degrees, between 0 and50% at 20 degrees (Brielmann 2011). Assuming that half of the fauna population behavoes like amphipods and half like asellids, 100% survival at 12 degrees, (75% survival at 16 degrees,) 25 % at 20 degrees. Taking the endpoint 20 degrees, 0.25 of the population survives aafter an increase by 8 degrees. This means that 0.75 have died. Thus , for each degree of increase, 0.75/8 = 0.09375 , roughly 1 % of the individuals die. Let's assume that it is the same below 12 deg

dMortality_dt    = function(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1, GWTEMP_ti ) {
  Mortality      <- mortalityRate * Fauna_ti_minus_1 * Fauna_ti_minus_1  #* Fauna_ti_minus_1 #sis why is mortality 2nd order? 24.1.25 changed that.  1.6.25 no, mortality rate is PER conc. thus multiplying twice by conc gives conc / day - need to calculate everythin again with the new function
  
  if(is.na(mortalityFraction_per_degree)){
    Mortality <- Mortality
    return(Mortality)
  }
  if(!is.na(mortalityFraction_per_degree)){
    if(GWTEMP_ti  -12 < 0){
      Mortality      <- Mortality #implemented on 7.6.25
    }else {
      Mortality      <- Mortality + mortalityFraction_per_degree * abs(GWTEMP_ti  -12)* Fauna_ti_minus_1 
    }
  }
  return(Mortality)
}

