Fulda_prec_plot <- function(enddate){
  Fulda_daily_temp_ <- Fulda_daily_temp_ %>%
  dplyr::filter(lubridate::year(dateRi) > 1977 & lubridate::year(dateRi) <= lubridate::year(as.Date(enddate)))

Fulda_daily_prec <- Fulda_daily_prec %>%
  dplyr::filter(RS > -999) %>% #das hier erst am 21.6. zugefuegt - warum hatte das fkt?
  dplyr::filter(lubridate::year(dateRi) > 1977 & lubridate::year(dateRi) <= lubridate::year(as.Date(enddate)))

  #1.12.24 precipitaiton plot requires coeff.
  coeff <- 2

  #with precipitaiton and temperature 
  #TODO is this maxRange ok with concentrations as well that might be plotted along side?
  maxRange= 1.1*(max(Fulda_daily_prec$RS) + max(Fulda_daily_temp_$TT_TER #11.5.25 added in desparation
  #-min(Fulda_daily_temp_$TT_TER)
  ))

  #a slimmer version for ggarrange
  (Fulda_daily_prec_plot <- 
      #   ggplot(data = results_df_long, aes(x = date_ri, y = value, colour = variable)) +
      #ggplot(data = results_df_long ) +
      ggplot(data = Fulda_daily_prec ) +
      #not necessary ylim = c(min(results$DOC, results$DETRITUS), max(results$DOC, results$DETRITUS))+
      # scale_colour_discrete("Variable", #pch = 16, 
      #                       labels = c("COD", "Detritus", "MO_het" )#, col = c("brown1", "brown4")
      # )+
      
      #geom_bar(aes(x=dateRi, y=(RS/coeff) ), stat="identity", colour = NA, fill = "blue")+
      geom_tile(data = Fulda_daily_prec, aes(x=dateRi, y = -1*(RS/coeff-maxRange
      ), # y = the center point of each bar
                                             height = RS,   width = 1),
                fill = "blue",
                color = NA) +#https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot
      #geom_bar(aes(x=dateRi, y=((RS/coeff)-min(Kassel_daily_temp_$TMK)+2) ), stat="identity", colour = NA, fill = "blue")+#((RS/coeff)-min(Kassel_daily_temp_$TMK)) 
      #https://stackoverflow.com/questions/73181349/create-climate-diagram-in-r-with-temperature-and-precipitation
      #geom_bar(data = Fulda_Horas_daily_prec, stat="identity")+
      geom_line(data = Fulda_daily_temp_, aes(x = dateRi, y = TT_TER))+
      
      scale_y_continuous(#limits = c(min(Kassel_daily_temp_$TMK, Fulda_daily_prec$RS), max(Kassel_daily_temp_$TMK, Fulda_daily_prec$RS)),
        expression("Air temperature " ( degree*C)), 
        #sec.axis = sec_axis(~ . * coeff, name = "Precipitation (mm)")
        #sec.axis = sec_axis(trans = ~  -1*(. -10 ), 
        sec.axis = sec_axis(trans = ~  -1*(.-maxRange ), 
                            name = "Precipitation (mm)")
      )+ #https://stackoverflow.com/questions/73181349/create-climate-diagram-in-r-with-temperature-and-precipitation
      #https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot
      #geom_point(data = Fulda_daily_prec_for_mean, aes(dateRi, RS, size = RS), col = "blue")+
      labs (x = "Date",   y = "Precipitation [mm / day]")+
      scale_x_date(limits = c(t_0, t_max))+
      theme(panel.background = element_rect(fill = "white",  colour = "black", #size = 0.5, 
                                            linetype = "solid" ),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y.right = element_line(color = "blue"), #https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
            axis.ticks.y.right = element_line(color = "blue"),
            axis.text.y.right = element_text(color = "blue"), 
            axis.title.y.right = element_text(color = "blue"),
            # strip.text.y.right = element_text(angle = 0),
            # strip.text.x.top = element_text(angle = 90, hjust = 0),
            axis.text = element_text(colour = "black"), 
            axis.text.x = element_text(#angle = 45, vjust = 0.4, #lineheight = 2, 
              #rel (20)#, margin(t = 2, r = 2, b = .3, l = 2, unit = "pt")
            ),
            legend.key = element_blank()#, #get rid of boxes around line - point things
            # legend.position = "right" #
      )
  )
}
