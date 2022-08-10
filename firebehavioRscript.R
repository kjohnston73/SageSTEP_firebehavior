#install.packages('firebehavioR')
library(firebehavioR)
data(fuelMoisture)

#Set working directory, then read in fuel models csv set up for the firebehavioR package.
#Fuel model csv can have any number of fuel models (i.e. plots) and header names are 
#not important as long as data is complete and in the correct units and order. 
#See documentation for firebehavioR package for fuel model csv set up. I added 
#plot name, slope, and total fuel load to my csv for use in my ROSIFL() function.
plotFuelModels<- read.csv(file = 'plotFuelModels.csv') 

#canFuel function returns canopy fuel parameters given basal area, height, 
#trees per hectare, and type. NOT TO BE USED FOR FUEL BEDS THAT DO NOT HAVE TREES.
#canFuel(5, 0.75, 15,"mc")

#Canopy fuel parameters returned from canFuel function above.
#rangeCrownFuel = data.frame(
#  CBD = 0.0037, FMC = 100,
#  CBH = 0.1, CFL = 0.0722
#)

#Canopy fuel parameters to be used for grass fires (i.e. fuel beds without trees).
#The rothermel() function requires canopy fuels as an input but we use no 
#function outputs dependent on the canopy fuels, so the values here are irrelevant.
rangeCrownFuel = data.frame(
  CBD = Inf, FMC = 100,
  CBH = Inf, CFL = Inf
)

#Extract desired fuel moisture scenarios from firebehavioR 'fuelmoisture' dataset.
FM_inputs <- rbind(fuelMoisture['D1L1',], fuelMoisture['D1L2',], 
                              fuelMoisture['D1L3',], fuelMoisture['D1L4',])
#Name your fuel moisture scenarios
FM_inputs$name <- c('fullcure', 'two3rdcured', 'one3rdcured', 'uncured')

#Create environmental dataframe. Slope (%) can be set to anything here, it is reset
#to the slope of the plot later on. [Open] windspeed (km/hr) is the 20-ft windspeed of your choice. 
#A [wind] direction (from up slope) of 0 degrees yields most liberal estimation 
#of fire behavior. Wind adjustment factor (waf) is a value between 0 and 1 and 
#is used to adjust the wind speed measured 20-feet above the vegetation (i.e. 
#'open windspeed') to midflame windspeed. A WAF of 0.5 is recommended for 
#unsheltered fuels (when surface fuels are not effectively sheltered from the 
#wind, no to sparse overstory).
fbEnviro <- data.frame(
  slope = 3, 
  windspeed = 15, 
  direction = 0, 
  waf = 0.5)

#Function that returns rate of spread (m/min and km/hr), fireline intensity (kW/m and BTU/ft), and 
#flame length (m).
ROSIFL <-function(FuelMoistures, FuelModels, CrownFuel, Enviro){
  outputs <- data.frame(FuelModels$description)
  inputs <- data.frame(FuelModels$description)
  inputs$heat_BTU_lb <- FuelModels$heat * 0.000947817 * 453.592
  inputs$load_lb_ft2 <- FuelModels$totalLoad_g_m2 / (453.592*10.7639)
  for (i in 1:nrow(FuelMoistures)){
    moisture <- FuelMoistures[i,]
    for (i in 1:nrow(FuelModels)){
      runFuelModel <- FuelModels[i,]
      Enviro$slope <- FuelModels$slope[i]
      outputs[[paste0("ros_", moisture$name)]][i] = rothermel(runFuelModel,
                                                              moisture, 
                                                              CrownFuel, 
                                                              Enviro)$fireBehavior[,3]
      inputs[[paste0("ros_ft_s_", moisture$name)]][i] <- ((outputs[[paste0
                                                                    ("ros_", moisture$name)]][i])/60) * 3.28084
      outputs[[paste0("intensity_BTU_ft_", moisture$name)]] <- (inputs$heat_BTU_lb * 
                                                                  inputs$load_lb_ft2 * 
                                                                  (inputs[[paste0("ros_ft_s_", moisture$name)]]))
      inputs[[paste0("fl_ft_", moisture$name)]]<- 0.451 * (outputs[[paste0("intensity_BTU_ft_", moisture$name)]])^0.46
      outputs[[paste0("intensity_kW_m_", moisture$name)]] <- ((outputs[[paste0("intensity_BTU_ft_", moisture$name)]])*0.000293071)/0.3048
      outputs[[paste0("fl_m_", moisture$name)]] <- inputs[[paste0("fl_ft_", moisture$name)]] * 0.3048
      outputs[[paste0("ros_km_hr_", moisture$name)]]<- (outputs[[paste0("ros_", moisture$name)]] * 60) / 1000
    } 
  }
  return (outputs)
}

#Run ROS function
firebehavior_outputs <- ROSIFL(FM_inputs, plotFuelModels, rangeCrownFuel, fbEnviro)

#Write outputs to a csv file
write.csv(firebehavior_outputs, file = 'LFB_firebehavior.csv', row.names = FALSE)


#NOTES
#Rate of spread (ros) in the rothermel() function is computed using the equations from Rothermel (1972)

#Fireline intensity (J s^-1 m^-1): I = hwr (Byram 1959)
#           h = heat yield of fuel... heat yield of grass = 16890 J/g (Trollope 1998)
#               heat yield of sagebrush = 18610 (Albini 1976), >19000(Qi et al. 2016 )
#           w = weight of available fuel (g m^-2)
#           r = rate of spread (m s^-1) (Rothermel 1972)


#Flame length (ft): 
#     fl (ft) = 4.28(X1) + 0.00151(X2) - 2.92 (Rothermel 1972)
#               X1 = total fuel loading, lb/ft^2
#               X2 = average SAV ratio weighted by fuel surface area in each size class, ft^-1
#     fl (ft) = 0.451 * I^0.46 (Byram 1959) *** This is the equation used above ***
#               I = fireline intensity (BTU/ft)

#Puckett et al. 1979 suggest wildfire with intensity <50BTU/ft/s & flame length <0.9m is 
#easily attacked and controlled. Wildfire with intensity up to 100BTU/ft/s & 
#flame length up to 1.2m is the limit for eased direct attack. Wildfire 
#with intensity 500-700BTU/ft/s & flame length 2.4-2.7m are when spotting begins 
#to be a problem and the limit of direct attack is reached.


#Citations
#Albini, F.A., 1976. Computer-based models of wildland fire behavior: a user's manual. 
#Intermountain Forest and Range Experiment Station, Forest Service, US Department of Agriculture.

#Byram, G.M., 1959. Combustion of forest fuels. Forest fire: control and use, 
#pp.61-89.

#Puckett, J.V., E.M. Johnston, FA. Albini, J.K. Brown, D.L. Bunnell, W.E. 
#Fischer, and JAK. Snell. 1979. User's guide to debris prectiction and hazard 
#appraisal. US Department of Agriculture, Forest Service, Northern Region, Missoula, MT. 

#Qi, Y., Jolly, W.M., Dennison, P.E. and Kropp, R.C., 2016. Seasonal relationships 
#between foliar moisture content, heat content and biochemistry of lodgepole line 
#and big sagebrush foliage. International Journal of Wildland Fire, 25(5), pp.574-578.

#Rothermel, R.C., 1972. A mathematical model for predicting fire spread in 
#wildland fuels (Vol. 115). Intermountain Forest & Range Experiment Station, 
#Forest Service, US Department of Agriculture.

#Trollope, W.S.W., 1998. Effect and use of fire in the savanna areas of 
#southern Africa. University of Fort Hare.


#Other literature of interest
#Brown, J.K., 1982. Fuel and fire behavior prediction in big sagebrush 
#(Vol. 290). US Department of Agriculture, Forest Service, Intermountain 
#Forest and Range Experiment Station.

#Cheney, P. and Sullivan, A. eds., 2008. Grassfires: fuel, weather and fire 
#behaviour. Csiro Publishing.