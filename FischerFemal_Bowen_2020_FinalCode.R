###Code for conducting validation of soil carbonate model
library(raster)
library(rgdal)
library(RColorBrewer)
library(scales)
library(xlsx)

#Prepare gridded climate data

#read in worldclim 2.5min mean temperature grids, downloaded 9-18-18
files = list.files(pattern="wc2.0_2.5m_tavg*")
mtemp = stack(files)
mat = mean(mtemp)
writeRaster(mat, "wc2.0_2.5m_tavg_ma.tif")
plot(mat)

mat = list.files(pattern="wc2.0_2.5m_tavg_ma.tif")
mat = stack(mat)

#now make quarterly seasonal averages
qtemp = stack(mean(subset(mtemp, c(12,1,2))), 
              mean(subset(mtemp, 3:5)), 
              mean(subset(mtemp, 6:8)), 
              mean(subset(mtemp, 9:11)))

qtemp_djf = stack(mean(subset(mtemp, c(12,1,2))))
qtemp_mam = stack(mean(subset(mtemp, c(3:5))))
qtemp_jja = stack(mean(subset(mtemp, c(6:8))))
qtemp_son = stack(mean(subset(mtemp, c(9:11))))

#find the hottest quarter
hotq = which.max(qtemp)
plot(hotq)

#read in worldclim 2.5min precip grids, downloaded 9-18-18
files = list.files(pattern="wc2.0_2.5m_prec*")
mprec = stack(files)
map = sum(mprec)
writeRaster(map, "wc2.0_2.5m_prec_ma.tif", overwrite=TRUE)
plot(map)

#now make quarterly seasonal sums
qprec = stack(sum(subset(mprec, c(12,1,2))), 
              sum(subset(mprec, 3:5)), 
              sum(subset(mprec, 6:8)), 
              sum(subset(mprec, 9:11)))

#find the driest quarter
dryq = subset(qprec, min(layer))
plot(dryq)

#now pull hot quarter temps into a single raster
hqt = max(qtemp)
plot(hqt)

#this is what that would look like as tseas
djf.offset = qtemp_djf - mat
mam.offset = qtemp_mam - mat
jja.offset = qtemp_jja - mat
son.offset = qtemp_son - mat
hqt.offset = hqt - mat
plot(hqt.offset)

#and get hot quarter precip as well
#first pull the values from the raster into a matrix
qprec.mat = matrix(c(qprec[[1]][], qprec[[2]][], qprec[[3]][], qprec[[4]][]), length(qprec[[1]]), 4)
#now set up space in a raster to store results
hqp = hotq
#now use the magic of cbind to select the matrix colums for the hot quarter and combine
hqp[] = qprec.mat[cbind(1:nrow(qprec.mat), hotq[])]
plot(hqp)

#then this is what we want as pseas for the model
hqp.frac = hqp/map
plot(hqp.frac)

#now get dry quarter temp, using the same method
qtemp.mat = matrix(c(qtemp[[1]][], qtemp[[2]][], qtemp[[3]][], qtemp[[4]][]), length(qtemp[[1]]), 4)
dqt = dryq
dqt[] = qtemp.mat[cbind(1:nrow(qtemp.mat), dryq[])]
plot(dqt)

dqt.offset = dqt - mat
plot(dqt.offset)

#and lastly the dry quarter precip
dqp = min(qprec)
plot(dqp)

dqp.frac = dqp/map
plot(dqp.frac)

#####That's all the grid processing

#extract relevant values at sites
precipcomp <- read.csv("valsites_sel_d18O_P.csv")
rawsites = read.xlsx("modern_comparison.xlsx", sheetIndex = 3)
sites = read.xlsx("modern_comparison_raw.xlsx", sheetIndex = 1)
coords = matrix(c(sites$Lon, sites$Lat), nrow(sites), 2)
sites$mat.wc = extract(mat, coords)
sites$map.wc = extract(map, coords)
sites$hqt.offset = extract(hqt.offset, coords)
sites$hqp.frac = extract(hqp.frac, coords)
sites$dqt.offset = extract(dqt.offset, coords)
sites$dqp.frac = extract(dqp.frac, coords)

write.csv(sites, "valsites.csv")

#This does the same for a screened subset of sites
sites = read.xlsx("modern_comparison.xlsx", sheetIndex = 2)
coords = matrix(c(sites$Lon, sites$Lat),nrow(sites),2)
sites$mat.wc = extract(mat, coords)
sites$map.wc = extract(map, coords)
sites$hqt.offset = extract(hqt.offset, coords)
sites$hqp.frac = extract(hqp.frac, coords)
sites$dqt.offset = extract(dqt.offset, coords)
sites$dqp.frac = extract(dqp.frac, coords)
sites$djf.offset = extract(djf.offset, coords)
sites$mam.offset = extract(mam.offset, coords)
sites$jja.offset = extract(jja.offset, coords)
sites$son.offset = extract(son.offset, coords)


### Record which quarter is dry quarter. Change southern hemisphere sites to jja = summer, ect... 

for (i in  1:nrow(sites)){
  sites$DQ =  ifelse(almost.equal(sites$djf.offset, sites$dqt.offset, 1e-5), "djf", 
                     ifelse(almost.equal(sites$mam.offset, sites$dqt.offset, 1e-5), "mam",
                            ifelse(almost.equal(sites$son.offset, sites$dqt.offset, 1e-5), "son", "jja")))
  
  
}
# Precipitation isotope vs. MAT error calculation
PrecipData <- read.csv("Oma.csv")

# OIPC data - d18Op validation

for (i in 1:nrow(precipcomp)){
  precipcomp[i,"d18O_OIPC_mam"] <- mean(c(precipcomp[i,"d18O_OIPC_mar"],precipcomp[i,"d18O_OIPC_apr"],precipcomp[i,"d18O_OIPC_may"]))
  precipcomp[i,"d18O_OIPC_jja"] <- mean(c(precipcomp[i,"d18O_OIPC_jun"],precipcomp[i,"d18O_OIPC_jul"],precipcomp[i,"d18O_OIPC_aug"]))
  precipcomp[i,"d18O_OIPC_son"] <- mean(c(precipcomp[i,"d18O_OIPC_sep"],precipcomp[i,"d18O_OIPC_oct"],precipcomp[i,"d18O_OIPC_nov"]))
  precipcomp[i,"d18O_OIPC_djf"] <- mean(c(precipcomp[i,"d18O_OIPC_dec"],precipcomp[i,"d18O_OIPC_jan"],precipcomp[i,"d18O_OIPC_feb"]))
}

coords = matrix(c(PrecipData$Longitude, PrecipData$Latitude),nrow(PrecipData),2)

PrecipData$MAT <- extract(MAT,coords)

# Subset to include only mid-latitude northern and southern hemisphere sites
PrecipData_midlat <- subset(PrecipData, abs(PrecipData$Latitude) < 70 & abs(PrecipData$Latitude) > 20) 
PrecipData_midlat <- subset(PrecipData_midlat, -500 < MAT & nyrs >=3)
dO_MAT <- lm(PrecipData_midlat$d18O ~ PrecipData_midlat$MAT)
summary(dO_MAT)
SE <- ((-14.15 + 0.48 * PrecipData_midlat$MAT) - (PrecipData_midlat$d18O))^2
MSE <- mean(SE)
RMSE <- sqrt(MSE)

#Radiation at the top of the atmosphere with latitude measurement - based on 400 W/m2 (34.5 MJ / m2 / day) at equator and cos(latitude)
Ra = 34.5 * cos(abs(sites[,"Lat"]*0.01745))
sites[,"Ra"] <- Ra

#read in real data
data = read.xlsx("modern_comparison.xlsx", sheetIndex = 3)
data.aves = aggregate.data.frame(data, list(data$Site), mean, simplify = TRUE)
data.aves$Site = NULL
data.comp = merge.data.frame(sites, data.aves, by.x = "Site", by.y = "Group.1")

#number of iterations to calculate error
nsynth = 10000

#set seed
set.seed(1)

sm_forward_evap = function(MAP, MAT, P_seas, T_seas, Ra, hqt.offset, DQ){
  
  #Assume constant pre-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7, sd of ~ 0.1.
  ST_mean = 0.5
  ST_var = 0.01
  size = ST_mean*(1-ST_mean)/ST_var - 1
  alpha = ST_mean * size
  beta = (1-ST_mean) * size
  ST = rbeta(nsynth, alpha, beta)
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  #Evaporated soil water (100%)
  esw = 1
  
  #Seasonal precipitation (0)
  spre = 0
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #maxiumum depth of L
  z <- pmin(z, L)
  #depth in meters
  z_m <- z/100
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(T_seas == hqt.offset, 0.29, ifelse(DQ == "djf", 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  #Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS 
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  dO_P_m <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  dO_P = rnorm(nsynth, dO_P_m, 2.49)
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm_m <- (R_O_atm / RO.vsmow - 1) * 1000
  dO_atm = rnorm(nsynth, dO_atm_m, 1)
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA_var = rnorm(nsynth, 1, 0.2) 
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Respiration rate
  R_day_m <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)  #Raich 2002, gC/m2day
  #Using mean residual of 50% based on Raich validation data 
  theta = (R_day_m*0.5) ^ 2 / R_day_m 
  k = R_day_m / theta 
  R_day = rgamma(nsynth, shape = k, scale = theta)
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W_m <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  W = rnorm(nsynth, W_m, 2.40)
  
  #CO2 effect on discrimination, Schubert and Jahren
  deltaP_pCO2_m <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  deltaP_pCO2 = rnorm(nsynth, deltaP_pCO2_m, 0.61)
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Soil water evaporation
  #evap is 6%+/-4% of total ET globally Good et. al. 2010
  e_mean = 0.06  
  e_var = 0.04^2
  size = e_mean*(1-e_mean)/e_var - 1
  alpha = e_mean * size
  beta = (1-e_mean) * size
  E = rbeta(nsynth, alpha, beta) * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  
  #Carb isotope values
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  dat = c(median(dC_Carb), median(dO_Carb), sd(dC_Carb), sd(dO_Carb), median(Sz))
  
  return(dat)
}

## Run Evap Model with site-specific climate data

hq_pred = data.frame(d13C=numeric(0), d18O=numeric(0), d13C_sd=numeric(0), d18O_sd=numeric(0), Sz=numeric(0))
for(i in 1: nrow(sites)){
  hq_pred[i,] = sm_forward_evap(sites$map.wc[i], sites$mat.wc[i], sites$hqp.frac[i], sites$hqt.offset[i], sites$Ra[i], sites$hqt.offset[i], sites$DQ[i])
  
}
hq_pred$Site = sites$Site

dq_pred = data.frame(d13C=numeric(0), d18O=numeric(0), d13C_sd=numeric(0), d18O_sd=numeric(0), Sz=numeric(0))
for(i in 1: nrow(sites)){
  dq_pred[i,] = sm_forward_evap(sites$map.wc[i], sites$mat.wc[i], sites$dqp.frac[i], sites$dqt.offset[i], sites$Ra[i], sites$hqt.offset[i], sites$DQ[i])
  
}
dq_pred$Site = sites$Site

## Plots of measured vs. predicted d13C and d18O values with MAP colors
hq.comp = merge.data.frame(hq_pred, sites, by.x = "Site", by.y = "Site", all.x=TRUE)
dq.comp = merge.data.frame(dq_pred, sites, by.x = "Site", by.y = "Site", all.x=TRUE)

hq.comp = merge.data.frame(hq.comp, data.aves, by.x = "Site", by.y = "Group.1", all.x=TRUE)
dq.comp = merge.data.frame(dq.comp, data.aves, by.x = "Site", by.y = "Group.1", all.x=TRUE)

c = ceiling(((hq.comp$map.wc - 100) / 700) * 7)

pal_map = brewer.pal(7, "Blues")

jpeg("Pre_Validation.jpg", units="in", res=300, width=8.5, height=7.5)

layout(matrix(c(1,2,3,4), 2, 2, byrow=T), heights=c(1,1.1,1,1.1), widths=c(1,1.05,1,1.05))
par(mar=c(2,5,1,0))
plot(hq.comp$d13C.measured, hq.comp$d13C, pch=16, col=pal_map[c], xlim=c(-15,5), ylim=c(-15,5), main="", cex = 1.25,
     xlab="", cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{13}, "C"[carbonate], "(\u2030)")),
     xaxt='n')
arrows(hq.comp$d13C.measured, hq.comp$d13C - hq.comp$d13C_sd,hq.comp$d13C.measured, hq.comp$d13C + hq.comp$d13C_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(hq.comp$d13C.measured - 1.10, hq.comp$d13C, hq.comp$d13C.measured + 1.10, hq.comp$d13C, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(hq.comp$d13C.measured, hq.comp$d13C, pch=1, cex=1.3)
text(-15,5,"a", cex=1.5)


par(mar=c(2,5,1,1))
plot(hq.comp$d18O.measured, hq.comp$d18O, pch=16, col=pal_map[c], xlim=c(-17,2), ylim=c(-17,2),main="",cex = 1.25,
     xlab="",  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{18}, "O"[carbonate], "(\u2030)")),
     xaxt='n')
arrows(hq.comp$d18O.measured, hq.comp$d18O - hq.comp$d18O_sd,hq.comp$d18O.measured, hq.comp$d18O + hq.comp$d18O_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(hq.comp$d18O.measured - 1.09, hq.comp$d18O, hq.comp$d18O.measured + 1.09, hq.comp$d18O,  angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(hq.comp$d18O.measured, hq.comp$d18O, pch=1, cex=1.3)
text(-17,2,"b", cex=1.5)

par(mar=c(4,5,0,0))
plot(dq.comp$d13C.measured, dq.comp$d13C, pch=16, col=pal_map[c], xlim=c(-15,5), ylim=c(-15,5), main="",cex = 1.25,
     xlab=expression(paste("Observed ",delta^{13},"C"[carbonate], "(\u2030)")),  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{13}, "C"[carbonate], "(\u2030)")))
arrows(dq.comp$d13C.measured, dq.comp$d13C - dq.comp$d13C_sd,dq.comp$d13C.measured, dq.comp$d13C + dq.comp$d13C_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(dq.comp$d13C.measured - 1.10, dq.comp$d13C, dq.comp$d13C.measured + 1.10, dq.comp$d13C, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(dq.comp$d13C.measured, dq.comp$d13C, pch=1, cex=1.3)
text(-15,5,"c", cex=1.5)

par(mar=c(4,5,0,1))
plot(dq.comp$d18O.measured, dq.comp$d18O, pch=16, col=pal_map[c], xlim=c(-17,2), ylim=c(-17,2), main="",cex = 1.25,
     xlab=expression(paste("Observed ",delta^{18}, "O"[carbonate], "(\u2030)")),  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{18}, "O"[carbonate], "(\u2030)")))
arrows(dq.comp$d18O.measured, dq.comp$d18O - dq.comp$d18O_sd,dq.comp$d18O.measured, dq.comp$d18O + dq.comp$d18O_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(dq.comp$d18O.measured - 1.09, dq.comp$d18O, dq.comp$d18O.measured + 1.09, dq.comp$d18O,  angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(dq.comp$d18O.measured, dq.comp$d18O, pch=1, cex=1.3)
text(-17,2,"d", cex=1.5)

legend("bottomright", title = expression(paste("P"[a]," (mm)")), cex=0.8, fill=pal_map, legend=c("100 - 200", "200 - 300", "300 - 400", "400 - 500", "500 - 600", "600 - 700", "700 - 800"))

dev.off()

## Basic stats
pre_lm_hot_C <- lm(hq.comp$d13C ~ hq.comp$d13C.measured)
pre_lm_dry_C <- lm(dq.comp$d13C ~ dq.comp$d13C.measured)
pre_lm_hot_O <- lm(hq.comp$d18O ~ hq.comp$d18O.measured)
pre_lm_dry_O <- lm(dq.comp$d18O ~ dq.comp$d18O.measured)

summary(pre_lm_hot_C)
summary(pre_lm_dry_C)
summary(pre_lm_hot_O)
summary(pre_lm_dry_O)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

pre_hot_C_rmse <- rmse(hq.comp$d13C.measured - hq.comp$d13C)
pre_dry_C_rmse <- rmse(dq.comp$d13C.measured - dq.comp$d13C)
pre_hot_O_rmse <- rmse(hq.comp$d18O.measured - hq.comp$d18O)
pre_dry_O_rmse <- rmse(dq.comp$d18O.measured - dq.comp$d18O)

pre_hot_C_rmse
pre_dry_C_rmse
pre_hot_O_rmse
pre_dry_O_rmse

############### Forward model function for use in sensitivity testing
sm_optimizer_evap = function(MAP, MAT, P_seas, T_seas, Ra, spre, esw, hqt.offset, DQ){
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7, sd of ~ 0.1.
  ST_mean = 0.5
  ST_var = 0.01
  size = ST_mean*(1-ST_mean)/ST_var - 1
  alpha = ST_mean * size
  beta = (1-ST_mean) * size
  ST = rbeta(nsynth, alpha, beta)
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Isotope ratio constants
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #maxiumum depth of L
  z <- pmin(z, L)
  #depth in meters
  z_m <- z/100
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(T_seas == hqt.offset, 0.29, ifelse(DQ == "djf", 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  #Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS 
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  dO_P_m <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  dO_P = rnorm(nsynth, dO_P_m, 2.49)
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm_m <- (R_O_atm / RO.vsmow - 1) * 1000
  dO_atm = rnorm(nsynth, dO_atm_m, 1)
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA_var = rnorm(nsynth, 1, 0.2) 
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Soil water evaporation
  #evap is 6%+/-4% of total ET globally Good et. al. 2010
  e_mean = 0.06  
  e_var = 0.04^2
  size = e_mean*(1-e_mean)/e_var - 1
  alpha = e_mean * size
  beta = (1-e_mean) * size
  E = rbeta(nsynth, alpha, beta) * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  dat = c(median(dO_Carb))
  return(dat)
}

#Set up evap optimization
s = seq(0,0.95,0.05)
ss = seq(0, 1-0.05/20, 0.05/20)
ss = ss*20
ss = trunc(ss)
ss = ss/20

#Warm quarter evap optimization

parms_hq = data.frame(spres = ss, esws = rep(s, 20), rmse = numeric(400))

for(j in 1:nrow(parms_hq)){
  opt = numeric()
  for(i in 1: nrow(sites)){
    opt[i] = sm_optimizer_evap(sites$map.wc[i], sites$mat.wc[i], sites$hqp.frac[i], sites$hqt.offset[i], sites$Ra[i], parms_hq$spres[j], parms_hq$esws[j], sites$hqt.offset[i], sites$DQ[i])
  }
  
  opt = data.frame(Site = sites$Site, d18O = opt)
  opt = merge.data.frame(opt, data.comp, by.x = "Site", by.y = "Site", all.x=TRUE)
  
  mse = (opt$d18O - opt$d18O.measured)^2
  mse = mean(mse)
  parms_hq$rmse[j] = sqrt(mse)
}

min(parms_hq$rmse)
rmses_hq = matrix(parms_hq$rmse, 20, 20)
rmses_hq = rmses_hq[c(20:1),]
rmses.rast_hq = raster(rmses_hq, xmn=0, xmx=0.95, ymn=0, ymx=0.95)
spres_opt_hq = parms_hq[which.min(parms_hq$rmse),"spres"]
esws_opt_hq = parms_hq[which.min(parms_hq$rmse),"esws"]

#Dry quarter evap optimization

parms_dq = data.frame(spres = ss, esws = rep(s, 20), rmse = numeric(400))

for(j in 1:nrow(parms_dq)){
  opt = numeric()
  for(i in 1: nrow(sites)){
    opt[i] = sm_optimizer_evap(sites$map.wc[i], sites$mat.wc[i], sites$dqp.frac[i], sites$dqt.offset[i], sites$Ra[i], parms_dq$spres[j], parms_dq$esws[j], sites$hqt.offset[i], sites$DQ[i])
  }
  
  opt = data.frame(Site = sites$Site, d18O = opt)
  opt = merge.data.frame(opt, data.comp, by.x = "Site", by.y = "Site", all.x=TRUE)
  
  mse = (opt$d18O - opt$d18O.measured)^2
  mse = mean(mse)
  parms_dq$rmse[j] = sqrt(mse)
}

spres_opt_dq = parms_dq[which.min(parms_dq$rmse),"spres"]
esws_opt_dq = parms_dq[which.min(parms_dq$rmse),"esws"]
rmses_dq = matrix(parms_dq$rmse, 20, 20)
rmses_dq = rmses_dq[c(20:1),]
rmses.rast_dq = raster(rmses_dq, xmn=0, xmx=0.95, ymn=0, ymx=0.95)

spres_opt_hq
esws_opt_hq
spres_opt_dq
esws_opt_dq

#Graph

jpeg("Evap_OPT.jpg", units="in", res=300, width=9.15, height=4.68)

layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(1.001,1))

par(mar=c(5.5, 5, 2, 0))
plot(rmses.rast_hq, main="", cex.lab=1, cex.axis=1.3, xlab="SP (Seasonal Precipitation Bias, Fraction)", ylab=expression(paste("f"[evap]," (Evaporated Water Fraction)")), xlim=c(0,1), ylim=c(0,1), zlim=c(2.35,5), legend = F)
mtext("a",3,line=0.25,adj=0, cex=1.3)
par(mar=c(5.5, 0, 2, 7))
plot(rmses.rast_dq, yaxt='n', cex.lab=1, cex.axis=1.3, main="", xlab="SP (Seasonal Precipitation Bias, Fraction)", ylab="", xlim=c(0,1), ylim=c(0,1), zlim=c(2.35,5))  #now need to make a nice plot...
mtext("b",3,line=0.25,adj=0, cex=1.3)
mtext("RMSE, (\u2030)", 4, line=0.5, cex=1.3)

dev.off()




sm_optimizer_r = function(MAP, MAT, P_seas, T_seas, Ra, rr, hqt.offset, DQ){
  
  #Assume constant pre-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7, sd of ~ 0.1.
  ST_mean = 0.5
  ST_var = 0.01
  size = ST_mean*(1-ST_mean)/ST_var - 1
  alpha = ST_mean * size
  beta = (1-ST_mean) * size
  ST = rbeta(nsynth, alpha, beta)
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443 
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #maxiumum depth of L
  z <- pmin(z, L)
  #depth in meters
  z_m <- z/100
  
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(T_seas == hqt.offset, 0.29, ifelse(DQ == "djf", 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)  #mol/cm^3
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA_var = rnorm(nsynth, 1, 0.2) 
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day_m <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day_m <- R_day_m * rr
  #Using mean residual of 50% based on Raich validation data 
  theta = (R_day_m*0.5) ^ 2 / R_day_m 
  k = R_day_m / theta 
  R_day = rgamma(nsynth, shape = k, scale = theta)
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W_m <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  W = rnorm(nsynth, W_m, 2.40)
  
  #CO2 effect on discrimination, Schubert and Jahren
  deltaP_pCO2_m <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  deltaP_pCO2 = rnorm(nsynth, deltaP_pCO2_m, 0.61)
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Carb carbon isotope value
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  
  dat= c(median(dC_Carb), median(Sz))
  return(dat)
}

#Set up respiration ratio optimization
rr = seq(0.01, 1, 0.01)

parms_hq = data.frame(rr = rr, rmse = numeric(100), Sz = numeric(100))

## Warm Quarter rr optimization

for(j in 1:nrow(parms_hq)){
  opt = data.frame(d13C = numeric(nrow(sites)), Sz = numeric(nrow(sites)))
  for(i in 1: nrow(sites)){
    opt[i,] = sm_optimizer_r(sites$map.wc[i], sites$mat.wc[i], sites$hqp.frac[i], sites$hqt.offset[i], sites$Ra[i], parms_hq$rr[j], sites$hqt.offset[i], sites$DQ[i])
  }
  
  opt = data.frame(Site = sites$Site, d13C = opt$d13C, Sz = opt$Sz)
  opt = merge.data.frame(opt, data.comp, by.x = "Site", by.y = "Site", all.x=TRUE)
  
  mse = (opt$d13C - opt$d13C.measured)^2
  mse = mean(mse)
  parms_hq$rmse[j] = sqrt(mse)
  parms_hq$Sz[j] = mean(opt$Sz)
}
Sz_opt_hq = parms_hq[which.min(parms_hq$rmse),"Sz"]
rr_opt_hq = parms_hq[which.min(parms_hq$rmse),"rr"]

Sz_opt_hq
rr_opt_hq

## Dry quarter respiration optimization

parms_dq = data.frame(rr = rr, rmse = numeric(100))

for(j in 1:nrow(parms_dq)){
  opt = data.frame(d13C = numeric(nrow(sites)), Sz = numeric(nrow(sites)))
  for(i in 1: nrow(sites)){
    opt[i,] = sm_optimizer_r(sites$map.wc[i], sites$mat.wc[i], sites$dqp.frac[i], sites$dqt.offset[i], sites$Ra[i], parms_dq$rr[j], sites$hqt.offset[i], sites$DQ[i])
  }
  
  opt = data.frame(Site = sites$Site, d13C = opt$d13C, Sz = opt$Sz)
  opt = merge.data.frame(opt, data.comp, by.x = "Site", by.y = "Site", all.x=TRUE)
  
  mse = (opt$d13C - opt$d13C.measured)^2
  mse = mean(mse)
  parms_dq$rmse[j] = sqrt(mse)
  parms_dq$Sz[j] = mean(opt$Sz)
}

Sz_opt_dq = parms_dq[which.min(parms_dq$rmse),"Sz"]
rr_opt_dq = parms_dq[which.min(parms_dq$rmse),"rr"]

Sz_opt_dq
rr_opt_dq

#Graph

jpeg("Resp_OPT.jpg", units="in", res=300, width=5, height=4.5)

par(mar=c(4,5,1,1))
plot(parms_hq$rmse ~ parms_hq$rr, cex = 1.5, cex.axis=1.3, cex.lab=1.3, ylim=c(1,9), type="n", main = "", ylab = expression(paste("RMSE (\u2030)")), xlab = expression(paste("f"[R]," (Fraction of Estimated Respiration)")))
lines(parms_hq$rmse ~ parms_hq$rr, col="red")
lines(parms_dq$rmse ~ parms_dq$rr, col="blue")
legend("topright", legend=c("Dry Quarter", "Warm Quarter"), col=c("blue","red"), lty=1)

dev.off()


library(RColorBrewer)
library(scales)

sm_forward_opt_hq = function(MAP, MAT, P_seas, T_seas, Ra){
  
  #Assume constant post-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7, sd of ~ 0.1.
  ST_mean = 0.5
  ST_var = 0.01
  size = ST_mean*(1-ST_mean)/ST_var - 1
  alpha = ST_mean * size
  beta = (1-ST_mean) * size
  ST = rbeta(nsynth, alpha, beta)
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  #Evaporated soil water optimization
  esw = esws_opt_hq
  
  #Seasonal precipitation bias optimization
  spre = spres_opt_hq
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #maxiumum depth of L
  z <- pmin(z, L)
  #depth in meters
  z_m <- z/100
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- 0.29
  T_soil <- MAT + (T_seas * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  # Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  # Apply optimized spres to simple mixing model
  dO_P_m <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  dO_P = rnorm(nsynth, dO_P_m, 2.49)
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm_m <- (R_O_atm / RO.vsmow - 1) * 1000
  dO_atm = rnorm(nsynth, dO_atm_m, 1)
  
  #Potential ET
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  #PET in mm/day, Turc 1961. Error from Tabari (2009), average RMSEs from a variety of climates
  ETP_M <- ETP_D * 30  #mm/month
  
  #Actual ET
  ETA_var = rnorm(nsynth, 1, 0.2) #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) #AET in mm/quarter from Budyko curve
  #here scaled eta to quarter precip, assuming potential carry-over
  
  #Free air porosity
  #Have updated, now scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  FAP = pmax(FAP,0.01) #dimensionless. At least 1% free air porosity
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day_m <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day_m <- R_day_m * rr_opt_hq
  #Using mean residual of 50% based on Raich validation data 
  theta = (R_day_m*0.5) ^ 2 / R_day_m 
  k = R_day_m / theta 
  R_day = rgamma(nsynth, shape = k, scale = theta)
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W_m <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  W = rnorm(nsynth, W_m, 2.40)
  
  #CO2 effect on discrimination, Schubert and Jahren
  deltaP_pCO2_m <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  deltaP_pCO2 = rnorm(nsynth, deltaP_pCO2_m, 0.61)
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Soil water evaporation
  #evap is 6%+/-4% of total ET globally Good et. al. 2010
  e_mean = 0.06  
  e_var = 0.04^2
  size = e_mean*(1-e_mean)/e_var - 1
  alpha = e_mean * size
  beta = (1-e_mean) * size
  E = rbeta(nsynth, alpha, beta) * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  dat = c(median(dC_Carb), median(dO_Carb), sd(dC_Carb), sd(dO_Carb), median(Sz))
  
  return(dat)
}

sm_forward_opt_dq = function(MAP, MAT, P_seas, T_seas, Ra, hqt.offset, DQ){
  
  
  #Assume constant post-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7, sd of ~ 0.1.
  ST_mean = 0.5  
  ST_var = 0.01
  size = ST_mean*(1-ST_mean)/ST_var - 1
  alpha = ST_mean * size
  beta = (1-ST_mean) * size
  ST = rbeta(nsynth, alpha, beta)
  pores = 0.7 * ST
  tort = 1.4 * ST
  
  #Evaporated soil water optimization
  esw = esws_opt_dq
  
  #Seasonal precipitation bias optimization
  spre = spres_opt_dq
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #depth in meters for oxygen calcs
  z_m <- z/100
  #maxiumum depth of L for carbon calcs
  z <- pmin(z, L) 
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(T_seas == hqt.offset, 0.29, ifelse(DQ == "djf", 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  # Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  # Apply optimized spres to simple mixing model
  dO_P_m <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  dO_P = rnorm(nsynth, dO_P_m, 2.49)
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm_m <- (R_O_atm / RO.vsmow - 1) * 1000
  dO_atm = rnorm(nsynth, dO_atm_m, 1)
  
  #Potential ET
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  #PET in mm/day, Turc 1961. Error from Tabari (2009), average RMSEs from a variety of climates
  ETP_M <- ETP_D * 30  #mm/month
  
  #Actual ET
  ETA_var = rnorm(nsynth, 1, 0.2) #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) #AET in mm/quarter from Budyko curve
  #here scaled eta to quarter precip, assuming potential carry-over
  
  #Free air porosity
  #Have updated, now scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  FAP = pmax(FAP,0.01) #dimensionless. At least 1% free air porosity
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day_m <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day_m <- R_day_m * rr_opt_dq
  #Using mean residual of 50% based on Raich validation data 
  theta = (R_day_m*0.5) ^ 2 / R_day_m 
  k = R_day_m / theta 
  R_day = rgamma(nsynth, shape = k, scale = theta)
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W_m <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  W = rnorm(nsynth, W_m, 2.40)
  
  #CO2 effect on discrimination, Schubert and Jahren
  deltaP_pCO2_m <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  deltaP_pCO2 = rnorm(nsynth, deltaP_pCO2_m, 0.61)
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Soil water evaporation
  #evap is 6%+/-4% of total ET globally Good et. al. 2010
  e_mean = 0.06  
  e_var = 0.04^2
  size = e_mean*(1-e_mean)/e_var - 1
  alpha = e_mean * size
  beta = (1-e_mean) * size
  E = rbeta(nsynth, alpha, beta) * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  dat = c(median(dC_Carb), median(dO_Carb), sd(dC_Carb), sd(dO_Carb), median(Sz))
  
  return(dat)
}

## Post- validation after optimization

hq_pred = data.frame(d13C=numeric(0), d18O=numeric(0), d13C_sd=numeric(0), d18O_sd=numeric(0), Sz=numeric(0))
for(i in 1: nrow(sites)){
  hq_pred[i,] = sm_forward_opt_hq(sites$map.wc[i], sites$mat.wc[i], sites$hqp.frac[i], sites$hqt.offset[i], sites$Ra[i])
  
}
hq_pred$Site = sites$Site

dq_pred = data.frame(d13C=numeric(0), d18O=numeric(0), d13C_sd=numeric(0), d18O_sd=numeric(0), Sz=numeric(0))
for(i in 1: nrow(sites)){
  dq_pred[i,] = sm_forward_opt_dq(sites$map.wc[i], sites$mat.wc[i], sites$dqp.frac[i], sites$dqt.offset[i], sites$Ra[i], sites$hqt.offset[i], sites$DQ[i])
  
}
dq_pred$Site = sites$Site


hq.comp = merge.data.frame(hq_pred, sites, by.x = "Site", by.y = "Site", all.x=TRUE)
dq.comp = merge.data.frame(dq_pred, sites, by.x = "Site", by.y = "Site", all.x=TRUE)

hq.comp = merge.data.frame(hq.comp, data.aves, by.x = "Site", by.y = "Group.1", all.x=TRUE)
dq.comp = merge.data.frame(dq.comp, data.aves, by.x = "Site", by.y = "Group.1", all.x=TRUE)

#add it to the predictions and plot

c = ceiling(((hq.comp$map.wc - 100) / 700) * 7)

pal_map = brewer.pal(7, "Blues")

jpeg("Post_Validation.jpg", units="in", res=300, width=8.5, height=7.5)

layout(matrix(c(1,2,3,4), 2, 2, byrow=T), heights=c(1,1.1,1,1.1), widths=c(1,1.05,1,1.05))
par(mar=c(2,5,1,0))
plot(hq.comp$d13C.measured, hq.comp$d13C, pch=16, col=pal_map[c], xlim=c(-13,4), ylim=c(-13,4), main="", cex = 1.25,
     xlab="", cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{13}, "C"[carbonate], "(\u2030)")),
     xaxt='n')
arrows(hq.comp$d13C.measured, hq.comp$d13C - hq.comp$d13C_sd,hq.comp$d13C.measured, hq.comp$d13C + hq.comp$d13C_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(hq.comp$d13C.measured - 1.10, hq.comp$d13C, hq.comp$d13C.measured + 1.10, hq.comp$d13C, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(hq.comp$d13C.measured, hq.comp$d13C, pch=1, cex=1.3)
text(-13,4,"a", cex=1.5)


par(mar=c(2,5,1,1))
plot(hq.comp$d18O.measured, hq.comp$d18O, pch=16, col=pal_map[c], xlim=c(-17,1), ylim=c(-17,1),main="",cex = 1.25,
     xlab="",  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{18}, "O"[carbonate], "(\u2030)")),
     xaxt='n')
arrows(hq.comp$d18O.measured, hq.comp$d18O - hq.comp$d18O_sd,hq.comp$d18O.measured, hq.comp$d18O + hq.comp$d18O_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(hq.comp$d18O.measured - 1.09, hq.comp$d18O, hq.comp$d18O.measured + 1.09, hq.comp$d18O,  angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(hq.comp$d18O.measured, hq.comp$d18O, pch=1, cex=1.3)
text(-17,1,"b", cex=1.5)

par(mar=c(4,5,0,0))
plot(dq.comp$d13C.measured, dq.comp$d13C, pch=16, col=pal_map[c], xlim=c(-13,4), ylim=c(-13,4), main="",cex = 1.25,
     xlab=expression(paste("Observed ",delta^{13}, "C"[carbonate], "(\u2030)")),  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{13}, "C"[carbonate], "(\u2030)")))
arrows(dq.comp$d13C.measured, dq.comp$d13C - dq.comp$d13C_sd,dq.comp$d13C.measured, dq.comp$d13C + dq.comp$d13C_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(dq.comp$d13C.measured - 1.10, dq.comp$d13C, dq.comp$d13C.measured + 1.10, dq.comp$d13C, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(dq.comp$d13C.measured, dq.comp$d13C, pch=1, cex=1.3)
text(-13,4,"c", cex=1.5)

par(mar=c(4,5,0,1))
plot(dq.comp$d18O.measured, dq.comp$d18O, pch=16, col=pal_map[c], xlim=c(-17,1), ylim=c(-17,1), main="",cex = 1.25,
     xlab=expression(paste("Observed ",delta^{18}, "O"[carbonate], "(\u2030)")),  cex.axis=1.25, cex.lab=1.25,
     ylab=expression(paste("Predicted ",delta^{18}, "O"[carbonate], "(\u2030)")))
arrows(dq.comp$d18O.measured, dq.comp$d18O - dq.comp$d18O_sd,dq.comp$d18O.measured, dq.comp$d18O + dq.comp$d18O_sd, angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
arrows(dq.comp$d18O.measured - 1.09, dq.comp$d18O, dq.comp$d18O.measured + 1.09, dq.comp$d18O,  angle=90, length=0.1, code = 3, col=alpha("black", 0.2))
abline(0,1)
points(dq.comp$d18O.measured, dq.comp$d18O, pch=1, cex=1.3)
text(-17,1,"d", cex=1.5)

legend("bottomright", title = expression(paste("P"[a]," (mm)")), cex=1.05, fill=pal_map, legend=c("100 - 200", "200 - 300", "300 - 400", "400 - 500", "500 - 600", "600 - 700", "700 - 800"))

dev.off()

## Basic stats
opt_lm_hot_C <- lm(hq.comp$d13C ~ hq.comp$d13C.measured)
opt_lm_dry_C <- lm(dq.comp$d13C ~ dq.comp$d13C.measured)
opt_lm_hot_O <- lm(hq.comp$d18O ~ hq.comp$d18O.measured)
opt_lm_dry_O <- lm(dq.comp$d18O ~ dq.comp$d18O.measured)

summary(opt_lm_hot_C)
summary(opt_lm_dry_C)
summary(opt_lm_hot_O)
summary(opt_lm_dry_O)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

opt_hot_C_rmse <- rmse(hq.comp$d13C.measured - hq.comp$d13C)
opt_dry_C_rmse <- rmse(dq.comp$d13C.measured - dq.comp$d13C)
opt_hot_O_rmse <- rmse(hq.comp$d18O.measured - hq.comp$d18O)
opt_dry_O_rmse <- rmse(dq.comp$d18O.measured - dq.comp$d18O)

opt_hot_C_rmse
opt_dry_C_rmse
opt_hot_O_rmse
opt_dry_O_rmse

diff_hot_C_rmse <- opt_hot_C_rmse - pre_hot_C_rmse
diff_dry_C_rmse <- opt_dry_C_rmse - pre_dry_C_rmse
diff_hot_O_rmse <- opt_hot_O_rmse - pre_hot_O_rmse
diff_dry_O_rmse <- opt_dry_O_rmse - pre_dry_O_rmse

abs(diff_hot_C_rmse) / pre_hot_C_rmse * 100
abs(diff_dry_C_rmse) / pre_dry_C_rmse * 100
abs(diff_hot_O_rmse) / pre_hot_O_rmse * 100
abs(diff_dry_O_rmse) / pre_dry_O_rmse * 100

save.image("Final.RData")


sm_forward_sens = function(MAP, MAT, P_seas, T_seas, Ra, pCO2, ST){
  
  #Assume constant pre-industrial carbon isotope value of atmospheric CO2
  deltaA = -6.5
  
  #Make porosity and tortuosity scale from ST (soil texture parameter).
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  #Evaporated soil water optimization
  esw = esws_opt_hq
  
  #Seasonal precipitation bias optimization
  spre = spres_opt_hq
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = max(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z_mean = z_min + z_thick/2 
  #gamma scale parameter, using 22cm as variance from Retallack fit
  theta = 22^2/z_mean  
  #gamma shape parameter
  k = z_mean / theta
  z = rgamma(nsynth, shape = k, scale = theta)
  #maxiumum depth of L
  z <- pmin(z, L)
  #depth in meters
  z_m <- z/100
  
  #Soil temperatures at depth z
  #T is highest (avg JJA temps) at t = 0.3, mean at t = 0.05 (MAT), low at t = 0.8 (avg DJF temps)
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013, dry sandy soils
  
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  t <- 0.29
  T_soil <- MAT + (T_seas * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h_m <- 0.25 + 0.7 * (CQP / 900)
  #Large variance
  h_var = 0.1^2
  size = h_m*(1-h_m)/h_var - 1
  alpha = h_m * size
  beta = (1-h_m) * size
  h = rbeta(nsynth, alpha, beta)
  RH <- h * 100
  
  # Precipitation O isotope ratios - OIPC mid-latitudes
  dO_P_OOS <- -14.15 + 0.48 * T_OOS
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  # Apply optimized spres to simple mixing model
  dO_P_m <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  dO_P = rnorm(nsynth, dO_P_m, 2.49)
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm_m <- (R_O_atm / RO.vsmow - 1) * 1000
  dO_atm = rnorm(nsynth, dO_atm_m, 1)
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D_m <- ifelse (RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  ETP_D = rnorm(nsynth, ETP_D_m, 0.26)  
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #This noise parmeter limits ETA<CMP_mm but allows variation around ETP, as observed
  ETA_var = rnorm(nsynth, 1, 0.2) 
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)) * ETA_var)) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day_m <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day_m <- R_day_m * rr_opt_hq
  #Using mean residual of 50% based on Raich validation data 
  theta = (R_day_m*0.5) ^ 2 / R_day_m 
  k = R_day_m / theta 
  R_day = rgamma(nsynth, shape = k, scale = theta)
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W_m <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  W = rnorm(nsynth, W_m, 2.40)
  
  #CO2 effect on discrimination, Schubert and Jahren
  deltaP_pCO2_m <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  deltaP_pCO2 = rnorm(nsynth, deltaP_pCO2_m, 0.61)
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Soil water evaporation
  #evap is 6%+/-4% of total ET globally Good et. al. 2010
  e_mean = 0.06  
  e_var = 0.04^2
  size = e_mean*(1-e_mean)/e_var - 1
  alpha = e_mean * size
  beta = (1-e_mean) * size
  E = rbeta(nsynth, alpha, beta) * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  
  # Carb isotope values
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  dat = c(median(dC_Carb), median(dO_Carb), sd(dC_Carb), sd(dO_Carb))
  
  return(dat)
}

#####
#Constant input parameters

#MAP mean annual precipitation in mm
MAP = 500

#P seasonality term gives fraction of MAP falling during calcite 
# precip quarter. 0.25 = "no" seasonality - proportional precipitation for 1/4 of the year. Multiplicative.
P_seas = 0.25

#MAT mean annual precipitation in degrees C
MAT = 15

#T seasonality term gives temperature of calcite precip quarter relative 
# to mat. 0 is no seasonality. T_seas is additive
T_seas = 10

#pCO2 (ppm) atm conc co2
pCO2 = 280

#Soil texture parameter (at ST=0.5, porosity = 0.35 and tortuosity = 0.7)
ST = 0.5

#Atmospheric Radiation
Ra = 20

#Simulations for error
nsynth = 10000

#set seed
set.seed(1234)

MAP_sens = data.frame(d13C_MAP=numeric(0), d18O_MAP=numeric(0), d13C_sd_MAP=numeric(0), d18O_sd_MAP=numeric(0))
for(i in 1:40){
  MAP[i]<- i*40 + 110
  MAP_sens[i,] = sm_forward_sens(MAP[i], MAT, P_seas, T_seas, Ra, pCO2, ST)
}
SensData <- cbind(MAP_sens, MAP)

MAP = 500

T_seas_sens = data.frame(d13C_T_seas=numeric(0), d18O_T_seas=numeric(0), d13C_sd_T_seas=numeric(0), d18O_sd_T_seas=numeric(0))
for(i in 1:40){
  T_seas[i]<- i*0.5 - 5.5
  T_seas_sens[i,] = sm_forward_sens(MAP, MAT, P_seas, T_seas[i], Ra, pCO2, ST)
}

SensData <- cbind(SensData, T_seas_sens, T_seas)

T_seas = 10

P_seas_sens = data.frame(d13C_P_seas=numeric(0), d18O_P_seas=numeric(0), d13C_sd_P_seas=numeric(0), d18O_sd_P_seas=numeric(0))
for(i in 1:40){
  P_seas[i]<- i*0.01 + 0.01
  P_seas_sens[i,] = sm_forward_sens(MAP, MAT, P_seas[i], T_seas, Ra, pCO2, ST)
}

SensData <- cbind(SensData, P_seas_sens, P_seas)

P_seas = 0.25

MAT_sens = data.frame(d13C_MAT=numeric(0), d18O_MAT=numeric(0), d13C_sd_MAT=numeric(0), d18O_sd_MAT=numeric(0))
for(i in 1:40){
  MAT[i]<- i*0.5 - 0.5
  MAT_sens[i,] = sm_forward_sens(MAP, MAT[i], P_seas, T_seas, Ra, pCO2, ST)
}

SensData <- cbind(SensData, MAT_sens, MAT)

MAT = 15

pCO2_sens = data.frame(d13C_pCO2=numeric(0), d18O_pCO2=numeric(0), d13C_sd_pCO2=numeric(0), d18O_sd_pCO2=numeric(0))
for(i in 1:40){
  pCO2[i]<- i*50 + 50
  pCO2_sens[i,] = sm_forward_sens(MAP, MAT, P_seas, T_seas, Ra, pCO2[i], ST)
}

SensData <- cbind(SensData, pCO2_sens, pCO2)

pCO2 = 280

ST_sens = data.frame(d13C_ST=numeric(0), d18O_ST=numeric(0), d13C_sd_ST=numeric(0), d18O_sd_ST=numeric(0))
for(i in 1:40){
  ST[i]<- i * 0.0244 - 0.0244
  ST_sens[i,] = sm_forward_sens(MAP, MAT, P_seas, T_seas, Ra, pCO2, ST[i])
}

SensData <- cbind(SensData, ST_sens, ST)

# Graphing sensitivity tests. 

jpeg("SensitivityTestGraphs.jpg", units="in", res=300, width = 8, height = 11)

layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow=T), widths=c(1.2,1))

par(mar=c(4,5,1,1))
plot(d13C_MAT ~ MAT, data=SensData, ylim=c(-14, -2), cex=2, cex.axis = 1.5, cex.lab = 1.7, main="", lwd=2, type="l", col="black", yaxp=c(-2, -14, 4), xlab=expression(paste("Tm"[a], " (",degree,"C)")), ylab=expression(paste(delta[carbonate], ("\u2030"))), pch=16)
par(new=T)
plot(d18O_MAT ~ MAT, data=SensData, col="darkgray", cex=2, cex.axis = 1.3, cex.lab = 1.5, ylim=c(-14, -2), lwd=2, type="l", xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
text(0,-2.2,"a", cex=1.7)

par(mar=c(4,1,1,1))
plot(d13C_T_seas ~ T_seas, data=SensData, ylim=c(-15, -2), yaxp=c(-2, -14, 4),cex=1.3, cex.axis = 1.5, cex.lab = 1.7, main="", lwd=2, type="l", col="black", yaxt='n', xlab=expression(paste("Tm"[PCQ], " - Tm"[a]," (",degree,"C)")), ylab="", pch=16)
par(new=T)
plot(d18O_T_seas ~ T_seas, data=SensData, col="darkgray", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2), lwd=2, type="l", xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
legend("topright", legend=c("Carbon","Oxygen"), col=c("black","darkgray"), lty=1, pch=19, cex=1.5)
text(-5,-2.2,"b", cex=1.7)


par(mar=c(4,5,1,1))
plot(d13C_MAP ~ MAP, data=SensData, col="black", main="", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2), xlim=c(100, 1750),lwd=2, type="l", yaxp=c(-2, -14, 4), xlab=expression(paste("P"[a]," (mm)")), ylab=expression(paste(delta[carbonate], ("\u2030"))), pch=16)
par(new=T)
plot(d18O_MAP ~ MAP, data=SensData, col="darkgray", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2),  xlim=c(100, 1750),lwd=2, type="l",xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
text(100,-2.2,"c", cex=1.7)

par(mar=c(4,1,1,1))
plot(d13C_P_seas ~ P_seas, data=SensData, col="black", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, main="", yaxp=c(-2, -14, 4), ylim=c(-15, -2), xlim=c(0.01, 0.42), lwd=2, type="l", xaxp=c(0,0.5,5), xlab=expression(paste("Pf"[PCQ])), ylab="", yaxt='n', pch=16)
par(new=T)
plot(d18O_P_seas ~ P_seas, data=SensData, col="darkgray", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2),  xlim=c(0.01, 0.42),lwd=2, type="l", xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
text(0.01,-2.2,"d", cex=1.7)


par(mar=c(4,5,1,1))
plot(d13C_ST ~ ST, data=SensData, col="black", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2), yaxp=c(-2, -14, 4), lwd=2, type="l",xlab="Soil Texture Parameter", ylab=expression(paste(delta[carbonate], ("\u2030"))), pch=16)
par(new=T)
plot(d18O_ST ~ ST, data=SensData, col="darkgray", cex=1.3, cex.axis = 1.5, cex.lab = 1.7, ylim=c(-15, -2),lwd=2, type="l", xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
text(0,-2.2,"e", cex=1.7)

par(mar=c(4,1,1,1))
plot(d13C_pCO2 ~ pCO2, data=SensData, col="black",  cex=1.3, cex.axis = 1.5, cex.lab = 1.7, xlim=c(0, 2000), ylim=c(-15, -2), lwd=2, type="l", xlab=expression(paste(italic('p'),"CO"[2]," (ppm)")), yaxt='n', ylab="", pch=16)
par(new=T)
plot(d18O_pCO2 ~ pCO2, data=SensData, col="darkgray",  cex=1.3, cex.axis = 1.5, cex.lab = 1.7, xlim=c(0, 2000), ylim=c(-15, -2),lwd=2, type="l", xlab="", ylab="", xaxt='n', yaxt='n', pch=16)
text(0,-2.2,"f", cex=1.7)

dev.off()

setwd("C:/Users/u0730824/Dropbox/worldclim")

#Prepare gridded climate data
library(raster)
library(rgdal)

# Read in rasters if they are already saved
map <- raster("wc2.0_2.5m_MAP.tif")
mat <- raster("wc2.0_2.5m_MAT.tif")
hqp.frac <- raster("wc2.0_2.5m_hqp_frac.tif")
dqp.frac <- raster("wc2.0_2.5m_dqp_frac.tif")
hqt.offset <- raster("wc2.0_2.5m_hqt_offset.tif")
dqt.offset <- raster("wc2.0_2.5m_dqt_offset.tif")
Ra <- raster("Ra.tif")
dryq <- raster("dryq.tif")

## Create prediction maps for oxygen and carbon isotopes of pedogenic carbonates to test against
# Create raster for estimating radiation values at each latitude

Ra <- raster(ncol=8640, nrow=4320, res=c(0.04166667, 0.04166667), xmn=-180, xmx=180, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

for (i in 1:ncol(Ra)){
  
  lat = 0.0416667 * (i - 1 - 4320/2)
  Ra[i,] = 34.5 * cos(abs(lat * 0.01745))
  
}


plot(dryq)
dryq_S_ex <- extent(c(-180, 180, -90, 0))
dryq_S <- crop(dryq, dryq_S_ex)
dryq_N_ex <- extent(c(-180, 180, 0, 90))
dryq_N <- crop(dryq, dryq_N_ex)

plot(dryq_S)
dryq_S[dryq == 1] <- 5
dryq_S[dryq == 3] <- 1
dryq_S[dryq_S == 5] <- 3
dryq_S[dryq == 2] <- 5
dryq_S[dryq == 4] <- 2
dryq_S[dryq_S == 5] <- 4
plot(dryq_S)

dryq <- merge(dryq_N, dryq_S)
plot(dryq)

writeRaster(dryq, "dryq.tif", overwrite=T)

# Build fuctions for each isotope system and PCQ season

raster_opt_hq_dC = function(MAP, MAT, P_seas, T_seas, Ra) {
  
  #Assume constant pre-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7
  ST = 0.5
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  MAP = pmax(MAP, 5)
  P_seas = pmax(P_seas, 0.001)
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = pmax(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z = z_min + z_thick/2 
  #maxiumum depth of L
  z <- pmin(z, L) 
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- 0.29
  T_soil <- MAT + (T_seas * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)  #mol/cm^3
  
  #Relative humidity based on quarterly precip
  h <- 0.25 + 0.7 * (CQP / 900)
  RH <- h * 100
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D <- ifelse (is.na(RH) | RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)))) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day <- R_day * rr_opt_hq
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  
  #CO2 effect on discrimination, Schubert and Jahren 2012
  deltaP_pCO2 <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Carb carbon isotope value
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  
  return(dC_Carb)
  
}

raster_opt_dq_dC = function(MAP, MAT, P_seas, T_seas, Ra, hqt.offset, dryq) {
  
  #Assume constant pre-industrial pCO2 and carbon isotope value of atmospheric CO2
  deltaA = -6.5
  pCO2 = 280
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7
  ST = 0.5
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Diffusion ratio of carbon isotopes 13C/12C
  DIF.ratio = 1.004443
  
  #Isotope ratio constants
  RC.vpdb = 0.011237
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Basal depth
  L = 100
  
  #Climate
  MAP = pmax(MAP, 5)
  P_seas = pmax(P_seas, 0.001)
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = pmax(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z = z_min + z_thick/2 
  #maxiumum depth of L
  z <- pmin(z, L) 
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(is.na(dryq) | dryq == 3, 0.29, ifelse(dryq == 1, 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2*3.1415) * t - z/d) / exp(z/d)) 
  T_soil_K <- T_soil + 273.15
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)  #mol/cm^3
  
  #Relative humidity based on quarterly precip
  h <- 0.25 + 0.7 * (CQP / 900)
  RH <- h * 100
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D <- ifelse (is.na(RH) | RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)))) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Respiration rate, Raich 2002, gC/m2day
  R_day <- 1.25 * exp(0.0545 * CQT) * CMP_cm / (4.259 + CMP_cm)
  #Optimization (rr)
  R_day <- R_day * rr_opt_dq
  #Convert to molC/cm^3s
  R_day = R_day / (12.01 * 100^2)  #molC/cm2day
  R_sec <- R_day / (24 * 3600)  #molC/cm2s
  R_sec = R_sec / L / pores #molC/cm3s
  
  #CO2 Diffusion coefficient - based on temp, FAP, tort
  DIFC = FAP * tort * 0.1369 * (T_soil_K / 273.15) ^ 1.958
  
  #Water limitation effect of discriminaton, Diefendorf 2010, recalculated by Schubert and Jahren and made into a correction from water "saturation"
  W <- 22.65 - (1.2 * (MAP + 975)) / (27.2 + 0.04 * (MAP + 975))
  
  #CO2 effect on discrimination, Schubert and Jahren 2012
  deltaP_pCO2 <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))
  
  #Plant discrimination
  deltaP <- deltaA - (deltaP_pCO2 - W)
  
  #Convert pCO2 to units mol/cm^3
  pCO2_mcc = pCO2 / (0.08206 * T_soil_K * 10^9)
  
  #Soil CO2 C isotopes
  deltaA_hat <- (deltaA / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaA / 1000 + 1))
  deltaP_hat <- (deltaP / 1000 + 1) * RC.vpdb / (1 + RC.vpdb * (deltaP / 1000 + 1))
  dC_Soil.resp = R_sec/(DIFC) * (L * z - z^2 / 2)
  dC_Soil.num = dC_Soil.resp * DIF.ratio * deltaP_hat + pCO2_mcc * deltaA_hat
  dC_Soil.denom = dC_Soil.resp * (1 - DIF.ratio * deltaP_hat) + pCO2_mcc * (1 - deltaA_hat)
  dC_Soil = (dC_Soil.num / (dC_Soil.denom * RC.vpdb) - 1) * 1000
  R_Soil <- (dC_Soil / 1000 + 1) * RC.vpdb
  
  # Romanek et. al. 1992 Fractionation factor CO2 - calcite
  A_CO2_Carb <- 1 / (1.01198 - 1.2e-4 * T_soil) 
  R_Carb <- R_Soil / A_CO2_Carb
  
  #Calculate Sz
  Sz <- (dC_Soil.resp) * (0.08206 * T_soil_K * 10^9)
  
  #Carb carbon isotope value
  dC_Carb <- (R_Carb / RC.vpdb - 1) * 1000
  
  return(dC_Carb)
}

raster_opt_hq_dO = function(MAP, MAT, P_seas, T_seas, Ra){
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7
  ST = 0.5
  pores = 0.1 + 0.5 * ST
  tort = 0.6 + 0.2 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Isotope ratio constants
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Evaporated soil water optimization
  esw = esws_opt_hq
  
  #Seasonal precipitation bias optimization
  spres = spres_opt_hq
  
  #Climate
  MAP = pmax(MAP, 5)
  P_seas = pmax(P_seas, 0.001)
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = pmax(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP/12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z = z_min + z_thick/2 
  #maxiumum depth
  z = pmin(z, 100)
  #depth in meters for oxygen calcs
  z_m = z/100
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2*0.0007)/((2*3.1415/3.154e7)*0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- 0.29
  T_soil <- MAT + (T_seas * sin((2*3.1415) * t - z/d) / exp(z/d))
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h <- 0.25 + 0.7 * (CQP / 900)
  RH <- h * 100
  
  #Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS 
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  dO_P <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm <- (R_O_atm / RO.vsmow - 1) * 1000
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D <- ifelse (is.na(RH) | RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)))) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Soil water evaporation
  #evap is 6% of total ET globally Good et. al. 2010
  e_mean = 0.06
  E = e_mean * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  return(dO_Carb)
  
}

raster_opt_dq_dO = function(MAP, MAT, P_seas, T_seas, Ra, hqt.offset, dryq){
  
  #Make porosity and tortuosity scale from ST (soil texture parameter). Average of 0.35 and 0.7
  ST = 0.5
  pores = 0.7 * ST
  tort = 1.4 * ST
  
  # Solar radiation estimate - Hargreaves and Samani (1982)
  Rs <- Ra * 0.16 * sqrt(12)
  
  #Isotope ratio constants
  RO.vsmow = 0.0020052
  RO.vpdb = 0.002067181
  
  #Evaporated soil water optimization
  esw = esws_opt_dq
  
  #Seasonal precipitation bias optimization
  spre = spres_opt_dq
  
  #Climate
  #At least MAP of 5 mm and P_seas of 0.001
  MAP = pmax(MAP, 5)
  P_seas = pmax(P_seas, 0.001)
  CQP <- MAP * P_seas
  #At least 3mm rain in the carbonate precipitation quarter
  CQP = pmax(CQP, 3)
  CMP_mm <- CQP / 3
  CMP_cm <- CQP / 30
  CQT <- MAT + T_seas
  CQT_K <- CQT + 273.15
  T_OOS <- (MAT * 4 -  CQT) / 3
  
  #Depth to carbonate
  #top of Bk equation based on Retallack 2005 data
  z_min <- MAP * 0.0925 + 13.4  
  #thickness of Bk, CMP as proxy for seasonality 
  z_thick = abs(CMP_mm - MAP / 12) * 0.74 + 17.3 
  #find middle of Bk in cm
  z = z_min + z_thick / 2
  #maxiumum depth
  z <- pmin(z, 100) 
  #depth in meters for oxygen calcs
  z_m <- z / 100
  
  #Soil temperatures at depth z
  #Assume thermal conductivity = 0.0007 cal / cm2  s  *C, volumetric heat capacity of 0.3 cal / cm2 *C, Quade 2013
  d <- sqrt((2 * 0.0007) / ((2 * 3.1415 / 3.154e7) * 0.3))
  #T is highest (avg JJA temps) at t = 0.29, mean at t = 0 (avg MAM and SON temps), low at t = 0.79 (avg DJF temps)
  t <- ifelse(is.na(dryq) | dryq == 3, 0.29, ifelse(dryq == 1, 0.79, 0))
  T_soil <- MAT + (hqt.offset * sin((2 * 3.1415) * t - z / d) / exp(z / d))
  T_soil_K <- T_soil + 273.15
  
  #Relative humidity based on quarterly precip
  h <- 0.25 + 0.7 * (CQP / 900)
  RH <- h * 100
  
  #Precipitation O isotope ratios - OIPC
  dO_P_OOS <- -14.15 + 0.48 * T_OOS 
  dO_P_PCQ <- -14.15 + 0.48 * CQT
  dO_P <- (dO_P_PCQ * P_seas + dO_P_OOS * (1 - spre) * (1 - P_seas)) / (P_seas + (1 - spre) * (1 - P_seas))
  R_O_P = (dO_P / 1000 + 1) * RO.vsmow
  
  #Atmospheric vapor O isotope ratios
  A_atmP <- 2.71828 ^ ((5.9702e6 / CQT_K ^ 2 - 3.2801e4 / CQT_K + 52.227) / 1000)
  R_O_atm <- R_O_P / A_atmP
  dO_atm <- (R_O_atm / RO.vsmow - 1) * 1000
  
  #Potential Evapotranspiration
  #PET in mm/day, Turc 1961
  ETP_D <- ifelse (is.na(RH) | RH < 50, 0.013 * (CQT / (CQT + 15)) * (23.8856 * Rs + 50)* (1 + ((50 - RH) / 70)), 0.0133 * (CQT / (CQT + 15)) * (23.885 * Rs + 50))
  #convert to mm/month
  ETP_M <- ETP_D * 30 
  
  #Actual Evapotranspiration
  #AET in mm/quarter from Budyko curve
  ETA = CQP * (1 / (sqrt(1 + (1 / ((ETP_M / (CQP)))) ^ 2))) 
  
  #Free air porosity
  #Scales volumetrically w/ excess precipitation relative to pore space, assume a minimum of 5% volumetric water content
  FAP <- pmin((pores - ((CMP_mm - ETA)/(L*10*pores))), pores-0.05)
  #At least 1% free air porosity
  FAP = pmax(FAP,0.01) 
  
  #Soil water evaporation
  #evap is 6% of total ET globally Good et. al. 2010
  e_mean = 0.06
  E = e_mean * ETA 
  #minimum of 1 mm of evap / month
  E = pmax(E, 1) 
  
  #Soil water diffusion evaporation balance
  #evaporation in m/sec
  E_s <- E / (1000 * 30 * 24 * 3600) 
  #Diffusion, scaled to temperature, soil water content and tortuosity
  DIFO <- 1.637e-8 * (T_soil_K / 216.25 - 1) ^ 2.074 * (pores-FAP) * tort
  #mean penetration depth of evap, in m
  z_i <- DIFO / E_s 
  #Diffusion ratio factor
  DRF <- 1 + 0.8 * (1 / 0.9723 - 1)
  #Surface water isotopes
  R_O_surface <- ((1 - h) * DRF * R_O_P + h * R_O_atm) / (1 / A_atmP)
  #Water isotopes at depth
  R_O_soil <- ((R_O_surface - R_O_P) * 2.71828 ^ (-z_m / z_i)) + R_O_P
  #soil water is esw % evaporated fraction
  R_O_soil = R_O_soil * esw + R_O_P * (1 - esw)  
  dO_soil <- (R_O_soil/RO.vsmow - 1) * 1000
  
  #Soil carbonate O isotope fractionation - Kim and O'neal 1997
  A_O <- 2.71828 ^ ((1.803e4 / T_soil_K - 32.42) / 1000)
  R_O_Carb <- R_O_soil * A_O
  dO_Carb <- (R_O_Carb / RO.vpdb - 1) * 1000
  
  return(dO_Carb)
  
}

#Vectorize the functions so we can use overlay
raster_opt_hq_dO <- Vectorize(raster_opt_hq_dO)
raster_opt_dq_dO <- Vectorize(raster_opt_dq_dO)
raster_opt_hq_dC <- Vectorize(raster_opt_hq_dC)
raster_opt_dq_dC <- Vectorize(raster_opt_dq_dC)

#Use overlay to do complex raster calcs
dO_Carb_map_hq <- overlay(map, mat, hqp.frac, hqt.offset, Ra, fun=raster_opt_hq_dO)
dC_Carb_map_hq <- overlay(map, mat, hqp.frac, hqt.offset, Ra, fun=raster_opt_hq_dC)
dO_Carb_map_dq <- overlay(map, mat, dqp.frac, dqt.offset, hqt.offset, Ra, dryq, fun=raster_opt_dq_dO)
dC_Carb_map_dq <- overlay(map, mat, dqp.frac, dqt.offset, hqt.offset, Ra, dryq, fun=raster_opt_dq_dC)

dO_Carb_map_diff <- dO_Carb_map_hq - dO_Carb_map_dq
dC_Carb_map_diff <- dC_Carb_map_hq - dC_Carb_map_dq

# Raster of plant discrimination / difference between d13Cplant and d13Ccarb
pCO2 <- 280

#Water limitation of discriminaton raster, Diefendorf
W <- 22.65 - (1.2 * (map + 975)) / (27.2 + 0.04 * (map + 975))

#CO2 effect on discrimination raster, Schubert
deltaP_pCO2 <- 28.26 * 0.35 * (pCO2 + 15) / (28.26 + 0.35 * (pCO2 + 15))

#Plant discrimination raster
deltaP <- deltaA - (deltaP_pCO2 - W)
plant_carb_diff_map <- dC_Carb_map_hq - deltaP

plot(plant_carb_diff_map)

#Raster of difference between mean precipitation and carbonate o isotope ratio
dO_P_map <- -14.15 + 0.48 * mat
O_diff_map <- dO_Carb_map_hq - dO_P_map

plot(O_diff_map)

## Filter a raster to where we may find pedogenic carbonates worldwide - either MAP low overall (MAP < 500) or pronounced dry season (Dry season fraction < 0.1)
# make values NA for each condition
map <- raster("wc2.0_2.5m_prec_ma.tif")
dqp.frac <- raster("wc2.0_2.5m_dqp_frac.tif")
plot(map)
modern_carb_map <- Which(map > 500)
plot(modern_carb_map)
dqp <- map * dqp.frac
modern_carb_dqp <- Which(dqp > 100)
plot(modern_carb_dqp)

# Combine
modern_carb <- mosaic(modern_carb_map, modern_carb_dqp, fun=sum)
plot(modern_carb)
modern_carb[modern_carb < 1.5] <- NA

#Save rasters
writeRaster(dO_Carb_map_hq, "dO_Carb_map_hq.tif", overwrite=T)
writeRaster(dC_Carb_map_hq, "dC_Carb_map_hq.tif", overwrite=T)
writeRaster(dO_Carb_map_dq, "dO_Carb_map_dq.tif", overwrite=T)
writeRaster(dC_Carb_map_dq, "dC_Carb_map_dq.tif", overwrite=T)
writeRaster(dC_Carb_map_diff, "dC_Carb_map_diff.tif", overwrite=T)
writeRaster(dO_Carb_map_diff, "dO_Carb_map_diff.tif", overwrite=T)
writeRaster(modern_carb, "modern_carb.tif", overwrite=T)
writeRaster(O_diff_map, "O_diff_map.tif", overwrite=T)
writeRaster(plant_carb_diff_map, "plant_carb_diff_map.tif", overwrite=T)

#Read in rasters if they aren't already
dC_Carb_map_hq <- raster("dC_Carb_map_hq.tif")
dC_Carb_map_dq <- raster("dC_Carb_map_dq.tif")
dO_Carb_map_hq <- raster("dO_Carb_map_hq.tif")
dO_Carb_map_dq <- raster("dO_Carb_map_dq.tif")
dC_Carb_map_diff <- raster("dC_Carb_map_diff.tif")
dO_Carb_map_diff <- raster("dO_Carb_map_diff.tif")
modern_carb <- raster("modern_carb.tif")
O_diff_map <- raster("O_diff_map.tif")
plant_carb_diff_map <- raster("plant_carb_diff_map.tif")

# Graph

jpeg("OPT_MAPS.jpg", units="in", res=300, width=9.8, height=7)

layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow=T), heights=c(1,1,1,1), widths=c(1,1,1,1))

map_cols_O <- colorRampPalette(c("white", "orange1", "yellow2", "green4"), bias = 0.9, space="Lab")
map_cols_C <- colorRampPalette(c("white", "orange1", "yellow2", "green4"), bias = 3, space="Lab")

par(mar=c(1,0,1,4))
plot(dO_Carb_map_hq, zlim=c(-30, 5), col = map_cols_O(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(delta^{18}, "O"[wq] ," (\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"a", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

par(mar=c(1,0,1,4))
plot(dC_Carb_map_hq, zlim=c(-20, 25), col = map_cols_C(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(delta^{13}, "C"[wq] ," (\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"b", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

par(mar=c(1,0,1,4))
plot(dO_Carb_map_diff, col = map_cols_O(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(Delta^{18}, "O"[wq - dq] ,"(\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"c", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

par(mar=c(1,0,1,4))
plot(dC_Carb_map_diff, col = map_cols_O(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(Delta^{13}, "C"[wq - dq] ," (\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"d", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

par(mar=c(1,0,1,4))
plot(O_diff_map, col = map_cols_C(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(Delta^{18}, "O"[wq - Pa] ," (\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"e", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

par(mar=c(1,0,1,4))
plot(plant_carb_diff_map, col = map_cols_C(200), xaxt = 'n', yaxt = 'n')
title("")
mtext(expression(paste(Delta^{13}, "C"[wq - plant] ," (\u2030)")), 4, line=1.1, cex=1)
text(-170, 80 ,"f", cex=1.5)
plot(modern_carb, col=grey(0.5, alpha=0.4), add = T, legend=F)

dev.off()

#Save workspace

save.image(file="OPT_MAPS.RData")


