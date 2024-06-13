



#RELEASE 1
library(ggplot2)
download.file("https://github.com/GitTFJ/efi_workshop_model_error/raw/main/data_list.rds", "data_list.rds")
data_list = readRDS("data_list.rds") #Load data

plot(data_list$climate_list[[1]])#Visualise climate in 1970. Degrees C
plot(data_list$climate_list[[31]])#Visualise climate in 2000. Degrees C
plot(data_list$climate_list[[61]])#Visualise climate projection in 2030. Degrees C

plot(data_list$habitat)#Visualise climate projection in 2030. Degrees C

ggplot(data_list$density[which(data_list$density$time == 1970),]) +
  geom_tile(aes(x = lon, y = lat, fill = is.na(y))) +
  scale_fill_manual(guide = "none", values = "black") +
  theme_classic() #Look at the distribution of data over space

ggplot(data_list$density) +
  geom_histogram(aes(x = y)) +
  theme_classic() #Very skewed

ggplot(data_list$density) +
  geom_histogram(aes(x = log(y))) +
  theme_classic() #Exponential population growth. And there are no zeros

assess_sites = c("16.5_16.5", "1.5_19.5", "14.5_4.5", "0.5_0.5")

ggplot(data_list$density[data_list$density$site %in% assess_sites,]) +
  geom_line(aes(x = time, y = log(y))) +
  facet_wrap(site~., scales = "free") +
  theme_classic() #Density time sereis in each site

View(data_list$density) #Missing density data from 2025 onwards


#RELEASE 2
library(prioritizr)
library(terra)
library(brms)
library(dplyr)
library(pgirmess)

m = brm(log(y) ~ s(temp) + hab, data = data_list$density, family = gaussian(), iter = 1000, chains = 1, cores = 1) #A simple model strucutre and introduction to brms


#RELEASE 3
summary(m) #Investigate the model summary
plot(conditional_smooths(m, smooths = "s(temp)"), ask = FALSE) #We can look at confitional effects of smooths
plot(conditional_effects(m, effects = "hab"), ask = FALSE) #And linear terms

data_list$density$pred = predict(m)[,1] #Extrating predicted values
data_list$predict$pred = predict(m, newdata = data_list$predict)[,1] #Extracting forecasts

#We can look at our model predictions
ggplot() +
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = y)) + #Observed values
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = exp(pred)), colour = "darkred") +  #Predicted values
  geom_line(data = data_list$predict[data_list$predict$site %in% assess_sites,], 
            aes(x = time, y = exp(pred)), colour = "red") + #Forecast
  scale_y_log10() +
  facet_wrap(site~., scales = "free") +
  theme_classic()

#Here we can store the mean residual value
data_list$density$resid = resid(m1)[,1]

#This segment of code allows you to explore whether their is temporal structure in the residuals. See: https://www.rpubs.com/markpayne/164550
par(mfrow=c(2,2))
for(a in assess_sites){
  plot(acf(data_list$density[data_list$density$site %in% a,]$resid))
}

#This segment of code allows you to explore whether their is spatial structure in the residuals. See: https://rpubs.com/Averysaurus/spatial_epi_4
par(mfrow=c(1,1))
spa_cor = correlog(coords = data_list$density[data_list$density$time == 1970,c("lon", "lat")], 
                   z = data_list$density[data_list$density$time == 1970,c("resid")], 
                   method = "Moran", 
                   nbclass = 10) 
plot(spa_cor) #

#We can also just look at the residuals over space, but this can be tough when the data are sparse
ggplot(data_list$density[data_list$density$time %in% sample(unique(data_list$density$time),4),]) +
  geom_tile(aes(x = lon, y = lat, fill = resid)) +
  facet_wrap(time~.) +
  theme_classic() #Looks like there is some spatial signal

#We can look how the residuals are distrubuted by latitude and longitude too, but again this can be noisy!
ggplot(data_list$density) +
  geom_jitter(aes(x = lat, y = resid)) +
  theme_classic()

ggplot(data_list$density) +
  geom_jitter(aes(x = lon, y = resid)) +
  theme_classic()

#We can also look how the residuals are distrubuted through time, avergaing across sites
ggplot(data_list$density[data_list$density$site %in% sample(unique(data_list$density$site),4),]) +
  geom_boxplot(aes(x = time, group = time, y = resid)) +
  theme_classic() #Weird structure through time

#And also just visualise residuals through time, seperated by space
ggplot(data_list$density[data_list$density$site %in% sample(unique(data_list$density$site),4),]) +
  geom_line(aes(x = time, group = site, y = resid)) +
  facet_wrap(site~.) +
  theme_classic() #



#RELEASE 4
m1 = brm(log(y) ~ s(temp) + hab  + ar(p = 1, gr = site), data = data_list$density, family = gaussian(), iter = 1000, chains = 1, cores = 1)  
summary(m1)
data_list$density$pred1 = predict(m1)[,1]
data_list$predict$pred1 = predict(m1, newdata = data_list$predict)[,1]

prediction_variable = "pred1"

density_subset = data_list$density %>%
  mutate(pred_adj = data_list$density[,prediction_variable]) %>%
  select(site, time, pred_adj) %>%
  filter(time == 2024)

predict_subset = data_list$predict %>%
  mutate(pred_adj = data_list$predict[,prediction_variable]) %>%
  select(site, time, pred_adj) %>%
  filter(time == 2025)

data_list$predict = left_join(data_list$predict, predict_subset[,c(1,3)], by = "site")
data_list$predict[,prediction_variable] = data_list$predict[,prediction_variable] - data_list$predict$pred_adj
data_list$predict$pred_adj = NULL
data_list$predict = left_join(data_list$predict, density_subset[,c(1,3)], by = "site")
data_list$predict[,prediction_variable] = data_list$predict[,prediction_variable] + data_list$predict$pred_adj
data_list$predict$pred_adj = NULL


ggplot() +
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = y)) +
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = exp(pred1)), colour = "darkred") +  
  geom_line(data = data_list$predict[data_list$predict$site %in% assess_sites,], 
            aes(x = time, y = exp(pred1)), colour = "red") +
  scale_y_log10() +
  facet_wrap(site~., scales = "free") +
  theme_classic()


#RELEASE 5
m2 = brm(log(y) ~ s(temp) + hab  + t2(lat, lon) + ar(p = 1, gr = site), data = data_list$density, family = gaussian(), iter = 1000, chains = 2, cores = 2)  
plot(conditional_smooths(m2, smooths = "t2(lat, lon)"), ask = FALSE, stype = "raster") 
plot(conditional_smooths(m2, smooths = "s(temp)"), ask = FALSE) 
summary(m2)
data_list$density$pred2 = predict(m2)[,1]
data_list$predict$pred2 = predict(m2, newdata = data_list$predict)[,1]

prediction_variable = "pred2"

density_subset = data_list$density %>%
  mutate(pred_adj = data_list$density[,prediction_variable]) %>%
  select(site, time, pred_adj) %>%
  filter(time == 2024)

predict_subset = data_list$predict %>%
  mutate(pred_adj = data_list$predict[,prediction_variable]) %>%
  select(site, time, pred_adj) %>%
  filter(time == 2025)

data_list$predict = left_join(data_list$predict, predict_subset[,c(1,3)], by = "site")
data_list$predict[,prediction_variable] = data_list$predict[,prediction_variable] - data_list$predict$pred_adj
data_list$predict$pred_adj = NULL
data_list$predict = left_join(data_list$predict, density_subset[,c(1,3)], by = "site")
data_list$predict[,prediction_variable] = data_list$predict[,prediction_variable] + data_list$predict$pred_adj
data_list$predict$pred_adj = NULL


ggplot() +
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = y)) +
  geom_line(data = data_list$density[data_list$density$site %in% assess_sites,], 
            aes(x = time, y = exp(pred2)), colour = "darkred") +  
  geom_line(data = data_list$predict[data_list$predict$site %in% assess_sites,], 
            aes(x = time, y = exp(pred2)), colour = "red") +
  scale_y_log10() +
  facet_wrap(site~., scales = "free") +
  theme_classic()

