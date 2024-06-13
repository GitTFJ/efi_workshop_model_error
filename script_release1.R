



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
