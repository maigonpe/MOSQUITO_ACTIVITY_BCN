################################################################################

#Modelling mosquito abundance with environmental variables

################################################################################

#Load libraries

library(readr)
library(lubridate)
library(knitr)
library(ggsci)
library(magrittr) 
library(dplyr) 
library(nortest)
library(activity)
library(tidyr)
library (readr)
library(zoo)
library(stringi)
library(data.table)
library(stringr)
library(ggplot2)
library(patchwork) 
library(car) # vif
library(AER) # overdispersion
library(pscl) # Pseudo-R²
library(performance)
library(pscl) # Hurdle model
library(MASS) # Negbin models
library(splines)
library(glmmTMB)
library(mgcv) #GAM
library(ks)
library(randomForest)
library(caret)
library(rsample)
library(betareg)
library(pROC) #AUC

#Set up working directory

setwd("/yourdirectory")

#Load data

df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps_female.csv")

################################################################################

#Data exploration

################################################################################

#Correlation between environmental variables

corr_matrix <- cor(df_model_allTraps %>% 
               dplyr::select(temp_xema,av_temp_24h, av_temp_12h, av_temp_6h,
                             RH_xema, av_RH_24h, av_RH_12h, av_RH_6h,
                             precip_xema, cum_precip_24h, cum_precip_12h, cum_precip_6h,
                             solar_rad_xema,
                             dist_sunrise, dist_sunset, HOD_utc, daylength, DOY, 
                             ADH, cum_ADH_24h, cum_ADH_12h, cum_ADH_6h)%>%
               na.omit())

melted_corr_matrix <- as.data.frame(corr_matrix) %>%
  mutate(Var1 = rownames(.)) %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

#tiff("corr_matrix_red.tiff", width = 6, height = 4, units = "in", res = 300)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue1", high = "firebrick2", mid = "snow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation index") +
  geom_text(aes(label = round(Correlation, 1)), color = "black", size = 2.5) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1),  
    axis.ticks = element_blank(),  
    axis.ticks.length = unit(0, "pt"),  
    panel.grid = element_blank()  
  ) +
  coord_fixed()
#dev.off()

#Correlation index = 1 : temp_xema & ADH, av_temp_24h & cum_ADH_24h, av_temp_12h & cum_ADH_12h,
#av_temp_6h & cum_ADH_6h.
#Delete ADH, cum_ADH_24h, cum_ADH_12h and cum_ADH_6h from the df and repeat the correlation

corr_matrix <- cor(df_model_allTraps %>% 
                     dplyr::select(temp_xema,av_temp_24h, av_temp_12h, av_temp_6h,
                                   RH_xema, av_RH_24h, av_RH_12h, av_RH_6h,
                                   precip_xema, cum_precip_24h, cum_precip_12h, cum_precip_6h,
                                   solar_rad_xema,
                                   dist_sunrise, dist_sunset, HOD_utc, daylength, DOY)%>%
                     na.omit())

#Correlation index = 0.9 : temp_xema & av_temp_6h, av_temp_6h & av_temp_12h, av_temp_12h & av_temp_24h, av_RH_6h & av_RH_12h
#Correlation index = -0.9 : DOY & daylength
#Delete av_temp_6h, av_RH_6h, av_temp_24h, and DOY and repeat the correlation

corr_matrix <- cor(df_model_allTraps %>% 
                     dplyr::select(temp_xema, av_temp_12h, 
                                   RH_xema, av_RH_24h, av_RH_12h,
                                   precip_xema, cum_precip_24h, cum_precip_12h, cum_precip_6h,
                                   solar_rad_xema,
                                   dist_sunrise, dist_sunset, HOD_utc, daylength)%>%
                     na.omit())

#Correlation index = 0.8 : HOD_utc & dist_sunrise, cum_precip_6h & cum_precip_12h, cum_precip_12h & cum_precip_24h
#av_RH_24h & av_RH_12h
#Delete HOD_utc, cum_precip_6h, cum_precip_24h, av_RH_24h and repeat the correlation

corr_matrix <- cor(df_model_allTraps %>% 
                     dplyr::select(temp_xema, RH_xema, precip_xema, solar_rad_xema,
                                   dist_sunset,dist_sunrise, daylength,cum_precip_12h, 
                                   av_temp_12h, av_RH_12h)%>%
                     na.omit())

#Correlation index = 0.7 : temp_xema & av_temp_12h
#Delete av_temp_12h

corr_matrix <- cor(df_model_allTraps %>% 
                     dplyr::select(temp_xema, RH_xema, precip_xema, solar_rad_xema,
                                   dist_sunset, dist_sunrise, daylength,cum_precip_12h, 
                                   av_RH_12h)%>%
                     na.omit())

#Variance Inflation Factor (VIF)

lm_Ae <- lm(Aedes_SE ~ temp_xema + RH_xema + precip_xema + solar_rad_xema +
                       dist_sunset + dist_sunrise + daylength + cum_precip_12h +
                       av_RH_12h,
                       data = df_model_allTraps)
vif(lm_Ae)

#Ensure all variables have a VIF < 5

#Histogram and normality

ggplot(df_model_allTraps, aes(x = Aedes_SE)) + 
  geom_histogram(bins = 30, fill = "blue") + 
  labs(title = "Aedes counts / number of active traps")

ggplot(df_model_allTraps, aes(x = Culex_SE)) + 
  geom_histogram(bins = 30, fill = "red") + 
  labs(title = "Culex counts / number of active traps")

ad.test(df_model_allTraps$Aedes_SE) #p-value < 0.05
ad.test(df_model_allTraps$Culex_SE) #p-value < 0.05
#As deduced from the histogram, data distribution is not normal

#One-to-one relationships between explanatory and response variables

# List of explanatory variables considered for modeling
exp_vars <- c("temp_xema", "av_RH_12h", "cum_precip_12h", "dist_sunrise", "dist_sunset", "daylength")

#Aedes
plot_Ae <- lapply(exp_vars, function(var) {
  ggplot(df_model_allTraps, aes(x = .data[[var]], y = Aedes_SE)) +
    geom_point(alpha = 0.4, color = "darkgrey") +
    labs(x = var, y = "Aedes_SE") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = "bold"), 
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )})
panel_Ae <- wrap_plots(plot_Ae, ncol = 2)+
  plot_annotation(title = "a",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))# Combine plots
print(panel_Ae)

#Culex
plot_Cx <- lapply(exp_vars, function(var) {
  ggplot(df_model_allTraps, aes(x = .data[[var]], y = Culex_SE)) +
    geom_point(alpha = 0.4, color = "darkgrey") +
    labs(x = var, y = "Culex_SE") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = "bold"), 
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )
})

panel_Cx <- wrap_plots(plot_Cx, ncol = 2)+
  plot_annotation(title = "b",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))
print(panel_Cx)

#Relationships are non-linear

################################################################################

#Random Forests (RF)

################################################################################

df_model_allTraps_clean <- na.omit(df_model_allTraps) # Ensure no NAs

#Aedes

# Train model with CV

set.seed(123)
rf_Ae13 <- train(
  Aedes_SE ~ temp_xema + dist_sunrise + dist_sunset + daylength + cum_precip_12h + av_RH_12h ,
  data = trainData_Ae,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,     #Tune
  ntree = 500,
  importance = TRUE
)

# Best mtry selected
print(rf_Ae13$bestTune) 

#Visualize tunning results
plot(rf_Ae13)

# Predict on test set
pred_rf_Ae13 <- predict(rf_Ae13, newdata = testData_Ae) 

# Accuracy assessment
sqrt(mean((testData_Ae$Aedes_SE - pred_rf_Ae13)^2)) #RMSE = 0.26

cor(testData_Ae$Aedes_SE, pred_rf_Ae13)^2 #R² = 0.53

# Variable importance
varImpPlot(rf_Ae13$finalModel)#Predictive accuracy: dist_sunrise > dist_sunset > cum_precip_12h
#Node purity: dist_sunrise > dist_sunset > temp_xema

#Visualize predictions

pred_results_Ae13 <- testData_Ae
pred_results_Ae13$Aedes_SE_pred <- predict(rf_Ae13, newdata = testData_Ae)

################################################################################

#Culex

# Train model with CV
set.seed(123)
rf_Cx13 <- train(
  Culex_SE ~ temp_xema + dist_sunrise + dist_sunset + daylength + cum_precip_12h + av_RH_12h ,
  data = trainData_Cx,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,     #Tune
  ntree = 500,
  importance = TRUE
)

# Best mtry selected
print(rf_Cx13$bestTune)

#Visualize tunning results
plot(rf_Cx13)

# Predict on test set
pred_rf_Cx13 <- predict(rf_Cx13, newdata = testData_Cx)

# Accuracy assessment
sqrt(mean((testData_Cx$Culex_SE - pred_rf_Cx13)^2)) #RMSE = 0.19

cor(testData_Cx$Culex_SE, pred_rf_Cx13)^2 #R² = 0.33

# Variable importance
varImpPlot(rf_Cx13$finalModel)#Predictive accuracy: temp_xema > cum_precip_12h > dist_sunrise
#Node purity: dist_sunset > dist_sunrise > temp_xema

#Visualize predictions

pred_results_Cx13 <- testData_Cx
pred_results_Cx13$Culex_SE_pred <- predict(rf_Cx13, newdata = testData_Cx)