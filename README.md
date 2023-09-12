
# Baseball-Stats
 Exploratory data analysis and built multiple regression models to investigate the relationship between various offensive statistics and the number of runs scored in baseball.
 
 # Written in R code
 library(MASS)   # For fitting the Poisson regression model
 library(car)    # For the stepAIC function
 library(caret)
library(corrplot)
library(reshape2)
library(DataExplorer)
#inserting data and naming it data 
data = `baseballdatafinal`
#practice 3 variable manual model poison regression 
#model <- glm(r_run ~ player_age + b_ab + b_total_hits, family = poisson(link = "log"), data = data)
#making a corr matrix 

# Select independent variables
vars <- data[, c("player_age" , "b_ab" , "b_total_hits" , "b_total_pa", "b_single", "b_double",  "b_triple", "b_home_run", "b_strikeout" , "b_walk" , "b_k_percent" , "b_bb_percent" , "batting_avg" , "slg_percent" , "on_base_percent" , "on_base_plus_slg" , "woba" , "r_run" )]

# Calculate correlation matrix
cor_matrix <- cor(vars)

# Plot correlation matrix
corrplot(cor_matrix, type = "upper", method = "color", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, 
         addCoef.col = "black", addCoefasPercent = TRUE)
create_report(baseballdatafinal)
#Below models are made with all variables from data sets 
#Every Variable Model 
base_model <- glm(r_run ~ player_age + b_ab + b_total_hits + b_total_pa + b_single + b_double + b_triple + b_home_run + b_strikeout + b_walk + b_k_percent + b_bb_percent + batting_avg + slg_percent + on_base_percent + on_base_plus_slg + woba, family = poisson(link = "log"), data = data)
summary(base_model)

# AIC Building Form
step_model1 <- stepAIC(base_model, direction = "both")
summary(step_model1)
predict(step_model1,type="response")

# BIC model building form. 
step_model2 <- stepAIC(base_model, direction = "both",k=log(1112))
summary(step_model2)
predict(step_model2,type="response")

# split data and create testing and training set
#below models are made after a corr table was made and ratio based stats, and the b_ab is removed 
base_model2 <- glm(r_run ~ player_age + b_total_hits + b_total_pa + b_single + b_double + b_triple + b_home_run + b_strikeout + b_walk, family = poisson(link = "log"), data = data)
summary(base_model2)
