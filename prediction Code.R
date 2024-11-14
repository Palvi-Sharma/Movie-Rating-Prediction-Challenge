movies_data = read.csv("/Users/berlybrigith/Downloads/SEM 2 FALLL/MGSC 661/MIDTERM ASSIGNMENT/IMDB_data_Fall_2024.csv")
attach(movies_data)
require('psych')
require('car')
require('lmtest')
require('plm')




#TASK 1,2 & 3
movies=read.csv("/Users/berlybrigith/Downloads/SEM 2 FALLL/MGSC 661/MIDTERM ASSIGNMENT/IMDB_data_Fall_2024.csv")
attach(movies)
#install.packages("psych")
require(psych)

plot(duration, imdb_score)
plot(movie_budget, imdb_score)

plot(imdb_score~duration)


plot(imdb_score~movie_budget) 


plot(imdb_score~nb_news_articles)


plot(imdb_score~actor1_star_meter)


plot(imdb_score~actor2_star_meter)


plot(imdb_score~actor3_star_meter) 


plot(imdb_score~aspect_ratio) 


plot(imdb_score~nb_faces) 


plot(imdb_score~movie_meter_IMDBpro) 
require('car')
#Checking hetrosk
names(movies)
reg0=lm(imdb_score~duration)  
residualPlot(reg0, quadratic=FALSE) 
#yes present 

reg=lm(imdb_score~movie_budget)  
residualPlot(reg, quadratic=FALSE)


reg2=lm(imdb_score~nb_news_articles)  
residualPlot(reg2, quadratic=FALSE)

reg3=lm(imdb_score~actor1_star_meter)  
residualPlot(reg3, quadratic=FALSE) 

reg4=lm(imdb_score~aspect_ratio)  
residualPlot(reg4, quadratic=FALSE) 

reg5=lm(imdb_score~nb_faces)  
residualPlot(reg5, quadratic=FALSE) 

reg6=lm(imdb_score~movie_meter_IMDBpro) 
residualPlot(reg6, quadratic=FALSE)

reg7=lm(imdb_score~actor3_star_meter)  
residualPlot(reg7, quadratic=FALSE)

reg8=lm(imdb_score~actor2_star_meter)  
residualPlot(reg8, quadratic=FALSE)

#frm below we check collinearity amongst all quantitative variables and target, imdb_score variable

quantvars=movies[, c(4, 5, 9, 13, 15, 18, 20, 22, 25, 40)]
corr_matrix=cor(quantvars)
round(corr_matrix,3)

#TASK 4
#Checking collinearity with VIF function and residual plots
reg1=lm(imdb_score~movie_budget+duration +nb_news_articles+actor1_star_meter 
        +actor2_star_meter + actor3_star_meter +nb_faces +movie_meter_IMDBpro)
vif(reg1)
summary(reg1) 
residualPlots(reg1)



                  
#Running Simple Linear Regression 
regs=lm(imdb_score~movie_budget)
summary(regs) 

regs1=lm(imdb_score~duration)
summary(regs1) 

regs2=lm(imdb_score~nb_news_articles)
summary(regs2) 

regs3=lm(imdb_score~nb_faces)
summary(regs3) 

regs4=lm(imdb_score~actor1_star_meter)
summary(regs4) 

regs5=lm(imdb_score~actor2_star_meter)
summary(regs5)  

regs6=lm(imdb_score~actor3_star_meter)
summary(regs6) 

regs7=lm(imdb_score~movie_meter_IMDBpro)
summary(regs7)





#TASK 5
# Running regressions to see best degree fit
model_imdb_budget = lm(imdb_score ~ movie_budget)
summary(model_imdb_budget)

model_imdb_duration = lm(imdb_score ~ duration)
model_imdb_duration_poly2 = lm(imdb_score ~ poly(duration, 2))
model_imdb_duration_poly3 = lm(imdb_score ~ poly(duration, 3))
model_imdb_duration_poly4 = lm(imdb_score ~ poly(duration, 4))
anova(model_imdb_duration, model_imdb_duration_poly2, model_imdb_duration_poly3, model_imdb_duration_poly4) #degree 2 or 3 is best

summary(model_imdb_duration)

model_imdb_news = lm(imdb_score ~ nb_news_articles)
model_imdb_news_poly2 = lm(imdb_score ~ poly(nb_news_articles, 2))
model_imdb_news_poly3 = lm(imdb_score ~ poly(nb_news_articles, 3))
model_imdb_news_poly4 = lm(imdb_score ~ poly(nb_news_articles, 4))
anova(model_imdb_news, model_imdb_news_poly2, model_imdb_news_poly3, model_imdb_news_poly4) #degree 2 or 3 is best

summary(model_imdb_news)

model_imdb_meter = lm(imdb_score ~ movie_meter_IMDBpro)
model_imdb_meter_poly2 = lm(imdb_score ~ poly(movie_meter_IMDBpro, 2))
model_imdb_meter_poly3 = lm(imdb_score ~ poly(movie_meter_IMDBpro, 3))
model_imdb_meter_poly4 = lm(imdb_score ~ poly(movie_meter_IMDBpro, 4))
anova(model_imdb_meter, model_imdb_meter_poly2, model_imdb_meter_poly3, model_imdb_meter_poly4) #degree 3 or 4
anova(model_imdb_meter_poly3, model_imdb_meter_poly4) #degree 3

summary(model_imdb_meter)

# Linearity check with all quantitative variables
model_all_quant_vars = lm(imdb_score ~ movie_budget + duration + nb_news_articles + actor1_star_meter + actor2_star_meter + actor3_star_meter + nb_faces + movie_meter_IMDBpro)
vif(model_all_quant_vars)
summary(model_all_quant_vars)
model_all_quant_vars_robust = coeftest(model_all_quant_vars, vcov = vcovHC(model_all_quant_vars, type = 'HC1'))
summary(model_all_quant_vars_robust)

# Testing with combination of linear variables
model_combination_budget = lm(imdb_score ~ movie_budget)
summary(model_combination_budget)
levels(movies_data$country)
model_combination_budget_country = lm(imdb_score ~ movie_budget + movies_data$country)
summary(model_combination_budget_country) #better adj r2 but p-value high

# Adding maturity rating
movies_data$maturity_rating = as.factor(movies_data$maturity_rating)
levels(movies_data$maturity_rating)

model_combination_budget_maturity = lm(imdb_score ~ movie_budget + movies_data$maturity_rating)
summary(model_combination_budget_maturity)

library(multcomp)
anova_model = aov(imdb_score ~ maturity_rating, data = movies_data)
tukey_results = TukeyHSD(anova_model, "maturity_rating")

print(tukey_results)

# Adding maturity_rating for better adj r-square and Tukey test
movies_data$distributor = as.factor(movies_data$distributor)
levels(movies_data$distributor)
movies_data$director = as.factor(movies_data$director)
levels(movies_data$director)

#remove long duration movies
oreg=lm(imdb_score~duration)
qqPlot(oreg, envelope=list(style="none"))
#obs 191 and 395 
outlierTest(oreg)
#obs 191 and 395 and 1806

# Finalized non-linear model
library(splines)
movies_cleaned = movies_data[-c(191,395,1806), ] # Removed outliers
final_model_poly = lm(imdb_score ~ poly(movie_budget, 2) + poly(duration, 2) + poly(nb_news_articles, 2) + poly(movie_meter_IMDBpro, 2) + maturity_rating + distributor + director, data = movies_cleaned)
summary(final_model_poly)

# Final spline model
final_model_spline = lm(imdb_score ~ bs(movie_budget, degree = 3) + bs(duration, degree = 3) + bs(nb_news_articles, degree = 3) + bs(movie_meter_IMDBpro, degree = 3) + maturity_rating + distributor + director, data = movies_cleaned)
summary(final_model_spline)
#BEST R SQUARE AND ADJUSTED R SQUARE COMBINATION

library(caret)

# Cross-validation
formula_spline = imdb_score ~ bs(movie_budget, degree = 3) + bs(duration, degree = 3) + bs(nb_news_articles, degree = 3) + bs(movie_meter_IMDBpro, degree = 3) + maturity_rating + distributor + director

train_control = trainControl(method = "cv", number = 10)

cv_model_spline = train(formula_spline, data = movies_cleaned, method = "lm", trControl = train_control)
cv_results = cv_model_spline$results
mse_cv = cv_results$RMSE^2
print(mse_cv)
#at time of testing, mse=68594.6 , really bad predictive power.

# Testing without maturity_rating , director and distributor (categorical variables with way too many levels)
formula_spline_no_rating = imdb_score ~ bs(movie_budget, degree = 3) + bs(duration, degree = 3) + bs(nb_news_articles, degree = 3) + bs(movie_meter_IMDBpro, degree = 3)

# Calculate number of knots for each variable
spl_movie_budget = bs(movie_budget, degree = 3)
spl_duration = bs(duration, degree = 3)
spl_nb_news_articles = bs(nb_news_articles, degree = 3)
spl_movie_meter = bs(movie_meter_IMDBpro, degree = 3)

cat("Number of knots for movie_budget:", length(attr(spl_movie_budget, "knots")) + length(attr(spl_movie_budget, "Boundary.knots")), "\n")
cat("Number of knots for duration:", length(attr(spl_duration, "knots")) + length(attr(spl_duration, "Boundary.knots")), "\n")
cat("Number of knots for nb_news_articles:", length(attr(spl_nb_news_articles, "knots")) + length(attr(spl_nb_news_articles, "Boundary.knots")), "\n")
cat("Number of knots for movie_meter_IMDBpro:", length(attr(spl_movie_meter, "knots")) + length(attr(spl_movie_meter, "Boundary.knots")), "\n")

cv_model_spline_no_rating = train(formula_spline_no_rating, data = movies_cleaned, method = "lm", trControl = trainControl(method = "cv", number = 10))
mse_cv_no_rating = cv_model_spline_no_rating$results$RMSE^2
cat("K-fold CV MSE (without maturity_rating):", mse_cv_no_rating, "\n")
# at time of testing, mse=1.461638 , much better predictive power.


# Testing with LOOCV
loocv_model_spline = train(imdb_score ~ bs(movie_budget, degree = 3) + bs(duration, degree = 3) + bs(nb_news_articles, degree = 3) + bs(movie_meter_IMDBpro, degree = 3) + maturity_rating, data = movies_cleaned, method = "lm", trControl = trainControl(method = "LOOCV"))

print(loocv_model_spline)

loocv_mse = loocv_model_spline$results$RMSE^2
cat("LOOCV MSE (with maturity_rating):", loocv_mse, "\n")
#LOOCV MSE (with maturity_rating): 2.552849 

# Predicting scores on test data
test_data = read.csv("/Users/berlybrigith/Downloads/SEM 2 FALLL/MGSC 661/MIDTERM ASSIGNMENT/test_data_IMDB_Fall_2024.csv")
predicted_scores = predict(loocv_model_spline, newdata = test_data)
print(predicted_scores)

#predicted scores -  3.315259    6.124592    5.560282    6.210746    5.546682    6.096750    6.910481 
#-79.528378    6.342142    6.800831 -176.795280   -2.476172 

#model did not perform well on test data
#removing heteroskdasticity by transforming features

# Applying log transformation for better predictions
movies_cleaned$log_movie_budget = log(movies_cleaned$movie_budget + 1)
movies_cleaned$log_duration = log(movies_cleaned$duration + 1)
movies_cleaned$log_nb_news_articles = log(movies_cleaned$nb_news_articles + 1)
movies_cleaned$log_movie_meter = log(movies_cleaned$movie_meter_IMDBpro + 1)

final_model_log_spline = lm(imdb_score ~ bs(log_movie_budget, degree = 3) + bs(log_duration, degree = 3) + bs(log_nb_news_articles, degree = 3) + bs(log_movie_meter, degree = 3), data = movies_cleaned)
summary(final_model_log_spline)
#Multiple R-squared:  0.3814,	Adjusted R-squared:  0.3776 

# LOOCV on log-transformed model
loocv_model_log_spline = train(imdb_score ~ bs(log_movie_budget, degree = 3) + bs(log_duration, degree = 3) + bs(log_nb_news_articles, degree = 3) + bs(log_movie_meter, degree = 3), data = movies_cleaned, method = "lm", trControl = trainControl(method = "LOOCV"))

print(loocv_model_log_spline)

loocv_mse_log = loocv_model_log_spline$results$RMSE^2
cat("LOOCV MSE (log-transformed model):", loocv_mse_log, "\n")
#LOOCV MSE (log-transformed model): 0.7451304 , better results after transforming

# applying same log transformation to test data
test_data$log_movie_budget = log(test_data$movie_budget + 1)
test_data$log_duration = log(test_data$duration + 1)
test_data$log_nb_news_articles = log(test_data$nb_news_articles + 1)
test_data$log_movie_meter = log(test_data$movie_meter_IMDBpro + 1)

predicted_scores_log = predict(loocv_model_log_spline, newdata = test_data)
print(predicted_scores_log)

#final predictions on test data
#6.485643 6.478622 5.425752 6.627793 5.823882 6.197813 6.697027 6.466095 6.720808 6.880406
#6.793216 7.637150