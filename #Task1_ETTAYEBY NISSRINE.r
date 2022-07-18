#Task1_ETTAYEBY NISSRINE
#Reading the csv data
data <-read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
# viewing the first 6 entries of the data
head(data)
 Hours Scores
1   2.5     21
2   5.1     47
3   3.2     27
4   8.5     75
5   3.5     30
6   1.5     20

#summary of data
 summary(data)
 Hours           Scores     
 Min.   :1.100   Min.   :17.00  
 1st Qu.:2.700   1st Qu.:30.00  
 Median :4.800   Median :47.00  
 Mean   :5.012   Mean   :51.48  
 3rd Qu.:7.400   3rd Qu.:75.00  
 Max.   :9.200   Max.   :95.00  
 #ploting both variables i.e. Scores and Hours
plot(x = data$Hours, y = data$Scores, xlab="Hours", ylab="Scores", main = "Scores Vs Hours")
#Linear Regression 
data.regression <- lm(Scores ~ Hours, data=data)
abline(data.regression, col="red")
#summary of data.regression
summary(data.regression)
Call:
lm(formula = Scores ~ Hours, data = data)
Residuals:
    Min      1Q  Median      3Q     Max 
-10.578  -5.340   1.839   4.593   7.265 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.4837     2.5317   0.981    0.337    
Hours         9.7758     0.4529  21.583   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 5.603 on 23 degrees of freedom
Multiple R-squared:  0.9529,	Adjusted R-squared:  0.9509 
F-statistic: 465.8 on 1 and 23 DF,  p-value: < 2.2e-16
#import library
library(e1071)
StudentD <- data
head(StudentD)
Hours Scores
1   2.5     21
2   5.1     47
3   3.2     27
4   8.5     75
5   3.5     30
6   1.5     20

scatter.smooth(StudentD$Hours, StudentD$Scores , main="Time ~ Scores", xlab = "Time(h)", ylab="Scores(%)")
par(mfrow=c(1,2))
boxplot(StudentD$Hours, main="Time(h)")
boxplot(StudentD$Scores, main="Scores(%)")
par(mfrow=c(1,2))
plot(density(StudentD$Hours) , main="Density of Time(h)" , ylab="frequency" , sub=paste0("Skewness : ", round(skewness(StudentD$Hours),2)))
polygon(density(StudentD$Hours), col="blue")
plot(density(StudentD$Scores), main="Density of Scores(%)", ylab="frequency", 
     sub=paste("Skewness:", round(skewness(StudentD$Scores),2)))
polygon(density(StudentD$Scores), col="pink")

#Correlation
cor(StudentD$Hours, StudentD$Scores) 
[1] 0.9761907

#Splitting the Data
#Create Training and Test data
set.seed(100)    #Setting seed to reproduce results of random sampling
RowIndex <- sample(1:nrow(StudentD), 0.8*nrow(StudentD))
#trainig data
TrainingD <- StudentD[RowIndex, ]    
#test data
testD <- StudentD[-RowIndex, ] 

#Model Building and Evaluation
mod <- lm(Hours ~ Scores, data=TrainingD)
summary(mod)
Call:
lm(formula = Hours ~ Scores, data = TrainingD)
Residuals:
    Min      1Q  Median      3Q     Max 
-0.8112 -0.4367 -0.2106  0.4912  1.0939 
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.082165   0.309151  -0.266    0.793    
Scores       0.099843   0.005659  17.643 8.29e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5656 on 18 degrees of freedom
Multiple R-squared:  0.9453,	Adjusted R-squared:  0.9423 
F-statistic: 311.3 on 1 and 18 DF,  p-value: 8.29e-13

#Predict the test data
yPred <- predict(mod , testD) #Predict scores
#Measuring goodness of fit
AIC(mod)   #Calculate Akaike information criterion
[1] 37.85384
BIC(mod)    # Calculate Bayesian information criterion
[1] 40.84104


#Accuracy and Error Rates
pred <- data.frame(cbind(actuals=testD$Scores, predicteds=yPred))
head(pred)

   actuals predicteds
1       21   2.014535
9       81   8.005104
11      85   8.404475
15      17   1.615163
25      86   8.504318

cor(pred)    # correlation between actuals and predictors

           actuals predicteds
actuals             1          1
predicteds       1          1
min_max_accuracy <- mean(apply(pred, 1, min) / apply(pred, 1, max))
min_max_accuracy
[1] 0.09750637
# Mean Absolute Percentage Error Calculation
mean_abs_per_er <- mean(abs((pred$predicteds - pred$actuals))/pred$actuals) 
mean_abs_per_er
[1] 0.9024936

#what is the predicted score if a student studies for 9.25h/day?
predict(mod, data.frame(Scores=c(9.25)))
        1 
0.8413815














