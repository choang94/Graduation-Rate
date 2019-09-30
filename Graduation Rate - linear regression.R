#Install packages
install.packages("ISLR")
library(ISLR)

?College
str(College)

#Build a linear regression model of Grad.rates and student.faculty ratio
linear.model = lm(Grad.Rate ~ S.F.Ratio, data = College)
print(linear.model)

#a mathematical formula for grad.rate as a function for s.f ratio

summary(linear.model)
#the p value is way below 0.05. Therefore, the null hypothesis is rejected. 
#There is a relationship between Grad.Rate and S.F.Ratio
plot(College$S.F.Ratio,
     College$Grad.Rate,
     ylab = "Graduation Rate",
     xlab = "S.F Ratio",
     ylim = c(20,100),
     xlim = c(0,30))
abline(linear.model,col="red")

#S.F Ratio increases, graduation rate decreases

#Test to see if this relationship is linear using quadratic term
# College$S.F.Ratio^2
quadratic.model = lm(Grad.Rate ~ S.F.Ratio + I(S.F.Ratio^2), data = College)
#quadratic.model = lm(College$S.F.Ratio ~ College$Grad.Rate + College$S.F Ratio^2)
summary(quadratic.model)
#this is the linear model because the Ratio2(the quadratic model) has 
#no significant values >0.1

#Test the relationship between SF Ratio and number of apps (Apps)
lm2 = lm(S.F.Ratio ~ Apps, data = College)
summary(lm2)

plot(College$S.F.Ratio,College$Apps,xlim = c(0,30),ylim = c(0,20000))

qm2 = lm(S.F.Ratio ~ Apps + I(Apps^2), data = College)
summary(qm2)

#There is a slight relationship between SF Ratio and Apps. p value is 0.001 **. 

#2a,Build multiple linear regression model
lm3 = lm(Grad.Rate ~ Private + Top25perc + Outstate + Room.Board, data = College)

summary(lm3)

#We expect to see a slightly higher graduation rate for students who go to 
#private school

#b,Assessing model's fit by looking at Multiple R-squared: 0.3888

#c,95% confidence intervals 
confint(lm3)
#If Outstate is 50k, Our graduation rate will increase from 
#range 48.44% to 87.67%
50000*0.0009688043
50000*0.001753387
#d,predict the graduation rate
Outstate = 25000
Room.Board = 4000
Top25perc = 55
Private = "No"

new_obs = data.frame(Private,Top25perc,Outstate,Room.Board)
new_obs
predict(lm3,new_obs)
predict(lm3,new_obs,interval = "prediction")
#Our prediction interval shows that the graduation rate will range from 54.32% 
#to 109.09%.The prediction range gives us a sense of confidence in our prediction

#Use plot() for the diagnostic tools
par(mfrow = c(2,2))
plot(lm3)
#the residuals vs fitted plot shows us there's no violation, which means there's 
#nothing wrong with our model

#3.a,Build a regression model using the grad rate against all of the predictors
lm4 = lm(Grad.Rate~., data = College)
summary(lm4)
#The adjusted R-squared for the full model is 0.4495
#b,use forward selection to choose the best reduced model
install.packages("leaps")
library(leaps)
#apply forward selection to model with all variables
lm4_fwd = regsubsets(Grad.Rate~.,data = College,nvmax = NULL, method = "forward")
summary(lm4_fwd)

lm4_fwd_summary = summary(lm4_fwd) #store summary output
which.max(lm4_fwd_summary$adjr2)  #display best subset by adjr2
#the model with 13 variables is the one that has the best adjr2
summary(lm4_fwd)$which[13,]
#Accept, Enroll, Books and SF Ratio were left out. 
#c,Build a new model based on the subset
newlm4_fwd = lm(Grad.Rate ~ Private+Apps+Top10perc+Top25perc+F.Undergrad+
                  P.Undergrad+Outstate+Room.Board+Personal+PhD+Terminal+
                  perc.alumni+Expend,
                data = College)
summary(newlm4_fwd)

#R-squared value of the reduced model is 0.451 compared to the full 0.4495
#There is not much difference
#use anova to compare the mean sum of squares
anova(lm4,newlm4_fwd)
#pvalue is insignificant. so there is no statistical difference

#d, plot the ranking of subsets
par("mar")
par(mar = c(1,1,1,1))
plot(lm4_fwd, scale = "adjr2" ,
     main = "Forward Selection: AdjR2",
     ylim = c(0.45,0.45)) 

#the subset with fewest predictors - 8 with the adjr2 of at least 0.45
# the 8 predictors are Apps, Top25perc, P.Undergrad, Outstate, Room.Board,
# Personal, perc.alumni, Expend
lm5 = lm(Grad.Rate ~ Apps + Top25perc + P.Undergrad + Outstate + Room.Board+
           Personal + perc.alumni + Expend, data = College)
summary(lm5)
