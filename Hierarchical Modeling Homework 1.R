

math<-read.csv("math_scores.csv")
head(math)

#A. What level of LSD tissue concentration do you need to ensure a test score of >85%

x=math$LSD_concentration
hist(x)

modelq1=lm(math$MATH_score~math$LSD_concentration)
confint(modelq1)
plot(math$MATH_score~math$LSD_concentration);curve(89.123874+-9.009466*x,add = T)

a<-89.123874
b<--9.009466
y=math$MATH_score
y_hat=a+b*math$LSD_concentration

RSS=sum((math$MATH_score-y_hat)^2)
SS=sum((math$MATH_score-mean(math$MATH_score))^2)
R2=(SS-RSS)/SS

r2<-function(y_hat,y){
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}

r2(y_hat,y)
#Result 0.877835

rmse=function(y_hat,y) {return (sqrt(mean((y-y_hat)^2)))}
rmse(y_hat,y)
#Result 6.022355
(85-89.12)/-9.009
#result 0.4573205 LSD concentration

#B. How well does LSD tissue concentration predict test performance
#####0.877835 is our r^2
#C. Why might the normal distribution be inappropriate to model these data?
#Well these grades are limited by a 0 and a 100 they can not go to negative or positive infinity




#Question 2 Miracle food

miracle<-read.csv("miracle_food.csv")
x=miracle$pomegranate
hist(x)

modelq2=lm(miracle$Weight_loss~miracle$pomegranate)
coef(modelq2)
confint(modelq2)

plot(miracle$Weight_loss~miracle$pomegranate); curve(-0.179802+-0.5251053*x,add = T)

a<--0.1789802 
b<--0.5251053
y=miracle$Weight_loss
y_hat=a+b*miracle$pomegranate

RSS=sum((miracle$Weight_loss-y_hat)^2)
SS=sum((miracle$Weight_loss-mean(miracle$Weight_loss))^2)
r2=(SS-RSS)/SS

r2<-function(y_hat,y){
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}

rmse=function(y_hat,y){
  sqrt(mean((y-y_hat)^2,na.rm = T))}

plot(miracle$Weight_loss~miracle$pomegranate)
curve(-0.179+(-0.525*x),add=T,col="red")

#r^2=0.008
#result:9.96

###With the least squares output in mind, do you agree or disagree with the miracle claim?

#Question 3 MAE 
#A. translate the mathematical equation for MAE into a function in R

a<-89.123874
b<--9.009466
y=math$MATH_score
y_hat=a+b*math$LSD_concentration
n_math=nrow(math)
mae=function(y_hat,y){
  ABS<-(sum(abs((y)-(y_hat))))
  n<-n_math
  return(ABS/n)
}
#answer=7.98
a<-0.1789802
b<--0.5251053
y=miracle$Weight_loss
y_hat=a+b*miracle$pomegranate
n_miracle=nrow(miracle)
mae=function(y_hat,y){
  ABS<-(sum(abs((y)-(y_hat))))
  n<-n_miracle
  return(ABS/n)
}


##B. Compare RMSE, R2, and MAE for the linear models in questions 1 and 2. How do these metrics of model fit differ?
#Math scores
r^2:0.88
RMSE:6.02
MAE:4.89
#These are very different results in terms of model fit. R^2 leans toward a tighter correlation than MAE or RSME
#PIMPOMEGRANATE data
r^2:0.008
RMSE:9.96
MAE:7.98
#r^2 indicates a weak association between pomegranate consumption and hella weight loss





#Question 4
#Step 1: Create a predictor variable using runif or seq
#Step 2: Decide on a value for the intercept and slope
#Step 3: use rnorm to simulate draws from a normal distribution for your dataset
#A. Plot your data
#B. Estimate the slpe and intercept parameters from the data using linear regression.
#C. How do your estimates compare to the true values you came up with in step 2?

plants_watered<-runif(100)

slope<--0.5
intercept<-3
sigma<-0.3
fruits<-rnorm(n=100,mean = intercept+slope*plants_watered,sd=0.3)
fruits
model3<-lm(fruits~plants_watered)
coef(model3)
confint(model3)
plot(fruits~plants_watered)
curve(1.234+(-0.26*x),add=T,col="green")
#This is how you would assess amount of days plants are watered with the fruits produced.


#Question 5

lecture_days<-runif(100)
slope<-0.9
intercept<-12
sigma<-.5*lecture_days
test_scores<-rnorm(n=100,mean=intercept+slope*lecture_days,sd=sigma)
plot(test_scores~lecture_days)

#This data indicates that the more days spent in lecture the higher the test scores.
#A. Construct a plot showing your simulated data
#B. what is a potential biological explanation for the data you have simulated?
###More time spent in lecture the more a student is exposed to the material that will be on the test so the higher the score will be.

