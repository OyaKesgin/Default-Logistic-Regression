load("/Users/oyakesgin/Downloads/Default.rda")
#use load fuction to read rda files. 
view(Default)
#there are 4 variables and 10000 observations. 
head(Default)
attach(Default)
plot(balance,income)
plot(default,balance)
plot(default,income)

default.no=subset(Default,default=="No")
default.yes=subset(Default,default=="Yes")
# there are 333 default.yes observations and 9667 obs of default.no. 
plot(default.no$balance,default.no$income,col="blue")
points(default.yes$balance,default.yes$income,col="red")



default.lreg=glm(default ~ balance,family=binomial)
predict(default.lreg,data.frame(balance=1000),type="response")
range(balance)
0.000 2654.323
xvals=seq(0,2660,length=100)
yvals=predict(default.lreg,data.frame(balance=xvals),type="response")
plot(xvals,yvals,type="l")
abline(0.5,0,col="red")

default.lreg=glm(default ~ income+balance+student,family=binomial)
summary(default.lreg)

Call:
glm(formula = default ~ income + balance + student, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.4691  -0.1418  -0.0557  -0.0203   3.7383  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.087e+01  4.923e-01 -22.080  < 2e-16 ***
income       3.033e-06  8.203e-06   0.370  0.71152    
balance      5.737e-03  2.319e-04  24.738  < 2e-16 ***
studentYes  -6.468e-01  2.363e-01  -2.738  0.00619 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2920.6  on 9999  degrees of freedom
Residual deviance: 1571.5  on 9996  degrees of freedom
AIC: 1579.5

Number of Fisher Scoring iterations: 8

default.lreg=glm(default ~ balance+student,family=binomial)
summary(default.lreg)
 

Call:
glm(formula = default ~ balance + student, family = binomial)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.4578  -0.1422  -0.0559  -0.0203   3.7435  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.075e+01  3.692e-01 -29.116  < 2e-16 ***
balance      5.738e-03  2.318e-04  24.750  < 2e-16 ***
studentYes  -7.149e-01  1.475e-01  -4.846 1.26e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2920.6  on 9999  degrees of freedom
Residual deviance: 1571.7  on 9997  degrees of freedom
AIC: 1577.7

Number of Fisher Scoring iterations: 8
 

predict(default.lreg,data.frame(balance=1000,student="Yes"),type="response")
 
0.003248626 
 
#splitting training and test data set

testindex=sample(1:10000,3000)
default.train=Default[-testindex,]
default.test=Default[testindex,]
#Logistic Regression
default.lreg=glm(default ~ balance+student,family=binomial,data=default.train)
testprob=predict(default.lreg,default.test,type="response")
testpred=rep(0,3000)
testpred[testprob>0.1]=1
testval=rep(0,3000)
testval[default.test$default=="Yes"]=1
mean(testval==testpred)
0.9356667
mean(testval!=testpred)
0.06433333

testpred.subset=testpred[default.test$default=="Yes"]
mean(testpred.subset)

 0.7722772
# when test prob>0.5
testpred=rep(0,3000)
 testpred[testprob>0.5]=1
testval=rep(0,3000)
testval[default.test$default=="Yes"]=1
mean(testval==testpred)
[1] 0.973
mean(testval!=testpred)
[1] 0.027

testpred.subset=testpred[default.test$default=="Yes"]
mean(testpred.subset)
[1] 0.3168317
 
