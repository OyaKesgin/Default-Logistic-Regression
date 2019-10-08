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
xvals=seq(0,2660,length=100)
yvals=predict(default.lreg,data.frame(balance=xvals),type="response")
plot(xvals,yvals,type="l")
abline(0.5,0,col="red")

default.lreg=glm(default ~ income+balance+student,family=binomial)
summary(default.lreg)
default.lreg=glm(default ~ balance+student,family=binomial)
summary(default.lreg)
predict(default.lreg,data.frame(balance=1000,student="Yes"),type="response")

testindex=sample(1:10000,3000)
default.train=Default[-testindex,]
default.test=Default[testindex,]
default.lreg=glm(default ~ balance+student,family=binomial,data=default.train)
testprob=predict(default.lreg,default.test,type="response")
testpred=rep(0,3000)
testpred[testprob>0.5]=1
testval=rep(0,3000)
testval[default.test$default=="Yes"]=1
mean(testval==testpred)
mean(testval!=testpred)

testpred.subset=testpred[default.test$default=="Yes"]
mean(testpred.subset)

