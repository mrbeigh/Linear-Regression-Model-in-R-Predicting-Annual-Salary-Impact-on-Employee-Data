
#SAMPLE DATA
Employee.Sample.Data <- read.csv("C:/Users/user/Downloads/Employee Sample Data.csv")
View(Employee.Sample.Data)

#DATA We NEED
mydata <- Employee.Sample.Data[,c(3:8,10:13)]
View(mydata)


#remove $ and , sign from Salary and % from Bonus USING lapply
mydata$Annual.Salary[]<-lapply(mydata$Annual.Salary,gsub,pattern="$",fixed=TRUE,replacement="")
mydata$Annual.Salary[]<-lapply(mydata$Annual.Salary,gsub,pattern=",",fixed=TRUE,replacement="")
mydata$Bonus..[]<-lapply(mydata$Bonus..,gsub,pattern="%",fixed=TRUE,replacement="")


str(mydata)


#change salary and Bonus into integer data type
mydata$Annual.Salary <- as.integer(mydata$Annual.Salary)
mydata$Bonus.. <- as.integer(mydata$Bonus..)

View(mydata)
str(mydata)


#############################################################################
#  HYPOTHESIS TESTS TESTS TESTS TESTS TESTS TESTS TESTS TESTS TESTS TESTS




#################one sample t test

t.test(mydata$Annual.Salary, mu = 500000, alternative = "less")
t.test(mydata$Age,mu = 20, alternative = "greater")
t.test(mydata$Bonus.., mu = 15, alternative = "two.sided")

#############pearson correlation test


##########
###no outliers are present.....

r<-ggplot(mydata, aes(x= Bonus.., y=Annual.Salary, color=Bonus..))+
  geom_boxplot()
ggplotly(r)
res4 <- cor.test(mydata$Annual.Salary, mydata$Bonus.., method='pearson')
res4


r<-ggplot(mydata, aes(x= Age, y=Annual.Salary, color=Age))+
  geom_boxplot()
ggplotly(r)
res2 <- cor.test(mydata$Age, mydata$Annual.Salary, method='pearson')
res2


r<-ggplot(mydata, aes(x= Bonus.., y=Annual.Salary, color=Bonus..))+
  geom_boxplot()
ggplotly(r)
res3 <- cor.test(mydata$Age, mydata$Bonus.., method='pearson')
res3


################################Independent Sample T test

#for example::Men and women earn same income in this dataset? 


#check outliers
install.packages("plotly")
library(ggplot2)
library(plotly)
r<-ggplot(mydata, aes(x= Gender, y=Annual.Salary, color=Gender))+
  geom_boxplot()
ggplotly(r)


#check normality assumption
library(dplyr)
mydata1<-filter(mydata, Gender=="Male")
mydata2<-filter(mydata, Gender=="Female")

library(nortest)
hist(mydata1$Annual.Salary)
shapiro.test(mydata1$Annual.Salary)
ad.test(mydata1$Annual.Salary)
lillie.test(mydata1$Annual.Salary)

hist(mydata2$Annual.Salary)
shapiro.test(mydata2$Annual.Salary)
ad.test(mydata2$Annual.Salary)
lillie.test(mydata2$Annual.Salary)


##############Levene's test for equality of variance
install.packages('car')
library(car)


##Annualsalary vs gender

#Levene's test 
res<-leveneTest(Annual.Salary~Gender, data = mydata)
res
#Independent sample T test
result<- t.test(Annual.Salary~Gender, data=mydata, var.equal=T, alternative= "greater")
result



####Age vs Gender
#Levene's test
res<-leveneTest(Age~Gender, data = mydata)
res
#Independent sample T test
result<- t.test(Age~Gender, data=mydata, var.equal=T, alternative= "less")
result


####Bonus vs Gender
#Levene's test
res<-leveneTest(Bonus..~Gender, data = mydata)
res
#Independent sample T test
result<- t.test(Bonus..~Gender, data=mydata, var.equal=T, alternative= "two.sided")
result


########################### One Way ANOVA test

#one-way ANOVA was run to determine if there were differences in Annual Salary
#in different cities. Outlier was checked by boxplot method. No outlier 
#was found here. In addition,  Annual Salary in different cities were normally 
#distributed. There was homogeneity of variances, as assessed by Levene's test for equality of 
#variances (p = 0.311). This study found  F(12,987)=0.315, p=0.987.Tukey's post hoc was performed at
# 95% family-wise confidence level and highest p adj of 0.99 was found.


####Annualsalary vs City
###Levene's test
res<-leveneTest(Annual.Salary~City, data = mydata)
res
#one way ANOVA
RES<- aov(Annual.Salary~City, data = mydata)
summary(RES)
#TUKEYs post hoc
TukeyHSD(RES)




####Bonus vs Ethnicity
###Levene's test
res<-leveneTest(Bonus..~Ethnicity, data = mydata)
res
#one way ANOVA
RES<- aov(Bonus..~Ethnicity, data = mydata)
summary(RES)
#TUKEYs post hoc
TukeyHSD(RES)



#####Age vs Job Title
###Levene's test
res<-leveneTest(Age~Job.Title, data = mydata)
res
#one way ANOVA
RES<- aov(Age~Job.Title, data = mydata)
summary(RES)
#TUKEYs post hoc
TukeyHSD(RES)





###############################################################################
#REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION REGRESSION




####simple linear regression
r1<-lm(Annual.Salary~Job.Title,data=mydata)
summary(r1)





#MULTIPLE LINEAR REGRESSion#######################

###STEP 1###EXPLORE DATA SET
install.packages("GGally")
library(GGally)
library(ggplot2)
ggpairs(mydata, cardinality_threshold = 1000)


############run the model
model<-lm(Annual.Salary~.,data = mydata)
summary(model)


############remove variables
install.packages("datasets")
library(olsrr)

#all possible sets
new<-ols_step_all_possible(model)
new

best<- ols_step_best_subset(model)
best
plot(best)


#stepwise both aic
model1<-ols_step_both_aic(model, details = T)
model1

#stepwsise both p values
model2<-ols_step_both_p(model, prem = 0.05, penter=0.01, details = T)
summary(model2)


final_model<-lm(Annual.Salary~Job.Title+Gender+Ethnicity,data = mydata)
summary(final_model)



###############multicollinearity CHECK
install.packages("rms")
library(rms)
vif(final_model)

###########linearity, residuality and more
install.packages("performance")
library(performance)


final_model$residuals
final_model$fitted.values
check_model(final_model)
plot(final_model)


#normality
residual <- final_model$residuals
shapiro.test(residual)

#homogenity of variance
install.packages("lmtest")
library(car)
library(lmtest)

ncvTest(final_model)
#or
bptest(final_model)


#autocorrelation
library(car)
library(stats)

durbinWatsonTest(final_model)
#or
acf(final_model$residuals, type = "correlation")

#influential point check
check_model(final_model)
plot(final_model)


##########FINAL MODEL
summary(final_model)

###EQUATION IS: Annual Salary = 83031.36 + 1931.4 * Gender - 3186.8 * Ethnicity -17607.5 * Job Title

