#Project 3 on advanced stat - Product Service Management 


######Variable#######
## Prodquality 
##E-commerce
##Tech Support
##complaint res
##Advertising,prod line,salesforce image,competitive pricing
##Warrany claim,order & billing , delivery speed, customer satisfaction




##  Q1 Is there evidence of multicolinearity 
##  Q2 Perform FA by extracting 4 factors
##  Q3 Name the factors
##  Q4 Perform Multiple Linear Regression with customer satisfaction as dependent 
##  var and 4 factors as Independent var . Comments on model validity .

setwd("C:/Users/Mudgal/Documents/BACP/Module3 - Advance Stat/Mini project")
mydata=read.csv("Factor-Hair-Revised.csv", header=TRUE)
mydata
head(mydata)
attach(mydata)
mydata=mydata[,c(2:12)]  ###extracting 2 to 12 columns only

mydata
##correlation
mycorr = round (cor(mydata),2)
mycorr
plot(mydata)

#model =lm(Satisfaction~.,mydata)
#model
#aov(model)
#summary(model)
#library(car) ## required for VIF 
#vif(model) ## AS the vif value is less then 10 for all we can conclude there 
# no multicolinearity 

## E-Com ,Advertising and  sales force images are  correlated - sales related 
## Tech support and warratny claim are correlated - customer support 
## Complain res ,order billing and del speed are correlated - Ordering related
## Prod quality and prod line are correlated - Prod related 



library(nFactors)
plot(mycorr)
ev=eigen(cor(mydata)) ## eigen value is the basis for selecting # of factors
ev
EigenValue=ev$values
EigenValue  ### eigen values are varying in magnitude and are 
## in decreasing order then we have multicolinearity 

Factor=c(1,2,3,4,5,6,7,8,9,10,11)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue")
lines(Scree,col="Red")
#as per kaizer rule eigen value > 1 is only significant hence
##4 factors are selected .  

library(psych)
## get principal component factors and we have 4 factors 
Unrotate=principal(mydata, nfactors=4, rotate="none")
print(Unrotate,digits=4)


## h2 explaing communality that is percentage  variance explained by 
##   4 factods for that variable 

## orthogonal rotation
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata,nfactors=4,rotate="varimax")
print(Rotate,digits=4)
Rotate

RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)

## linear regression
## now will perfoem multiple linear regression with customer satisfaction as 
## dependent variable and 4 factors as independent variable .


Rotate$scores #will give scores for 4 factors, here we can check for these 4 factors 
#rather then going through all 11 fields.


OrderMangement=Rotate$scores[,1] #RC1
SalesMangement=Rotate$scores[,2] #RC2
CustomerMangement=Rotate$scores[,3] #RC3
ProductMangement=Rotate$scores[,4] #RC4

##getting customer satisfaciton variable 
mydata_Satisfation =read.csv("Factor-Hair-Revised.csv", header=TRUE)
mydata_Satisfation=mydata_Satisfation[,13]

##make data frame 
RegressionModel = data.frame(OrderMangement,SalesMangement,CustomerMangement,
                    ProductMangement,mydata_Satisfation)

 str(RegressionModel)
 pairs(RegressionModel)
 cor(RegressionModel)
#RegressionModel


##simple regression
plot(mydata_Satisfation~OrderMangement)
Model_OM = lm(mydata_Satisfation~OrderMangement,RegressionModel)
summary(Model_OM)
abline(Model_OM,col="red",lwd=2)

plot(mydata_Satisfation~SalesMangement)
Model_SM = lm(mydata_Satisfation~SalesMangement,RegressionModel)
summary(Model_SM)
abline(Model_SM,col="blue",lwd=2)

plot(mydata_Satisfation~CustomerMangement)
Model_CM = lm(mydata_Satisfation~CustomerMangement,RegressionModel)
summary(Model_CM)
abline(Model_CM,col="green",lwd=2)

plot(mydata_Satisfation~ProductMangement)
Model_PM = lm(mydata_Satisfation~ProductMangement,RegressionModel)
summary(Model_PM)
abline(Model_PM,col="orange",lwd=2)




#multiple linear regression 
plot(RegressionModel)

Model_MR = lm(mydata_Satisfation~OrderMangement+SalesMangement+CustomerMangement
              +ProductMangement,RegressionModel)
summary(Model_MR)

##testing regression assumptioms
##1 . Mean of the residuals is Zero

mean(Model_MR$residuals)

##2???	Homoscedasticity of residuals  

par(mfrow=c(2,2)) 
plot(Model_MR)

## 3.???	Errors   ni dependent variables are un-correlated 

cor.test(OrderMangement+SalesMangement+CustomerMangement
         +ProductMangement, Model_MR$residuals)

cor(ProductMangement,Model_MR$residuals)
plot(Model_MR$coefficients)

##dropping customerManagement and checking model


Model_MR1 = lm(mydata_Satisfation~OrderMangement+SalesMangement
               +ProductMangement,RegressionModel)
summary(Model_MR1)



AIC(Model_MR) ## with all 4 inedependent variables
AIC(Model_MR1) ## with 3 indepennt variables , dropping customer management
