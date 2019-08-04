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
##checking othere method for COL - VIF 
#mydata=mydata[,c(2:13)]  ###extracting 2 to 13 columns
names(mydata)

plot(ProdQual,ProdLine)
plot(mydata)

mydata
##correlation
mycorr = round (cor(mydata),2)
mycorr

install.packages("corrplot")
library(corrplot)
require("corrplot")
?corrplot

corrplot(mydata)
corrplot(mydata, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



model =lm(Satisfaction~.,mydata)
model
aov(model)
summary(model)
library(car) ## required for VIF 
vif(model) ## AS the vif value is less then 10 for all we can conclude there 
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
max(EigenValue)/min(EigenValue) ##1] 34.81738
##rule of thumb if the ration more then 100 then the multicolinearity is significant 

kappa(mycorr) ##1] 34.52059 - no multicolinearity as per kappa 



#EigenValue=EigenValue[1:4]
#EigenValue
Factor=c(1,2,3,4,5,6,7,8,9,10,11)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue")
lines(Scree,col="Red")
#as per kaizer rule 4 factors are selected . as eigen value>1 for 4 facots only
#hence will select 4 factors 

library(psych)
## get principal component factors
Unrotate=principal(mydata, nfactors=4, rotate="none")
print(Unrotate,digits=4)


## h2 explaing communality that is variance explained by 
## % of all 4 factods for that variable 

## orthogonal rotation
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata,nfactors=4,rotate="varimax")
print(Rotate,digits=4)

Rotate$scores #will give scores for 4 factors, here we can check for these 4 factors 
#rather then going through all 11 fields.

?Rotate$score


Model_MR1 = lm(mydata_Satisfation~OrderMangement+SalesMangement
              +ProductMangement,RegressionModel)
summary(Model_MR1)
mean(Model_MR$residuals)


aov(Model_MR)
fitted(Model_MR1)


par(mfrow=c(2,2)) 
plot(Model_MR)



AIC(Model_MR)
AIC(Model_MR1)

confint(Model_MR)


aov(Model_MR)


Model_MR2 = lm(mydata_Satisfation~SalesMangement+ProductMangement
               ,RegressionModel)
summary(Model_MR2)



