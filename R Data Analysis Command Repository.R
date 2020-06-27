#Data Analysis Command Respository 
#commands and their uses have been sourced from Dr. Woo's STAT 6021 class offered by the DSI at UVA

#read in data from already installed package
#data <- nameOfPackage #no additional syntax needed, just the name of the dataset

#########################################################################################
#reading in data and exploring the data set

#read in .txt file and store in "data" data frame 
data <-read.table("NameOfTable.txt", header=TRUE ,sep="")

#list the components in your data file
names(data)

#make data the default data frame
attach(data)
hydro
#undo default data frame
detach(data)

#########################################################################################
#calling and plotting the data frame

#call a variable from the data frame
data$nameOfVariable

##create a scatterplot of response variable (y) against predictor (x)
plot(predictor,response)
plot(response~predictor)

##label axes and provide a title
plot(predictor,response,xlab="Label for X axis", ylab="Label for Y axis", main="Main Chart Title")

#split the plotting device so plots are displayed in a 1 by 2 matrix
par(mfrow=c(1,2))
plot(predictor, main="Plot Title")
plot(response, main="Plot Title")

#save the plot to a jpg file, must include copy of plot command
jpeg("joint.jpg")
par(mfrow=c(1,2))
plot(predictor, main="Plot Title")
plot(response, main="Plot Title")
dev.off()

#########################################################################################
#performing regressions / data manipulations / statistical analysis

#Fit a regression model
lm(response~predictor)
result<-lm(response~predictor)
#to fit a MLR 
result<-lm(response~predictor1+predictor2+...etc)
summary(result)

#perfom ANOVA 
#SLR
lm(response~predictor)
result<-lm(response~predictor)
anova(result)

#MLR ANOVA partial F test 
result<-lm(y~x1+x2+x6+x10)
summary(result)

reduced<-lm(y~x1)
anova(reduced,result)

anova(result)

#cacluate correlation
cor(predictor,response)

#to produce 95% CIs for all regression coefficients
confint(result,level = 0.95)

#to produce 95% CI for the mean response when x=1.2, and the 95% PI for the response of an observation when x=1.2
newdata<-data.frame(hydro=1.2)
predict.lm(result,newdata,level=0.95, interval="confidence")
predict.lm(result,newdata,level=0.95, interval="prediction")

#extract the residuals from lm
result$residual

##scatterplot of data, with least squares line overlayed
plot(predictor, response, main="Plot Title")
abline(result,col="red")

##residual plot
plot(result$fitted.values,result$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

##ACF plot of residuals
acf(result$residuals, main="ACF of Residuals")

##Normal probability or QQ plot of residuals
qqnorm(result$residuals)
qqline(result$residuals, col="red")

#boxcox function found in MASS package. Need to install MASS package first
library(MASS)
boxcox(result.inv)
boxcox(result.inv, lambda = seq(0.6, 1.6, 0.01)) #you can adjust the range and interval of the boxcox graph

#create scatterplot of all predictors
pairs(data, lower.panel = NULL, main="Scatterplot of Quantitative Variables")

#check for pairwise correlations to investigate multicollinearity 
preds<-cbind(x1,x2,x6,x10) #combines vectors for each predictor by columns
cor(preds) #provides matrix of correlations b/w all possible pairings

#calc VIF for predictors
library(faraway)
vif(result)

#########################################################################################
#generate critical t values, f values, and p values

#to obtain t multiplier for a 95% CI with df=18, 
qt(0.975,18) #2-sided
qt(0.950, 18) #1-sided

#to obtain p-value from a 2-sided test
2*(1-pt(tStat,df))

#to obtain a critical F value  
qf(0.05, 2, 22) #(percentile, df1, df2), always one-sided

#to obtain b p value from F statistic 
1-pf(percentile,df1,df2)

#################################################################
#Module 6 Introduced categorical predictors 

#check whether your variable is being treated as quatitative or categorical
is.numeric(Region)

##have R treat region as categorical
Region<-factor(Region) 
is.factor(Region) #check that the predictor is now categorical

##check coding scheme
contrasts(Region)
#check the names of the classes in the categorical variable 
levels(Region)

##Give names to the classes
levels(Region) <- c("North", "Central", "Napa") 
contrasts(Region)#check coding scheme for sequence of of names

##Set a different reference class
Region<-relevel(Region, ref = "Napa") 
contrasts(Region) #check coding scheme for sequence of of names

##consider each region a subset
a1<-subset(data,Region=="1") 
a2<-subset(data,Region=="2") 
a3<-subset(data,Region=="3") 

##fit 3 separate regressions, one for each region
reg1<-lm(Quality~Flavor,data=a1) #response~predictor
reg2<-lm(Quality~Flavor,data=a2)
reg3<-lm(Quality~Flavor,data=a3)

##create a scatterplot with different colors and symbols for each region
plot(Flavor,Quality, main="Quality Rating against Flavor Rating, by Region")
points(a2$Flavor,a2$Quality, pch=2, col="red") 
points(a3$Flavor,a3$Quality, pch=12, col="blue")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
legend("topleft", c("North","Central","Napa"), lty=c(1,2,3), pch=c(1,2,12), col=c("black","red","blue")) 

##fit regression with interaction between the 2 predictors
result<-lm(Quality~Flavor*Region)
summary(result)

##fit regression with no interaction
reduced<-lm(Quality~Flavor+Region)
anova(reduced,result)

##residual plot of model with no interaction
plot(reduced$fitted.values,reduced$residuals,main="Residual plot")
abline(h=0,col="red")

##ACF plot of residuals
acf(reduced$residuals)

##QQ plot of residuals
qqnorm(reduced$residuals)
qqline(reduced$residuals, col="red")

##additional assumption to check with categorical predictor. Is the variance of the response variable constant between all classes of the categorical predictor?
boxplot(Quality~Region, main="Boxplot of Quality Rating by Region")

##perform levene's test. Null states the variances are equal for all classes. 
library(lawstat)
levene.test(Quality,Region)
summary(reduced)

##perform Tukey's multiple comparisons
library(multcomp)
pairwise<-glht(reduced, linfct = mcp(Region= "Tukey"))
summary(pairwise)

##obtain the variance-covariance matrix of the coefficients
reduced$coef
vcov(reduced)