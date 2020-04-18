#Naming our dataset to be found under the function "IQ".
IQ<-read.csv("IQ.csv")

#Test if it works and if it's comma separated.
colnames(IQ)

#Importing dataset into ggplot2.
ggplot(data = IQ)

#Viewing the data plotted.
plot(IQ)

#Viewing a summary of the data using the "summary" function.
summary(IQ)

#Viewing the dataset using ggplot.
View(IQ)

#Plotting the data points
ggplot(IQ, aes(x="age", y="duration")) + geom_point() + geom_jitter() + geom_smooth(method = 'lm')

#Visualizing a scatter plot of the "age" in comparison to the prior IQ before entering into a coma "piq" 
ggplot(IQ, aes(age, piq)) + geom_point() + scale_x_continuous("age", breaks = seq(0,80,by = 5))+ scale_y_continuous("piq", breaks = seq(0,150,by = 10))+ theme_bw()

#Visualizing the data using a histogram.
ggplot(IQ, aes(age)) + geom_histogram(binwidth = 2)+scale_x_continuous("age", breaks = seq(0,100,by = 5))+scale_y_continuous("piq", breaks = seq(0,150,by = 10))+labs(title = "Histogram")

#Testing for normality using the Shapiro-Wilk test "shapiro.test(x)" function. The Shapiro–Wilk test tests the null hypothesis that a sample x1, ..., xn came from a normally distributed population.
#Null hypothesis: The dats is normally distributed.
#Alternative hypothesis: The data is not normally distributed.

#Run the "summary(IQ)" command to determine the min and max age for the T-test.
summary(IQ)

#Define the age range of the T-test.
IQA<-IQ$age[6:80]

#Run the Shapiro-Wilk test to generate the p-value and W value to determine whether you accept or reject your null hypothesis for the ages.
shapiro.test(IQA)

#Run the Shapiro-Wilk test on the viq and piq
IQP<-IQ$piq[50:133]
IQV<-IQ$viq[64:132]
shapiro.test(IQP)
shapiro.test(IQV)

#Running a T-test on IQV and IQP.
t.test(IQP, IQV, paired=FALSE)

#Duration of the age in comparison to being in a coma.
age.duration<-function(age){
  + duration<-ggplot(IQ, aes(x="age", y="duration")) + geom_point() + geom_jitter() + geom_smooth(method = 'lm')
  + return(duration)}
age.duration()

#Defining the age range
age.ttest<-function(age.ttest){
  + IQage<-IQ$age[6:80]
  + return(IQage)}
> age.ttest()

#Running a T-test on piq and viq.
IQ.ttest<-function(IQttest){
  + IQT<-t.test(IQP, IQV, paired=FALSE)
  + return(IQT)}
> IQ.ttest()

#Generating a scatter plot for piq.
age.scatter<-function(scatterage){
agescatter<-ggplot(IQ, aes(age, piq)) + geom_point() + scale_x_continuous("age", breaks = seq(0,80,by = 5))+ scale_y_continuous("piq", breaks = seq(0,150,by = 10))+ theme_bw()
return(agescatter)}
age.scatter()

#Generating a scatter plot for viq.
age.scatters<-function(scattersage){
  agescatters<-ggplot(IQ, aes(age, viq)) + geom_point() + scale_x_continuous("age", breaks = seq(0,80,by = 5))+ scale_y_continuous("piq", breaks = seq(0,150,by = 10))+ theme_bw()
  return(agescatters)}
age.scatters()


#Generating a histogram plot for piq
piq.histogram<-function(histogrampiq)
histogram.piq<-ggplot(IQ, aes(age)) + geom_histogram(binwidth = 2)+scale_x_continuous("age", breaks = seq(0,100,by = 5))+scale_y_continuous("piq", breaks = seq(0,150,by = 10))+labs(title = "Histogram")
return(histogram.piq)

#Splitting the data into training and test data
set.seed(2)
library(caTools)
split<-sample.split(IQV, SplitRatio=0.7)
train<-subset(IQP, splot="TRUE")
