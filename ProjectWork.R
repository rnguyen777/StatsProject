library(readr)
rawdata1=read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")


rawdata2=read_csv("Student_Behaviour.csv")
newheight=(rawdata2$`Height(CM)`)*0.01
bmi=(rawdata2$`Weight(KG)`)/(newheight^2)
averageMARKS=(rawdata2$`10th Mark`+rawdata2$`12th Mark`+rawdata2$`college mark`)/3
student_behavior=data.frame(rawdata2) 
student_behavior["BMI"]=bmi #added bmi column to dataset
student_behavior["averageMARKS"]=averageMARKS

fullmodel=lm(
  averageMARKS ~ Department + Gender + BMI + Stress.Level + Financial.Status + Travelling.Time + social.medai...video + hobbies +daily.studing.time
  , data =student_behavior
)

interaction.plot(student_behavior$daily.studing.time, student_behavior$social.medai...video,averageMARKS,xlab = "Studying time",ylab="Average Marks"
                 ,trace.label = "Social Media Usage", main= "Interaction Plot between Study Time and Social Media Usage")

interaction.plot(student_behavior$Stress.Level,student_behavior$daily.studing.time,averageMARKS, xlab = "Stress Level",ylab="Average Marks"
                 ,trace.label = "Studying Time", main= "Interaction Plot between Stress Level and Studying Time")
interaction.plot(student_behavior$Stress.Level,student_behavior$social.medai...video,averageMARKS, xlab = "Stress Level",ylab="Average Marks"
                 ,trace.label = "Social Media Usage", main= "Interaction Plot between Stress Level and Social Media Usage" )
interaction.plot(student_behavior$Stress.Level,student_behavior$Financial.Status,averageMARKS, xlab = "Stress Level",ylab="Average Marks"
                 ,trace.label = "Financial Status", main= "Interaction Plot between Stress Level and Financial Status")


interaction.model= fullmodel=lm(
  averageMARKS ~ Department + Gender + 
    BMI + Stress.Level + 
    Financial.Status + Travelling.Time + 
    social.medai...video + hobbies +daily.studing.time +
    daily.studing.time*Stress.Level+social.medai...video*Stress.Level +
    + daily.studing.time*social.medai...video+Financial.Status*Stress.Level,
  data =student_behavior
)
summary(interaction.model)

significant.model=lm((averageMARKS)^2~Gender+Stress.Level+social.medai...video
                     +Stress.Level*social.medai...video
                     +daily.studing.time+daily.studing.time*social.medai...video,
                     data = student_behavior)

library(MASS)
final.model=stepAIC(significant.model)

plot(final.model$fitted.values, final.model$residuals,xlab = "Fitted Values", ylab="Residuals", main="Diagnostic Plot of AIC Model")
qqnorm(fullmodel$residuals,main = "QQ Plot of AIC Model")
qqline(full.model$residuals)
library(lmtest)
dwtest(fullmodel)
library(car)
vif(final.model,type="predictor")
shapiro.test(student_behavior$averageMARKS)





