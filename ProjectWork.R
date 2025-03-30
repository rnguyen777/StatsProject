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
  college.mark ~ Department + Gender + BMI + Stress.Level + Financial.Status + Travelling.Time + salary.expectation
  , data =student_behavior
)
summary(fullmodel)
plot(fullmodel$residuals,fullmodel$fitted.values)
qqnorm(fullmodel$residuals)
qqline(fullmodel$residuals)
dwtest(fullmodel)
fullmodel.log=lm(
  sqrt(college.mark) ~ Department + Gender + BMI + Stress.Level + Financial.Status + Travelling.Time + salary.expectation
  , data =student_behavior
)

plot(fullmodel.log$residuals,fullmodel$fitted.values)
qqnorm(fullmodel.log$residuals)
qqline(fullmodel.log$residuals)
library(lmtest)
dwtest(fullmodel.log)

library(MASS)
reduced=stepAIC(fullmodel)
summary(reduced)

plot(reduced$residuals,fullmodel$fitted.values)
qqnorm(reduced$residuals)
qqline(reduced$residuals)
dwtest(reduced)


