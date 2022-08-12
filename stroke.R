library(dplyr)
library(stringr)
library(ggplot2)     
library(plotrix)
library(ggmosaic)
library(tidyverse)
library(data.table)
library(corrplot)
library(fastDummies)
library(caret)
library(randomForest)
library(gridExtra)
library(knitr)
library(smotefamily)
library(DMwR2)
suppressMessages(library(dplyr))
suppressMessages(library(unbalanced))
data<-read.csv("stroke.csv")
data
str(data)
ncol(data)

data <- data %>%
  select(-id) %>%
  mutate(bmi = as.numeric(bmi)) %>%
  mutate(stroke = as.factor(stroke))
str(data)

colSums(is.na(data))

mean_f_bmi = mean(data[data$gender == "Female", ]$bmi, na.rm = TRUE)
mean_m_bmi =mean(data[data$gender == "Male", ]$bmi, na.rm = TRUE)
mean_f_bmi
mean_m_bmi

df_replacedNA <- data  
df_replacedNA[df_replacedNA$gender == "Female", ]$bmi[which(is.na(df_replacedNA[df_replacedNA$gender == "Female", ]$bmi))] <- mean_f_bmi
df_replacedNA[df_replacedNA$gender == "Male", ]$bmi[which(is.na(df_replacedNA[df_replacedNA$gender == "Male", ]$bmi))] <- mean_m_bmi
data<- df_replacedNA
colSums(is.na(data))

unique(data$gender)
unique(data$ever_married)
unique(data$work_type)
unique(data$Residence_type)
unique(data$smoking_status)
unique(data$hypertension)
unique(data$heart_disease)
unique(data$stroke)

# Save a copy of the dataset
stroke <- copy(data)
data1<-data

# Transform binary variables to factor
data$stroke <- as.factor(data$stroke)
data$hypertension <- as.factor(data$hypertension)
data$heart_disease <- as.factor(data$heart_disease)
data$ever_married <- as.factor(data$ever_married)
data$Residence_type <- as.factor(data$Residence_type)

# examine the dataset
data %>% head()

#Exploratory Data Analysis
colnames(data)
summary(data)
p1 <- ggplot(data = data) +geom_bar(mapping = aes(x = gender))
p2 <-ggplot(data = data) +geom_bar(mapping = aes(x = hypertension))
p3 <-ggplot(data = data) +geom_bar(mapping = aes(x = heart_disease)) 
p4 <-ggplot(data = data) +geom_bar(mapping = aes(x = ever_married)) 
grid.arrange(p1,p2,p3,p4, ncol= 2)


p5 <-ggplot(data = data) +geom_bar(mapping = aes(x = work_type))
p6 <-ggplot(data = data) +geom_bar(mapping = aes(x = Residence_type))
p7 <-ggplot(data = data) +geom_bar(mapping = aes(x = smoking_status))
p8 <-ggplot(data = data) +geom_bar(mapping = aes(x = stroke))
grid.arrange(p5,p6,p7,p8, ncol= 2)


#for continous variables
c1 <- ggplot(data = data) + geom_histogram(mapping = aes(x = age), binwidth = 0.5, col = 'steelblue')
c2 <- ggplot(data = data) + geom_histogram(mapping = aes(x = avg_glucose_level), binwidth = 0.5, col = 'steelblue')
c3 <- ggplot(data = data) + geom_histogram(mapping = aes(x = bmi), binwidth = 0.5, col = 'steelblue')
grid.arrange(c1,c2,c3, ncol= 2)


#stroke on hypertension
ggplot(data = data, mapping = aes(x = stroke, y = avg_glucose_level)) +geom_boxplot()+labs(title='Stroke on Glucose Level')



#stroke on age
ggplot(data = data, mapping = aes(x = stroke, y = age)) +geom_boxplot()+labs(title='Stroke on Age')


#stroke on bmi
ggplot(data = data, mapping = aes(x = stroke, y = bmi)) +geom_boxplot()+labs(title='Stroke on bmi')


#Because of the outliers, the people who have stroke because of high bmi and glucose levels is not significant

ggplot(data = data) +geom_mosaic(aes(x = product(stroke,work_type), fill=work_type)) + labs(title='Stroke on Work Type')

ggplot(data = data) +geom_mosaic(aes(x = product(stroke,smoking_status), fill=smoking_status)) + labs(title='Stroke on smoking_status')

ggplot(data = data) +geom_mosaic(aes(x = product(stroke,ever_married), fill=ever_married)) + labs(title='Stroke on ever_married')

ggplot(data = data) +geom_mosaic(aes(x = product(stroke,hypertension), fill=hypertension)) + labs(title='Stroke on hypertension')



# Residence_type: Urban for 0, Rural for 1
stroke$Residence_type[stroke$Residence_type == "Urban"] <- 0
stroke$Residence_type[stroke$Residence_type == "Rural"] <- 1

# ever_married: No for 0, Yes for 1
stroke$ever_married[stroke$ever_married == "Yes"] <- 1
stroke$ever_married[stroke$ever_married == "No"] <- 0

# gender: Male for 0, Female for 1
stroke$gender[stroke$gender == "Male"] <- 0
stroke$gender[stroke$gender == "Female"] <- 1

stroke$stroke = as.numeric(stroke$stroke)

suppressWarnings(stroke$Residence_type <- as.numeric(as.character(stroke$Residence_type)))
suppressWarnings(stroke$ever_married <- as.numeric(as.character(stroke$ever_married)))
suppressWarnings(stroke$gender <- as.numeric(as.character(stroke$gender)))

# Quantitative Variables: Correlation Map
data.quant = subset(stroke, select = -c(work_type, smoking_status))
data.quant
str(data.quant)

data.cor = round(cor(data.quant),2)
ggplot(data = reshape2::melt(data.cor),aes(x=Var1, y=Var2, fill=value)) + geom_tile() +  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 30))

data %>% head()


cont.plot <- ggplot(data = data, aes(x= age, y = bmi, color = stroke))+geom_point()
cont.plot

cont.plot <- ggplot(data=data, aes(x=age,y=avg_glucose_level,color=stroke))+geom_point()
cont.plot

# Training set creation

#For multi-level variables, get dummy
stroke_dummy <- dummy_cols(data,select_columns = c("gender","work_type","smoking_status"),remove_first_dummy = TRUE, remove_selected_columns = TRUE)
stroke_dummy %>% head()

Training <- createDataPartition(y = stroke_dummy$stroke, p = 0.7, list = FALSE)
training <- stroke_dummy[Training,]
testing <- stroke_dummy[-Training,]

dim(training)

dim(testing)

model <- glm(stroke ~.,family=binomial(link='logit'), data=training)
summary(model)

anova(model, test="Chisq")

#predict the stroke variable based on the testing dataset
model.prob = predict(model, testing, type="response")
summary(model.prob)

# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(as.numeric(model.prob>0.5)), reference = as.factor(testing$stroke))

threhold <- seq(0.3,0.6,0.01)
accuracy <- NULL
for (i in seq(along = threhold)){
  prediction <- ifelse(model$fitted.values >= threhold[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(training$stroke ==prediction))/length(prediction)*100)
}
plot(threhold, accuracy, pch =15,type='b',col= "red",main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

stroke.rf <- randomForest(stroke ~ ., data = data, importance = TRUE,proximity = TRUE)
print(stroke.rf)
plot


#--------dealing with the imbalance----------------------

predictor_variables <- data1[,0:10] # Select everything except stroke
response_variable <- data1$stroke  # Only select response variable

levels(response_variable) <- c(0, 1)
predictor_variables



#apply undersampling
data_df <- ubBalance(X=predictor_variables, Y=response_variable, type="ubUnder", perc=50,  method="percPos")
#undersampled dataset
underData <- cbind(data_df$X, Class=data_df$Y)
#check the frequency of the target variable after oversampling
summary(underData)
ud<-underData
head(underData)
#levels(underData$stroke) <- c(0,1)
cont.plot <- ggplot(data = underData, aes(x= age, y = bmi, color = Class))+geom_point()
cont.plot

p8 <-ggplot(data = underData) +geom_bar(mapping = aes(x = Class,fill=Class))
p8
underData$Class <- as.factor(underData$Class)
underData$hypertension <- as.factor(underData$hypertension)
underData$heart_disease <- as.factor(underData$heart_disease)
underData$ever_married <- as.factor(underData$ever_married)
underData$Residence_type <- as.factor(underData$Residence_type)


# Residence_type: Urban for 0, Rural for 1
ud$Residence_type[ud$Residence_type == "Urban"] <- 0
ud$Residence_type[ud$Residence_type == "Rural"] <- 1

# ever_married: No for 0, Yes for 1
ud$ever_married[ud$ever_married == "Yes"] <- 1
ud$ever_married[ud$ever_married == "No"] <- 0

# gender: Male for 0, Female for 1
ud$gender[ud$gender == "Male"] <- 0
ud$gender[ud$gender == "Female"] <- 1

ud$Class = as.numeric(ud$Class)

suppressWarnings(ud$Residence_type <- as.numeric(as.character(ud$Residence_type)))
suppressWarnings(ud$ever_married <- as.numeric(as.character(ud$ever_married)))
suppressWarnings(ud$gender <- as.numeric(as.character(ud$gender)))

# Quantitative Variables: Correlation Map
data.quant = subset(ud, select = -c(work_type, smoking_status))
data.quant
str(data.quant)

data.cor = round(cor(data.quant),2)
ggplot(data = reshape2::melt(data.cor),aes(x=Var1, y=Var2, fill=value)) + geom_tile() +  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 30))

underData
#---------------------
stroke_dummy1 <- dummy_cols(underData,select_columns = c("gender","work_type","smoking_status"),remove_first_dummy = TRUE, remove_selected_columns = TRUE)
stroke_dummy1 %>% head()
underData
Training <- createDataPartition(y = stroke_dummy1$Class, p = 0.7, list = FALSE)
training <- stroke_dummy1[Training,]
testing <- stroke_dummy1[-Training,]

dim(training)

dim(testing)

model <- glm(Class ~.,family=binomial(link='logit'), data=training)
summary(model)

anova(model, test="Chisq")

#predict the stroke variable based on the testing dataset
model.prob = predict(model, testing, type="response")


# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(as.numeric(model.prob>0.5)), reference = as.factor(testing$Class))

threhold <- seq(0.3,0.6,0.01)
accuracy <- NULL
for (i in seq(along = threhold)){
  prediction <- ifelse(model$fitted.values >= threhold[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(training$Class ==prediction))/length(prediction)*100)
}
plot(threhold, accuracy, pch =15,type='b',col= "red",main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

stroke.rf <- randomForest(Class ~ ., data = underData, importance = TRUE,proximity = TRUE)
print(stroke.rf)




#--------------------apply oversampling-----------------------------------#
data_over <- ubBalance(X=predictor_variables, Y=response_variable, type="ubOver", k=0)
#oversampled dataset
overData <- cbind(data_over$X, Class=data_over$Y)
#check the frequency of the target variable after oversampling
summary(overData$Class)
cont.plot <- ggplot(data = overData, aes(x= age, y = avg_glucose_level, color = Class))+geom_point()
cont.plot

p8 <-ggplot(data = overData) +geom_bar(mapping = aes(x = Class,fill=Class))
p8

ud<-overData
overData$Class <- as.factor(overData$Class)
overData$hypertension <- as.factor(overData$hypertension)
overData$heart_disease <- as.factor(overData$heart_disease)
overData$ever_married <- as.factor(overData$ever_married)
overData$Residence_type <- as.factor(overData$Residence_type)


# Residence_type: Urban for 0, Rural for 1
ud$Residence_type[ud$Residence_type == "Urban"] <- 0
ud$Residence_type[ud$Residence_type == "Rural"] <- 1

# ever_married: No for 0, Yes for 1
ud$ever_married[ud$ever_married == "Yes"] <- 1
ud$ever_married[ud$ever_married == "No"] <- 0

# gender: Male for 0, Female for 1
ud$gender[ud$gender == "Male"] <- 0
ud$gender[ud$gender == "Female"] <- 1

ud$Class = as.numeric(ud$Class)

suppressWarnings(ud$Residence_type <- as.numeric(as.character(ud$Residence_type)))
suppressWarnings(ud$ever_married <- as.numeric(as.character(ud$ever_married)))
suppressWarnings(ud$gender <- as.numeric(as.character(ud$gender)))

# Quantitative Variables: Correlation Map
data.quant = subset(ud, select = -c(work_type, smoking_status))
data.quant
str(data.quant)

data.cor = round(cor(data.quant),2)
ggplot(data = reshape2::melt(data.cor),aes(x=Var1, y=Var2, fill=value)) + geom_tile() +  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 30))


stroke_dummy2 <- dummy_cols(overData,select_columns = c("gender","work_type","smoking_status"),remove_first_dummy = TRUE, remove_selected_columns = TRUE)
stroke_dummy2 %>% head()

Training <- createDataPartition(y = stroke_dummy2$Class, p = 0.7, list = FALSE)
training <- stroke_dummy2[Training,]
testing <- stroke_dummy2[-Training,]

dim(training)

dim(testing)

model <- glm(Class ~.,family=binomial(link='logit'), data=training)
summary(model)

anova(model, test="Chisq")

#predict the stroke variable based on the testing dataset
model.prob = predict(model, testing, type="response")


# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(as.numeric(model.prob>0.5)), reference = as.factor(testing$Class))

threhold <- seq(0.3,0.6,0.01)
accuracy <- NULL
for (i in seq(along = threhold)){
  prediction <- ifelse(model$fitted.values >= threhold[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(training$Class ==prediction))/length(prediction)*100)
}
plot(threhold, accuracy, pch =15,type='b',col= "red",main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

stroke.rf <- randomForest(Class ~ ., data = overData, importance = TRUE,proximity = TRUE)
print(stroke.rf)
