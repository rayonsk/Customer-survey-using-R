#importing libraries
library(readr)
library(tidyverse)
library(caret)
require(scales)
library(corrplot)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(xgboost)
library(ranger)
library(tidyverse)
library(lubridate)

SurveyIncomplete <- read_csv("Data Set/SurveyIncomplete.csv")
CompleteResponses <- read_csv("Data Set/CompleteResponses.csv")
#View the data
View(CompleteResponses)
str(CompleteResponses)
summary(CompleteResponses)

#Preprcessing
table(factor(is.na(CompleteResponses)))
table(CompleteResponses$zipcode)
table(CompleteResponses$elevel)
table(CompleteResponses$car)


str(CompleteResponses)
#removing the na's
na.omit(CompleteResponses)
# CompleteResponses$credit_bin <- cut(CompleteResponses$credit,
#                                     breaks = 4, 
#                                     labels = c("Level1", "Level2", "Level3","Level4"))

#duplicate
table(duplicated(CompleteResponses)) #gives you no:of duplicate
unique(CompleteResponses)
#remove duplicate based on all columns
CompleteResponses%>% distinct()
