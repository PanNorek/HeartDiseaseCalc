# g = getwd()
# setwd(paste0(g,"//heartfailure"))
#setwd("C:\\Users\\Admin\\OneDrive\\Pulpit\\coding\\R\\heartfailure\\HeartFailureApp")
# read our data
df = read.csv("heart.csv")



# check for missing values
colSums(is.na(df))


# load what you need
library(dplyr)
library(ggplot2)
library(fastDummies)
library(randomForest)
require(tree)
require(caTools)
library(rattle)
library(caret)
# A little bit of data pre-processing
str(df)


# One Hot Encoding Algorithm
# categorical_variables = c("Sex","ChestPainType","RestingECG", "ExerciseAngina", "ST_Slope")

# df <- dummy_cols(df, select_columns = categorical_variables)
# df <- df %>% select(-all_of(categorical_variables))
df <- df %>% transform(Sex=as.factor(Sex),
                       ChestPainType=as.factor(ChestPainType),
                       RestingECG=as.factor(RestingECG),
                       ExerciseAngina=as.factor(ExerciseAngina),
                       ST_Slope=as.factor(ST_Slope),
                       HeartDisease=as.factor(HeartDisease)
) 


colnames(df)

# unnecessary_cols <- c("Sex_M","ExerciseAngina_N","RestingECG_Normal","ST_Slope_Flat")
# df <- df %>% select(-all_of(unnecessary_cols))
# X <- df %>%  select(-c("HeartDisease"))
# y<-df["HeartDisease"]

head(df)
summary(df)
str(df)


# sample for model
sample = sample.split(df$HeartDisease, SplitRatio = .75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)



# BDT

tree.df = tree(df$HeartDisease~. , data=df)
summary(tree.df)

str(df)
plot(tree.df)
text(tree.df, pretty = 3)
# title(main="Drzewo decyzyjne ...",sub=paste("Dokładność predykcji:",(a/nrow(test)))

predict(tree.df,newdata = test,type="class")
a <- sum(test$HeartDisease==predict(tree.df,newdata = test,type="class"))
nrow(test)
print("Dokładność predykcji:")
print(a/nrow(test))


fancyRpartPlot(rpart(HeartDisease~., data=df),yesno=2,split.col="black",nn.col="black", 
               caption="",palette="Set2",branch.col="black")

#random forest for classification
rf <- randomForest(HeartDisease~., data=train, proximity=TRUE)
print(rf)
p1 <- predict(rf, train)
confusionMatrix(p1, train$HeartDisease)

p2 <- predict(rf, test)
confusionMatrix(p2, test$HeartDisease)


plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
# partialPlot(rf, train, MaxHR, "1")

# lets make another model with the most 8 important variables

colnames(df)
# 
new_df = df[c("ST_Slope","ChestPainType","Oldpeak","MaxHR","Cholesterol","HeartDisease","ExerciseAngina","Age","RestingBP")]


sample = sample.split(new_df$HeartDisease, SplitRatio = .75)
train = subset(new_df, sample == TRUE)
test  = subset(new_df, sample == FALSE)

rf <- randomForest(HeartDisease~., data=train, proximity=TRUE)
print(rf)
p1 <- predict(rf, train)
confusionMatrix(p1, train$HeartDisease)

p2 <- predict(rf, test)
confusionMatrix(p2, test$HeartDisease)


plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")



