library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)

#Import Dataset
credit_record <- read_csv("credit_record.csv")
credit_record
application_record <- read_csv("application_record.csv")
application_record

#Join Dataset
creditJoin <- inner_join(credit_record, application_record, by = "ID")
creditJoin <- drop_na(creditJoin,`CODE_GENDER`)
creditJoin

#Change STATUS
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "C",0)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "X",0)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "0",1)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "1",2)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "2",3)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "3",4)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "4",5)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "5",6)
hist(creditJoin$STATUSYN)

#Score Customer
creditJoin %>%
  group_by(ID) %>%
  summarise(Score = mean(STATUSYN)) -> Scorecustomer

#EvaluteCustomer
Scorecustomer <- mutate(Scorecustomer, 
                        EvaluateCustomer = ifelse(Score <= 0.9 , 1, 0)) 
hist(Scorecustomer$EvaluateCustomer)

#JoinData Predict
DataComplete <- inner_join(Scorecustomer, application_record, by = "ID")
DataComplete$OCCUPATION_TYPE <- DataComplete$OCCUPATION_TYPE %>% replace_na('Unspecified')
DataComplete <- mutate(DataComplete, AGE = DAYS_BIRTH/-365)
DataComplete$AGE <- as.integer(DataComplete$AGE)
DataComplete <- mutate(DataComplete, EMPLOYED = DAYS_EMPLOYED/-365)
DataComplete$WORKYEARS <- as.integer(DataComplete$EMPLOYED)
DataComplete$WORKYEARS <- replace(DataComplete$WORKYEARS, DataComplete$WORKYEARS == -1000,-1)
DataComplete %>%
  select(-DAYS_BIRTH,-DAYS_EMPLOYED,-EMPLOYED,-FLAG_MOBIL) -> DataComplete
DataComplete

#------------------------------- Logistic regression -----------------------------------------
DataComplete$EvaluateCustomer <- as.factor(DataComplete$EvaluateCustomer)
DataComplete$CODE_GENDER <- as.factor(DataComplete$CODE_GENDER)
DataComplete$FLAG_OWN_CAR <- as.factor(DataComplete$FLAG_OWN_CAR)
DataComplete$FLAG_OWN_REALTY <- as.factor(DataComplete$FLAG_OWN_REALTY)
DataComplete$NAME_INCOME_TYPE <- as.factor(DataComplete$NAME_INCOME_TYPE)
DataComplete$NAME_EDUCATION_TYPE <- as.factor(DataComplete$NAME_EDUCATION_TYPE)
DataComplete$NAME_FAMILY_STATUS <- as.factor(DataComplete$NAME_FAMILY_STATUS)
DataComplete$NAME_HOUSING_TYPE <- as.factor(DataComplete$NAME_HOUSING_TYPE)
DataComplete$FLAG_WORK_PHONE <- ifelse(test = DataComplete$FLAG_WORK_PHONE == 1, yes = "Y", no = "N")
DataComplete$FLAG_WORK_PHONE <- as.factor(DataComplete$FLAG_WORK_PHONE)
DataComplete$FLAG_PHONE <- ifelse(test = DataComplete$FLAG_PHONE == 1, yes = "Y", no = "N")
DataComplete$FLAG_PHONE <- as.factor(DataComplete$FLAG_PHONE)
DataComplete$FLAG_EMAIL <- ifelse(test = DataComplete$FLAG_EMAIL == 1, yes = "Y", no = "N")
DataComplete$FLAG_EMAIL <- as.factor(DataComplete$FLAG_EMAIL)
DataComplete$OCCUPATION_TYPE <- as.factor(DataComplete$OCCUPATION_TYPE)
str(DataComplete)

#split data
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(DataComplete), replace=TRUE, prob=c(0.7,0.3))
train  <- DataComplete[sample, ]
test   <- DataComplete[!sample, ]

#modeling
model <- glm(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
             + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
             + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + OCCUPATION_TYPE
             + CNT_FAM_MEMBERS + AGE + WORKYEARS, train, family = binomial)
summary(model)

#model1 <- glm(EvaluateCustomer ~ CODE_GENDER, train, family = binomial)
#model2 <- glm(EvaluateCustomer ~ FLAG_OWN_CAR, train, family = binomial)
#model3 <- glm(EvaluateCustomer ~ FLAG_OWN_REALTY, train, family = binomial)
#model4 <- glm(EvaluateCustomer ~ CNT_CHILDREN, train, family = binomial)
#model5 <- glm(EvaluateCustomer ~ AMT_INCOME_TOTAL, train, family = binomial)
#model6 <- glm(EvaluateCustomer ~ NAME_INCOME_TYPE, train, family = binomial)
#model7 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE, train, family = binomial)
#model8 <- glm(EvaluateCustomer ~ NAME_FAMILY_STATUS, train, family = binomial)
#model9 <- glm(EvaluateCustomer ~ NAME_HOUSING_TYPE, train, family = binomial)
#model10 <- glm(EvaluateCustomer ~ OCCUPATION_TYPE, train, family = binomial)
#model11 <- glm(EvaluateCustomer ~ CNT_FAM_MEMBERS, train, family = binomial)
#model12 <- glm(EvaluateCustomer ~ AGE, train, family = binomial)
#summary(model1) #R 26717 AIC 26721
#summary(model2) #R 26718 AIC 26722
#summary(model3) #R 26719 AIC 26723
#summary(model4) #R 26718 AIC 26722
#summary(model5) #R 26716 AIC 26720
#summary(model6) #R 26713 AIC 26723
#summary(model7) #R 26702 AIC 26712 *
#summary(model8) #R 26708 AIC 26718 *
#summary(model9) #R 26708 AIC 26720 *
#summary(model10) #R 26688 AIC 26726 ***
#summary(model11) #R 26718 AIC 26722
#summary(model12) #R 26717 AIC 26721

#model_multi1 <- glm(EvaluateCustomer ~ NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + OCCUPATION_TYPE, train, family = binomial)
#model_multi2 <- glm(EvaluateCustomer ~ NAME_FAMILY_STATUS + NAME_HOUSING_TYPE, train, family = binomial)
#model_multi3 <- glm(EvaluateCustomer ~ NAME_HOUSING_TYPE + OCCUPATION_TYPE, train, family = binomial)
#model_multi4 <- glm(EvaluateCustomer ~ NAME_FAMILY_STATUS + OCCUPATION_TYPE, train, family = binomial)
#model_multi5 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + OCCUPATION_TYPE, train, family = binomial)
#model_multi6 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + OCCUPATION_TYPE, train, family = binomial)
#model_multi7 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS, train, family = binomial)
#model_multi8 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_HOUSING_TYPE, train, family = binomial)
#model_multi9 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + NAME_HOUSING_TYPE, train, family = binomial)
#model_multi10 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS + OCCUPATION_TYPE, train, family = binomial)
#model_multi11 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_HOUSING_TYPE + OCCUPATION_TYPE, train, family = binomial)
#model_multi12 <- glm(EvaluateCustomer ~ NAME_EDUCATION_TYPE + NAME_HOUSING_TYPE + OCCUPATION_TYPE, train, family = binomial)
#summary(model_multi1) #R 26665 AIC 26721
#summary(model_multi2) #R 26697 AIC 26717
#summary(model_multi3) #R 26677 AIC 26725
#summary(model_multi4) #R 26676 AIC 26722
#summary(model_multi5) #R 26648 AIC 26712 *
#summary(model_multi6) #R 26672 AIC 26718
#summary(model_multi7) #R 26691 AIC 26709
#summary(model_multi8) #R 26691 AIC 26711
#summary(model_multi9) #R 26679 AIC 26707 *
#summary(model_multi10) #R 26659 AIC 26713 *
#summary(model_multi11) #R 26661 AIC 26717 ***

#------------------------------- Tree -----------------------------------------
#Tree 
tree <- rpart(EvaluateCustomer~CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + OCCUPATION_TYPE
              + CNT_FAM_MEMBERS + AGE + WORKYEARS, train)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Cross-validation
train_control <- trainControl(method="cv",number = 5)
model <- train(EvaluateCustomer~CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
               + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
               + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + OCCUPATION_TYPE
               + CNT_FAM_MEMBERS + AGE + WORKYEARS, train,
               trControl = train_control,
               method = "rpart")
model

#Tree-cp-Before
tree <- rpart(EvaluateCustomer~CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + OCCUPATION_TYPE
              + CNT_FAM_MEMBERS + AGE + WORKYEARS, train, parms=list(split=c("information","gini")),
              cp = 0.0004, minsplit=20, minbucket=5, maxdepth=30)
tree
#rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
tree$variable.importance
res1 <- predict(tree, test, type = "class")
confusionMatrix(res1, test$EvaluateCustomer, mode = "prec_recall", positive = "1")

#Tree-cp-After
tree <- rpart(EvaluateCustomer~CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + OCCUPATION_TYPE
              + CNT_FAM_MEMBERS + AGE + WORKYEARS, train, parms=list(split=c("information","gini")),
              cp = 0.0003,minsplit=7, minbucket=5, maxdepth=10)
tree
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
tree$variable.importance
res1 <- predict(tree, test, type = "class")
confusionMatrix(res1, test$EvaluateCustomer, mode = "prec_recall", positive = "1")

#------------------------------- Predict & Evaluation -----------------------------------------
#predict
res1 <- predict(model, test, type = "response")
hist(res1)
res1c <- factor(ifelse(res1 > 0.75, "1", "0"))
table(res1c)
confusionMatrix(res1c, test$EvaluateCustomer, mode = "prec_recall", positive = "1")
