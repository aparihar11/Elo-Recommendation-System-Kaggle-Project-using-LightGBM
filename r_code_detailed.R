
x<-c("tidyverse", "dplyr", "lubridate","ggplot2","caret","lightgbm","caret","lightgbm","corrplot","Rmisc","GA","Rmisc","magrittrain","scales","Metrics")
lapply(x, require, character.only = TRUE)
setwd("C:\\Users\\aparihar\\Documents\\GitHub\\elo_recommendation\\dataset")


#read data
train <- read_csv("train.csv")
test <- read_csv("test.csv")
histdata <- read.csv("historical_transactions.csv",nrows = 200000)
newdata <- read.csv("new_merchant_transactions.csv",nrow=200000)
merchants <- read_csv("merchants.csv")
sample <- read_csv("sample_submission.csv")






####feature engineering


# trainain/testst
## Features
#Let's plot how the datests of the first purchases are distrainibutestd:
train %>% 
  bind_rows(test) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train")),
         first_active_month = ymd(first_active_month, truncated = 1)) %>% 
  ggplot(aes(x = first_active_month, fill = set)) +
  geom_bar() +
  theme_minimal()

#The distrainibutions are quitest similar - this makes cross-validation easier.
#Let's have a look at the counts of the anonymized features:
train %>% 
  bind_rows(test) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train"))) %>% 
  select(-first_active_month, -card_id, -target) %>% 
  gather(key = "feature", value = "value", -set) %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = set)) +
  geom_bar(aes(y=..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(set ~ feature, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

#We can observe almost identical distrainibutions within trainain and testst sets. These features
#look like strainatified 2-folds - perfectly simulatestd data

## Target
summary(train$target)


#This is how the mean and sum target changes over the time - there is a growing trend for the mean value
train %>% 
  select(first_active_month, target) %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1)) %>% 
  group_by(first_active_month) %>% 
  summarise_all(funs(sum, mean)) %>% 
  ungroup() %>% 
  gather(key = "feature", value = "value", -first_active_month) %>% 
  ggplot(aes(x = first_active_month, y = value, colour = feature)) +
  geom_smooth() +
  facet_wrap(~ feature, ncol = 1, scales = "free") + 
  theme_minimal() + 
  theme(legend.position="none")

train %>% 
  ggplot(aes(target)) +
  geom_histogram(bins = 100, fill = "steelblue") +
  theme_minimal()

## Correlations
##Let's convert categorical features to dummy variables and check correlations:
train %>% 
  mutate(feature_1 = factor(feature_1),
         feature_2 = factor(feature_2),
         feature_2 = factor(feature_2)) %>% 
  select(-first_active_month, -card_id) %>% 
  model.matrix(~.-1, .) %>% 
  cor(method = "spearman") %>%
  corrplot(type="lower", method = "number", tl.col = "black", diag=FALSE, tl.cex = 0.9, number.cex = 0.9)

# Historical and new merchant transactions
## Features
#There is a total of 14 features in each dataset:


#Let's plot some categorical features:

p1 <- histdata%>%
  select(authorized_flag, category_1, category_2, category_3) %>% 
  mutate(category_2 = as.character(category_2)) %>% 
  gather(key = "feature", value = "value") %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = feature)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  ggtitle("Historical") +
  theme(legend.position = "none")

p2 <- newdata%>%
  select(authorized_flag, category_1, category_2, category_3) %>% 
  mutate(category_2 = as.character(category_2)) %>% 
  gather(key = "feature", value = "value") %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = feature)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  ggtitle("New") +
  theme(legend.position = "none")

#plot
multiplot(p1, p2, cols = 2)

#The next figures show the **id's** with the largest mean **purchase_amount**:
histdata %>%
  select(contains("_id"), purchase_amount) %>% 
  mutate_all(as.character) %>% 
  mutate(purchase_amount = as.numeric(purchase_amount)) %>% 
  gather(key = "id", value = "value", -purchase_amount) %>% 
  group_by(id, value) %>% 
  summarise_all(mean) %>% 
  arrange(desc(purchase_amount)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(value = factor(value), 
         id = factor(id)) %>% 
  ggplot(aes(x = value, y = purchase_amount, fill = id)) +
  geom_col() +
  facet_wrap(~ id, scales = "free") +
  theme_minimal() +
  ggtitle("Historical") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

newdata %>%
  select(contains("_id"), purchase_amount) %>% 
  mutate_all(as.character) %>% 
  mutate(purchase_amount = as.numeric(purchase_amount)) %>% 
  gather(key = "id", value = "value", -purchase_amount) %>% 
  group_by(id, value) %>% 
  summarise_all(mean) %>% 
  arrange(desc(purchase_amount)) %>% 
  slice(1:5) %>% 
  ungroup() %>% 
  mutate(value = factor(value), 
         id = factor(id)) %>% 
  ggplot(aes(x = value, y = purchase_amount, fill = id)) +
  geom_col() +
  facet_wrap(~ id, scales = "free") +
  theme_minimal() +
  ggtitle("New") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) 


# Merchants
#There is a total of 22 features:
#This additional dataset contains many numerical and lagged features. Let's plot their distributions:

merchants %>%
  select(contains("lag")) %>% 
  gather(key = "feature", value = "value", factor_key = TRUE) %>% 
  mutate(value = ifelse(is.infinite(value), NA, value)) %>% 
  ggplot(aes(x = value, fill = feature)) +
  geom_density() +
  scale_x_continuous(limits=c(0, 5)) + 
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")







save(train,test,histdata,newdata,merchants,file="data10.Rdata")
dim(train)
head(train)
glimpse(train)
head(test)
dim(histdata)
head(histdata)

#For Category 1, 2, 3.
#Group by card_id and category by count, changing category to columns.

hist_cat1 <- histdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)


hist_cat2 <- histdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)


hist_cat3 <- histdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)

#Unique card id purchases at merchant_id by merchant_category_id by category by types. Exclude NA
hist_summary2 <- histdata %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)


#Change purchae date to int, for later use of
histdata$purchase_date<-as.integer(histdata$purchase_date)


#Number of transaction
#Difference of purchase days, range will give min, max.
#Purchase term, how many days between each transaction.

hist_summary3 <- histdata %>% group_by(card_id)%>%
  summarize(no_trans=n(), 
            pur_term = diff(range(purchase_date)),
            avg_term = mean(abs(diff(order(purchase_date))))                   
  )

#generate a named list of functions
fn <- funs(sum, mean, min, max, sd, n_distinct, .args = list(na.rm = TRUE))


#authorized_flag to dummy
histdata$authorized_flag <- ifelse(histdata$authorized_flag == "Y",1,0)


#Group by card_id, with variables selected, get info from funtions in fn line 65(min, max ....)
#Then Left join the table to hist summary with out NA/V1

hist_summary <- histdata %>%
  group_by(card_id) %>% 
  select(c("card_id","purchase_amount","month_lag","installments","authorized_flag")) %>%
  summarize_all(fn) %>%
  left_join(hist_summary2,by="card_id") %>%
  left_join(hist_summary3,by="card_id") %>%
  left_join(hist_cat1,by="card_id") %>%
  left_join(hist_cat2[,-7],by="card_id") %>%
  left_join(hist_cat3[,-5],by="card_id") 

head(hist_summary)




# new merchants transaction data
# newdata category
new_cat1 <- newdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)

new_cat2 <- newdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)

new_cat3 <- newdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)



# rename columns
colnames(new_cat1) <- c("card_id","new_N", "new_Y")
colnames(new_cat2) <- c("card_id","new1", "new2","new3","new4","new5","na")
colnames(new_cat3) <- c("card_id","new_A", "new_B", "new_C", "na")



# merge new transaction & merchants data
new_merge <- newdata %>%
  left_join(merchants, by="merchant_id",suffix = c("_new", "_m"))

#Unique card id purchases at merchant_id by merchant_category_id by category by types. Exclude NA
new_summary2 <- new_merge %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)

#Change purchae date to int, for later use of
new_merge$purchase_date<-as.integer(new_merge$purchase_date)


#count of transaction
#Difference of purchase days, range will give min, max.
#Purchase term, how many days between each transaction.
new_summary3 <- new_merge %>%
  group_by(card_id) %>% 
  summarise(
    new_no_trans=n(), 
    new_pur_term = diff(range(purchase_date)),
    new_avg_term = mean(abs(diff(order(purchase_date))))
  )

#authorized_flag to dummy
new_merge$authorized_flag <- ifelse(new_merge$authorized_flag == "Y",1,0)


#Group by card_id, with variables selected, get info from funtions in fn(min, max ....)
#Then Left join the table to hist summary with out NA/V1
new_summary <- new_merge %>%
  group_by(card_id) %>% 
  select(c("card_id","purchase_amount","month_lag","installments","authorized_flag","avg_purchases_lag3","avg_purchases_lag6","avg_purchases_lag12")) %>%
  summarize_all(fn) %>%
  left_join(new_summary2,by="card_id") %>%
  left_join(new_summary3,by="card_id") %>%
  left_join(new_cat1, by="card_id") %>%
  left_join(new_cat2[,-7],by="card_id") %>%
  left_join(new_cat3[,-5],by="card_id") 

head(new_summary)



# training data
train_data <- train %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         howlong = as.integer(ymd("2018-02-01") - first_active_month)) %>%
  left_join(hist_summary, by="card_id") %>%
  left_join(new_summary,by="card_id",suffix = c("", "_new"))

head(train_data)


#Giva na to 0
train_data[is.na(train_data)] <- 0
head(train_data)
dim(train_data)


#Create year, month, relationship period
#left join tables from previous
test_data <- test %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         howlong = as.integer(ymd("2018-02-01") - first_active_month)) %>%
  left_join(hist_summary, by="card_id") %>%
  left_join(new_summary,by="card_id",suffix = c("", "_new"))

#drop first two column
test_data1 <- test_data[,-c(1,2)]


#NA to 0
test_data1[is.na(test_data1)] <- 0


#drop first two column, final data
data <- train_data[,-c(1,2)]
test.final <- test_data1


set.seed(1)
#write.csv(data,"finaltable.csv")



#split to train test target

inTrain <- createDataPartition(data$target, p=.8)[[1]]
dataTr <- data[inTrain,]
dataTs <- data[-inTrain,]
dataTr$target<-as.factor(dataTr$target)
dataTr<-na.omit(dataTr)


#MACHINE LEARNING MODELS XGBOOST & LIGHTGBM
####here we are testing gbtree which is default booster for xgboot and lightgbm and also random forest

params <- list(objective = "reg:linear",
               booster = "gbtree",
               eval_metric = "rmse",
               nthread = 4,
               eta = 0.01,
               max_depth = 8,
               min_child_weight = 5,
               gamma = 1,
               subsample = 0.8,
               colsample_bytree = 0.7,
               colsample_bylevel = 0.6,
               alpha = 0.1,
               lambda = 5)

data_train_final <- as.matrix(data[,-4])
dtrain_final <- xgb.DMatrix(data = data_train_final, label = data$target)

data_test_final <- as.matrix(test.final)
dtest_final <- xgb.DMatrix(data = data_test_final)

xgb_model <- xgboost(params = params, 
                     data=dtrain_final, 
                     nrounds = 1000, 
                     print_every_n = 100, 
                     early_stopping_rounds = 50)

# prediction
preds <- predict(xgb_model, newdata = dtest_final) 
summary(preds)


train_s <- as.matrix(dataTr[,-4])
dtrain <- lgb.Dataset(data=train_s, label = dataTr$target)

test_s <- as.matrix(dataTs[,-4])

params <- list(objective="regression",
               metric = "l2",
               feature_fraction = 0.7,
               bagging_fraction = 0.7,
               bagging_freq = 5,
               max_bin = 255,
               lambda_l1 = 8,
               lambda_l2 = 1.3
)



####TRAIN/TEST SPLIT

inTrain <- createDataPartition(data$target, p=.8)[[1]]
dataTr <- data[inTrain,]
dataTs <- data[-inTrain,]

train_s <- as.matrix(dataTr[,-4])
dtrain <- lgb.Dataset(data=train_s, label = dataTr$target)

test_s <- as.matrix(dataTs[,-4])


#####GBTREE#########

params <- list(objective="regression",
               metric = "l2",
               feature_fraction = 0.7,
               bagging_fraction = 0.7,
               bagging_freq = 5,
               max_bin = 50,
               lambda_l1 = 8,
               lambda_l2 = 1.3
           
)

####### RANDOM FOREST

paramsrandom <- list(objective="regression",
               metric = "l2",
               boosting_type="rf",
               feature_fraction = 0.7,
               bagging_fraction = 0.7,
               bagging_freq = 5,
               max_bin = 50,
               lambda_l1 = 8,
               lambda_l2 = 1.3
            
)





###################  TRAIn MODEL##############################################



####GBDT#######

lgb_m <- lgb.train(params=params,
                   data=dtrain,
                   min_data =1, 
                   learning_rate=0.1, 
                   nrounds = 500)


####RANDOM######

lgb_r <- lgb.train(params=paramsrandom,
                   data=dtrain,
                   min_data =1, 
                   learning_rate=0.1, 
                   nrounds = 500)



#### PREDICTION########

preds_lgb <- predict(lgb_m, test_s)
preds_lgbr <- predict(lgb_r, test_s)


####MODEL SUMMARY

summary(preds_lgb)
summary(preds_lgbr)


#######RMSE#############

rmse(dataTs$target,preds_lgb)
rmse(dataTs$target,preds_lgbr)




