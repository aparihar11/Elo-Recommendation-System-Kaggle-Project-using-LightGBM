x<-c("tidyverse", "dplyr", "lubridate","ggplot2","caret")
lapply(x, require, character.only = TRUE)
setwd("C:/Users/aparihar/Documents/GitHub/elo_recommendation/dataset")

train <- read_csv("train.csv")
test <- read_csv("test.csv")
histdata <- read.csv("historical_transactions.csv",nrows = 100000)
newdata <- read.csv("new_merchant_transactions.csv",nrow=100000)
merchants <- read_csv("merchants.csv")
sample <- read_csv("sample_submission.csv")

save(train,test,histdata,newdata,merchants,sample,file="data.Rdata")
load("data.Rdata")
dim(train)
head(train)
glimpse(train)
head(test)
dim(histdata)
head(histdata)

hist_cat1 <- histdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)

hist_cat2 <- histdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)

hist_cat3 <- histdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)

hist_summary2 <- histdata %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)

histdata$purchase_date<-as.integer(histdata$purchase_date)
# purchase term, how many days between each transaction.
hist_summary3 <- histdata %>% group_by(card_id)%>%
  summarize(no_trans=n(), 
            pur_term = diff(range(purchase_date)),
            avg_term = mean(abs(diff(order(purchase_date))))                   
  )


fn <- funs(sum, mean, min, max, sd, n_distinct, .args = list(na.rm = TRUE))
histdata$authorized_flag <- ifelse(histdata$authorized_flag == "Y",1,0)

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


new_summary2 <- new_merge %>% group_by(card_id)%>%
  summarise_at(vars(starts_with("merchant_"),starts_with("category")), n_distinct, na.rm = TRUE)

new_merge$purchase_date<-as.integer(new_merge$purchase_date)
new_summary3 <- new_merge %>%
  group_by(card_id) %>% 
  summarise(
    new_no_trans=n(), 
    new_pur_term = diff(range(purchase_date)),
    new_avg_term = mean(abs(diff(order(purchase_date))))
  )

new_merge$authorized_flag <- ifelse(new_merge$authorized_flag == "Y",1,0)

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



train_data[is.na(train_data)] <- 0
head(train_data)
dim(train_data)


# test data
test_data <- test %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         howlong = as.integer(ymd("2018-02-01") - first_active_month)) %>%
  left_join(hist_summary, by="card_id") %>%
  left_join(new_summary,by="card_id",suffix = c("", "_new"))


test_data1 <- test_data[,-c(1,2)]
test_data1[is.na(test_data1)] <- 0

data <- train_data[,-c(1,2)]
test.final <- test_data1

#ML model

library(caret)
set.seed(1)

inTrain <- createDataPartition(data$target, p=.8)[[1]]
dataTr <- data[inTrain,]
dataTs <- data[-inTrain,]
install.packages(lgbdl)
source("lgbdl.R")

lgb.dl(commit = "master", compiler = "gcc", libdll = "",
       repo = "https://github.com/Microsoft/LightGBM", use_gpu = FALSE,
       cores = 1)

library(lightgbm)
# lgb dataset
train_s <- as.matrix(dataTr[,-4])
dtrain <- lgb.Dataset(data=train_s, label = dataTr$target)

test_s <- as.matrix(dataTs[,-4])

params <- list(objective="regression",
               metric = "l2",
               #                 min_sum_hessian_in_leaf = 1,
               feature_fraction = 0.7,
               bagging_fraction = 0.7,
               bagging_freq = 5,
               #                 min_data = 100,
               max_bin = 50,
               lambda_l1 = 8,
               lambda_l2 = 1.3
               #                 min_data_in_bin=100,
               #                 min_gain_to_split = 10,
               #                 min_data_in_leaf = 30
               #is_unbalance = TRUE
)


lgb_m <- lgb.train(params=params,
                   data=dtrain,
                   min_data =1, 
                   learning_rate=0.1, 
                   nrounds = 500)






















