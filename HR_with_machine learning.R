library(caret)
library(tidyverse)
library(ranger)

## read csv
df <- read.csv("Generate_HR_Dataset.csv")

## convert data
df_clean <- df %>%
    mutate_if(is.character, as.factor) %>% 
    # สร้าง Target Variable
    mutate(is_active = ifelse(emp_status == "Active", "Yes", "No")) %>%
    
    # --- DROP COLUMN ที่เป็นเฉลย หรือ ไม่เกี่ยวข้อง ทิ้งให้หมด ---
    select(
        -emp_status,         
        -emp_id,         
        -resigned_Date,       
        -termination_type,    
        -exit_reason,
        -exit_sentiment,
        -employment_Date,
        -satisfaction_Score, ## drop-out for prevent model overfitting
    ) %>%
    
    na.omit()

df_clean$is_active <- as.factor(df_clean$is_active)

# 1.split
# 2.train
# 3.score model (predict)
# 4.evaluate model

## split data
set.seed(64)
n <- nrow(df_clean)
train_id <- sample(1:n,size=0.8*n)

train_df <- df_clean[train_id,]
test_df <- df_clean[-train_id,]

set.seed(64)
train_Ctrl <- trainControl(method ="cv",
                           number=5,verboseIter = TRUE)

my_grid <- expand.grid(mtry = seq(2,10,by=2),
                       splitrule = "gini",
                       min.node.size = c(1, 5, 10, 20) )


rf_model <- train(is_active ~ .,
                  data = train_df,
                  method = "ranger",  
                  trControl = train_Ctrl,
                  tuneGrid = my_grid,
                  importance = 'impurity'
                 )


print(rf_model$bestTune)
plot(rf_model)

p_active_rf <- predict(rf_model, train_df)
p_test_rf <- predict(rf_model,newdata = test_df)


confusionMatrix(p_active_rf, train_df$is_active, mode = "everything", positive = "No")

confusionMatrix(p_test_rf, test_df$is_active, mode = "everything", positive = "No")

varImp(rf_model)
plot(varImp(rf_model))

