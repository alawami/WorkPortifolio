
### load packages ----------------------------------------------------------------------------------------
suppressMessages(purrr::walk(c("stringr", "caret", "magrittr", "tidyverse", "randomForest"),
                             library, character.only = TRUE))

### read in data -----------------------------------------------------------------------------------------

suppressMessages(train <- read_csv("task_data.csv"))
train %<>% select(-1) %>% mutate(class_label = factor(class_label, labels = c("N", "P")))

# glimpse(train)
# summary(train)
# corrplot::corrplot(cor(train[-1]), type = "upper", method = "number")

myControl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE
)

### Fit random forest

rf_model <- train(class_label ~ ., 
                  data = train,
                  method = "rf",
                  trControl = myControl,
                  importanceSD = TRUE
) %>% suppressMessages()

### Fit glm model
# glm_model <- train(class_label ~ ., 
#                    data = train,
#                    method = "glmStepAIC",
#                    family = binomial,
#                    trControl = myControl
# )
# 
# glm_model <- train(class_label ~ .,
#                    data = train,
#                    method = "glm",
#                    family = binomial,
#                    trControl = myControl
# )

# print("Variable Importance (Logistic Regression):")
# varImp(glm_model$finalModel) %>% rownames_to_column("sensor") %>% arrange(desc(Overall))

### Print out
print("Variable Importance (Random Forest):")
print(varImp(rf_model$finalModel, scale = TRUE) %>% rownames_to_column("sensor") %>% arrange(desc(Overall)))
varImpPlot(rf_model$finalModel, scale = TRUE, main = "Variable Importance")


