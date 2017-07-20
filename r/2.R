
# Benchmark: Base model p = 0.50

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  verboseIter = TRUE
)

# Fit glm model: model
model <- train(outcome ~ ., 
             data = train %>% mutate(outcome = as.character(outcome) %>% paste0("X", .) %>% factor()),
             method = "glmStepAIC",
             family = "binomial",
             trControl = myControl
             )

model <- train(outcome ~ ., 
               data = train %>% mutate(outcome = as.character(outcome) %>% paste0("X", .) %>% factor()),
               method = "ranger",
               trControl = myControl
)
# 0.8279598


# Predict on test: p
p <- predict(model, validate, type = "prob")

# Make ROC curve
library(caTools)
colAUC(p[, 2], validate$outcome, plotROC = TRUE)

library(ROCR)
pred <- ROCR::prediction(p[, 2], validate$outcome)
perf <- performance(pred, "auc")
perf@y.values[[1]]


# Fit logistic model
set.seed(123)
log_model <- train(
  outcome ~ .,
  data = train,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "AUC",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE
                           )
)



