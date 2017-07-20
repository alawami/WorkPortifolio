
### load packages ----------------------------------------------------------------------------------------
purrr::walk(c("stringr", "caret", "magrittr", "tidyverse"), library, character.only = TRUE)


### read in data -----------------------------------------------------------------------------------------

train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

glimpse(train)

# validation set
set.seed(1234)
validationIndex <- sample(nrow(train), round(nrow(train) * 0.30))
validate <- train[validationIndex, ]
train <- train[-validationIndex, ]

### Format data

train <- train %>% mutate(device_type = factor(device_type), 
                          gender = factor(ifelse(is.na(gender), "U", gender)),
                          in_initial_launch_location = factor(in_initial_launch_location),
                          n_vehicles = factor(n_vehicles),
                          outcome = factor(outcome))

validate <- validate %>% mutate(device_type = factor(device_type), 
                          gender = factor(ifelse(is.na(gender), "U", gender)),
                          in_initial_launch_location = factor(in_initial_launch_location),
                          n_vehicles = factor(n_vehicles),
                          outcome = factor(outcome))

test <- test %>% mutate(device_type = factor(device_type), 
                          gender = factor(ifelse(is.na(gender), "U", gender)),
                          in_initial_launch_location = factor(in_initial_launch_location),
                          n_vehicles = factor(n_vehicles)
                          )

summary(train)

### Data summary
set.seed(123)
dataSample <- train %>% sample_frac(0.6)

ggplot(train, aes(age, cost_of_ad, col = factor(prior_ins_tenure))) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(income, cost_of_ad, col = factor(gender))) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(n_drivers, n_vehicles, col = factor(gender))) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(age, n_vehicles, col = factor(gender))) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(device_type, cost_of_ad, col = age)) +
  geom_point(size = 0.5, position = position_jitter())

# Custome visualization would be neat here
ggplot(train, aes(device_type, cost_of_ad, col = outcome)) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(cost_of_ad, outcome, col = factor(gender))) +
  geom_point(size = 0.5, position = position_jitter())

ggplot(train, aes(prior_ins_tenure, outcome, col = factor(in_initial_launch_location))) +
  geom_point(size = 0.5, position = position_jitter())
