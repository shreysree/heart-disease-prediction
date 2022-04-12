#load libraries
library(tidyverse)
library(kableExtra)
library(rsample)
library(recipes)
library(parsnip)
library(yardstick)
library(viridisLite)
library(GGally)


#load the dataset
heart_disease_dataset <- read.csv("processed_cleveland.csv")

#DATA CLEANING

#Prepare column names
names <- c("Age",
           "Sex",
           "Chest_Pain_Type",
           "Resting_Blood_Pressure",
           "Serum_Cholesterol",
           "Fasting_Blood_Sugar",
           "Resting_ECG",
           "Max_Heart_Rate_Achieved",
           "Exercise_Induced_Angina",
           "ST_Depression_Exercise",
           "Peak_Exercise_ST_Segment",
           "Num_Major_Vessels_Flouro",
           "Thalassemia",
           "Diagnosis_Heart_Disease")

#Apply column names to the dataframe
colnames(heart_disease_dataset) <- names

#Glimpse data
heart_disease_dataset %>% glimpse()


#Determine the number of values in each level of dependent variable
heart_disease_dataset %>% 
  drop_na() %>%
  group_by(Diagnosis_Heart_Disease) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)

#Identify the different levels of Thalassemia
heart_disease_dataset %>% 
  drop_na() %>%
  group_by(Thalassemia) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)

#Identify the different levels of Num_Major_Vessels_Flouro
heart_disease_dataset %>% 
  drop_na() %>%
  group_by(Num_Major_Vessels_Flouro) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)


#Drop NA's, convert to factors, lump target variable to 2 levels, remove "?", reorder variables
heart_dataset_clean_tbl <- heart_disease_dataset %>% 
  drop_na() %>%
  mutate_at(c("Resting_ECG", 
              "Fasting_Blood_Sugar", 
              "Sex", 
              "Diagnosis_Heart_Disease", 
              "Exercise_Induced_Angina",
              "Peak_Exercise_ST_Segment", 
              "Chest_Pain_Type"), as_factor) %>%
  filter(Num_Major_Vessels_Flouro!= "?") %>%
  mutate(Num_Major_Vessels_Flouro = as.numeric(Num_Major_Vessels_Flouro)) %>%
  mutate(Diagnosis_Heart_Disease = fct_lump(Diagnosis_Heart_Disease, other_level = "1")) %>% 
  filter(Thalassemia != "?") %>%
  select(Age, 
         Resting_Blood_Pressure, 
         Serum_Cholesterol, 
         Max_Heart_Rate_Achieved, 
         ST_Depression_Exercise,
         Num_Major_Vessels_Flouro,
         everything())


#Glimpse data
heart_dataset_clean_tbl %>%  glimpse()

#DATA VISUALIZATION

#plot diagnosis of heart disease using pie chart
yes=sum(heart_dataset_clean_tbl$Diagnosis_Heart_Disease==1)
no=sum(heart_dataset_clean_tbl$Diagnosis_Heart_Disease==0)
pie(c(yes,no),labels=c(paste("Heart Disease","\n",round((yes/297)*100,2),"%"),paste("No Heart Disease","\n",round((no/297)*100,2),"%")),col=c("#DC143C","#F08080"))
title(main="Diagnosis of Heart Disease")

#Select categorical vars,change them into text
hd_long_fact_tbl <- heart_dataset_clean_tbl  %>%
  select(Sex,
         Chest_Pain_Type,
         Fasting_Blood_Sugar,
         Resting_ECG,
         Exercise_Induced_Angina,
         Peak_Exercise_ST_Segment,
         Thalassemia,
         Diagnosis_Heart_Disease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female",`1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                         `2` = "atypical",
                                         `3` = "non-angina", 
                                         `4` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                             `1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",
                                     `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                 `1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                  `2` = "flat",
                                                  `3` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia, `3` = "normal",
                                     `6` = "fixed defect",
                                     `7` = "reversible defect")) %>%
gather(key = "key", value = "value", -Diagnosis_Heart_Disease)

#Visualize categorical variables with bar plot
hd_long_fact_tbl %>% 
  ggplot(aes(value)) +
  geom_bar(aes(x=value,fill= Diagnosis_Heart_Disease), 
           alpha    = 0.6, 
           position = "dodge", 
           color    = "black",
           width    = 0.8) +
  labs(x = "",y = "",title = "Scaled Effect of Categorical Variables") +
  theme(axis.text.y  = element_blank(),axis.ticks.y = element_blank()) +
  facet_wrap(~ key, scales = "free", nrow = 4) +
  scale_fill_manual(values = c("orchid", "#00FF80"),name="Heart\nDisease",labels = c("No HD", "Yes HD"))


#Visualizing numeric variables using box plots
hd_long_cont_tbl <- heart_dataset_clean_tbl  %>%
  select(Age,
         Resting_Blood_Pressure,
         Serum_Cholesterol,
         Max_Heart_Rate_Achieved,
         ST_Depression_Exercise,
         Diagnosis_Heart_Disease) %>% 
  gather(key="key",value="value",-Diagnosis_Heart_Disease)

hd_long_cont_tbl %>% 
  ggplot(aes(y = value)) +
  geom_boxplot(aes(fill = Diagnosis_Heart_Disease),alpha  = .6,fatten = .7) +
  labs(x = "",y = "",title = "Boxplots for Numeric Variables") +
  scale_fill_manual(values = c("#6A5ACD","#FFB6C1"),name="Heart\nDisease",labels = c("No HD", "Yes HD")) +
  theme(axis.text.x  = element_blank(),axis.ticks.x = element_blank()) +
  facet_wrap(~ key,scales = "free",ncol=2) 

#Correlation matrix using Pearson method
heart_dataset_clean_tbl%>% ggcorr( high       = "#20a486ff",
                                   low        = "#fde725ff",
                                   label      = TRUE, 
                                   hjust      = 0.75, 
                                   size       = 3, 
                                   label_size = 3,
                                   nbreaks    = 5) + 
  labs(title = "Correlation Matrix",subtitle = "Pearson Method Using Pairwise Obervations")


#LOGISTIC REGRESSION FOR PREDICTION

set.seed(1333)
#create split object 
train_test_split <- heart_dataset_clean_tbl %>% initial_split(prop = 0.8, strata = "Diagnosis_Heart_Disease")
train_tbl <- train_test_split %>% training()
test_tbl <- train_test_split %>% testing()


#Set up and train the model using processed training_data_obj
set.seed(100)
log_regr_hd_model <- logistic_reg(mode="classification") %>%
  set_engine("glm") %>% 
  fit(Diagnosis_Heart_Disease ~ ., data=train_tbl)

#Make predictions using testing set
first_training_prediction <- predict(log_regr_hd_model,new_data = test_tbl,type     = "class")
#Add predictions as new column in data set
first_training_prediction_full_tbl <- test_tbl %>% 
  mutate(Predicted_Heart_Disease = first_training_prediction$.pred_class)

#Glimpse data
first_training_prediction_full_tbl %>% glimpse()


#Use predictions col and truth col to make a confusion matrix object
conf_mat_obj <- first_training_prediction_full_tbl %>% 
  conf_mat(truth    = Diagnosis_Heart_Disease, 
           estimate = Predicted_Heart_Disease)
#Call conf_mat and supply columns for truth, prediction
#Pluck() to extract the conf_matrix data into cols and convert to tibble for plotting
conf_matrix_plt_obj <- first_training_prediction_full_tbl %>% 
  conf_mat(truth    = Diagnosis_Heart_Disease, 
           estimate = Predicted_Heart_Disease) %>%
  pluck(1) %>%
  as_tibble() %>%
  mutate("outcome" = c("true_negative",
                       "false_positive",
                       "false_negative",
                       "true_positive")) %>%
  mutate(Prediction = recode(Prediction, "0" = "No Heart Disease",
                             "1" = "Heart Disease")) %>%
  mutate(Truth = recode(Truth,  "0" = "No Heart Disease",
                        "1" = "Heart Disease"))
#Convert to kable format
conf_matrix_plt_obj %>% kable(align = rep("c", 4))%>% kable_styling("full_width" = F)

#Plot confusion matrix
p1 <- conf_matrix_plt_obj %>% ggplot(aes(x = Truth, y = Prediction)) +
  geom_tile(aes(fill = n), alpha = .8) +
  geom_text(aes(label = n), color = "white") +
  scale_fill_viridis_c() +
  theme(legend.title = element_blank()) +
  labs(
    title    = "Confusion Matrix",
    subtitle = "Heart Disease Prediction Using Logistic Regression"
  )
p1


#Calling summary() on the confusion_matrix_obj gives all the performance measures
#Filter to the ones we care about
log_reg_performance_tbl <- summary(conf_mat_obj) %>% filter(
  .metric == "accuracy" | 
    .metric == "sens" |
    .metric == "spec" |
    .metric == "ppv"  |
    .metric == "npv"  |
    .metric == "f_meas") %>%
  select(-.estimator) %>%
  rename("metric" = .metric, 
         "estimate" = .estimate) %>%
  mutate("estimate" = estimate %>% signif(digits = 3)) %>%
  mutate(metric = recode(metric, "sens" = "sensitivity"),
         metric = recode(metric, "spec" = "specificity"),
         metric = recode(metric, "ppv"  = "positive predictive value"),
         metric = recode(metric, "npv"  = "negative predictive value")) %>%
  kable(align = rep("c", 3))%>% kable_styling("full_width" = F)

#Display performance summary as kable
log_reg_performance_tbl 
