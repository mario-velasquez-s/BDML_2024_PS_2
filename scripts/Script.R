# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 2
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr,
       dplyr,
       ggplot2,
       visdat,
       caret,
       xtable,  # For predictive model assessment
       fixest, 
       zoo,
       smotefamily,
       ROSE,
       leaps,
       fastDummies,
       doParallel,
       rpart,
       rpart.plot,
       Metrics,
       ranger,
       MLmetrics,
       glmnet
       )

# 1: Initial Data Manipulation -----------------------------------------------


username <- Sys.info()["user"]
if (username == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 2")
} else if (username == "marti") {
  setwd("C:/Users/marti/OneDrive - Universidad de los andes/BDML - Datos")
} else if (username == "mario") {
  setwd("C:/Users/mario/Desktop/TODO/UNI ANDES/SEM 8 (2024-1)/Big Data y Machine Learning/Taller 2 - R")
} else {
  setwd("/Users/danielavlasak/Library/CloudStorage/OneDrive-UniversidaddelosAndes/ANDES/Semestre 8/BDML/Datos_PS2")
}

train_hogares <- read.csv("data/train_hogares.csv")
train_personas <- read.csv("data/train_personas.csv")
test_hogares <- read.csv("data/test_hogares.csv")
test_personas <- read.csv("data/test_personas.csv")

str(train_hogares)
str(train_personas)

## I choose the individual-level variables that could be helpful

pre_process_personas<-  function(data, ...) {
  
  data <- data %>% mutate(
    mujer = ifelse(P6020==2,1,0), 
    H_Head = ifelse(P6050== 1, 1, 0), #Household head
    menor = ifelse(P6040<=11,1,0), # Menores
    EducLevel = ifelse(P6210==9,0,P6210), #Replace 9 with 0
    ocupado = ifelse(is.na(Oc),0,1),
    afiliadoSalud = ifelse(P6090 == 9, NA, ifelse(P6090==2,0,P6090)),
    edad = P6040,
    ciudad = as.factor(Dominio),
    edad_trabajar = case_when(
      P6040 >= 12 & Clase == 1 ~ 1,  #Cuando tiene 12 o más años y vive en cabecer
      P6040 >= 10 & Clase == 2 ~ 1,  # Cuando tiene 10 o más años y vive en zona rural
      TRUE ~ 0                        # Otherwise
    )
  ) %>% 
    dplyr::select(id, Orden,mujer,H_Head,menor,EducLevel,ocupado, afiliadoSalud, edad, ciudad, edad_trabajar)
  
  
}

train_personas <- pre_process_personas(train_personas)
test_personas <- pre_process_personas(test_personas)


## We detect missing values

train_miss <- skim(train_personas)
print(train_miss) ## Missing values in EducLevel and afiliadoSalud

## Imputation of afiliadoSalud

train_personas_AfiSaludmiss <- train_personas %>% 
  mutate(AfiSalud_miss = ifelse(is.na(afiliadoSalud) == TRUE,1,0)) %>% 
  dplyr::filter(AfiSalud_miss == 1)

afiliado_missing <- ggplot(train_personas_AfiSaludmiss, aes(x=edad))+
  geom_histogram(fill="#0099F8") +
  labs(x="Edad", y = "Cuenta") + 
  theme_bw()

##ggsave("./views/afiliado_missing.pdf", plot = afiliado_missing)

summary(train_personas_AfiSaludmiss$edad) ## Conclusion: Those missing in afiliacionSalud are children and someone with 84 years. 
## I can impute them with the values of their parents. I assume that if someone in the household has health security, the
## child must have.

train_personas$afiliadoSalud <- ave(train_personas$afiliadoSalud, 
                                    train_personas$id, FUN = function(x) {
                                      if (all(is.na(x))) {
                                        return(0)  # Asigna 0 si todos los valores son NA
                                      } else {
                                        return(ifelse(is.na(x), max(x, na.rm = TRUE), x))  # Calcula el máximo si hay valores no NA
                                      }
                                    })

test_personas$afiliadoSalud <- ave(test_personas$afiliadoSalud, 
                                    test_personas$id, FUN = function(x) {
                                      if (all(is.na(x))) {
                                        return(0)  # Asigna 0 si todos los valores son NA
                                      } else {
                                        return(ifelse(is.na(x), max(x, na.rm = TRUE), x))  # Calcula el máximo si hay valores no NA
                                      }
                                    })


## Imputation of EducLevel

train_personas_Edumiss <- train_personas %>% 
  mutate(Edu_miss = ifelse(is.na(EducLevel) == TRUE,1,0)) %>% 
  dplyr::filter(Edu_miss == 1)

ggplot(train_personas_Edumiss, aes(x=edad))+
  geom_histogram(fill="#0099F8") +
  labs(x="Edad") + 
  theme_bw()

summary(train_personas_Edumiss$edad) ## Conclusion: Those missing in EducLevel are children. 
##  This makes sense because below 2 years children haven't gone to school. I can impute them with EducLevel=0.
impute_Educlevel <- function(data) {
  data <- data %>% mutate(EducLevel = ifelse(is.na(EducLevel) ==TRUE,0,EducLevel))
}

train_personas <- impute_Educlevel(train_personas)
test_personas <- impute_Educlevel(test_personas)

## Creation of variables at individual level

train_personas_nivel_hogar<- train_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE),
            edad_trabajar = sum(edad_trabajar, na.rm = TRUE)
  )

train_personas_hogar<- train_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud, edad) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado,
         H_Head_afiliadoSalud = afiliadoSalud,
         H_Head_edad = edad) %>% 
  left_join(train_personas_nivel_hogar)

test_personas_nivel_hogar<- test_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE),
            edad_trabajar = sum(edad_trabajar, na.rm = TRUE)
  )

test_personas_hogar<- test_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud, edad) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado,
         H_Head_afiliadoSalud = afiliadoSalud,
         H_Head_edad = edad) %>% 
  left_join(test_personas_nivel_hogar)


## Household variables
train_hogares<- train_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0),
         propia_pagada =ifelse(P5090==1,1,0),
         propia_enpago =ifelse(P5090==2,1,0),
         en_usufructo = ifelse(P5090==4,1,0),
         sin_titulo = ifelse(P5090==5,1,0),
         num_cuartos = P5000,
         cuartos_usados = P5010,
         total_personas = Nper
         ) %>% 
  dplyr::select(id,Dominio,arrienda,Pobre, Ingtotug, Ingtotugarr, Ingpcug, propia_pagada,propia_enpago,en_usufructo, sin_titulo, num_cuartos,cuartos_usados,total_personas)


test_hogares<- test_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0),
          propia_pagada =ifelse(P5090==1,1,0),
          propia_enpago =ifelse(P5090==2,1,0),
          en_usufructo = ifelse(P5090==4,1,0),
          sin_titulo = ifelse(P5090==5,1,0),
          num_cuartos = P5000,
          cuartos_usados = P5010,
         total_personas = Nper
      ) %>% 
  dplyr::select(id,Dominio,arrienda,propia_pagada,propia_enpago, en_usufructo, sin_titulo, num_cuartos,cuartos_usados,total_personas) 

## Finally, Train & Test data
train<- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  dplyr::select(-id) #no longer need id

test<- test_hogares %>% 
  left_join(test_personas_hogar)

train<- train %>% 
  mutate(Dominio=factor(Dominio),
         Pobre = factor(Pobre, levels = c(0, 1),labels=c("No","Yes")),
         arrienda=factor(arrienda,levels=c(0,1),labels=c("No","Yes")),
         propia_pagada = factor(propia_pagada, levels = c(0, 1),labels=c("No","Yes")),
         propia_enpago = factor(propia_enpago, levels = c(0, 1),labels=c("No","Yes")),
         en_usufructo = factor(en_usufructo, levels = c(0, 1),labels=c("No","Yes")),
         sin_titulo = factor(sin_titulo, levels = c(0, 1),labels=c("No","Yes")),
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes")),
         Dominio = factor(Dominio)
  )

test<- test %>% 
  mutate(Dominio=factor(Dominio),
         arrienda=factor(arrienda,levels=c(0,1),labels=c("No","Yes")),
         propia_pagada = factor(propia_pagada, levels = c(0, 1),labels=c("No","Yes")),
         propia_enpago = factor(propia_enpago, levels = c(0, 1),labels=c("No","Yes")),
         en_usufructo = factor(en_usufructo, levels = c(0, 1),labels=c("No","Yes")),
         sin_titulo = factor(sin_titulo, levels = c(0, 1),labels=c("No","Yes")),
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes")),
         Dominio = factor(Dominio)
  )

## Variable creation

train <- train %>%
  mutate(
    perc_mujer = (nmujeres / total_personas) * 100,
    perc_edad_trabajar = (edad_trabajar / total_personas) * 100,
    perc_ocupados = (nocupados / total_personas) * 100,
    perc_menores = (nmenores/total_personas) *100,
    perc_uso_cuartos = (cuartos_usados/num_cuartos) *100
  )

test <- test %>%
  mutate(
    perc_mujer = (nmujeres / total_personas) * 100,
    perc_edad_trabajar = (edad_trabajar / total_personas) * 100,
    perc_ocupados = (nocupados / total_personas) * 100,
    perc_menores = (nmenores/total_personas) *100,
    perc_uso_cuartos = (cuartos_usados/num_cuartos) *100
  )

##------------------------------------------------------------------------------
## SMOTE


smote_subset  <- train
smote_subset <- smote_subset %>%
  mutate(
    Pobre = as.integer(train$Pobre == "Yes"),
    Dominio = as.integer(train$Dominio),
    arrienda = as.integer(train$arrienda == "Yes"),
    propia_pagada = as.integer(train$propia_pagada == "Yes"),
    propia_enpago = as.integer(train$propia_enpago == "Yes"),
    en_usufructo = as.integer(train$en_usufructo == "Yes"),
    sin_titulo = as.integer(train$sin_titulo == "Yes"),
    H_Head_mujer = as.integer(train$H_Head_mujer == "Yes"),
    H_Head_ocupado = as.integer(train$H_Head_ocupado == "Yes"),
    H_Head_afiliadoSalud = as.integer(train$H_Head_afiliadoSalud == "Yes")
  )
smote_subset_clean <- smote_subset %>%
  select_if(is.numeric)

columns_to_exclude <- c("Pobre", "Ingtotug", "Ingtotugarr", "Ingpcug")
predictors <- setdiff(colnames(smote_subset_clean), columns_to_exclude)
#predictors <- colnames(smote_subset_clean)[-which(colnames(smote_subset_clean) == "Pobre")]
head( smote_subset_clean[predictors])
smote_output <- SMOTE(X = smote_subset_clean[predictors],
                      target = smote_subset_clean$Pobre)
smote_data_train <- smote_output$data
skim(smote_data_train)
prop.table(table(train$Pobre))
prop.table(table(smote_data_train$class))

smote_data_train<- smote_data_train %>% 
  mutate(Dominio=factor(Dominio),
        arrienda=factor(arrienda),
         propia_pagada = factor(propia_pagada),
         propia_enpago = factor(propia_enpago),
         en_usufructo = factor(en_usufructo),
         sin_titulo = factor(sin_titulo),
         H_Head_mujer = factor(H_Head_mujer),
         H_Head_ocupado = factor(H_Head_ocupado),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud)
  )
skim(smote_data_train)

smote_data_train <- smote_data_train %>% rename(Pobre = class)
smote_data_train <- smote_data_train %>%
  mutate(Pobre = factor(smote_data_train$Pobre,levels = c(0, 1),labels=c("No","Yes")))
#smote_data_train$class <- smote_data_train %>% mutate(Pobre = ifelse(class == "X1", 0,1))

## ROSE

rose_train <- ROSE(Pobre ~ ., data  = train)$data 

## UPSAMPLING

upSampledTrain <- upSample(x = train,
                           y = train$Pobre,
                           ## keep the class variable name the same:
                           yname = "Pobre")

## DOWNSAMPLING

downSampledTrain <- downSample(x = train,
                               y = train$Pobre,
                               ## keep the class variable name the same:
                               yname = "Pobre")

##Remove all dfs

objects <- ls()

objects_to_remove <- setdiff(objects, c("train", "test", "downSampledTrain", "upSampledTrain", "rose_train", "smote_data_train"))
rm(list = objects_to_remove)


#2 Best Subset Selection Pobre ---------------------------------------------

### Backward subset selection
train_pobre_numeric <- dplyr::select(train, -Ingpcug, -Ingtotug, -Ingtotugarr)
train_pobre_numeric <- train_pobre_numeric %>%
  mutate(
    Pobre = as.integer(train$Pobre == "Yes"))

model_form <- Pobre ~ . + (cuartos_usados + H_Head_mujer + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad + nmujeres + noafiliados + perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos)^2 + (cuartos_usados + H_Head_mujer + H_Head_ocupado + H_Head_afiliadoSalud + 
                                                                                                                                                                                                                                    H_Head_edad + nmujeres + noafiliados + perc_mujer + perc_edad_trabajar + 
                                                                                                                                                                                                                                    perc_ocupados + perc_menores + perc_uso_cuartos)^3


backward_model <- regsubsets(model_form, ## formula
                             data = train_pobre_numeric, ## data frame Note we are using the training sample
                             method = "backward" )  ## apply Forward Stepwise Selection

max_nvars= backward_model[["np"]]-1  ## minus one because it counts the intercept.
max_nvars

predict.regsubsets<- function (object , newdata , id, ...) {
  form<- model_form
  mat <- model.matrix(form , newdata) ## model matrix in the test data
  coefi <- coef(object , id = id) ## coefficient for the best model with id vars
  xvars <- names (coefi)  ## variables in the model
  mat[, xvars] %*% coefi  ## prediction 
  
}

k <- 10
n <- nrow (train_pobre_numeric)
folds <- sample (rep (1:k, length = n))

calculateF1Score <- function(actual, predicted) {
  tp <- sum(predicted == 1 & actual == 1)  # True Positives
  fp <- sum(predicted == 1 & actual == 0)  # False Positives
  fn <- sum(predicted == 0 & actual == 1)  # False Negatives
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  f1Score <- 2 * ((precision * recall) / (precision + recall))
  return(f1Score)
}

cv.f1Scores_back <- matrix(NA, k, max_nvars, dimnames = list(NULL, paste(1:max_nvars)))

for (j in 1:k) {
  best_fit <- regsubsets(model_form, data = train_pobre_numeric[folds != 1, ], nvmax = max_nvars, method = "backward")
  
  for (i in 1:max_nvars) {
    # Assuming your outcome is binary, and you're making binary predictions
    # You may need to adjust the threshold or method of generating predictions based on your specific context
    predicted_probs <- predict(best_fit, train_pobre_numeric[folds == j, ], id = i)
    predicted_classes <- ifelse(predicted_probs > 0.3, 1, 0) # Adjust threshold as necessary
    
    f1Score <- calculateF1Score(train_pobre_numeric$Pobre[folds == j], predicted_classes)
    cv.f1Scores_back[j, i] <- f1Score
  }
}

mean.f1Scores_back <- apply(cv.f1Scores_back, 2, mean)
maxF1ModelIndex_back <- which.max(mean.f1Scores_back)
plot(mean.f1Scores_back, type = "b", main = "Mean F1 Score Backward", xlab = "Number of Variables", ylab = "Mean F1 Score")
maxF1ModelIndex_back

### Forward subset selection

cv.f1Scores_for <- matrix(NA, k, max_nvars, dimnames = list(NULL, paste(1:max_nvars)))

for (j in 1:k) {
  best_fit <- regsubsets(model_form, data = train_pobre_numeric[folds != 1, ], nvmax = max_nvars, method = "forward")
  
  for (i in 1:max_nvars) {
    # Assuming your outcome is binary, and you're making binary predictions
    # You may need to adjust the threshold or method of generating predictions based on your specific context
    predicted_probs <- predict(best_fit, train_pobre_numeric[folds == j, ], id = i)
    predicted_classes <- ifelse(predicted_probs > 0.3, 1, 0) # Adjust threshold as necessary
    
    f1Score <- calculateF1Score(train_pobre_numeric$Pobre[folds == j], predicted_classes)
    cv.f1Scores_for[j, i] <- f1Score
  }
}

mean.f1Scores_for <- apply(cv.f1Scores_for, 2, mean)
maxF1ModelIndex_for <- which.max(mean.f1Scores_for)
plot(mean.f1Scores_for, type = "b", main = "Mean F1 Score Forward", xlab = "Number of Variables", ylab = "Mean F1 Score")
maxF1ModelIndex_for

#3 Best Subset Selection Ingreso -------------------------------------------

#Backward
train_ing <- dplyr::select(train, -Pobre, -Ingtotug, -Ingtotugarr)
model_form_ing <- Ingpcug ~ . + (cuartos_usados + H_Head_mujer + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad + nmujeres + noafiliados + perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos)^2


backward_model_ing <- regsubsets(model_form_ing, ## formula
                             data = train_ing, ## data frame Note we are using the training sample.
                             nvmax = 118, ## show only the first 3  models models
                             method = "backward" )  ## apply Forward Stepwise Selection

max_nvars_ing= backward_model[["np"]]-1  ## minus one because it counts the intercept.
max_nvars_ing

k_ing <- 10
n_ing <- nrow (train_ing)
folds <- sample (rep (1:k, length = n_ing))

cv.Scores_back_ing <- matrix(NA, k_ing, max_nvars_ing, dimnames = list(NULL, paste(1:max_nvars_ing)))

predict.regsubsets<- function (object , newdata , id, ...) {
  form<- model_form_ing
  mat <- model.matrix(form , newdata) ## model matrix in the test data
  coefi <- coef(object , id = id) ## coefficient for the best model with id vars
  xvars <- names (coefi)  ## variables in the model
  mat[, xvars] %*% coefi  ## prediction 
  
}

for (j in 1:k) {
  best_fit <- regsubsets(model_form_ing,
                         data = train_ing[folds != j, ],
                         nvmax = max_nvars_ing, 
                         method = "backward") 
  for (i in 1:max_nvars) {
    pred <- predict(best_fit , train_ing[folds == j, ], id = i)
    cv.Scores_back_ing[j, i] <-
      mean ((train_ing$Ingpcug[folds == j] - pred)^2)
  }
}

mean.cv.Scores_back_ing <- apply (cv.Scores_back_ing , 2, mean)
mean.cv.Scores_back_ing
which.min (mean.cv.Scores_back_ing)
plot (mean.cv.Scores_back_ing , type = "b")

#Forward

cv.Scores_for_ing <- matrix(NA, k_ing, max_nvars_ing, dimnames = list(NULL, paste(1:max_nvars_ing)))

for (j in 1:k) {
  best_fit <- regsubsets(model_form_ing,
                         data = train_ing[folds != j, ],
                         nvmax = max_nvars_ing, 
                         method = "backward") 
  for (i in 1:max_nvars) {
    pred <- predict(best_fit , train_ing[folds == j, ], id = i)
    cv.Scores_for_ing[j, i] <-
      mean ((train_ing$Ingpcug[folds == j] - pred)^2)
  }
}

mean.cv.Scores_for_ing <- apply (cv.Scores_for_ing , 2, mean)
mean.cv.Scores_for_ing
which.min (mean.cv.Scores_for_ing)
plot (mean.cv.Scores_for_ing , type = "b")

#4: CLASSIFICATION APPROACH ----------------------------------------------------

## 4.1: Linear Regression----------
set.seed(685397)

colnames(train)

## I will use k-fold validation to test my training models
k <- 5
nrow(train)/k ## Each fold must have 32992 obs
nrow(smote_data_train)/k ##3 folds must have 36539 obs and two 36538

## Generate an index for each fold
train <-train  %>% mutate(fold=c(rep(1,32992),
                              rep(2,32992),
                              rep(3,32992),
                              rep(4,32992),
                              rep(5,32992)))

smote_data_train <-smote_data_train  %>% mutate(fold=c(rep(1,36539),
                                 rep(2,36539),
                                 rep(3,36539),
                                 rep(4,36538),
                                 rep(5,36538)))

## Models
    
    mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
      H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados
    
    mod2 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores + H_Head_Educ_level
    
    mod3 <- Pobre ~ Dominio* H_Head_mujer + arrienda + propia_pagada + propia_enpago + en_usufructo + sin_titulo*H_Head_mujer +
      num_cuartos + cuartos_usados + total_personas + H_Head_Educ_level* H_Head_mujer + H_Head_ocupado* H_Head_mujer + 
      H_Head_afiliadoSalud + H_Head_edad + nmujeres + nmenores* H_Head_mujer + nocupados + noafiliados + edad_trabajar +
      perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos
    
    mod4 <- Pobre ~ Dominio + arrienda + propia_pagada + propia_enpago + en_usufructo + sin_titulo +
      num_cuartos + cuartos_usados + total_personas + H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + 
      H_Head_afiliadoSalud + H_Head_edad + nmujeres + nmenores + nocupados + noafiliados + edad_trabajar +
      perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos
      
    mod5 <- Pobre ~ arrienda + propia_pagada + propia_enpago + en_usufructo + sin_titulo*H_Head_mujer +
      num_cuartos + cuartos_usados + total_personas +  H_Head_ocupado* H_Head_mujer + 
      H_Head_afiliadoSalud + H_Head_edad + nmujeres + nmenores* H_Head_mujer + nocupados + noafiliados + edad_trabajar +
      perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos

cv_mse_f1 <- function(base,fold_size,modelo,c,...){
  l <- fold_size
  
  db_train <- list()
  db_test <- list()
  
  for(i in 1:l){
    db_train[[i]] <- base %>% filter(fold!=i) 
    db_test[[i]] <- base %>% filter(fold==i)
    
    fit <-lm(modelo, data= db_train[[i]])
    db_test[[i]] <- db_test[[i]] %>% mutate(Pobre_hat = ifelse(predict(fit, newdata=db_test[[i]]) >= c, 1, 0))
  }
  
  MSE <- list()
  for(i in 1:l){
    MSE[[i]] <- mean((db_test[[i]]$Pobre - db_test[[i]]$Pobre_hat)^2, na.rm=TRUE)
  }
  MSE<-do.call(rbind,MSE)
  #print(paste("MSE:",round(mean(MSE, na.rm = TRUE), digits=3)))
  
  
  F1 <- list()
  for(i in 1:l){
    TP <- sum(db_test[[i]]$Pobre == 1 & db_test[[i]]$Pobre_hat == 1 )
    TN <- sum(db_test[[i]]$Pobre == 0 & db_test[[i]]$Pobre_hat == 0 )
    FP <- sum(db_test[[i]]$Pobre == 0 & db_test[[i]]$Pobre_hat == 1 )
    FN <- sum(db_test[[i]]$Pobre == 1 & db_test[[i]]$Pobre_hat == 0 )
    
    recall <- TP / (FN + TP)
    precision <- TP / (TP + FP)
    
    F1[[i]] <- 2 * precision * recall / (precision + recall)
  }
  F1<-do.call(rbind,F1)
  #print(paste("F1:",round(mean(F1, na.rm = TRUE), digits=3)))
 
  return(mean(F1, na.rm = TRUE))
      
}
  

## I choose models minimizing MSE
cv_mse_f1(train,k,mod5,0.5)


## Choosing the best thresholds



best_thresh_cv<- function(base,nfolds,model,...){
  thresholds <- seq(0.3, 0.4, by = 0.01)
  f1_scores <- numeric(length(thresholds))
  max_f1 <- 0
  best_threshold <- 0
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    # Store the F1 score for this threshold
    f1_scores[i] <- cv_mse_f1(base,nfolds,model,threshold)
    
    # Update max_f1 and best_threshold if current F1 score is higher
    if (f1_scores[i] > max_f1) {
      max_f1 <- f1_scores[i]
      best_threshold <- threshold
    }
  }
  
  # Create a data frame with threshold and F1 score data
  threshold_f1_data <- data.frame(threshold = thresholds, f1_score = f1_scores)
  
  # Plot the relationship between threshold and F1 score
  graph_thresh_f1 <- ggplot(threshold_f1_data, aes(x = threshold, y = f1_score)) +
    geom_line() +
    geom_point() +
    labs(x = "Threshold", y = "F1 Score", title = "F1 Score vs. Threshold")
  
  print(paste("Best Threshold:",best_threshold))
  print(paste("F1:",max_f1))
  graph_thresh_f1
}

best_thresh_cv(train,k,mod3)


## Precicting and generating prediction file
    predictSample <- test %>%
      mutate(pobre_lab = ifelse(predict(lm(mod3, train), newdata=test) >= 0.33, 1, 0)) %>%
      dplyr::select(id,pobre_lab)
    
    head(predictSample)
    write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)




## 4.2: ElasticNet-------

  # Installing needed packages

# Set seed for reproducibility
set.seed(21032024)

# Check and install glmnet package if not already installed
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}
library(glmnet)

# Installing Packages 
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load required libraries
library(dplyr)
library(ggplot2)
library(caret)

# Sample rows for the training set
train_index <- sample(1:nrow(train), 0.7 * nrow(train))

train_net$Pobre <- factor(train_net$Pobre, levels = c(0, 1))

# Create the training and test datasets
train_net <- train[train_index, ]
test_net <- train[-train_index, ]

train_net$Pobre <- factor(train_net$Pobre, levels = c(0, 1))
test_net$Pobre <- factor(test_net$Pobre, levels = c(0, 1))

# Select relevant variables (if selected_variables is defined)
# train_net <- train_net[, selected_variables, drop = FALSE]
# test_net <- test_net[, selected_variables, drop = FALSE]

selected_variables <- c("Pobre", "total_personas", "nmujeres", "nmenores", "nocupados", 
                        "noafiliados", "edad_trabajar", "perc_mujer", "perc_ocupados", 
                        "perc_edad_trabajar", "arrienda", "H_Head_mujer", "H_Head_Educ_level",
                        "H_Head_ocupado", "H_Head_afiliadoSalud", "rural", "H_Head_edad",
                        "maxEducLevel")

train_net <- train_net[, selected_variables, drop = FALSE]
test_net <- test_net[, selected_variables, drop = FALSE]

for (variable in names(train_net)) {
  if (is.factor(test_net[[variable]])) {
    test_net[[variable]] <- as.integer(test_net[[variable]])
    train_net[[variable]] <- as.integer(train_net[[variable]])
  }
}

# X and Y datasets 
x_train <- as.matrix(train_net[, -ncol(train_net)])  
y_train <- as.numeric(train_net$Pobre)

# Model Building : Elastic Net Regression 
control <- trainControl(method = "repeatedcv", 
                        number = 5, 
                        repeats = 5, 
                        search = "random", 
                        verboseIter = TRUE) 

# Training Elastic Net Regression model 
elastic_model <- train(Pobre ~ .,  
                       data = cbind(x_train, y_train), 
                       method = "glmnet", 
                       preProcess = c("center", "scale"), 
                       tuneLength = 25, 
                       trControl = control) 

# Model Prediction 
x_hat_pre <- predict(elastic_model, y_train) 

# Multiple R-squared 
rsq <- cor(x_train, x_hat_pre)^2 

# Plot 
plot(elastic_model, main = "Elastic Net Regression") 


















#############################################
  
  set.seed(21032024)
  
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    install.packages("glmnet")
  }
  library(glmnet)

# Función de Elastic Net

  elastic_net_f1 <- function(train, selected_variables, corte, lambda, alpha) {
    
    # Sample rows for the training set
    train_index <- sample(1:nrow(train), 0.7 * nrow(train))
    
    # Create the training and test datasets
    train_net <- train[train_index, ]
    test_net <- train[-train_index, ]
     
    train_net <- train_net[, selected_variables, drop = FALSE]
    test_net <- test_net[, selected_variables, drop = FALSE]
    
    for (variable in names(train_net)) {
      if (is.factor(test_net[[variable]])) {
        test_net[[variable]] <- as.integer(test_net[[variable]])
        train_net[[variable]] <- as.integer(train_net[[variable]])
      }
    }
    
    # Fiting the Elastic Net model
    x_train <- as.matrix(train_net[, -ncol(train_net)])  
    y_train <- as.numeric(train_net$Pobre)
    
    enet_model <- glmnet(x_train, y_train, alpha = alpha, lambda = lambda, family = "binomial")
    
    # Predict on the testing dataset
    age_col_index <- which(names(test_net) == "Pobre")
    x_test <- as.matrix(test_net[, -age_col_index])
    probabilities <- predict(enet_model, newx = x_test, type = "response")
    
    # Assuming a threshold of 0.9 for classification
    predicted_classes <- ifelse(probabilities > corte, 1, 0)
    
    # Confusion matrix
    actual_classes <- as.numeric(test_net$Pobre)
    
    precision <- sum(predicted_classes == 1 & actual_classes == 1) / sum(predicted_classes == 1)
    recall <- sum(predicted_classes == 1 & actual_classes == 1) / sum(actual_classes == 1)
    
    
    if (!is.na(precision) && !is.na(recall) && precision != 0 && recall != 0) {
      F1_score <- 2 * (precision * recall) / (precision + recall)
    } else {
      F1_score <- 0
    }
    
    #print(paste("F1 Score:", F1_score))
    return(F1_score)
  }
  
  selected_variables <- c("Pobre", "total_personas", "nmujeres", "nmenores", "nocupados", 
                          "noafiliados", "edad_trabajar", "perc_mujer", "perc_ocupados", 
                          "perc_edad_trabajar", "arrienda", "H_Head_mujer", "H_Head_Educ_level",
                          "H_Head_ocupado", "H_Head_afiliadoSalud", "rural", "H_Head_edad",
                          "maxEducLevel")
  
  # Probamos la función
  elastic_net_f1(train, selected_variables, 0.9, 0.5, 0.1)
  
  # Creamos una función que itere sobre valores de lambda, alpha y el punto de corte
  
  elastic_net_iter <- function(train, selected_variables, n){
    alphas <- seq(0, 1, by = 0.025)
    cortes <- seq(0, 1, by = 0.025)
    lambdas <- seq(0, 5, by = 0.1)
    f1_max = 0
    alpha_opt <- 0
    lambda_opt <- 0
    corte_opt <- 0
    f1_scores <- numeric(length(alphas)*length(lambdas)*length(cortes))
    for (i in seq_along(alphas)){
      for (j in seq_along(lambdas)){
        for (k in seq_along(cortes)){
          print(paste("i: ",i," - j: ",j," - k: ",k))
          alpha <- alphas[i]
          lambda <- lambdas[j]
          corte <- cortes[k]
          F1_value <- 0
          for(m in 1:n) {
            F1_value <- F1_value + elastic_net_f1(train, selected_variables, corte, lambda, alpha)
          }
          F1_value <- (F1_value/n)
          if (F1_value > f1_max) {
            f1_max <- F1_value
            alpha_opt <- alpha
            lambda_opt <- lambda
            corte_opt <- corte
          }
          print(paste("F1 max: ", f1_max))
        }
      }
    }
    print("Optimal Values:")
    print(paste("F1: ", f1_max))
    print(paste("Alpha: ", alpha_opt))
    print(paste("Lambda: ", lambda_opt))
    print(paste("Corte: ", corte_opt))
  }
  
  elastic_net_iter(train, selected_variables, 3)
  
  #Result: F1: 0.4099015 - alpha: 0.025 - lambda: 0.9 - corte: 0.825
  
  elastic_net_f1(train, selected_variables, 0.825, 0.9, 0.025)
  
  elastic_net_iter_2 <- function(train, selected_variables, corte, lambda, alpha, n){
    alphas <- seq(alpha-0.02, alpha+0.02, by = 0.0025)
    cortes <- seq(corte-0.02, corte+0.02, by = 0.0025)
    lambdas <- seq(lambda-0.05, lambda+0.05, by = 0.01)
    f1_max = 0
    alpha_opt <- 0
    lambda_opt <- 0
    corte_opt <- 0
    f1_scores <- numeric(length(alphas)*length(lambdas)*length(cortes))
    for (i in seq_along(alphas)){
      for (j in seq_along(lambdas)){
        for (k in seq_along(cortes)){
          print(paste("i: ",i," - j: ",j," - k: ",k))
          alpha <- alphas[i]
          lambda <- lambdas[j]
          corte <- cortes[k]
          F1_value <- 0
          for(m in 1:n) {
            F1_value <- F1_value + elastic_net_f1(train, selected_variables, corte, lambda, alpha)
          }
          F1_value <- (F1_value/n)
          if (F1_value > f1_max) {
            f1_max <- F1_value
            alpha_opt <- alpha
            lambda_opt <- lambda
            corte_opt <- corte
          }
          print(paste("F1 max: ", f1_max))
        }
      }
    }
    print("Optimal Values:")
    print(paste("F1: ", f1_max))
    print(paste("Alpha: ", alpha_opt))
    print(paste("Lambda: ", lambda_opt))
    print(paste("Corte: ", corte_opt))
  }

  
  elastic_net_iter_2(train, selected_variables, 0.825, 0.9, 0.025, 5)
  
## 4.3: CART - Logit-------

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)

calculate_f1_and_plot <- function(model, data, class_variable = "Yes") {
  predicted_probabilities <- predict(model, newdata = data, type = "prob")[, class_variable]
  thresholds <- seq(0, 1, by = 0.001)
  f1_scores <- numeric(length(thresholds))
  max_f1 <- 0
  best_threshold <- 0
  
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    # Convert probabilities to binary predictions based on the threshold
    binary_predictions <- ifelse(predicted_probabilities > threshold, class_variable, "No")
    
    # Compute confusion matrix
    if (class_variable == "Yes") {
      confusion <- table(binary_predictions, data$Pobre)
    } else {
      confusion <- table(binary_predictions, data$class)
    }
    
    # Check if confusion matrix is 2x2
    if (ncol(confusion) != 2 || nrow(confusion) != 2) {
      next  # Skip to the next threshold if the confusion matrix is not 2x2
    }
    
    # Calculate precision, recall, and F1 score
    precision <- confusion[2, 2] / sum(confusion[, 2])
    recall <- confusion[2, 2] / sum(confusion[2, ])
    f1_score <- 2 * precision * recall / (precision + recall)
    
    # Store the F1 score for this threshold
    f1_scores[i] <- f1_score
    
    # Update max_f1 and best_threshold if current F1 score is higher
    if (f1_score > max_f1) {
      max_f1 <- f1_score
      best_threshold <- threshold
    }
  }
  
  # Print the best threshold and corresponding max F1 score
  cat("Best Threshold:", best_threshold, "\n")
  cat("Max F1 Score:", max_f1, "\n")
  
  # Create a data frame with threshold and F1 score data
  threshold_f1_data <- data.frame(threshold = thresholds, f1_score = f1_scores)
  
  # Plot the relationship between threshold and F1 score
  ggplot(threshold_f1_data, aes(x = threshold, y = f1_score)) +
    geom_line() +
    geom_point() +
    labs(x = "Threshold", y = "F1 Score", title = "F1 Score vs. Threshold")
}



### Model List

#Uploaded
mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
  H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados

#Uploaded
mod2 <- Pobre ~ Dominio * H_Head_Educ_level +
  arrienda * rural +
  H_Head_ocupado * H_Head_afiliadoSalud +
  total_personas * nmujeres +
  nmenores * perc_ocupados

#Uploaded
mod3 <- Pobre ~ Dominio + arrienda + H_Head_mujer + 
  H_Head_Educ_level*H_Head_mujer + H_Head_ocupado + 
  H_Head_afiliadoSalud + rural*H_Head_mujer + total_personas + 
  nmujeres + nmenores + poly(perc_mujer, 2) + poly(perc_ocupados, 2) + 
  poly(perc_mujer, 2) + poly(perc_menores, 2) + poly(H_Head_edad, 2)*nmenores

### Calculating the best models

#Model 1. F1 is 0.5631557. Threshold is 0.29
glm_1 <- train(
  formula(mod1),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

#Model 2. F1 is  Threshold is 
glm_2 <- train(
  formula(mod2),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

#Model 3. F1 is 0.5775019. Threshold is 0.296
glm_3 <- train(
  formula(mod3),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

#Model 4. F1 is  Threshold is 
glm_4 <- train(
  formula(model_form),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)


smote_spec <- Pobre ~  cuartos_usados + H_Head_mujer + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad + nmujeres + noafiliados + perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos + (cuartos_usados + H_Head_mujer + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad + nmujeres + noafiliados + perc_mujer + perc_edad_trabajar + perc_ocupados + perc_menores + perc_uso_cuartos)^2
glm_5 <- train(formula(smote_spec), 
               data = rose_train, 
               method = "glm",
               trControl = train_control,
               family = "binomial")




### Applying the function

calculate_f1_and_plot(glm_1, train)

calculate_f1_and_plot(glm_2, train)

calculate_f1_and_plot(glm_3, train)

calculate_f1_and_plot(glm_4, train)

calculate_f1_and_plot(glm_5, rose_train)

#calculate_f1_and_plot(glm_5, smote_data_train, class_variable = "X1")

### Exporting predictions

predictSample_glm_1 <- test %>%
  mutate(pobre_lab = predict(glm_4, newdata = test, type = "prob") %>%
           `[[`("Yes")) %>%
  dplyr::select(id,pobre_lab)
predictSample_glm_1$pobre <- ifelse(predictSample_glm_1$pobre_lab > 0.307, 1, 0)
predictSample_glm_1 <- predictSample_glm_1[, c("id", "pobre")]
predictSample_glm_1

write.csv(predictSample_glm_1,"classification_logit.csv", row.names = FALSE)



## 4.4: CART - LDA and QDA----


mod0 <- Pobre ~ nmenores + arrienda

mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
  H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados

mod2 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores + H_Head_Educ_level

mod3 <- Pobre ~ H_Head_mujer*H_Head_ocupado + poly(nocupados, 3, raw= TRUE) + nmujeres  + nmenores
mod4 <- Pobre ~ .



# Convert Pobre into a factor
train$Pobre <- factor(train$Pobre)
levels(train$Pobre) <- make.names(levels(train$Pobre))


# Define train control
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     verbose = FALSE
)

# Perform LDA classification
lda0 <- train(mod0, 
             data = train, 
             method = "lda",
             trControl = ctrl)


test<- test  %>% mutate(Pobre_hat_lda=predict(lda,newdata = test,
                                                      type = "raw"))

test$Pobre_hat_lda<-factor(test$Pobre_hat_lda)

confusionMatrix(data = lda$pred$pred, 
                reference = lda$pred$obs, 
                positive="Yes", mode = "prec_recall")

calculate_f1_and_plot(lda0, train) # Best Threshold: 0.12 ; Max F1 Score: 0.4284724 

lda1 <- train(mod1, 
              data = train, 
              method = "lda",
              trControl = ctrl)


test<- test  %>% mutate(Pobre_hat_lda1=predict(lda1,newdata = test,
                                               type = "raw"))

test$Pobre_hat_lda1<-factor(test$Pobre_hat_lda1)

confusionMatrix(data = lda1$pred$pred, 
                reference = lda1$pred$obs, 
                positive="Yes", mode = "prec_recall")

calculate_f1_and_plot(lda1, train) # Best Threshold: 0.26 ; Max F1 Score: 0.5605039 





# Perform QDA classification
qda1 <- train(mod1, 
             data = train, 
             method = "qda",
             trControl = ctrl)


test<- test  %>% mutate(Pobre_hat_qda1=predict(qda1,newdata = test,
                                              type = "raw"))

test$Pobre_hat_qda1<-factor(test$Pobre_hat_qda1)

confusionMatrix(data = qda1$pred$pred, 
                reference = qda1$pred$obs, 
                positive="Yes", mode = "prec_recall")


## 4.5 Tree ------------------------------------------------------------------
#Do until line 273, then...
## For a classification approach, we will drop the income variables
names(train)
train <- train %>% dplyr::select(-c("Ingtotug","Ingtotugarr","Ingpcug"))

## I set the reference value for the classification in Poor = "No"
train <- train %>% mutate(Pobre=relevel(Pobre,ref="No"))

set.seed(307795)
inTrain <- createDataPartition(y = train$Pobre,
                               p = 0.7,
                               list = FALSE)
trainbase <- train[inTrain,]
testbase <- train[-inTrain,]

##The same distribution between poors and not poors
prop.table(table(trainbase$Pobre))
prop.table(table(testbase$Pobre))

## We estimate the tree by using rpart
tree1 <- rpart(Pobre ~ .,
               data = trainbase,
               method = "class",
               cp = 0,
               minbucket = 1000)

tree2 <- rpart(Pobre ~ arrienda + propia_pagada + sin_titulo + num_cuartos + total_personas +
                 H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad +
                 nmenores + perc_ocupados + perc_edad_trabajar,
               data = trainbase,
               method = "class",
               cp = 0,
               minbucket = 1000)


#prp(tree1, under = TRUE, yesno = 2, faclen = 0, varlen=15, box.palette = "-RdYlGn")


pobre0<- ifelse(testbase$Pobre=="Yes",1,0) #Volder default en test  numérico
pred_prob0 <- predict(tree2, newdata = testbase, type = "prob")    ## Predecir la probabilidad (en lugar de la clase)
aucval_arbol <- Metrics::auc(actual = pobre0,predicted = pred_prob0[,2]) #calcular el AUC
print(paste0("AUC: ",aucval_arbol))


pobre_hat <- predict(tree2, newdata = testbase, type = "class")
f1_tree1 <- F1_Score(pobre_hat,testbase$Pobre)
print(paste0("F1: ",f1_tree1))

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))  ## Para usar ROC) (u otras más) para tuning

ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     verbose = FALSE,
                     savePredictions = TRUE)

## We specify the grid for alpha
grid <- expand.grid(cp = seq(0, 0.1, 0.02))

cv_tree <- train(Pobre ~ arrienda + propia_pagada + sin_titulo + num_cuartos + total_personas +
                   H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + H_Head_afiliadoSalud + H_Head_edad +
                   nmenores + perc_ocupados + perc_edad_trabajar,
                 data = train,
                 method = "rpart", 
                 trControl = ctrl, 
                 tuneGrid = grid, 
                 metric= "F"
)

cv_tree <- train(Pobre ~ .,
                 data = train,
                 method = "rpart2", 
                 trControl = ctrl, 
                 tuneGrid = expand.grid(maxdepth=1:20), 
                 metric= "F",
                 control = rpart.control(minsplit = 4, 
                                         minbucket =  900, 
                                         cp = 0)
)

cv_tree
plot(cv_tree, pch="")
cv_tree$bestTune$cp

## Predicting and generating prediction file for trees
predictSample <- test %>%
  mutate(pobre_lab = ifelse(predict(tree2, newdata = test, type = "class") == "Yes",1,0)) %>%
  dplyr::select(id,pobre_lab)


head(predictSample)
write.csv(predictSample,"predictions/classification_tree.csv", row.names = FALSE)



## Bagging variation
##Naive approach
bagged <- ranger(Pobre ~ .,
                 data = trainbase,
                 num.trees = 500,
                 mtry = 24,
                 min.node.size = 1000)
bagged

##I obtain bagging model's predictions
bagged_pred  <- predict(bagged,
                       data = testbase, 
                       predict.all = TRUE # para obtener la clasificación de cada arbol. 
)
bagged_pred  <- as.data.frame( bagged_pred$predictions )

ntrees <- ncol(bagged_pred)
probabilities <- rowSums(bagged_pred == 2) / ntrees
aucval_ipred <- Metrics::auc(actual = pobre0, predicted = probabilities)
aucval_ipred

## Optimizing parameters approach
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = TRUE)
mtry_grid<-expand.grid(mtry =c(2,5,8,10,13,24), # 24 inclueye bagging
                       min.node.size= c(100, 500, 1000), #controla la complejidad del arbol
                       splitrule= 'gini') #splitrule fija en gini. 
mtry_grid

cv_RForest <- train(Pobre ~ .,
                    data = trainbase,
                    method = "ranger",
                    trControl = ctrl,
                    metric = "ROC",
                    tuneGrid = mtry_grid,
                    ntree = 500)
cv_RForest$finalModel

rf_pred <- predict(cv_RForest, 
                   newdata = testbase, 
                   type="prob" ## class for class prediction
)
aucval_rf <- Metrics::auc(actual = pobre0,predicted =rf_pred[,2])
aucval_rf

#Best forest
best_forest <- ranger(Pobre ~ .,
                 data = trainbase,
                 num.trees = 500,
                 mtry = 5,
                 min.node.size = 100,
                 importance = "impurity")

imp<-importance(best_forest)
imp2<- data.frame(variables= names(imp),
                  importance= imp)

ggplot(imp2, aes(x = reorder(variables, importance) , y =importance )) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Variable ", x = "Importance", y="Variable") +
  theme_minimal() +
  coord_flip() 



  ## Predicting and generating prediction file for bagging
  predictSample <- test %>%
    mutate(pobre_lab = ifelse(predict(best_forest, data = test, type = "prob") > 0.3,1,0)) %>%
    dplyr::select(id,pobre_lab)
  
  
  head(predictSample)
  write.csv(predictSample,"predictions/classification_tree.csv", row.names = FALSE)



#3: INCOME REGRESSION APPROACH -------------------------------------------------

#Linear regression 
set.seed(123)
all_vars <- names(train)
exclude_vars <- c("Ingtotug", "Ingtotugarr", "Pobre")
include_vars <- setdiff(all_vars, exclude_vars)
target_var <- "Ingpcug"
formula_str <- paste(target_var, "~", paste(setdiff(include_vars, target_var), collapse = " + "))
inc_mod1 <- as.formula(formula_str)
train_inc <- train 
K <- 10
nrow(train_inc)/K
train_inc<-train_inc  %>% mutate(fold=c(rep(1,16496),
                              rep(2,16496),
                              rep(3,16496),
                              rep(4,16496),
                              rep(5,16496),
                              rep(6,16496),
                              rep(7,16496),
                              rep(8,16496),
                              rep(9,16496),
                              rep(10,16496)))

fit1<- lm(inc_mod1, data= train_inc  %>% filter(fold!=1))
yhat1<- predict(fit1,newdata=train_inc  %>% filter(fold==1) )

db_train<-list()
db_test<-list()

for(i in 1:10){
  db_train[[i]] <- train_inc %>% filter(fold != i) # Trains
  db_test[[i]] <- train_inc %>% filter(fold == i) # Tests
  
  fit <- lm(inc_mod1, data = db_train[[i]])
  # Storing predictions in a separate column
  db_test[[i]]$Ingpcug_hat <- predict(fit, newdata = db_test[[i]])
  # Classify as 'pobre' based on the threshold
  db_test[[i]]$pobre_pred <- ifelse(db_test[[i]]$Ingpcug_hat < 300000, 1, 0)
}

precision_list <- list()
recall_list <- list()
f1_score_list <- list()

for(i in 1:10){
  # Assuming db_test[[i]] already has the 'pobre_pred' column from previous steps
  true_positives <- sum(db_test[[i]]$pobre_pred == 1 & db_test[[i]]$Pobre == "Yes")
  false_positives <- sum(db_test[[i]]$pobre_pred == 1 & db_test[[i]]$Pobre == "No")
  false_negatives <- sum(db_test[[i]]$pobre_pred == 0 & db_test[[i]]$Pobre == "Yes")
  
  # Calculate precision and recall for each fold
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  
  # Handle cases where precision or recall are NaN due to division by zero
  precision <- ifelse(is.nan(precision), 0, precision)
  recall <- ifelse(is.nan(recall), 0, recall)
  
  # Calculate F1 score
  f1_score <- 2 * (precision * recall) / (precision + recall)
  # Handle case where F1 score is NaN due to division by zero in the formula
  f1_score <- ifelse(is.nan(f1_score), 0, f1_score)
  
  # Store precision, recall, and F1 score
  precision_list[[i]] <- precision
  recall_list[[i]] <- recall
  f1_score_list[[i]] <- f1_score
}

# Calculate average F1 score across folds
mean_f1_score <- mean(unlist(f1_score_list), na.rm = TRUE)
mean_f1_score


calculate_f1_for_threshold <- function(threshold, train_inc, inc_mod1) {
  db_train <- list()
  db_test <- list()
  f1_score_list <- vector("list", 10)
  
  for(i in 1:10) {
    db_train[[i]] <- train_inc %>% filter(fold != i)
    db_test[[i]] <- train_inc %>% filter(fold == i)
    
    fit <- lm(inc_mod1, data = db_train[[i]])
    db_test[[i]]$Ingpcug_hat <- predict(fit, newdata = db_test[[i]])
    db_test[[i]]$pobre_pred <- ifelse(db_test[[i]]$Ingpcug_hat < threshold, 1, 0)
    
    true_positives <- sum(db_test[[i]]$pobre_pred == 1 & db_test[[i]]$Pobre == "Yes")
    false_positives <- sum(db_test[[i]]$pobre_pred == 1 & db_test[[i]]$Pobre == "No")
    false_negatives <- sum(db_test[[i]]$pobre_pred == 0 & db_test[[i]]$Pobre == "Yes")
    
    precision <- true_positives / (true_positives + false_positives)
    recall <- true_positives / (true_positives + false_negatives)
    
    precision[is.nan(precision)] <- 0
    recall[is.nan(recall)] <- 0
    
    f1_score <- 2 * (precision * recall) / (precision + recall)
    f1_score[is.nan(f1_score)] <- 0
    
    f1_score_list[[i]] <- f1_score
  }
  
  mean_f1_score <- mean(unlist(f1_score_list), na.rm = TRUE)
  return(mean_f1_score)
}

start_threshold <- 100000
end_threshold <- 500000
step_size <- 10000

# Initialize variables to store the best threshold and its F1 score
best_threshold <- start_threshold
best_f1_score <- -Inf  # Start with the lowest possible value

# Iterate over the interval in steps of 1000
for(threshold in seq(from = start_threshold, to = end_threshold, by = step_size)) {
  current_f1_score <- calculate_f1_for_threshold(threshold, train_inc, inc_mod1)
  
  # Update the best threshold if the current one is better
  if(current_f1_score > best_f1_score) {
    best_threshold <- threshold
    best_f1_score <- current_f1_score
  }
}

# Output the best threshold and its F1 score
best_threshold # 370000
best_f1_score # 0.5479155

# Elastic Net 

X <- model.matrix(~perc_ocupados + H_Head_Educ_level + nmenores + num_cuartos + H_Head_edad,train)
X<-X[,-1] #remove constant
y<-train$Ingpcug

enet0 <- glmnet(
  x = X,
  y = y,
  
  alpha = 0.5 # Elastic Net penalty (0 for Ridge, 1 for Lasso)
)
coef(enet0, s= 0.1) 
plot(enet0, xvar = "lambda")

tuneGrid<- expand.grid(alpha= seq(0,1, 0.05), # between 0 and 1. 
                       lambda=seq(0.5, 1.5, 0.5) ) 

model_form <- Pobre ~ perc_ocupados + H_Head_Educ_level + nmenores + num_cuartos + H_Head_edad #(perc_ocupados + H_Head_Educ_level + nmenores + num_cuartos + H_Head_edad)^2

trainControl <- trainControl( 
  method = "cv",
  number = 10)

ENet<-train(model_form,
            data=rose_train,
            method = 'glmnet', 
            trControl = trainControl,
            tuneGrid = tuneGrid )  #specify the grid 

plot(ENet)
