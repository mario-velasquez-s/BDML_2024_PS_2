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
       ROSE)  # Fixed effects 

# 1: Initial Data Manipulation -----------------------------------------------


username <- Sys.info()["user"]
if (username == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 2")
} else if (username == "marti") {
  setwd("C:/Users/marti/OneDrive - Universidad de los andes/BDML - Datos")
} else if (username == "") {
  # Mario
  setwd("/default/path/for/other/users")
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
    rural = ifelse(Clase == 2, 1, 0),
    edad_trabajar = case_when(
      P6040 >= 12 & Clase == 1 ~ 1,  #Cuando tiene 12 o más años y vive en cabecer
      P6040 >= 10 & Clase == 2 ~ 1,  # Cuando tiene 10 o más años y vive en zona rural
      TRUE ~ 0                        # Otherwise
    )
  ) %>% 
    dplyr::select(id, Orden,mujer,H_Head,menor,EducLevel,ocupado, afiliadoSalud, edad, ciudad, rural, edad_trabajar)
  
  
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

ggplot(train_personas_AfiSaludmiss, aes(x=edad))+
  geom_histogram(fill="#0099F8") +
  labs(x="Edad") + 
  theme_bw()

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

## Número de personas en el hogar

train_personas <- train_personas %>%
  group_by(id) %>%
  mutate(total_personas = n()) %>%
  ungroup()

test_personas <- test_personas %>%
  group_by(id) %>%
  mutate(total_personas = n()) %>%
  ungroup()

## Creation of variables at individual level

train_personas_nivel_hogar<- train_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE),
            total_personas = max(total_personas, na.rm = TRUE),
            edad_trabajar = sum(edad_trabajar, na.rm = TRUE)
  )

train_personas_hogar<- train_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud, rural, total_personas, edad) %>% 
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
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE),
            total_personas = max(total_personas, na.rm = TRUE),
            edad_trabajar = sum(edad_trabajar, na.rm = TRUE)
  )

test_personas_hogar<- test_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud, rural, total_personas, edad) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado,
         H_Head_afiliadoSalud = afiliadoSalud,
         H_Head_edad = edad) %>% 
  left_join(test_personas_nivel_hogar)


## Household variables
## (POR AHORA DEJÉ LAS VARIABLES DE HOGAR QUE PUSO ANDRÉS PERO TENEMOS QUE ESCOGER MEJOR)
train_hogares<- train_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  dplyr::select(id,Dominio,arrienda,Pobre)


test_hogares<- test_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  dplyr::select(id,Dominio,arrienda) 

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
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes")),
         rural=factor(rural,levels=c(0,1),labels=c("No","Yes"))
  )

test<- test %>% 
  mutate(Dominio=factor(Dominio),
         arrienda=factor(arrienda,levels=c(0,1),labels=c("No","Yes")),
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes")),
         rural=factor(rural,levels=c(0,1),labels=c("No","Yes"))
  )

## Variable creation

train <- train %>%
  mutate(
    perc_mujer = (nmujeres / total_personas) * 100,
    perc_edad_trabajar = (edad_trabajar / total_personas) * 100,
    perc_ocupados = (nocupados / total_personas) * 100
  )

test <- test %>%
  mutate(
    perc_mujer = (nmujeres / total_personas) * 100,
    perc_edad_trabajar = (edad_trabajar / total_personas) * 100,
    perc_ocupados = (nocupados / total_personas) * 100
  )

#2: CLASSIFICATION APPROACH ----------------------------------------------------

## 2.1: Linear Regression
set.seed(685397)

colnames(train)

## I will use k-fold validation to test my training models
k <- 5
nrow(train)/k

## Generate an index for each fold
train <-train  %>% mutate(fold=c(rep(1,32992),
                              rep(2,32992),
                              rep(3,32992),
                              rep(4,32992),
                              rep(5,32992)))

## Models
    
    mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
      H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados
    
    mod2 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores + H_Head_Educ_level
    
    mod3 <- Pobre ~ H_Head_mujer*H_Head_ocupado + poly(nocupados, 3, raw= TRUE) + nmujeres  + nmenores
    mod4 <- Pobre ~ .
      


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
  print(paste("MSE:",round(mean(MSE, na.rm = TRUE), digits=3)))
  
  
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
  print(paste("F1:",round(mean(F1, na.rm = TRUE), digits=3)))
 
  return(mean(F1, na.rm = TRUE))
      
}
  

## I choose models minimizing MSE
cv_mse_f1(train,k,mod4,0.5)


## Choosing the best thresholds



best_thresh_cv<- function(base,nfolds,model,...){
  thresholds <- seq(0.25, 0.35, by = 0.005)
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

best_thresh_cv(train,k,mod1)


## Precicting and generating prediction file
    predictSample <- test %>%
      mutate(pobre_lab = ifelse(predict(lm(mod1, train), newdata=test) >= 0.305, 1, 0)) %>%
      dplyr::select(id,pobre_lab)
    
    head(predictSample)
    write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)

## Predicting and generating prediction file
predictSample <- test %>%
  mutate(pobre_lab = ifelse(predict(lm(mod1, train), newdata=test) >= 0.5, 1, 0)) %>%
  dplyr::select(id,pobre_lab)


head(predictSample)
write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)



## 2.2: ElasticNet

## 2.3: CART - Logit

train_control <- trainControl(
  method = "cv",
  number = 30,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)

calculate_f1_and_plot <- function(model, data) {

  predicted_probabilities <- predict(model, newdata = data, type = "prob")[, "Yes"]
  predict(model, newdata = data, type = "prob")[, "Yes"]
  thresholds <- seq(0, 1, by = 0.01)
  f1_scores <- numeric(length(thresholds))
  max_f1 <- 0
  best_threshold <- 0
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    print(threshold)
    # Convert probabilities to binary predictions based on the threshold
    binary_predictions <- ifelse(predicted_probabilities > threshold, "Yes", "No")
    
    # Compute confusion matrix
    confusion <- table(binary_predictions, train$Pobre)
    
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

mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
  H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados

mod2 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores + H_Head_Educ_level

mod3 <- Pobre ~ H_Head_mujer*H_Head_ocupado + poly(nocupados, 3, raw= TRUE) + nmujeres  + nmenores

### Calculating the best models

#Model 1. F1 is 0.5631557. Threshold is 0.29
glm_1 <- train(
  formula(mod1),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)


#Model 2. F1 is 0.5165361. Threshold is 0.26
glm_2 <- train(
  formula(mod2),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

#Model 3. F1 is 0.4738163. Threshold is 0.25
glm_3 <- train(
  formula(mod3),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

confusionMatrix(data = glm_2$pred$pred, 
                reference = glm_2$pred$obs, 
                positive="Yes", mode = "prec_recall")

### Applying the function

calculate_f1_and_plot(glm_1, train)

calculate_f1_and_plot(glm_2, train)

calculate_f1_and_plot(glm_3, train)

### Exporting predictions

predictSample_glm_1 <- test %>%
  mutate(pobre_lab = predict(glm_1, newdata = test, type = "prob") %>%
           `[[`("Yes")) %>%
  dplyr::select(id,pobre_lab)
predictSample_glm_1$pobre <- ifelse(predictSample_glm_1$pobre_lab > 0.29, 1, 0)
predictSample_glm_1 <- predictSample_glm_1[, c("id", "pobre")]
predictSample_glm_1

write.csv(predictSample_glm_1,"classification_logit.csv", row.names = FALSE)

## 2.4: CART - LDA


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
lda <- train(mod0, 
             data = train, 
             method = "lda",
             trControl = ctrl)


test<- test  %>% mutate(Pobre_hat_lda=predict(lda,newdata = test,
                                                      type = "raw"))

test$Pobre_hat_lda<-factor(test$Pobre_hat_lda)

confusionMatrix(data = lda$pred$pred, 
                reference = lda$pred$obs, 
                positive="Yes", mode = "prec_recall")


#3: INCOME REGRESSION APPROACH -------------------------------------------------



