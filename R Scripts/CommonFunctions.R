library(tidyverse)
library(bupaR)
library(lubridate)
library(ggplot2)
library(ranger)
library(caret)
library(purrr)
library(rsample)
library(Metrics)
library(gbm)
library(Hmisc)
library(readxl)
library(naniar)

# function to enrich log with additional feaures - elaped time, remaining time & next resource
AddFeatures <- function(filtered_log) {
  grouping <- filtered_log$Case.ID
  split.soc <- split(filtered_log1,grouping)
  for (i in seq_along(split.soc)){
    compTime <- split.soc[[i]][which.max(split.soc[[i]][["Complete.Timestamp"]]),"Complete.Timestamp"]
    startTime <- split.soc[[i]][which.min(split.soc[[i]][["Complete.Timestamp"]]),"Complete.Timestamp"]
    for (j in 1:nrow(split.soc[[i]])){
      split.soc[[i]][j,"next.resource"] <- if_else(j<nrow(split.soc[[i]]),as.character(split.soc[[i]][j+1,"Resource"]),as.character(NA))
      split.soc[[i]][j,"RemTime"] <- difftime(compTime,split.soc[[i]][j,"Complete.Timestamp"],unit="days")
      split.soc[[i]][j,"ElapsedTime"] <- difftime(split.soc[[i]][j,"Complete.Timestamp"],startTime,unit="days")
    }
  }
  soc <- do.call(rbind,split.soc)
}

# function to calculate group centrality functions for each trace
ExtractGroupCentrality <- function(filtered_log,AdjacencyMatrix) {
  grouping <- filtered_log$Case.ID
  split.soc <- split(filtered_log1,grouping)
  Grouped_Scores <- data.frame(Case.ID=character(), Grp_Deg=numeric(),Grp_Bet=numeric(),Grp_Eig=numeric(), Grp_Clo=numeric(),stringsAsFactors = FALSE)
  for (i in seq_along(split.soc)){
    Resource <- unique(split.soc[[i]][["Resource"]]) # unique names of resources
    ind <- which(colnames(AdjacencyMatrix) %in% Resource) 
    # write each line into the dataframe using kpercent
    Grouped_Scores[i,"Case.ID"] <- unique(split.soc[[i]][["Case.ID"]])
    Grouped_Scores[i,"Grp_Deg"] <- kpcent(AdjacencyMatrix,ind,type="degree",binary=FALSE,cmode="total")
    Grouped_Scores[i,"Grp_Bet"] <- kpcent(AdjacencyMatrix,ind,type="betweenness",binary=FALSE)
    Grouped_Scores[i,"Grp_Eig"] <- kpcent(AdjacencyMatrix,ind,type="evcent",binary=FALSE)
    Grouped_Scores[i,"Grp_Clo"] <- kpcent(AdjacencyMatrix,ind,type="closeness",binary=FALSE)
  }
}

CensorTraces <- function(filtered_log,terminal_activities) {
  surv_log <- filtered_log %>% 
    throughput_time(level="case",append = TRUE) %>% 
    group_by_case() %>% 
    # dplyr::group_by(Case.ID) %>% 
    dplyr::mutate(start_activity=Activity[which.min(Complete.Timestamp)],end_activity=Activity[which.max(Complete.Timestamp)]) %>% 
    distinct(Case.ID,start_activity,end_activity,throughput_time_case) %>% 
    mutate(censured=ifelse(str_detect(end_activity,terminal_activities),1,0))
}

# function to find optimal k using scree plot - pass in distance matrix

optimal_k <- function(log_clust_data){
  tot_withinss <- map_dbl(2:10,  function(k){
    model <- kmeans(x = log_clust_data, centers = k)
    model$tot.withinss
  })
  
  # Generate a data frame containing both k and tot_withinss
  elbow_df <- data.frame(
    k = 2:10,
    tot_withinss = tot_withinss
  )
}

# function to calculate optimal value of mtry which will optimise MAE 
CalcBestMtry <- function (training_data){
  log_nested <- training_data %>%
    dplyr::group_by(cluster) %>%
    nest()
  
  #5. Use rf to build a cross validated model. Find lowest mtry for each cluster & use to build model -pass in dataset with clusters
  set.seed(42)
  # Prepare the dataframe containing the cross validation partitions
  
  cv_split <- map2(log_nested$cluster,log_nested$data,~data.frame(cluster=.x, a=vfold_cv(.y))) %>%
    map_df(~data.frame(.x))
  
  cv_data <- cv_split %>%
    dplyr::mutate(
      # Extract the train dataframe for each split
      train = map(a.splits, ~training(.x)),
      # Extract the validate dataframe for each split
      validate = map(a.splits, ~testing(.x))
    )
  # Prepare for tuning your cross validation folds by varying mtry
  cv_tune <- cv_data %>%
    tidyr::crossing(mtry = 2:5)
  
  # Build a model for each fold & mtry combination
  cv_model_tunerf <- cv_tune %>%
    dplyr::mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = RemTime ~ .,
                                                              data = .x, mtry = .y,
                                                              num.trees = 100, seed = 42)))
  cv_prep_tunerf <- cv_model_tunerf %>%
    dplyr::mutate(validate_actual = map(validate, ~.x$RemTime),validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))
  
  # Calculate validate MAE for each fold and mtry combination
  cv_eval_tunerf <- cv_prep_tunerf %>%
    dplyr::mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))
  
  # Calculate the mean validate_mae for each mtry used  
  cv_best_models <- 
    cv_eval_tunerf %>%
    dplyr::group_by(mtry) %>%
    dplyr::summarise(mean_mae = mean(validate_mae))
  
  #cv_best_models %>% group_by(cluster) %>% filter(mean_mae==min(mean_mae)) 
  cv_best_models %>% dplyr::filter(mean_mae==min(mean_mae))
}