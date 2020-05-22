library(flexmix)

# Note: Run the CommonFunctions and Survival scripts prior to this script as that will ensure that the required objects are created in your R environment

RemTime_mm <- stepFlexmix(cbind(Grouped_Scores$Grp_Deg,Grouped_Scores$Grp_Bet,Grouped_Scores$Grp_Eig,Grouped_Scores$Grp_Clo) ~ 1,
                          k = 1:5,
                          nrep = 5,
                          model = FLXMCmvnorm(diag = FALSE),
                          control = list(tolerance = 1e-15, iter.max = 1000))  


# Select the model that minimize the BIC
best_RemTime_mm <- getModel(RemTime_mm, which = "BIC")
prior(best_RemTime_mm)

# Select the model that minimize the BIC
best_RemTime_mm <- getModel(RemTime_mm, which = "BIC")
prior(best_RemTime_mm)

Grouped_Scores <- Grouped_Scores %>% dplyr::select(-cluster) # run only if cluster has already been assigned and needs to be removed 
Grouped_Scores <- Grouped_Scores %>% dplyr::mutate(cluster = flexmix::clusters(best_RemTime_mm))

#remove previously assigned cluster and assign new cluster 
training_data <- training_data %>% dplyr::select(-cluster) %>% dplyr::left_join(clusters, by = c("Case.ID"))
testing_data <- testing_data %>% dplyr::select(-cluster) %>% dplyr::left_join(clusters,by = c("Case.ID")) %>% dplyr::filter(censured==1)

# create training and test set at event rather than at trace level
training <- soc %>% dplyr::left_join(training_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)
training <- training %>% filter(!is.na(Grp_Deg))
testing <- soc %>% dplyr::left_join(testing_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)
testing <- testing %>% filter(!is.na(Grp_Deg))

# determine value of mtry which minimises mae
CalcBestMtry(training)

best_model <- training %>%
  dplyr::group_by(cluster) %>%
  nest() %>%
  dplyr::mutate(model = map(data, ~ranger(formula = RemTime~., data = .x,
                                          mtry = x , num.trees = 100, seed = 42))) # replace x with optimal mtry value calculated above

#calculate mean mae & sd
CalcMAE(testing) %>% dplyr::summarize(mean_mae=mean(validate_mae)) %>% dplyr::summarize(mean = mean(mean_mae),sd_mae=sd(mean_mae))
