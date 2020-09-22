
# Note: Run the CommonFunctions and Survival scripts prior to this script as that will ensure that the required objects are created in your R environment

# this method uses same clustering method used in Survival so recommend running this after that approach


# create training and test set at event rather than at trace level
training <- soc %>% dplyr::left_join(training_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)%>% dplyr::filter(censured==1)
training <- training %>% filter(!is.na(Grp_Deg))
testing <- soc %>% dplyr::left_join(testing_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)%>% dplyr::filter(censured==1)
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
