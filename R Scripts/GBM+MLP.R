library(caret)

# Note: Run the CommonFunctions and Survival scripts prior to this script as that will ensure that the required objects are created in your R environment

#create the required test and trainng sets
training <- soc %>% dplyr::left_join(training_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)
training <- training %>% filter(!is.na(Grp_Deg))
testing <- soc %>% dplyr::left_join(testing_data,by= c("Case.ID")) %>% dplyr::select(Case.ID,Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo,start_activity,end_activity,cluster,RemTime)
testing <- testing %>% filter(!is.na(Grp_Deg))

# Train model to determine optimal model
# gbm
gbm_model <- caret::train(RemTime ~ Grp_Deg+Grp_Bet+Grp_Eig+Grp_Clo+start_activity+end_activity+cluster, 
                          data = training, 
                          method = "gbm", 
                          distribution = "gaussian",
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          na.action = na.pass,
                          verbose = FALSE,
                          tuneLength=5)

pred_gbm <- predict(gbm_model,newdata = testing,type="raw")
mae<-mae(testing$RemTime, pred_gbm)

#print mae 
print(mae)

#print sd 
testing %>% mutate(pred = pred_gbm) %>% 
  group_by(cluster) %>%
  summarise(mae=mae(RemTime,pred),mean_mae=mean(mae)) %>%
  summarise(sd_mae=sd(mean_mae))

#multi-layer perceptron

mlp_model <- caret::train(RemTime ~ Grp_Deg+Grp_Bet+Grp_Eig+Grp_Clo+start_activity+end_activity+cluster, 
                          data = training, 
                          method = "mlp", 
                          distribution = "gaussian",
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          na.action = na.pass,
                          verbose = FALSE,
                          tuneLength=5)

pred_mlp <- predict(mlp_model,newdata = testing,type="raw")
mae<-mae(testing$RemTime, pred_mlp)

#print mae 
print(mae)

#print sd 
testing %>% mutate(pred = pred_mlp) %>% 
  group_by(cluster) %>%
  summarise(mae=mae(RemTime,pred),mean_mae=mean(mae)) %>%
  summarise(sd_mae=sd(mean_mae))
