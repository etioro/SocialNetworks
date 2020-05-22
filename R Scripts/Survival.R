#load libraries
library(igraph)
library(Matrix)
library(rsample)
library(Metrics)
library(keyplayer)
library(survival)
library(survminer)
library(stringr)

#read in the log file: in format 'BPIC1x_filtered_log' where 1x is the year e.g. BPIC12_filtered_log (2012), BPIC14_filtered_log (2014), etc

# add remaining & elapsed time, etc 
soc <- AddFeatures(BPIC1x_filtered_log) #amend function input to match log name for appropriate year

# create a handover-of-work edgelist and convert into  dataframe 
soc.df <- soc %>% select(Resource, next.resource) %>% filter(!is.na(next.resource)) %>% as.data.frame()
View(soc.df)
# create a sociogram
soc.net <- graph_from_data_frame(soc.df,directed =TRUE)

# Extract the adjacency matrix
AdjacencyMatrix <- as_adjacency_matrix(soc.net)

# Extract the group centrality scores
Grouped_Scores <- ExtractGroupCentrality (BPIC1x_filtered_log,AdjacencyMatrix)
View(Grouped_Scores)

#create the censored traces - see below for set of terminal activity labels for each year (execute code after semicolon)
#BPIC12: terminal_activity <- "W_Nabellen offertes_COMPLETE|W_Valideren aanvraag_COMPLETE"
#BPIC14: terminal_activity <- "Closed"
#BPIC15: terminal_activity <- "close case|enter date publication decision environmental permit|enter senddate decision environmental permit"
#BPIC17: terminal_activity <- "O_"
#BPIC18: handled different as all traces have same terminal activity. if trace has any event with activity label = "finish payment", its not censored; otherwise it is
surv_log <- CensorTraces(BPIC1x_filtered_log,terminal_activity)

Grouped_Scores <- Grouped_Scores %>% left_join(surv_log)

# Create cluster based on group centrality scores
log_clust_data <- Grouped_Scores %>% dplyr::select(Grp_Deg,Grp_Bet,Grp_Eig,Grp_Clo) %>% scale()

#determine optima; number of clusters
elbow_df <- optimal_k(log_clust_data)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

clusters_kn <- kmeans(x = log_clust_data, centers =x) # replace x with appropriate value based on scree plot
Grouped_Scores <- Grouped_Scores %>% dplyr::mutate(cluster = clusters_kn$cluster)

# Split into test & training
log_split <- initial_split(Grouped_Scores, prop = 0.75)

# Extract the testing dataframe
testing_data <- log_split %>% testing() 
training_data <- log_split %>% training()
 
#build survival functions from training set
wbmod_tr <- survreg(Surv(throughput_time_case, censured) ~ Grp_Deg+Grp_Bet+Grp_Eig+Grp_Clo+start_activity+end_activity+cluster,control = list(maxiter=120),data = training_data)

#predict survival time using median
total_pred <- predict(wbmod_tr, newdata=testing_data, type="quantile", p=.5)
testing_data <- testing_data %>% mutate(pred = total_pred)

# create test set at event rather than at trace level
testing <- soc %>% dplyr::left_join(testing_data,by= c("Case.ID")) %>% filter(censured==1) 

# calculate mean mae & sd
testing %>% 
  group_by(cluster) %>%
  summarise(mae=mae(RemTime,(pred-ElapsedTime)),mean_mae=mean(mae)) %>%
  summarise(sd_mae=sd(mean_mae))