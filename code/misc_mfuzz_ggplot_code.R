

#start with the input data
fcm_plotting_df <- data.frame(plasma_time_df)

#add genes
fcm_plotting_df$Feature <- row.names(fcm_plotting_df)

#bind cluster assinment
fcm_plotting_df$cluster <- mfuzz_cl$cluster

#fetch the membership for each gene/top scoring cluster
fcm_plotting_df$membership <- sapply(1:length(fcm_plotting_df$cluster),function(row){
  clust <- fcm_plotting_df$cluster[row]
  mfuzz_cl$membership[row,clust]
})


#subset the dataframe by the cluster and get it into long form
#using a little tidyr action
cluster_plot_df <- fcm_plotting_df %>%
  dplyr::select(.,1:length(colnames(fcm_centroids)), membership, Feature, cluster) %>%
  tidyr::gather(.,"sample",'value',1:length(colnames(fcm_centroids)))

#order the dataframe by score
cluster_plot_df <- cluster_plot_df[order(cluster_plot_df$membership),]
#set the order by setting the factors using forcats
cluster_plot_df$Feature = forcats::fct_inorder(cluster_plot_df$Feature)

#subset the cores by cluster
ggplot(cluster_plot_df, aes(x=sample,y=value)) +
  facet_wrap(~cluster, scales="free_y") +
  geom_line(aes(colour=membership, group=Feature), lwd=1, alpha=0.5) +
  #scale_colour_gradientn(colours=c('blue1', 'red2')) +
  scale_colour_gradient(low = "white", high = "red") +
  #scale_color_viridis(option = "mako", direction = -1) +
  #this adds the core
  geom_line(data=centroids_long, aes(sample,value, group=cluster), lwd=1.5, color="black",inherit.aes=FALSE) +
  xlab("Time") +
  ylab("Expression") +
  labs(title= paste0("Per-cluster membership"),color = "Score") +
  theme_cowplot()







#start with the input data
fcm_plotting_df <- data.frame(plasma_time_df)

#add genes
fcm_plotting_df$Feature <- row.names(fcm_plotting_df)

#bind cluster assinment
fcm_plotting_df$cluster <- mfuzz_cl$cluster

#fetch the membership for each gene/top scoring cluster
fcm_plotting_df$membership <- sapply(1:length(fcm_plotting_df$cluster),function(row){
  clust <- fcm_plotting_df$cluster[row]
  mfuzz_cl$membership[row,clust]


k_to_plot = 3

#subset the dataframe by the cluster and get it into long form
#using a little tidyr action
cluster_plot_df <- dplyr::filter(fcm_plotting_df, cluster == k_to_plot) %>%
  dplyr::select(.,1:length(colnames(fcm_centroids)),membership,Feature) %>%
  tidyr::gather(.,"sample",'value',1:length(colnames(fcm_centroids)))

#order the dataframe by score
cluster_plot_df <- cluster_plot_df[order(cluster_plot_df$membership),]
#set the order by setting the factors using forcats
cluster_plot_df$Feature = forcats::fct_inorder(cluster_plot_df$Feature)

#subset the cores by cluster
core <- dplyr::filter(centroids_long, cluster == k_to_plot)

ggplot(cluster_plot_df, aes(x=sample,y=value)) +
  geom_line(aes(colour=membership, group=Feature), lwd=1, alpha=0.5) +
  #scale_colour_gradientn(colours=c('blue1', 'red2')) +
  scale_colour_gradient(low = "white", high = "red") +
  #scale_color_viridis(option = "mako", direction = -1) +
  #this adds the core
  geom_line(data=core, aes(sample,value, group=cluster), lwd=1.5, color="black",inherit.aes=FALSE) +
  xlab("Time") +
  ylab("Expression") +
  labs(title= paste0("Cluster ",k_to_plot," Expression by Time"),color = "Score") +
  theme_cowplot()

