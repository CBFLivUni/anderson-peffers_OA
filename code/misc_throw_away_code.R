-----------------------------------------------------------------------------

  ## Ignore code below this point for now

  Next prepare the per cluster feature membership data and plot:

  ```{r}

# Create copy of dataframe containing average z-score per time
plasma_dtw_df <- data.frame(plasma_time_df)

# Add row with feature names
plasma_dtw_df$Feature <- row.names(plasma_dtw_df)

# Bind the cluster assignment
plasma_dtw_df$cluster <- dtwclust_cl@cluster

# Fetch the membership for each feature in its top scoring cluster
#plasma_membership_df$membership <- sapply(1:length(plasma_membership_df$cluster),function(row){
#  clust <- plasma_membership_df$cluster[row]
#  mfuzz_cl$membership[row,clust]
#})


# Convert membership dataframe into long format
plasma_dtw_long_df <- plasma_dtw_df %>%
  dplyr::select(.,1:length(colnames(plasma_dtw_centroids)), Feature, cluster) %>%
  tidyr::gather(.,"sample",'value',1:length(colnames(plasma_dtw_centroids)))


# Remove "Day_" prefix from "sample" column
plasma_dtw_long_df$sample <- gsub("Day_", "", plasma_cluster_plot_df$sample)


```

```{r}

# Per-cluster membership plot

plasma_dtw_plot_membership <- ggplot(plasma_dtw_long_df, aes(x=sample,y=value)) +
  # Facet-wrap by cluster
  facet_wrap(~cluster, scales="free_y") +
  # Colour each panel by cluster
  # The oppacity of each feature is determined by membership
  geom_line(aes(colour=as.factor(cluster), group =Feature), lwd=1) +
  scale_colour_brewer(palette = "Set1") +
  # Add the centroids as a black line
  geom_line(data=plasma_dtw_centroids_long, aes(x=sample, y=value, group=cluster), lwd=1.2, color="black",inherit.aes=FALSE) +
  xlab("Day") +
  ylab("Expression") +
  labs(title="Per-cluster membership") +
  theme_cowplot() +
  theme(legend.position = "none")


plasma_dtw_plot_membership

```


---------------------------------

```{r, fig.height = 14, fig.width = 12, fig.align="center"}

ggarrange(ggarrange(plasma_mfuzz_centroids_plot, plasma_mfuzz_plot_membership,
                    nrow = 1,
                    ncol = 2,
                    labels = c("A", "B")),
          ggarrange(sf_mfuzz_centroids_plot, sf_mfuzz_plot_membership,
                    nrow = 1,
                    ncol = 2,
                    labels = c("C", "D")),
          ggarrange(plasma_dtw_centroids_plot, sf_dtw_centroids_plot,
                    ggarrange(plasma_cl_OL_plot[[4]],
                              sf_cl_OL_plot[[4]],
                              ncol = 1,
                              labels = c("G", "H")),
                    nrow = 1,
                    ncol = 3,
                    labels = c("E", "F", "")),
          nrow = 3,
          ncol = 1)

```




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



