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


