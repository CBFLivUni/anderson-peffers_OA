
weights <- do.call(rbind, get_weights(MOFAobject.trained, views = "proteomics", factors = 1,
                                as.data.frame = FALSE))


data <- get_data(MOFAobject.trained, views = "proteomics")[[1]]

if (is(data, "list")) {
  data <- do.call(cbind, data)
}


features <- rownames(weights)[tail(order(abs(weights)), n = 30)]
data <- data[features, ]
row.names(data) <- plasma_annotation[row.names(data),]


tmp <- MOFAobject.trained@samples_metadata
rownames(tmp) <- tmp$sample
tmp$sample <- NULL
annotation_samples <- tmp[, c("Day", "Horse"), drop = FALSE]


pheatmap(data,
         annotation_col = annotation_samples,
         cluster_rows = TRUE, cluster_cols = FALSE,
         show_rownames = TRUE, show_colnames = FALSE,
         scale = "row",
         color = heatmap_cols,
         border_color = NA,
         annotation_samples = c("Day", "Horse"),
         fontsize = 12)
