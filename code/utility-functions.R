#### Function to save pheatmap outputs ####

save_pheatmap_png <- function(x, filename, width=1200, height=1000, res = 150) {
  png(filename, width = width, height = height, res = res)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}



#### Function to make a ggplot2 boxplot #####

library(tidyverse)
library(reshape2)
library(cowplot)

gg_boxplot <- function(x, log.transform=F, pheno=NULL, title="", xlab="", ylab="", pheno.lab="") {

  # Function to create a ggplot2 boxplot from a matrix
  # Intended to have similar functionality to the default boxplot function
  # Data can be log transformed as an argument
  # Axis labels and titles can be provided
  # Phenotype information can also be provided, if so, the boxplots will be coloured by the pheno
  # Data in phenotype needs to be in the same order as the raw data

  if(!is.null(pheno)==TRUE){

    if(log.transform){

      t(log2(x)) %>%
        as.data.frame() %>%
        mutate(pheno = pheno) %>%
        rownames_to_column("Sample_ID") %>%
        melt(id.vars=c("Sample_ID", "pheno")) %>%
        ggplot(aes(x=Sample_ID, y=value, fill=pheno)) +
        geom_boxplot() + theme_cowplot() +
        theme(axis.text.x = element_text(angle = 90, size = 8)) +
        xlab(xlab) +
        ylab(ylab) +
        labs(fill=pheno.lab) +
        ggtitle(title)

    } else {

      t(x) %>%
        as.data.frame() %>%
        rownames_to_column("Sample_ID") %>%
        melt(id.vars=c("Sample_ID", "pheno")) %>%
        ggplot(aes(x=Sample_ID, y=value, fill=pheno)) +
        geom_boxplot() + theme_cowplot() +
        theme(axis.text.x = element_text(angle = 90, size = 8)) +
        xlab(xlab) +
        ylab(ylab) +
        labs(fill=pheno.lab) +
        ggtitle(title)
    }

  } else {

    if(log.transform){

      t(log2(x)) %>%
        as.data.frame() %>%
        rownames_to_column("Sample_ID") %>%
        melt() %>%
        ggplot(aes(x=Sample_ID, y=value, fill=Sample_ID)) +
        geom_boxplot(show.legend = FALSE) + theme_cowplot() +
        theme(axis.text.x = element_text(angle = 90, size = 8)) +
        xlab(xlab) +
        ylab(ylab) +
        ggtitle(title)

    } else {

      t(x) %>%
        as.data.frame() %>%
        rownames_to_column("Sample_ID") %>%
        melt() %>%
        ggplot(aes(x=Sample_ID, y=value, fill=Sample_ID)) +
        geom_boxplot(show.legend = FALSE) + theme_cowplot() +
        theme(axis.text.x = element_text(angle = 90, size = 8)) +
        xlab(xlab) +
        ylab(ylab) +
        ggtitle(title)
    }


  }

}



#### Function for multi-level PCA ####

library(mixOmics)

multi_level_pca <- function(x, multilevel="", group="", group.lab="", title="") {

  pca_multi_level <- mixOmics::pca(t(x), scale = TRUE, center = TRUE, ncomp = 10,
                                   multilevel = multilevel) %>%
    plotIndiv(., group = group,
              ind.names = multilevel,
              legend = TRUE, legend.title = group.lab)


  pca_multi_level_plot <- ggplot(pca_multi_level$df, aes(x=x, y=y, colour = group)) +
    geom_point(size = 3) +
    xlab(pca_multi_level[["graph"]][["labels"]]$x) +
    ylab(pca_multi_level[["graph"]][["labels"]]$y) +
    #scale_color_viridis(option = "plasma") +
    theme_cowplot() +
    labs(colour = group.lab) +
    ggtitle(title)


  return(pca_multi_level_plot)

}


#### Function to replace InF values with NA values ####
# Useful after log2 transformations etc


Inf2NA <- function(x) {

  x[x == Inf] <- NA
  x[x == -Inf] <- NA

  return(x)

}


#### Function for PCA correlation plot ####

library(PCAtools)

eigencorplotPCA <- function(x, metavars="") {

  # Wrapper around the PCAtools eigencorplot to use preferred aesthetics
  # Takes as input PCA object from PCAtools and a vector of metavars to measure correlation with
  # The metavars have to be present in the original metadata provided to the PCA object

  use_cex <- 16/16

  corplot <- PCAtools::eigencorplot(x,
                   metavars = metavars,
                   col = c( "blue2", "blue1", "black", "red1", "red2"),
                   colCorval = "white",
                   scale = TRUE,
                   main = "",
                   plotRsquared = FALSE,
                   cexTitleX= use_cex,
                   cexTitleY= use_cex,
                   cexLabX = use_cex,
                   cexLabY = use_cex,
                   cexMain = use_cex,
                   cexLabColKey = use_cex,
                   cexCorval = use_cex)

return(corplot)

}



#### Function to create PCA plot ####

library(PCAtools)

plotPCA <- function(x, PCs="", colours=NULL, colour.data=NULL, shape.data=NULL, colour.lab="", shape.lab="") {


  if(!is.null(colour.data) & !is.null(shape.data)){

    if(!is.null(colours)){

      df_out <- data.frame(PC1=x$rotated[,PCs[1]], PC2=x$rotated[,PCs[2]])

        pca_plot <- ggplot(df_out, aes(x=PC1, y=PC2, colour = colour.data, shape=shape.data)) +
          geom_point(size = 4.5) +
          xlab(paste0('PC', PCs[1], ': ', round(as.numeric(x$variance[PCs[1]])), '% expl.var')) +
          ylab(paste0('PC', PCs[2], ': ', round(as.numeric(x$variance[PCs[2]])), '% expl.var')) +
          scale_color_gradientn(colours = colours) +
          theme_cowplot() +
          scale_shape_manual(values=c(19,15,17,18)) +
          labs(colour = colour.lab, shape = shape.lab)

      return(pca_plot)

      } else {

        df_out <- data.frame(PC1=x$rotated[,PCs[1]], PC2=x$rotated[,PCs[2]])

        pca_plot <- ggplot(df_out, aes(x=PC1, y=PC2, colour = colour.data, shape=shape.data)) +
          geom_point(size = 4) +
          xlab(paste0('PC', PCs[1], ': ', round(as.numeric(x$variance[PCs[1]])), '% expl.var')) +
          ylab(paste0('PC', PCs[2], ': ', round(as.numeric(x$variance[PCs[2]])), '% expl.var')) +
          scale_color_viridis(option = "plasma") +
          theme_cowplot() +
          scale_shape_manual(values=c(19,15,17,18)) +
          labs(colour = colour.lab, shape = shape.lab)


      return(pca_plot)


    }


  } else if(!is.null(colour.data) & is.null(shape.data)) {

      df_out <- data.frame(PC1=x$rotated[,PCs[1]], PC2=x$rotated[,PCs[2]])

      pca_plot <- ggplot(df_out, aes(x=PC1, y=PC2, colour = colour.data)) +
        geom_point(size = 4) +
        xlab(paste0('PC', PCs[1], ': ', round(as.numeric(x$variance[PCs[1]])), '% expl.var')) +
        ylab(paste0('PC', PCs[2], ': ', round(as.numeric(x$variance[PCs[2]])), '% expl.var')) +
        scale_color_viridis(option = "plasma") +
        theme_cowplot() +
        labs(colour = colour.lab)

      return(pca_plot)


  } else {

      df_out <- data.frame(PC1=x$rotated[,PCs[1]], PC2=x$rotated[,PCs[2]])

      pca_plot <- ggplot(df_out, aes(x=PC1, y=PC2)) +
        geom_point(size = 4) +
        xlab(paste0('PC', PCs[1], ': ', round(as.numeric(miRNA_plasma_pca_res$variance[PCs[1]])), '% expl.var')) +
        ylab(paste0('PC', PCs[2], ': ', round(as.numeric(miRNA_plasma_pca_res$variance[PCs[2]])), '% expl.var')) +
        theme_cowplot()

      return(pca_plot)

  }

}



#### Function to plot sequencing depth correlation with missingness ####

library(R.utils) # function to count zero values

depth_by_missing_scatter <- function(x, read.count="", x.lab="", y.lab="") {

  # 'x' is a matrix with the missing values
  # read.count is a vector containing the sequencing depth for each column in x
  # They need to be in the same order

  df <- data.frame(MissingCount = colSums(isZero(x)), ReadCount = read.count)

  scatter_plot <- ggscatter(df, x = "MissingCount", y = "ReadCount",
                             add = "reg.line", conf.int = TRUE,
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = x.lab, ylab = y.lab)

  return(scatter_plot)

}



#### Function to create histogram ####

gg_histogram <- function(x, title="", xlab="", ylab="") {

  t(x) %>%
    as.data.frame() %>%
    melt() %>%
    ggplot(aes(x=value)) +
    geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
    geom_density(lwd = 1, colour = 2) +
    theme_cowplot() +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title)

}



########################################################################

#### Generic function ####

generic_function <- function(x, pheno=NULL, title="", xlab="", ylab="") {

  # Generic function structure with a few conditions I can edit as needed

  if(!is.null(pheno)==TRUE){

    if (identical(rownames(pheno), colnames(x))) {
      return(print("Condition 1"))
    } else {
      stop("Error message.")
    }

  } else {

    return(print("Condition 2"))

  }

}








