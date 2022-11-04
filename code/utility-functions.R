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

gg_boxplot <- function(x, log.transform=T, title="", xlab="", ylab="") {

  # Function to create a ggplot2 boxplot from a matrix
  # Intended to have similar functionality to the default boxplot function
  # Will log2 transform the data by default
  # Axis labels and titles can be provided

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



## Function to make a ggplot2 boxplot with phenotype information ####

gg_boxplot_pheno <- function(x, title="", xlab="", ylab="", pheno=pheno, pheno.lab="") {

    t(log2(x)) %>%
      as.data.frame() %>%
      mutate(Pheno = as.factor(pheno)) %>%
      rownames_to_column("Sample_ID") %>%
      melt(., id.vars=c("Sample_ID", "Pheno")) %>%
      ggplot(aes(x=Sample_ID, y=value, fill=Pheno)) +
      geom_boxplot() + theme_cowplot() +
      theme(axis.text.x = element_text(angle = 90, size = 8)) +
      xlab(xlab) +
      ylab(ylab) +
      labs(fill=pheno.lab) +
      ggtitle(title)

}









