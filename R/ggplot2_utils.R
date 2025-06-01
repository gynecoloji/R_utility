suppressPackageStartupMessages({
  library(Seurat)
  library(dplyr)
  library(stringr)
  library(pheatmap)
  library(RColorBrewer)
  library(ggplot2)
  library(ggrepel)
  library(SingleR)
  library(patchwork)
  library(grid)
  library(gridExtra)
  library(pals)
  library(harmony)
  library(future)
  library(msigdbr)
  library(tidyr)
})

# umap ------------
# important information extracted from umap
# x_min <- min(Embeddings(immune.combined, "umap")[,1])
# y_min <- min(Embeddings(immune.combined, "umap")[,2])
# x_max <- max(Embeddings(immune.combined, "umap")[,1])
# y_max <- max(Embeddings(immune.combined, "umap")[,2])

# umap theme with texts inside the plot
umap_theme_with_texts <- function() {
  theme(
    text = element_text(size = 20),
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(hjust = 0.1, size = 10),
    axis.title.y = element_text(hjust = 0.1, size = 10),
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )
}

# umap theme without texts inside the plot
umap_theme_without_texts <- function() {
  theme(
    text = element_text(size = 20),
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(hjust = 0.1, size = 10),
    axis.title.y = element_text(hjust = 0.1, size = 10),
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    panel.background = element_blank(),
    legend.title = element_blank()
  )
}


# Function to add arrows to UMAP
add_umap_arrows <- function(plot, x_min, x_max, y_min, y_max) {
  x_arrow <- annotation_custom(
    grob = linesGrob(
      x = unit(c(0, 1), "cm"),
      y = unit(c(0, 0), "cm"),
      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
      gp = gpar(col = "black", lwd = 2, fill="black")
    ),
    xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max
  )
  
  y_arrow <- annotation_custom(
    grob = linesGrob(
      x = unit(c(0, 0), "cm"),
      y = unit(c(0, 1), "cm"),
      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
      gp = gpar(col = "black", lwd = 2, fill="black")
    ),
    xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max
  )
  # Add arrows to plot
  plot + x_arrow + y_arrow +coord_fixed()
}





upper = 0.05
lower = 0
median = (upper+lower)/2
p = ggplot(data = df, 
       aes(x = UMAP_1, y=UMAP_2, 
           color = GO_EMT)
)+
  geom_point(alpha = .8, size=3)+
  scale_color_viridis_c(limits = c(0,upper),
                        breaks = seq(0,upper,median),
                        oob = function(x, ...){scales::squish(x, c(0,upper))})+
  umap_theme_without_texts()

add_umap_arrows(p, x_min, x_max, y_min, y_max)




# Function for barplot theme
barplot_theme <- function() {
  theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black", 
                               arrow = arrow(length = unit(0.25, "cm"), type = "closed")),
      text = element_text(size = 20)
    )
}

# Function for boxplot theme
boxplot_theme <- function() {
  theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black", 
                               arrow = arrow(length = unit(0.25, "cm"), type = "closed")),
      text = element_text(size = 20)
    )
}




