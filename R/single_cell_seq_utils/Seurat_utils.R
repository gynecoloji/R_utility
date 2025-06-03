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

#' Quality Control Visualization
#' 
#' @param seurat_obj Seurat object
#' @param sample_name Name of the sample for plot titles
#' @param output_dir Directory to save plots
#' 
#' @return The Seurat object with QC metrics added
plot_qc_metrics <- function(seurat_obj, sample_name, output_dir) {
  # Create output directory if it doesn't exist
  dir.create(file.path(output_dir, sample_name), showWarnings = FALSE, recursive = TRUE)
  
  # Add QC metrics
  seurat_obj$log10GenesPerUMI <- log10(seurat_obj$nFeature_RNA)/log10(seurat_obj$nCount_RNA)
  seurat_obj$mitoRatio <- PercentageFeatureSet(object = seurat_obj, pattern = "^MT-")
  
  df <- seurat_obj@meta.data
  df <- df %>% dplyr::rename(nUMI = nCount_RNA, nGene = nFeature_RNA)
  df$sample <- sample_name
  
  # UMI count distribution
  p1 <- ggplot(df, aes(color=sample, x=nUMI, fill=sample)) + 
    geom_density(alpha = 0.2) + 
    scale_x_log10() + 
    theme_classic() +
    ylab("Cell density") +
    geom_vline(xintercept = 500)
  ggsave(file.path(output_dir, sample_name, paste0("QC_Cell_Density_UMI.pdf")),
         plot = p1, width = 8, height = 7)
  
  # Gene count distribution
  p2 <- df %>% 
    ggplot(aes(color=sample, x=nGene, fill=sample)) + 
    geom_density(alpha = 0.2) + 
    theme_classic() +
    scale_x_log10() + 
    geom_vline(xintercept = 300)
  ggsave(file.path(output_dir, sample_name, paste0("QC_Cell_Density_genes.pdf")),
         plot = p2, width = 8, height = 7)
  
  # Mitochondrial ratio distribution
  p3 <- df %>% 
    ggplot(aes(color=sample, x=mitoRatio, fill=sample)) + 
    geom_density(alpha = 0.2) +
    theme_classic() +
    geom_vline(xintercept = 25)
  ggsave(file.path(output_dir, sample_name, paste0("QC_Cell_Density_mitoRatio.pdf")),
         plot = p3, width = 8, height = 7)
  
  # Scatter plot of nUMI vs nGene colored by mitoRatio
  p4 <- df %>% 
    ggplot(aes(x=nUMI, y=nGene, color=mitoRatio)) + 
    geom_point(aes(size=log10GenesPerUMI)) + 
    scale_colour_gradient(low = "gray90", high = "black") +
    geom_smooth(method = "lm") +
    scale_x_log10() + 
    scale_y_log10() + 
    theme_classic() +
    geom_vline(xintercept = 500) +
    geom_hline(yintercept = 300)
  ggsave(file.path(output_dir, sample_name, paste0("QC_Cell_correlations.pdf")),
         plot = p4, width = 8, height = 7)
  
  # Gene complexity
  p5 <- df %>%
    ggplot(aes(x=log10GenesPerUMI, color=sample, fill=sample)) +
    geom_density(alpha = 0.2) +
    theme_classic() +
    geom_vline(xintercept = 0.8)
  ggsave(file.path(output_dir, sample_name, paste0("QC_Cell_complexity.pdf")),
         plot = p5, width = 8, height = 7)
  
  return(seurat_obj)
}


#' Filter cells and genes based on QC metrics
#' 
#' @param seurat_obj Seurat object with QC metrics
#' @param sample_name Name of the sample
#' @param output_dir Directory to save plots
#' 
#' @return Filtered Seurat object
filter_cells_and_genes <- function(seurat_obj, sample_name, output_dir) {
  # Cell-level filtering
  seurat_filtered <- subset(
    x = seurat_obj, 
    subset = (nCount_RNA >= 500) & 
      (nFeature_RNA >= 300) & 
      (log10GenesPerUMI > 0.80) & 
      (mitoRatio < 25)
  )
  
  # Gene-level filtering
  counts <- GetAssayData(object = seurat_filtered, slot = "counts")
  nonzero <- counts > 0
  
  # Plot gene expression distribution
  df <- data.frame(
    Gene_in_nCells = Matrix::rowSums(nonzero),
    sample = sample_name
  )
  
  p <- df %>% 
    ggplot(aes(color=sample, x=Gene_in_nCells, fill=sample)) + 
    geom_density(alpha = 0.2) + 
    theme_classic() + 
    geom_vline(xintercept = 10)
  ggsave(file.path(output_dir, sample_name, paste0("QC_genes_density.pdf")),
         plot = p, width = 8, height = 7)
  
  # Keep genes expressed in at least 10 cells
  keep_genes <- Matrix::rowSums(nonzero) >= 10
  filtered_counts <- counts[keep_genes, ]
  
  # Create new Seurat object with filtered data
  seurat_filtered <- CreateSeuratObject(
    filtered_counts, 
    meta.data = seurat_filtered@meta.data
  )
  
  return(seurat_filtered)
}


#' Process and cluster Seurat object
#' 
#' @param seurat_object Seurat object
#' @param npca Number of PCs to compute
#' @param ndim Number of dimensions to use for clustering
#' @param resolution Resolution parameter for clustering
#' @param var_to_regress Variables to regress out
#' 
#' @return Processed Seurat object
process_seurat <- function(seurat_object, npca=50, ndim=50, resolution=0.5, 
                           var_to_regress=c("G2M.Score","S.Score","nCount_RNA","percent.mt")) {
  
  # Add mitochondrial percentage
  seurat_object[["percent.mt"]] <- PercentageFeatureSet(seurat_object, pattern = "^MT-")
  
  # Normalize data
  seurat_object <- NormalizeData(
    seurat_object, 
    normalization.method = "LogNormalize", 
    scale.factor = 10000
  )
  
  # Find variable features
  seurat_object <- FindVariableFeatures(
    seurat_object, 
    selection.method = "vst", 
    nfeatures = 3000
  )
  
  # Cell cycle scoring
  s.genes <- cc.genes$s.genes
  g2m.genes <- cc.genes$g2m.genes
  seurat_object <- CellCycleScoring(
    seurat_object, 
    s.features = s.genes, 
    g2m.features = g2m.genes, 
    set.ident = TRUE
  )
  
  # Scale data
  all.genes <- rownames(seurat_object)
  seurat_object <- ScaleData(
    seurat_object, 
    features = all.genes, 
    vars.to.regress = var_to_regress
  )
  
  # Run PCA
  seurat_object <- RunPCA(
    seurat_object, 
    features = VariableFeatures(object = seurat_object), 
    npcs = npca
  )
  
  # Determine significant PCs
  seurat_object <- JackStraw(seurat_object, num.replicate = 100, dims = npca)
  seurat_object <- ScoreJackStraw(seurat_object, dims = 1:npca)
  
  # Find neighbors and clusters
  seurat_object <- FindNeighbors(seurat_object, dims = 1:ndim)
  seurat_object <- FindClusters(seurat_object, resolution = resolution)
  
  # Run dimensionality reduction
  seurat_object <- RunUMAP(seurat_object, dims = 1:ndim)
  seurat_object <- RunTSNE(seurat_object, dims = 1:ndim)
  
  return(seurat_object)
}































