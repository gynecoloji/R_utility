
# Function to check, install, and load packages
check_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c(
  "Seurat",
  "harmony",
  "batchelor",
  "dplyr",
  "stringr",
  "pheatmap",
  "RColorBrewer",
  "ggplot2",
  "ggrepel",
  "SingleR",
  "patchwork",
  "grid",
  "gridExtra",
  "pals",
  "harmony",
  "future",
  "msigdbr",
  "tidyr"
)

suppressPackageStartupMessages({
  check_and_load_packages(required_packages)
})

#' Quality Control Visualization
#' @param seurat_obj Seurat object
#' @param sample_name Name of the sample for plot titles
#' @param output_dir Directory to save plots
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
#' @param seurat_obj Seurat object with QC metrics
#' @param sample_name Name of the sample
#' @param output_dir Directory to save plots
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



#' Process Seurat Object with Batch Correction
#' 
#' This function performs comprehensive batch correction on a Seurat object
#' using various methods available in the Seurat package
#' 
#' @param seurat_obj Seurat object containing single-cell data
#' @param batch_var Character string specifying the batch variable column name in metadata
#' @param method Character string specifying batch correction method:
#'   "harmony" (default), "CCA", "RPCA", "SCTransform (not integration method)", or "fastMNN"
#' @param dims Integer specifying number of dimensions to use (default: 30)
#' @param resolution Numeric value for clustering resolution (default: 0.5)
#' @param vars_to_regress Character vector of variables to regress out during scaling
#' @param verbose Logical indicating whether to print progress messages
#' @param return_uncorrected Logical indicating whether to return uncorrected object for comparison
#' 
#' @return Processed Seurat object with batch correction applied
#' 
#' @examples
#' # Basic usage with Harmony
#' corrected_obj <- process_seurat_batch_correction(seurat_obj, batch_var = "orig.ident")
#' 
#' # Using CCA integration
#' corrected_obj <- process_seurat_batch_correction(
#'   seurat_obj, 
#'   batch_var = "batch", 
#'   method = "CCA",
#'   dims = 20
#' )

process_seurat_batch_correction <- function(
    seurat_obj,
    batch_var,
    method = "harmony",
    dims = 30,
    resolution = 0.5,
    vars_to_regress = NULL,
    verbose = TRUE,
    return_uncorrected = FALSE
) {
  
  # Validate inputs
  if (!inherits(seurat_obj, "Seurat")) {
    stop("Input must be a Seurat object")
  }
  
  if (!batch_var %in% colnames(seurat_obj@meta.data)) {
    stop(paste("Batch variable", batch_var, "not found in metadata"))
  }
  
  method <- match.arg(method, c("harmony", "CCA", "RPCA", "SCTransform", "fastMNN"))
  
  if (verbose) cat("Starting batch correction pipeline...\n")
  
  # Store original object if requested
  if (return_uncorrected) {
    seurat_uncorrected <- seurat_obj
  }
  
  # Basic preprocessing steps
  if (verbose) cat("Step 1: Basic preprocessing\n")
  
  # Calculate mitochondrial gene percentage if not present
  if (!"percent.mt" %in% colnames(seurat_obj@meta.data)) {
    seurat_obj[["percent.mt"]] <- PercentageFeatureSet(seurat_obj, pattern = "^MT-")
  }
  
  # Calculate ribosomal gene percentage
  if (!"percent.rb" %in% colnames(seurat_obj@meta.data)) {
    seurat_obj[["percent.rb"]] <- PercentageFeatureSet(seurat_obj, pattern = "^RP[SL]")
  }
  
  # Quality control filtering (adjust thresholds as needed)
  seurat_obj <- subset(seurat_obj, 
                       subset = nFeature_RNA > 200 & 
                         nFeature_RNA < 5000 & 
                         percent.mt < 20)
  
  if (verbose) cat("Step 2: Normalization and feature selection\n")
  
  # Method-specific processing
  if (method == "SCTransform") {
    # SCTransform handles normalization, scaling, and feature selection
    seurat_obj <- SCTransform(seurat_obj, 
                              vars.to.regress = c("percent.mt", vars_to_regress),
                              verbose = verbose)
  } else {
    # Standard normalization for other methods
    seurat_obj <- NormalizeData(seurat_obj, verbose = verbose)
    seurat_obj <- FindVariableFeatures(seurat_obj, 
                                       selection.method = "vst",
                                       nfeatures = 2000,
                                       verbose = verbose)
    
    # Scale data
    all_genes <- rownames(seurat_obj)
    vars_regress <- c("percent.mt", vars_to_regress)
    seurat_obj <- ScaleData(seurat_obj, 
                            features = all_genes,
                            vars.to.regress = vars_regress,
                            verbose = verbose)
  }
  
  if (verbose) cat("Step 3: Dimensionality reduction\n")
  
  # PCA
  seurat_obj <- RunPCA(seurat_obj, 
                       features = VariableFeatures(object = seurat_obj),
                       verbose = verbose)
  
  # Apply batch correction method
  if (verbose) cat(paste("Step 4: Applying", method, "batch correction\n"))
  
  if (method == "harmony") {
    # Harmony integration
    if (!requireNamespace("harmony", quietly = TRUE)) {
      stop("harmony package is required for this method. Install with: install.packages('harmony')")
    }
    library(harmony)
    
    seurat_obj <- RunHarmony(seurat_obj, 
                             group.by.vars = batch_var,
                             dims.use = 1:dims,
                             verbose = verbose)
    
    # Use harmony embeddings for downstream analysis
    seurat_obj <- RunUMAP(seurat_obj, 
                          reduction = "harmony",
                          dims = 1:dims,
                          verbose = verbose)
    
    seurat_obj <- FindNeighbors(seurat_obj, 
                                reduction = "harmony",
                                dims = 1:dims,
                                verbose = verbose)
    
  } else if (method %in% c("CCA", "RPCA")) {
    # Integration methods (CCA or RPCA)
    
    # Split object by batch
    obj_list <- SplitObject(seurat_obj, split.by = batch_var)
    
    # Find integration anchors
    if (method == "CCA") {
      anchors <- FindIntegrationAnchors(object.list = obj_list, 
                                        dims = 1:dims,
                                        verbose = verbose)
    } else if (method == "RPCA") {
      # Run PCA on each object first
      obj_list <- lapply(obj_list, function(x) {
        x <- RunPCA(x, features = VariableFeatures(x), verbose = FALSE)
        return(x)
      })
      
      anchors <- FindIntegrationAnchors(object.list = obj_list,
                                        reduction = "rpca",
                                        dims = 1:dims,
                                        verbose = verbose)
    }
    
    # Integrate data
    seurat_obj <- IntegrateData(anchorset = anchors, 
                                dims = 1:dims,
                                verbose = verbose)
    
    # Set default assay to integrated
    DefaultAssay(seurat_obj) <- "integrated"
    
    # Scale integrated data
    seurat_obj <- ScaleData(seurat_obj, verbose = verbose)
    
    # Run PCA on integrated data
    seurat_obj <- RunPCA(seurat_obj, verbose = verbose)
    
    # UMAP and clustering
    seurat_obj <- RunUMAP(seurat_obj, 
                          dims = 1:dims,
                          verbose = verbose)
    
    seurat_obj <- FindNeighbors(seurat_obj, 
                                dims = 1:dims,
                                verbose = verbose)
    
  } else if (method == "fastMNN") {
    # fastMNN integration
    if (!requireNamespace("batchelor", quietly = TRUE)) {
      stop("batchelor package is required for fastMNN. Install with: BiocManager::install('batchelor')")
    }
    
    # This would require additional setup for fastMNN
    # Implementation depends on specific requirements
    stop("fastMNN integration requires additional BioConductor setup. Please use other methods.")
    
  }
  
  if (verbose) cat("Step 5: Clustering\n")
  
  # Find clusters
  seurat_obj <- FindClusters(seurat_obj, 
                             resolution = resolution,
                             verbose = verbose)
  
  # Add batch correction method to metadata
  seurat_obj@meta.data$batch_correction_method <- method
  
  if (verbose) {
    cat("Batch correction completed successfully!\n")
    cat("Number of cells:", ncol(seurat_obj), "\n")
    cat("Number of features:", nrow(seurat_obj), "\n")
    cat("Number of clusters:", length(levels(seurat_obj@meta.data$seurat_clusters)), "\n")
  }
  
  # Return results
  if (return_uncorrected) {
    return(list(corrected = seurat_obj, uncorrected = seurat_uncorrected))
  } else {
    return(seurat_obj)
  }
}

#' Plot Batch Correction Results
#' 
#' Generate diagnostic plots to evaluate batch correction effectiveness
#' 
#' @param seurat_obj Processed Seurat object
#' @param batch_var Batch variable used for correction
#' @param plot_type Type of plot: "umap", "pca", or "both"
#' 
#' @return ggplot object or list of ggplot objects

plot_batch_correction <- function(seurat_obj, batch_var, plot_type = "both") {
  
  library(ggplot2)
  
  plots <- list()
  
  if (plot_type %in% c("umap", "both")) {
    # UMAP plot colored by batch
    p1 <- DimPlot(seurat_obj, 
                  reduction = "umap", 
                  group.by = batch_var,
                  pt.size = 0.5) +
      ggtitle(paste("UMAP colored by", batch_var))
    
    # UMAP plot colored by clusters
    p2 <- DimPlot(seurat_obj, 
                  reduction = "umap", 
                  group.by = "seurat_clusters",
                  pt.size = 0.5,
                  label = TRUE) +
      ggtitle("UMAP colored by clusters")
    
    plots$umap_batch <- p1
    plots$umap_clusters <- p2
  }
  
  if (plot_type %in% c("pca", "both")) {
    # PCA plot colored by batch
    p3 <- DimPlot(seurat_obj, 
                  reduction = "pca", 
                  group.by = batch_var,
                  pt.size = 0.5) +
      ggtitle(paste("PCA colored by", batch_var))
    
    plots$pca_batch <- p3
  }
  
  if (plot_type == "both") {
    return(plots)
  } else if (plot_type == "umap") {
    return(list(batch = plots$umap_batch, clusters = plots$umap_clusters))
  } else {
    return(plots$pca_batch)
  }
}
