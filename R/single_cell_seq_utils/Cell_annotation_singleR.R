# Optimized SingleR Annotation and Heatmap Function
# Required libraries
library(SingleR)
library(celldex)
library(pheatmap)
library(viridis)

#' Perform SingleR annotation and create heatmap
#'
#' @param seurat_obj Seurat object containing single-cell data
#' @param reference_data Reference dataset for annotation (default: HumanPrimaryCellAtlasData)
#' @param cluster_column Column name in meta.data containing cluster information (default: "seurat_clusters")
#' @param output_file Path for output PDF file (optional)
#' @param width PDF width in inches (default: 12)
#' @param height PDF height in inches (default: 14)
#' @param cluster_colors Named vector of colors for clusters (optional)
#' @param show_scores Logical, whether to return scores data frame (default: FALSE)
#' @param fontsize Font size for heatmap (default: 12)
#' @param viridis_option Viridis color palette option (default: "D")
#' @param n_colors Number of colors in palette (default: 20)
#'
#' @return List containing prediction results and optionally scores data frame
#' @export
annotate_and_plot_singleR <- function(seurat_obj, 
                                       reference_data = NULL,
                                       cluster_column = "seurat_clusters",
                                       output_file = NULL,
                                       width = 12,
                                       height = 14,
                                       cluster_colors = NULL,
                                       show_scores = FALSE,
                                       fontsize = 12,
                                       viridis_option = "D",
                                       n_colors = 20) {
  
  # Input validation
  if (!inherits(seurat_obj, "Seurat")) {
    stop("seurat_obj must be a Seurat object")
  }
  
  if (!cluster_column %in% colnames(seurat_obj@meta.data)) {
    stop(paste("Column", cluster_column, "not found in seurat object metadata"))
  }
  
  # Load reference data if not provided
  if (is.null(reference_data)) {
    message("Loading HumanPrimaryCellAtlasData...")
    reference_data <- HumanPrimaryCellAtlasData()
  }
  
  # Perform SingleR annotation
  message("Performing SingleR annotation...")
  pred_results <- SingleR(
    test = as.matrix(seurat_obj@assays$RNA@data), 
    ref = reference_data,
    labels = reference_data$label.main
  )
  
  # Print summary statistics
  message("First labels distribution:")
  print(table(pred_results$first.labels))
  message("\nPruned labels distribution:")
  print(table(pred_results$pruned.labels))
  
  # Prepare data for heatmap
  scores_df <- pred_results$scores
  rownames(scores_df) <- colnames(seurat_obj@assays$RNA@data)
  
  # Create temporary dataframe with cluster information
  heatmap_data <- as.data.frame(scores_df)
  heatmap_data$Cluster <- seurat_obj@meta.data[[cluster_column]]
  
  # Order by cluster
  heatmap_data <- heatmap_data[order(heatmap_data$Cluster), ]
  
  # Prepare annotation
  annotation_col <- data.frame(
    row.names = rownames(heatmap_data), 
    Cluster = heatmap_data$Cluster
  )
  
  # Set up annotation colors
  annotation_colors <- list()
  if (!is.null(cluster_colors)) {
    annotation_colors$Cluster <- cluster_colors
  }
  
  # Create heatmap
  create_heatmap <- function() {
    pheatmap(
      t(heatmap_data[, -ncol(heatmap_data)]),
      show_colnames = FALSE,
      cluster_cols = FALSE,
      annotation_col = annotation_col,
      annotation_colors = if(length(annotation_colors) > 0) annotation_colors else NULL,
      scale = "column",
      color = viridis(n_colors, option = viridis_option),
      fontsize = fontsize,
      main = "SingleR Cell Type Annotation Scores"
    )
  }
  
  # Save to PDF if output file specified
  if (!is.null(output_file)) {
    message(paste("Saving heatmap to:", output_file))
    
    # Create directory if it doesn't exist
    output_dir <- dirname(output_file)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    pdf(file = output_file, width = width, height = height)
    create_heatmap()
    dev.off()
    message("Heatmap saved successfully!")
  } else {
    # Display heatmap
    create_heatmap()
  }
  
  # Prepare return object
  return_list <- list(
    predictions = pred_results,
    first_labels_table = table(pred_results$first.labels),
    pruned_labels_table = table(pred_results$pruned.labels)
  )
  
  if (show_scores) {
    return_list$scores_data <- heatmap_data
  }
  
  return(return_list)
}

# Example usage function
#' Example usage of the SingleR annotation function
#'
#' @param seurat_obj Seurat object
#' @param output_dir Output directory for results
#' @param sample_name Sample name for file naming
#'
#' @return Results from annotate_and_plot_singleR
#' @export
example_singleR_analysis <- function(seurat_obj, output_dir = "../analysis/clustering", sample_name = "sample") {
  
  # Define custom colors (example)
  n_clusters <- length(unique(seurat_obj@meta.data$seurat_clusters))
  custom_colors <- rainbow(n_clusters)
  names(custom_colors) <- sort(unique(seurat_obj@meta.data$seurat_clusters))
  
  # Create output file path
  output_file <- file.path(output_dir, paste0("SingleR_heatmap_", sample_name, ".pdf"))
  
  # Run analysis
  results <- annotate_and_plot_singleR(
    seurat_obj = seurat_obj,
    output_file = output_file,
    cluster_colors = custom_colors,
    width = 12,
    height = 14,
    fontsize = 14,
    show_scores = TRUE
  )
  
  return(results)
}

# Utility function to compare different reference datasets
#' Compare annotations using different reference datasets
#'
#' @param seurat_obj Seurat object
#' @param reference_list List of reference datasets
#' @param output_dir Output directory
#'
#' @return List of results for each reference
#' @export
compare_references <- function(seurat_obj, reference_list, output_dir = "../analysis/clustering") {
  
  results_list <- list()
  
  for (ref_name in names(reference_list)) {
    message(paste("Processing reference:", ref_name))
    
    output_file <- file.path(output_dir, paste0("SingleR_", ref_name, "_heatmap.pdf"))
    
    results_list[[ref_name]] <- annotate_and_plot_singleR(
      seurat_obj = seurat_obj,
      reference_data = reference_list[[ref_name]],
      output_file = output_file,
      fontsize = 10  # Smaller font for comparison plots
    )
  }
  
  return(results_list)
}

# Example of how to use with multiple references:
# refs <- list(
#   HPCA = HumanPrimaryCellAtlasData(),
#   BlueprintEncode = BlueprintEncodeData(),
#   MonacoImmune = MonacoImmuneData()
# )
# comparison_results <- compare_references(seurat_obj, refs)



