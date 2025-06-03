# Focused General Marker-Based Cell Type Annotation
# Required libraries
library(Seurat)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(viridis)

#' Get focused marker genes for major cell types
#'
#' @return List of marker genes for epithelial, stromal, and immune cells
#' @export
get_focused_markers <- function() {
  markers <- list(
    # Epithelial cell markers
    Epithelial = c("EPCAM", "CDH1", "KRT8", "KRT18", "KRT19", "CLDN1", "OCLN"),
    
    # Stromal cell markers
    Fibroblast = c("COL1A1", "COL1A2", "COL3A1", "VIM", "PDGFRA", "FAP", "DCN"),
    Endothelial = c("PECAM1", "VWF", "CDH5", "ENG", "TEK", "PLVAP", "KDR"),
    
    # Immune cell markers
    T_cell = c("CD3D", "CD3E", "CD3G", "CD2", "CD7", "IL7R", "TRAC"),
    B_cell = c("CD19", "MS4A1", "CD79A", "CD79B", "PAX5", "BCL11A", "BANK1"),
    NK_cell = c("KLRB1", "KLRD1", "KLRF1", "NCR1", "GNLY", "NKG7", "GZMB"),
    Myeloid = c("CD68", "CD14", "LYZ", "CSF1R", "MARCO", "S100A8", "S100A9")
  )
  
  return(markers)
}

#' Calculate expression scores for marker genes
#'
#' @param seurat_obj Seurat object
#' @param markers List of marker genes
#' @param assay Assay to use (default: "RNA")
#' @param slot Data slot to use (default: "data")
#' @param method Scoring method ("mean", "median", "sum")
#'
#' @return Matrix of expression scores (cells x cell_types)
calculate_expression_scores <- function(seurat_obj, 
                                        markers, 
                                        assay = "RNA", 
                                        slot = "data",
                                        method = "mean") {
  
  # Get expression data
  expr_data <- GetAssayData(seurat_obj, assay = assay, slot = slot)
  
  # Initialize score matrix
  n_cells <- ncol(expr_data)
  n_types <- length(markers)
  score_matrix <- matrix(0, nrow = n_cells, ncol = n_types)
  rownames(score_matrix) <- colnames(expr_data)
  colnames(score_matrix) <- names(markers)
  
  # Calculate scores for each cell type
  for (cell_type in names(markers)) {
    marker_genes <- markers[[cell_type]]
    
    # Find genes present in the dataset
    present_genes <- intersect(marker_genes, rownames(expr_data))
    
    if (length(present_genes) > 0) {
      message(paste("Found", length(present_genes), "out of", length(marker_genes), 
                    "markers for", cell_type))
      message(paste("Present genes:", paste(present_genes, collapse = ", ")))
      
      # Calculate scores based on method
      if (length(present_genes) == 1) {
        scores <- as.numeric(expr_data[present_genes, ])
      } else {
        scores <- switch(method,
          "mean" = colMeans(expr_data[present_genes, , drop = FALSE]),
          "median" = apply(expr_data[present_genes, , drop = FALSE], 2, median),
          "sum" = colSums(expr_data[present_genes, , drop = FALSE])
        )
      }
      
      score_matrix[, cell_type] <- scores
      
    } else {
      warning(paste("No markers found for", cell_type, "- genes not present in dataset"))
      message(paste("Missing genes:", paste(marker_genes, collapse = ", ")))
    }
  }
  
  return(score_matrix)
}

#' Annotate cells using focused general markers
#'
#' @param seurat_obj Seurat object
#' @param markers List of marker genes (optional, uses defaults if NULL)
#' @param method Annotation method ("highest_score", "threshold_single", "threshold_multi")
#' @param threshold Minimum score threshold (for threshold methods)
#' @param score_method Method for calculating marker scores ("mean", "median", "sum")
#' @param assay Assay to use (default: "RNA")
#' @param slot Data slot to use (default: "data")
#' @param min_score_diff Minimum difference between top two scores for confident assignment
#' @param add_to_metadata Whether to add results to Seurat object metadata
#' @param metadata_prefix Prefix for metadata column names
#'
#' @return Seurat object with annotations or list of results
#' @export
annotate_focused_cell_types <- function(seurat_obj, 
                                         markers = NULL,
                                         method = "highest_score",
                                         threshold = 0.1,
                                         score_method = "mean",
                                         assay = "RNA",
                                         slot = "data",
                                         min_score_diff = 0.05,
                                         add_to_metadata = TRUE,
                                         metadata_prefix = "focused_celltype") {
  
  # Use default markers if not provided
  if (is.null(markers)) {
    markers <- get_focused_markers()
    message("Using default focused markers for 7 cell types")
  }
  
  # Calculate marker scores
  message("Calculating expression scores...")
  score_matrix <- calculate_expression_scores(seurat_obj, markers, assay, slot, score_method)
  
  # Perform annotation based on method
  message(paste("Performing annotation using method:", method))
  
  annotations <- switch(method,
    "highest_score" = {
      # Assign cell type with highest score
      apply(score_matrix, 1, function(x) {
        if (all(x == 0)) {
          return("Unknown")
        }
        
        # Check if there's a clear winner
        sorted_scores <- sort(x, decreasing = TRUE)
        if (length(sorted_scores) > 1 && 
            (sorted_scores[1] - sorted_scores[2]) < min_score_diff) {
          return("Ambiguous")
        }
        
        max_idx <- which.max(x)
        return(names(x)[max_idx])
      })
    },
    
    "threshold_single" = {
      # Assign highest scoring cell type if above threshold
      apply(score_matrix, 1, function(x) {
        max_score <- max(x)
        if (max_score > threshold) {
          max_idx <- which.max(x)
          return(names(x)[max_idx])
        } else {
          return("Unknown")
        }
      })
    },
    
    "threshold_multi" = {
      # Assign all cell types above threshold
      apply(score_matrix, 1, function(x) {
        above_threshold <- x > threshold
        if (any(above_threshold)) {
          cell_types <- names(x)[above_threshold]
          return(paste(cell_types, collapse = ";"))
        } else {
          return("Unknown")
        }
      })
    }
  )
  
  # Calculate confidence scores
  confidence_scores <- apply(score_matrix, 1, function(x) {
    if (all(x == 0)) return(0)
    sorted_scores <- sort(x, decreasing = TRUE)
    if (length(sorted_scores) == 1) return(1)
    return((sorted_scores[1] - sorted_scores[2]) / sorted_scores[1])
  })
  
  # Create summary statistics
  annotation_summary <- table(annotations)
  message("\nAnnotation Summary:")
  print(annotation_summary)
  
  # Calculate mean scores per cell type
  mean_scores <- colMeans(score_matrix)
  message("\nMean expression scores per cell type:")
  print(round(mean_scores, 3))
  
  # Identify top expressing cells for each type
  top_cells <- list()
  for (cell_type in colnames(score_matrix)) {
    top_idx <- order(score_matrix[, cell_type], decreasing = TRUE)[1:5]
    top_cells[[cell_type]] <- rownames(score_matrix)[top_idx]
  }
  
  # Create results list
  results <- list(
    annotations = annotations,
    scores = score_matrix,
    confidence = confidence_scores,
    summary = annotation_summary,
    mean_scores = mean_scores,
    top_cells = top_cells,
    method = method,
    parameters = list(
      threshold = threshold,
      score_method = score_method,
      min_score_diff = min_score_diff
    )
  )
  
  # Add to Seurat object metadata if requested
  if (add_to_metadata) {
    # Main annotation column
    seurat_obj@meta.data[[paste0(metadata_prefix, "_annotation")]] <- annotations
    seurat_obj@meta.data[[paste0(metadata_prefix, "_confidence")]] <- confidence_scores
    
    # Individual cell type scores
    for (i in 1:ncol(score_matrix)) {
      score_name <- paste0(metadata_prefix, "_score_", colnames(score_matrix)[i])
      seurat_obj@meta.data[[score_name]] <- score_matrix[, i]
    }
    
    message(paste("\nAdded", ncol(score_matrix) + 2, "columns to Seurat object metadata"))
    return(seurat_obj)
  } else {
    return(results)
  }
}
