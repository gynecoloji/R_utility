# Load required library
library(pheatmap)

# Function to calculate gap positions for heatmap
calculate_gaps <- function(categories) {
  # Get frequency table
  freq_table <- table(categories)
  
  # Calculate positions
  positions <- numeric(length(freq_table))
  running_sum <- 0
  
  for(i in 1:length(freq_table)) {
    running_sum <- running_sum + freq_table[i]
    positions[i] <- running_sum
  }
  
  return(positions)
}

#' Create a customizable heatmap using pheatmap
#'
#' @param data A numeric matrix or data frame for the heatmap
#' @param title Character string for the plot title
#' @param cluster_rows Logical, whether to cluster rows (default: TRUE)
#' @param cluster_cols Logical, whether to cluster columns (default: TRUE)
#' @param show_rownames Logical, whether to show row names (default: TRUE)
#' @param show_colnames Logical, whether to show column names (default: TRUE)
#' @param color_palette Color palette for the heatmap (default: NULL uses pheatmap default)
#' @param scale Character, scale data by "row", "column", or "none" (default: "none")
#' @param annotation_row Data frame for row annotations (default: NULL)
#' @param annotation_col Data frame for column annotations (default: NULL)
#' @param annotation_colors List of named vectors specifying colors for annotations (default: NULL)
#' @param row_gap_categories Vector for calculating row gaps based on categories (default: NULL)
#' @param col_gap_categories Vector for calculating column gaps based on categories (default: NULL)
#' @param fontsize Numeric, base font size (default: 10)
#' @param save_plot Logical, whether to save the plot (default: FALSE)
#' @param filename Character, filename if saving plot (default: "heatmap.png")
#' @param width Numeric, width of saved plot in inches (default: 7)
#' @param height Numeric, height of saved plot in inches (default: 5)
#' @param ... Additional arguments passed to pheatmap()
#'
#' @return A pheatmap object
#' @export
plot_heatmap <- function(data,
                         title = "Heatmap",
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         color_palette = NULL,
                         scale = "none",
                         annotation_row = NULL,
                         annotation_col = NULL,
                         annotation_colors = NULL,
                         row_gap_categories = NULL,
                         col_gap_categories = NULL,
                         fontsize = 10,
                         save_plot = FALSE,
                         filename = "heatmap.png",
                         width = 7,
                         height = 5,
                         ...) {
  
  # Check if data is numeric
  if (!is.numeric(as.matrix(data))) {
    stop("Data must be numeric for heatmap plotting")
  }
  
  # Convert to matrix if data frame
  if (is.data.frame(data)) {
    data_matrix <- as.matrix(data)
  } else {
    data_matrix <- data
  }
  
  # Set default color palette if none provided
  if (is.null(color_palette)) {
    color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
  }
  
  # Calculate gaps if categories are provided
  gaps_row <- NULL
  gaps_col <- NULL
  
  if (!is.null(row_gap_categories)) {
    if (cluster_rows) {
      warning("Row clustering is enabled but row gaps are specified. Consider setting cluster_rows=FALSE for meaningful gaps.")
    }
    # Remove the last position (total length) to get gap positions
    gaps_row <- row_gap_categories
  }
  
  if (!is.null(col_gap_categories)) {
    if (cluster_cols) {
      warning("Column clustering is enabled but column gaps are specified. Consider setting cluster_cols=FALSE for meaningful gaps.")
    }
    # Remove the last position (total length) to get gap positions
    gaps_col <- col_gap_categories
  }
  
  # Create the heatmap
  heatmap_plot <- pheatmap(
    data_matrix,
    main = title,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    show_rownames = show_rownames,
    show_colnames = show_colnames,
    color = color_palette,
    scale = scale,
    annotation_row = annotation_row,
    annotation_col = annotation_col,
    annotation_colors = annotation_colors,
    gaps_row = gaps_row,
    gaps_col = gaps_col,
    fontsize = fontsize,
    filename = if (save_plot) filename else NA,
    width = width,
    height = height,
    ...
  )
  
  return(heatmap_plot)
}

# Example usage function
demo_heatmap <- function() {
  # Create sample data
  set.seed(123)
  sample_data <- matrix(rnorm(200), nrow = 20, ncol = 10)
  rownames(sample_data) <- paste("Gene", 1:20, sep = "_")
  colnames(sample_data) <- paste("Sample", 1:10, sep = "_")
  
  # Create sample annotations
  col_annotation <- data.frame(
    Group = rep(c("Control", "Treatment"), each = 5),
    Batch = rep(c("A", "B"), times = 5)
  )
  rownames(col_annotation) <- colnames(sample_data)
  
  row_annotation <- data.frame(
    Category = sample(c("Type1", "Type2", "Type3"), 20, replace = TRUE)
  )
  rownames(row_annotation) <- rownames(sample_data)
  
  # Create custom annotation colors
  custom_annotation_colors <- list(
    Group = c("Control" = "#3498db", "Treatment" = "#e74c3c"),
    Batch = c("A" = "#2ecc71", "B" = "#f39c12"),
    Category = c("Type1" = "#9b59b6", "Type2" = "#1abc9c", "Type3" = "#34495e")
  )
  
  # Plot basic heatmap
  plot1 <- plot_heatmap(
    data = sample_data,
    title = "Basic Heatmap"
  )
  
  # Plot annotated heatmap with scaling and custom colors
  plot2 <- plot_heatmap(
    data = sample_data,
    title = "Annotated Heatmap with Custom Colors",
    scale = "row",
    annotation_col = col_annotation,
    annotation_row = row_annotation,
    annotation_colors = custom_annotation_colors,
    fontsize = 8
  )
  
  # Plot with custom colors and gaps
  row_categories <- sample(c("Group1", "Group2", "Group3"), 20, replace = TRUE)
  col_categories <- rep(c("TypeA", "TypeB"), each = 5)
  
  custom_colors <- colorRampPalette(c("green", "black", "red"))(100)
  plot3 <- plot_heatmap(
    data = sample_data,
    title = "Custom Colors with Gaps",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    color_palette = custom_colors,
    row_gap_categories = row_categories,
    col_gap_categories = col_categories,
    show_rownames = FALSE
  )
  
  cat("Demo heatmaps created successfully!\n")
  return(list(basic = plot1, annotated_custom = plot2, gaps = plot3))
}

# Helper function to create custom color palettes for annotations
create_annotation_colors <- function(annotation_df, color_schemes = NULL) {
  #' Create custom color palettes for pheatmap annotations
  #'
  #' @param annotation_df Data frame with annotation categories
  #' @param color_schemes Named list of color vectors for specific columns (optional)
  #' @return List of named color vectors for each annotation column
  
  annotation_colors <- list()
  
  for (col_name in colnames(annotation_df)) {
    unique_values <- unique(annotation_df[[col_name]])
    
    if (!is.null(color_schemes) && col_name %in% names(color_schemes)) {
      # Use provided color scheme
      colors <- color_schemes[[col_name]]
      if (length(colors) < length(unique_values)) {
        warning(paste("Not enough colors provided for", col_name, ". Using default colors for remaining values."))
        additional_colors <- rainbow(length(unique_values) - length(colors))
        colors <- c(colors, additional_colors)
      }
    } else {
      # Generate default colors
      if (length(unique_values) <= 8) {
        # Use predefined palette for small number of categories
        default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", 
                            "#9b59b6", "#1abc9c", "#34495e", "#e67e22")
        colors <- default_colors[1:length(unique_values)]
      } else {
        # Use rainbow colors for many categories
        colors <- rainbow(length(unique_values))
      }
    }
    
    names(colors) <- unique_values
    annotation_colors[[col_name]] <- colors
  }
  
  return(annotation_colors)
}
# Quick plot function for simple use cases
quick_heatmap <- function(data, title = "Heatmap") {
  plot_heatmap(
    data = data,
    title = title,
    fontsize = 8,
    show_rownames = nrow(data) <= 50,  # Hide row names if too many rows
    show_colnames = ncol(data) <= 50   # Hide col names if too many columns
  )
}