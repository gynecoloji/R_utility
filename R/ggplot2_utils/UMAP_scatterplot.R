# Required packages - auto-install if missing
required_packages <- c("ggplot2", "grid")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Clean UMAP Theme with Legend Text
#'
#' Creates a minimalist ggplot2 theme specifically designed for UMAP plots.
#' This theme removes axes, grid lines, and ticks while preserving legend text
#' for group identification. Ideal for UMAP visualizations where you want to
#' show group labels in the legend.
#'
#' @return A ggplot2 theme object that can be added to any ggplot
#'
#' @details
#' The theme applies the following modifications:
#' \itemize{
#'   \item Sets base text size to 20 for better readability
#'   \item Removes axis lines, grid lines, axis text, and axis ticks
#'   \item Positions axis titles at 10% from origin (hjust = 0.1) with smaller font (size 10)
#'   \item Left-aligns plot title and positions it at plot edge
#'   \item Removes panel background for clean appearance
#'   \item Removes legend key background but preserves legend text
#'   \item Removes legend title for cleaner legend appearance
#' }
#'
#' This theme is particularly suitable for UMAP plots where:
#' - You want to display group/cluster information via legend
#' - Axis values are not meaningful (typical for UMAP dimensions)
#' - Clean, publication-ready appearance is desired
#'
#' @examples
#' # Basic UMAP plot with legend
#' library(ggplot2)
#' ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = cluster)) +
#'   geom_point(size = 2) +
#'   umap_theme_with_texts() +
#'   labs(title = "UMAP Clustering Results")
#'
#' # UMAP with custom colors and legend
#' ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = cell_type)) +
#'   geom_point(size = 1.5, alpha = 0.7) +
#'   scale_color_brewer(type = "qual", palette = "Set2") +
#'   umap_theme_with_texts() +
#'   labs(title = "Cell Type Distribution", x = "UMAP 1", y = "UMAP 2")
#'
#' @seealso 
#' \code{\link{umap_theme_without_texts}} for theme without legend text,
#' \code{\link{add_umap_arrows}} for adding directional arrows
#'
#' @author Your Name
#' @export
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

#' Clean UMAP Theme without Legend Text
#'
#' Creates a minimalist ggplot2 theme specifically designed for UMAP plots
#' without any legend text. This theme provides the cleanest possible
#' appearance by removing all text elements except the plot title and axis labels.
#' Ideal for UMAP visualizations where groupings are evident from visual patterns
#' or when legend text would be redundant.
#'
#' @return A ggplot2 theme object that can be added to any ggplot
#'
#' @details
#' The theme applies the following modifications:
#' \itemize{
#'   \item Sets base text size to 20 for better readability
#'   \item Removes axis lines, grid lines, axis text, and axis ticks
#'   \item Positions axis titles at 10% from origin (hjust = 0.1) with smaller font (size 10)
#'   \item Left-aligns plot title and positions it at plot edge
#'   \item Removes panel background for clean appearance
#'   \item Removes legend title for minimal appearance
#'   \item Note: Preserves legend key (unlike umap_theme_with_texts)
#' }
#'
#' This theme is particularly suitable for:
#' - UMAP plots where color patterns are self-explanatory
#' - Publications requiring minimal visual elements
#' - Presentations where legend text might be distracting
#' - Multi-panel figures where legends are explained in captions
#'
#' @examples
#' # Simple UMAP plot without legend text
#' library(ggplot2)
#' ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = cluster)) +
#'   geom_point(size = 2) +
#'   umap_theme_without_texts() +
#'   labs(title = "UMAP Embedding")
#'
#' # Continuous color mapping without legend labels
#' ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = expression_level)) +
#'   geom_point(size = 1.5) +
#'   scale_color_viridis_c() +
#'   umap_theme_without_texts() +
#'   labs(title = "Gene Expression UMAP", x = "UMAP 1", y = "UMAP 2")
#'
#' @seealso 
#' \code{\link{umap_theme_with_texts}} for theme with legend text,
#' \code{\link{add_umap_arrows}} for adding directional arrows
#'
#' @author Your Name
#' @export
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

#' Add Directional Arrows to UMAP Plots
#'
#' Adds coordinate system arrows to UMAP plots to indicate the direction
#' of UMAP dimensions. This function overlays horizontal and vertical arrows
#' at specified coordinates to help viewers understand the orientation
#' of the UMAP embedding space.
#'
#' @param plot A ggplot object (typically a UMAP plot) to which arrows will be added
#' @param x_min Numeric value specifying the left boundary for arrow placement
#' @param x_max Numeric value specifying the right boundary for arrow placement  
#' @param y_min Numeric value specifying the bottom boundary for arrow placement
#' @param y_max Numeric value specifying the top boundary for arrow placement
#'
#' @return A ggplot object with added directional arrows and fixed aspect ratio
#'
#' @details
#' This function adds two arrows to the plot:
#' \itemize{
#'   \item Horizontal arrow: Points rightward to indicate positive X direction (UMAP1)
#'   \item Vertical arrow: Points upward to indicate positive Y direction (UMAP2)
#'   \item Both arrows are 1 cm long with 0.25 cm closed arrowheads
#'   \item Arrows are black with line width of 2 for clear visibility
#'   \item Automatically applies coord_fixed() for proper aspect ratio
#' }
#'
#' The arrows are positioned as annotation overlays within the specified coordinate
#' boundaries, making them visible regardless of the underlying data distribution.
#' This is particularly useful for UMAP plots where the coordinate system might
#' not be immediately apparent.
#'
#' @examples
#' # Basic UMAP plot with arrows
#' library(ggplot2)
#' p <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = cluster)) +
#'   geom_point(size = 2) +
#'   umap_theme_with_texts() +
#'   labs(title = "UMAP with Direction Arrows")
#' 
#' # Add arrows at the bottom-left corner
#' add_umap_arrows(p, x_min = -8, x_max = -6, y_min = -8, y_max = -6)
#'
#' # Position arrows based on data range
#' x_range <- range(umap_data$UMAP1)
#' y_range <- range(umap_data$UMAP2)
#' add_umap_arrows(p, 
#'                 x_min = x_range[1], x_max = x_range[1] + 2,
#'                 y_min = y_range[1], y_max = y_range[1] + 2)
#'
#' # Multi-panel plot with consistent arrow placement
#' p1 <- ggplot(umap_subset1, aes(x = UMAP1, y = UMAP2, color = condition)) +
#'   geom_point() + umap_theme_without_texts()
#' p2 <- ggplot(umap_subset2, aes(x = UMAP1, y = UMAP2, color = condition)) +
#'   geom_point() + umap_theme_without_texts()
#' 
#' add_umap_arrows(p1, -10, -8, -10, -8)
#' add_umap_arrows(p2, -10, -8, -10, -8)
#'
#' @note
#' - The coord_fixed() is automatically applied to maintain proper aspect ratios
#' - Arrow positioning should be chosen based on your data range to ensure visibility
#' - Arrows will appear in the same location regardless of zoom level
#' - Consider the arrow placement carefully to avoid overlapping with data points
#'
#' @seealso 
#' \code{\link{umap_theme_with_texts}}, \code{\link{umap_theme_without_texts}},
#' \code{\link[ggplot2]{annotation_custom}}, \code{\link[grid]{linesGrob}}
#'
#' @import ggplot2 grid
#' @author Your Name
#' @export
add_umap_arrows <- function(plot, x_start, y_start, x_length = 2, y_length = 2) {
  plot +
    annotate("segment",
             x = x_start, xend = x_start + x_length,
             y = y_start, yend = y_start,
             arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
             linewidth = 1.2, color = "black") +
    annotate("segment",
             x = x_start, xend = x_start,
             y = y_start, yend = y_start + y_length,
             arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
             linewidth = 1.2, color = "black") +
    coord_fixed()
}
