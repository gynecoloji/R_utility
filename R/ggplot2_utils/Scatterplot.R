#' Create Simple Scatterplots with ggplot2
#'
#' This function generates clean, customizable scatterplots using ggplot2.
#' It supports color grouping, trend lines, and various styling options
#' while maintaining simplicity for quick data exploration.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (continuous variable).
#'              Required parameter.
#' @param y_var Character string specifying the column name for the y-axis (continuous variable).
#'              Required parameter.
#' @param color_var Character string specifying the column name for color grouping.
#'                  Default: NULL
#' @param size_var Character string specifying the column name for point size mapping.
#'                 Default: NULL
#' @param shape_var Character string specifying the column name for point shape mapping.
#'                  Default: NULL
#' @param add_smooth Logical indicating whether to add a trend line (loess/linear).
#'                   Default: FALSE
#' @param smooth_method Character string specifying smoothing method: "loess", "lm", "gam".
#'                      Default: "loess"
#' @param smooth_se Logical indicating whether to show confidence interval around trend line.
#'                  Default: TRUE
#' @param smooth_se_group Logical indicating whether to show confidence interval around trend line for each group.
#'                  Default: FALSE
#' @param point_size Numeric value specifying the size of points (when size_var is NULL).
#'                   Default: 2
#' @param point_alpha Numeric value (0-1) controlling transparency of points.
#'                    Default: 0.7
#' @param title Character string for the plot title. Default: ""
#' @param subtitle Character string for the plot subtitle. Default: NULL
#' @param x_label Character string for the x-axis label. Default: NULL (uses x_var)
#' @param y_label Character string for the y-axis label. Default: NULL (uses y_var)
#' @param color_palette Character string or vector specifying color palette.
#'                     Options: "viridis", "plasma", "Set1", "Set2", or custom vector.
#'                     Default: "Set2"
#' @param theme_style Character string specifying ggplot2 theme: "minimal", "classic", 
#'                    "bw", "dark". Default: "minimal"
#' @param legend_position Character string specifying legend position: "top", "bottom",
#'                        "left", "right", or "none". Default: "right"
#' @param facet_var Character string specifying the column name for faceting.
#'                  Default: NULL
#' @param facet_type Character string specifying facet type: "wrap" or "grid".
#'                   Default: "wrap"
#' @param x_limits Numeric vector of length 2 specifying x-axis limits. Default: NULL
#' @param y_limits Numeric vector of length 2 specifying y-axis limits. Default: NULL
#' @param add_correlation Logical indicating whether to add correlation coefficient.
#'                        Default: FALSE
#' @param custom_theme Function specifying custom ggplot2 theme. Default: NULL
#'
#' @return A ggplot object with scatterplot visualization
#'
#' @examples
#' # Basic scatterplot
#' simple_scatterplot(mtcars, x_var = "wt", y_var = "mpg", 
#'                    title = "Weight vs MPG")
#'
#' # Scatterplot with color grouping and trend line
#' simple_scatterplot(iris, x_var = "Sepal.Length", y_var = "Sepal.Width",
#'                    color_var = "Species", add_smooth = TRUE,
#'                    title = "Sepal Dimensions by Species")
#'
#' # Advanced scatterplot with multiple aesthetics
#' simple_scatterplot(mtcars, x_var = "hp", y_var = "mpg",
#'                    color_var = "factor(cyl)", size_var = "wt",
#'                    add_smooth = TRUE, smooth_method = "lm",
#'                    add_correlation = TRUE)
#'
#' # Faceted scatterplot
#' mtcars$gear_factor <- as.factor(mtcars$gear)
#' simple_scatterplot(mtcars, x_var = "wt", y_var = "mpg",
#'                    color_var = "gear_factor", facet_var = "gear_factor",
#'                    add_smooth = TRUE)
#'
#' @details
#' This function creates publication-ready scatterplots with options for:
#' \itemize{
#'   \item Color, size, and shape mapping for multiple variables
#'   \item Trend lines with confidence intervals
#'   \item Correlation coefficient display
#'   \item Flexible theming and color palettes
#'   \item Faceting for grouped comparisons
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_smooth}}
#' @import ggplot2 dplyr
#' @export
simple_scatterplot_categorical <- function(data,
                               x_var,
                               y_var,
                               color_var = NULL,
                               size_var = NULL,
                               shape_var = NULL,
                               add_smooth = FALSE,
                               smooth_method = "loess",
                               smooth_se = TRUE,
                               smooth_se_group = FALSE,
                               point_size = 2,
                               point_alpha = 0.7,
                               title = "",
                               subtitle = NULL,
                               x_label = NULL,
                               y_label = NULL,
                               color_palette = "Set2",
                               theme_style = "minimal",
                               legend_position = "right",
                               facet_var = NULL,
                               facet_type = "wrap",
                               x_limits = NULL,
                               y_limits = NULL,
                               add_correlation = FALSE,
                               custom_theme = NULL) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  
  # Set default labels
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Create base aesthetic mapping
  aes_mapping <- aes_string(x = x_var, y = y_var)
  
  # Add color mapping if specified
  if (!is.null(color_var)) {
    aes_mapping$colour <- as.symbol(color_var)
  }
  
  # Add size mapping if specified
  if (!is.null(size_var)) {
    aes_mapping$size <- as.symbol(size_var)
  } else {
    # Use fixed size if no size mapping
    aes_mapping$size <- point_size
  }
  
  # Add shape mapping if specified
  if (!is.null(shape_var)) {
    aes_mapping$shape <- as.symbol(shape_var)
  }
  
  # Create base plot
  p <- ggplot(data, aes_mapping)
  
  # Add points layer
  if (is.null(size_var)) {
    p <- p + geom_point(alpha = point_alpha, size = point_size)
  } else {
    p <- p + geom_point(alpha = point_alpha)
  }
  
  # Add smooth line if requested
  if (add_smooth) {
    if (!is.null(color_var) && smooth_se_group) {
      p <- p + geom_smooth(method = smooth_method, se = smooth_se,linetype = "dashed", show.legend = FALSE, size=1.8)
    } else if (!is.null(color_var)) {
      p <- p + geom_smooth(aes(group = 1),
                           method = smooth_method, 
                           se = smooth_se, color = "black",linetype = "dashed", show.legend = FALSE, size=1.8)
    } else {
      p <- p + geom_smooth(aes(group = 1),
                           method = smooth_method, 
                           se = smooth_se, color = "black",linetype = "dashed", show.legend = FALSE, size=1.8)
    }
  }
  
  # Add correlation coefficient if requested
  if (add_correlation) {
    # Calculate correlation
    cor_coef <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
    
    cor_pvalue <- cor.test(mtcars[[x_var]], mtcars[[y_var]])$p.value
    cor_label <- paste("r =", round(cor_coef, 3), "\n", "p =", round(cor_pvalue, 5))
    
    # Position for correlation text
    x_pos <- min(data[[x_var]], na.rm = TRUE) + 
      0.05 * diff(range(data[[x_var]], na.rm = TRUE))
    y_pos <- max(data[[y_var]], na.rm = TRUE) - 
      0.05 * diff(range(data[[y_var]], na.rm = TRUE))
    
    p <- p + annotate("text", x = x_pos, y = y_pos, 
                      label = cor_label, size = 4, 
                      hjust = 0, fontface = "bold")
  }
  
  # Apply color palette
  if (!is.null(color_var)) {
    if (color_palette == "viridis") {
      p <- p + scale_color_viridis_d()
    } else if (color_palette == "plasma") {
      p <- p + scale_color_viridis_d(option = "plasma")
    } else if (color_palette %in% c("Set1", "Set2", "Set3", "Dark2", "Paired")) {
      p <- p + scale_color_brewer(type = "qual", palette = color_palette)
    } else if (is.vector(color_palette) && length(color_palette) > 1) {
      p <- p + scale_color_manual(values = color_palette)
    }
  }
  
  # Add faceting if specified
  if (!is.null(facet_var)) {
    if (facet_type == "wrap") {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)))
    } else if (facet_type == "grid") {
      p <- p + facet_grid(as.formula(paste("~", facet_var)))
    }
  }
  
  # Add labels and title
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label
  )
  
  # Apply theme
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    if (theme_style == "minimal") {
      p <- p + theme_minimal()
    } else if (theme_style == "classic") {
      p <- p + theme_classic()
    } else if (theme_style == "bw") {
      p <- p + theme_bw()
    } else if (theme_style == "dark") {
      p <- p + theme_dark()
    }
    
    # Additional theme customizations
    p <- p + theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = legend_position,
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
  }
  
  # Set axis limits if specified
  if (!is.null(x_limits)) {
    p <- p + xlim(x_limits)
  }
  
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  return(p)
}

#' Quick Scatterplot Function
#'
#' A simplified wrapper around simple_scatterplot() for rapid scatterplot generation
#' with sensible defaults. Includes trend line and correlation by default.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis
#' @param color Character string specifying the column name for color grouping. Default: NULL
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with scatterplot, trend line, and correlation
#'
#' @examples
#' # Quick scatterplot with trend line
#' quick_scatterplot(mtcars, x = "wt", y = "mpg", title = "Weight vs MPG")
#'
#' # Quick scatterplot with color grouping
#' quick_scatterplot(iris, x = "Sepal.Length", y = "Sepal.Width", 
#'                   color = "Species", title = "Sepal Dimensions")
#'
#' @export
quick_scatterplot <- function(data, x, y, color = NULL, title = "") {
  simple_scatterplot(data = data,
                     x_var = x,
                     y_var = y,
                     color_var = color,
                     add_smooth = TRUE,
                     smooth_method = "loess",
                     add_correlation = TRUE,
                     title = title,
                     point_alpha = 0.6,
                     point_size = 2.5)
}





scatterplot_theme <- function() {
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

scatterplot_themea_with_legend_title <- function() {
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black", 
                               arrow = arrow(length = unit(0.25, "cm"), type = "closed")),
      text = element_text(size = 20)
    )
}




#' Create Scatterplots with Continuous Color Mapping
#'
#' This function generates scatterplots where color represents a continuous variable
#' rather than discrete categories. It's designed for exploring relationships between
#' three continuous variables simultaneously using position (x,y) and color gradients.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (continuous variable).
#'              Required parameter.
#' @param y_var Character string specifying the column name for the y-axis (continuous variable).
#'              Required parameter.
#' @param color_var Character string specifying the column name for continuous color mapping.
#'                  Required parameter.
#' @param size_var Character string specifying the column name for point size mapping.
#'                 Default: NULL
#' @param add_smooth Logical indicating whether to add a trend line.
#'                   Default: FALSE
#' @param smooth_method Character string specifying smoothing method: "loess", "lm", "gam".
#'                      Default: "loess"
#' @param smooth_se Logical indicating whether to show confidence interval around trend line.
#'                  Default: TRUE
#' @param point_size Numeric value specifying the size of points (when size_var is NULL).
#'                   Default: 3
#' @param point_alpha Numeric value (0-1) controlling transparency of points.
#'                    Default: 0.8
#' @param stroke_width Numeric value specifying the width of point borders.
#'                     Default: 0.5
#' @param stroke_color Character string specifying the color of point borders.
#'                     Default: "white"
#' @param title Character string for the plot title. Default: ""
#' @param subtitle Character string for the plot subtitle. Default: NULL
#' @param x_label Character string for the x-axis label. Default: NULL (uses x_var)
#' @param y_label Character string for the y-axis label. Default: NULL (uses y_var)
#' @param color_label Character string for the color legend label. Default: NULL (uses color_var)
#' @param color_palette Character string specifying color palette: "viridis", "plasma", 
#'                     "inferno", "magma", "cividis", "blues", "reds", "greens".
#'                     Default: "viridis"
#' @param color_direction Numeric value (1 or -1) specifying color scale direction.
#'                        Default: 1
#' @param color_trans Character string specifying color transformation: "identity", "log10", "sqrt".
#'                    Default: "identity"
#' @param theme_style Character string specifying ggplot2 theme: "minimal", "classic", 
#'                    "bw", "dark". Default: "minimal"
#' @param legend_position Character string specifying legend position: "top", "bottom",
#'                        "left", "right", or "none". Default: "right"
#' @param facet_var Character string specifying the column name for faceting.
#'                  Default: NULL
#' @param facet_type Character string specifying facet type: "wrap" or "grid".
#'                   Default: "wrap"
#' @param x_limits Numeric vector of length 2 specifying x-axis limits. Default: NULL
#' @param y_limits Numeric vector of length 2 specifying y-axis limits. Default: NULL
#' @param color_limits Numeric vector of length 2 specifying color scale limits. Default: NULL
#' @param add_contour Logical indicating whether to add contour lines based on color variable.
#'                    Default: FALSE
#' @param add_correlation Logical indicating whether to add correlation coefficient.
#'                        Default: TRUE
#' @param contour_bins Numeric value specifying number of contour bins. Default: 10
#' @param custom_theme Function specifying custom ggplot2 theme. Default: NULL
#'
#' @return A ggplot object with continuous color-mapped scatterplot
#'
#' @examples
#' # Basic continuous color scatterplot
#' simple_scatterplot_continuous(mtcars, x_var = "wt", y_var = "mpg", 
#'                               color_var = "hp", title = "Weight vs MPG by Horsepower")
#'
#' # With size mapping and trend line
#' simple_scatterplot_continuous(mtcars, x_var = "wt", y_var = "mpg",
#'                               color_var = "hp", size_var = "disp",
#'                               add_smooth = TRUE, smooth_method = "lm")
#'
#' # Different color palette and transformations
#' simple_scatterplot_continuous(mtcars, x_var = "hp", y_var = "qsec",
#'                               color_var = "mpg", color_palette = "plasma",
#'                               color_trans = "sqrt", add_contour = TRUE)
#'
#' # Faceted plot with custom styling
#' mtcars$gear_factor <- as.factor(mtcars$gear)
#' simple_scatterplot_continuous(mtcars, x_var = "wt", y_var = "mpg",
#'                               color_var = "hp", facet_var = "gear_factor",
#'                               color_palette = "inferno", point_size = 4)
#'
#' @details
#' This function is specifically designed for continuous color mapping and includes:
#' \itemize{
#'   \item Gradient color scales for continuous variables
#'   \item Multiple color palette options (viridis family and single-hue scales)
#'   \item Color transformations (log, sqrt) for skewed data
#'   \item Contour overlays to show color variable density
#'   \item Point borders for better visibility
#'   \item Size mapping for fourth variable visualization
#' }
#'
#' Best used when you want to visualize the relationship between three or more
#' continuous variables simultaneously.
#'
#' @seealso \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{scale_color_viridis_c}}
#' @import ggplot2 dplyr
#' @export
simple_scatterplot_continuous <- function(data,
                                          x_var,
                                          y_var,
                                          color_var,
                                          size_var = NULL,
                                          add_smooth = FALSE,
                                          smooth_method = "loess",
                                          smooth_se = TRUE,
                                          point_size = 3,
                                          point_alpha = 0.8,
                                          stroke_width = 0.5,
                                          stroke_color = "white",
                                          title = "",
                                          subtitle = NULL,
                                          x_label = NULL,
                                          y_label = NULL,
                                          color_label = NULL,
                                          color_palette = "viridis",
                                          color_direction = 1,
                                          color_trans = "identity",
                                          theme_style = "minimal",
                                          legend_position = "right",
                                          facet_var = NULL,
                                          facet_type = "wrap",
                                          x_limits = NULL,
                                          y_limits = NULL,
                                          color_limits = NULL,
                                          add_contour = FALSE,
                                          contour_bins = 10,
                                          custom_theme = NULL,
                                          add_correlation = TRUE) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  
  # Set default labels
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  if (is.null(color_label)) color_label <- color_var
  
  # Create base aesthetic mapping
  aes_mapping <- aes_string(x = x_var, y = y_var, color = color_var)
  
  # Add size mapping if specified
  if (!is.null(size_var)) {
    aes_mapping$size <- as.symbol(size_var)
  }
  
  # Create base plot
  p <- ggplot(data, aes_mapping)
  
  # Add contour lines if requested (before points for layering)
  if (add_contour) {
    p <- p + geom_density_2d(aes_string(z = color_var), 
                             bins = contour_bins, 
                             alpha = 0.3, 
                             color = "grey30")
  }
  
  # Add points layer
  if (is.null(size_var)) {
    p <- p + geom_point(alpha = point_alpha, 
                        size = point_size,
                        stroke = stroke_width,
                        color = stroke_color)
  } else {
    p <- p + geom_point(alpha = point_alpha,
                        stroke = stroke_width)
  }
  
  # Add smooth line if requested
  if (add_smooth) {
    p <- p + geom_smooth(method = smooth_method, 
                         se = smooth_se, 
                         color = "black",
                         linetype = "dashed",
                         alpha = 0.7, show.legend = FALSE)
  }
  
  # Add correlation coefficient if requested
  if (add_correlation) {
    # Calculate correlation
    cor_coef <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
    
    cor_pvalue <- cor.test(mtcars[[x_var]], mtcars[[y_var]])$p.value
    cor_label <- paste("r =", round(cor_coef, 3), "\n", "p =", round(cor_pvalue, 5))
    
    # Position for correlation text
    x_pos <- min(data[[x_var]], na.rm = TRUE) + 
      0.05 * diff(range(data[[x_var]], na.rm = TRUE))
    y_pos <- max(data[[y_var]], na.rm = TRUE) - 
      0.05 * diff(range(data[[y_var]], na.rm = TRUE))
    
    p <- p + annotate("text", x = x_pos, y = y_pos, 
                      label = cor_label, size = 4, 
                      hjust = 0, fontface = "bold")
  }
  
  
  
  # Apply continuous color scale
  if (color_palette == "viridis") {
    p <- p + scale_color_viridis_c(
      name = color_label,
      direction = color_direction,
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "plasma") {
    p <- p + scale_color_viridis_c(
      name = color_label,
      option = "plasma",
      direction = color_direction,
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "inferno") {
    p <- p + scale_color_viridis_c(
      name = color_label,
      option = "inferno",
      direction = color_direction,
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "magma") {
    p <- p + scale_color_viridis_c(
      name = color_label,
      option = "magma",
      direction = color_direction,
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "cividis") {
    p <- p + scale_color_viridis_c(
      name = color_label,
      option = "cividis",
      direction = color_direction,
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "blues") {
    p <- p + scale_color_gradient(
      name = color_label,
      low = "lightblue",
      high = "darkblue",
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "reds") {
    p <- p + scale_color_gradient(
      name = color_label,
      low = "pink",
      high = "darkred",
      trans = color_trans,
      limits = color_limits
    )
  } else if (color_palette == "greens") {
    p <- p + scale_color_gradient(
      name = color_label,
      low = "lightgreen",
      high = "darkgreen",
      trans = color_trans,
      limits = color_limits
    )
  }
  
  # Apply size scale if size variable is used
  if (!is.null(size_var)) {
    p <- p + scale_size_continuous(name = size_var, range = c(1, 6))
  }
  
  # Add faceting if specified
  if (!is.null(facet_var)) {
    if (facet_type == "wrap") {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)))
    } else if (facet_type == "grid") {
      p <- p + facet_grid(as.formula(paste("~", facet_var)))
    }
  }
  
  # Add labels and title
  p <- p + labs(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label
  )
  
  # Apply theme
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    if (theme_style == "minimal") {
      p <- p + theme_minimal()
    } else if (theme_style == "classic") {
      p <- p + theme_classic()
    } else if (theme_style == "bw") {
      p <- p + theme_bw()
    } else if (theme_style == "dark") {
      p <- p + theme_dark()
    }
    
    # Additional theme customizations
    p <- p + theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = legend_position,
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.key.height = unit(1, "cm"),
      legend.key.width = unit(0.5, "cm")
    )
  }
  
  # Set axis limits if specified
  if (!is.null(x_limits)) {
    p <- p + xlim(x_limits)
  }
  
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  return(p)
}

#' Quick Continuous Color Scatterplot
#'
#' A simplified wrapper around simple_scatterplot_continuous() for rapid generation
#' of continuous color-mapped scatterplots with sensible defaults.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis
#' @param color Character string specifying the column name for continuous color mapping
#' @param size Character string specifying the column name for size mapping. Default: NULL
#' @param palette Character string specifying color palette. Default: "viridis"
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with continuous color-mapped scatterplot
#'
#' @examples
#' # Quick continuous color scatterplot
#' quick_scatterplot_continuous(mtcars, x = "wt", y = "mpg", color = "hp",
#'                              title = "Weight vs MPG by Horsepower")
#'
#' # With size mapping and different palette
#' quick_scatterplot_continuous(mtcars, x = "hp", y = "qsec", 
#'                              color = "mpg", size = "wt",
#'                              palette = "plasma", title = "HP vs Quarter Mile")
#'
#' @export
quick_scatterplot_continuous <- function(data, x, y, color, size = NULL, 
                                         palette = "viridis", title = "") {
  simple_scatterplot_continuous(data = data,
                                x_var = x,
                                y_var = y,
                                color_var = color,
                                size_var = size,
                                add_smooth = TRUE,
                                smooth_method = "loess",
                                color_palette = palette,
                                title = title,
                                point_size = 3.5,
                                point_alpha = 0.7)
}

#' Heat Scatterplot - Density-based Continuous Color
#'
#' Creates a scatterplot where color represents the density of points in that region,
#' useful for visualizing patterns in large datasets where overplotting is an issue.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis
#' @param y_var Character string specifying the column name for the y-axis
#' @param bins Numeric value specifying number of bins for density calculation. Default: 30
#' @param palette Character string specifying color palette. Default: "viridis"
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with density-based color mapping
#'
#' @examples
#' # Heat scatterplot for large datasets
#' heat_scatterplot(mtcars, x_var = "wt", y_var = "mpg", 
#'                  title = "Weight vs MPG Density")
#'
#' @export
heat_scatterplot <- function(data, x_var, y_var, bins = 30, 
                             palette = "viridis", title = "") {
  
  require(ggplot2)
  require(dplyr)
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    stat_density_2d_filled(bins = bins, alpha = 0.8) +
    geom_point(size = 0.5, alpha = 0.3, color = "white") +
    scale_fill_viridis_d(option = if(palette == "viridis") "A" else palette) +
    labs(title = title, x = x_var, y = y_var) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.title = element_text(size = 11)
    )
}























