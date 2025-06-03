# Required packages
library(ggplot2)
library(dplyr)

# Optional packages (with conditional loading)
if (!requireNamespace("ggridges", quietly = TRUE)) {
  message("ggridges package not installed. Ridge plots will not be available.")
  message("Install with: install.packages('ggridges')")
}

if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  message("RColorBrewer package not installed. Some color palettes may not be available.")
  message("Install with: install.packages('RColorBrewer')")
}

#' Create Customizable Bar Plots with ggplot2
#'
#' This function generates bar plots using ggplot2 with extensive customization options.
#' It supports both count-based and value-based bar plots, with options for grouping,
#' stacking, dodging, and various styling choices.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (categorical variable).
#'              Required parameter.
#' @param y_var Character string specifying the column name for the y-axis (continuous variable).
#'              If NULL, creates a count-based bar plot. Default: NULL
#' @param fill_var Character string specifying the column name for fill grouping.
#'                 Creates grouped/stacked bars by category. Default: NULL
#' @param stat_type Character string specifying the statistical transformation:
#'                  "count" (frequency), "identity" (use y_var values), "prop" (proportions).
#'                  Default: "count"
#' @param position Character string specifying bar position: "stack", "dodge", "fill".
#'                 Default: "stack"
#' @param orientation Character string specifying plot orientation: "vertical", "horizontal".
#'                    Default: "vertical"
#' @param title Character string for the plot title. Default: ""
#' @param subtitle Character string for the plot subtitle. Default: NULL
#' @param x_label Character string for the x-axis label. Default: NULL (uses x_var)
#' @param y_label Character string for the y-axis label. Default: NULL (auto-generated)
#' @param color_palette Character string or vector specifying color palette.
#'                     Options: "viridis", "Set1", "Set2", "Dark2", or custom vector.
#'                     Default: "Set2"
#' @param bar_alpha Numeric value (0-1) controlling transparency of bars. Default: 0.8
#' @param bar_width Numeric value controlling width of bars. Default: 0.8
#' @param add_labels Logical indicating whether to add value labels on bars. Default: FALSE
#' @param label_size Numeric value specifying size of value labels. Default: 3
#' @param label_color Character string specifying color of value labels. Default: "black"
#' @param theme_style Character string specifying ggplot2 theme: "minimal", "classic", 
#'                    "bw", "dark". Default: "minimal"
#' @param legend_position Character string specifying legend position: "top", "bottom",
#'                        "left", "right", or "none". Default: "right"
#' @param facet_var Character string specifying the column name for faceting. Default: NULL
#' @param facet_type Character string specifying facet type: "wrap" or "grid". Default: "wrap"
#' @param x_angle Numeric value specifying rotation angle for x-axis labels. Default: 0
#' @param y_limits Numeric vector of length 2 specifying y-axis limits. Default: NULL
#' @param reorder_bars Logical indicating whether to reorder bars by value. Default: FALSE
#' @param reorder_desc Logical indicating descending order when reordering. Default: TRUE
#' @param add_error_bars Logical indicating whether to add error bars (requires y_var). Default: FALSE
#' @param error_var Character string specifying column name for error values. Default: NULL
#' @param custom_theme Function specifying custom ggplot2 theme. Default: NULL
#'
#' @return A ggplot object with bar plot visualization
#'
#' @examples
#' # Basic count bar plot
#' create_barplot(mtcars, x_var = "cyl", title = "Count by Cylinder")
#'
#' # Value-based bar plot with grouping
#' mtcars_summary <- mtcars %>% 
#'   group_by(cyl, gear) %>% 
#'   summarise(mean_mpg = mean(mpg), .groups = "drop")
#' create_barplot(mtcars_summary, x_var = "cyl", y_var = "mean_mpg", 
#'                fill_var = "gear", position = "dodge",
#'                title = "Mean MPG by Cylinder and Gear")
#'
#' # Horizontal bar plot with custom colors
#' create_barplot(mtcars, x_var = "cyl", fill_var = "gear", 
#'                orientation = "horizontal", position = "fill",
#'                color_palette = c("#E31A1C", "#1F78B4", "#33A02C"))
#'
#' # Bar plot with value labels and faceting
#' create_barplot(mtcars_summary, x_var = "gear", y_var = "mean_mpg",
#'                facet_var = "cyl", add_labels = TRUE,
#'                title = "Mean MPG by Gear, Faceted by Cylinder")
#'
#' @details
#' This function creates publication-ready bar plots with options for:
#' \itemize{
#'   \item Count-based or value-based plotting
#'   \item Grouped, stacked, or filled bar arrangements
#'   \item Horizontal or vertical orientations
#'   \item Custom color palettes and styling
#'   \item Value labels and error bars
#'   \item Faceting for grouped comparisons
#'   \item Bar reordering by value
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{geom_col}}
#' @import ggplot2 dplyr
#' @export
create_barplot <- function(data,
                           x_var,
                           y_var = NULL,
                           fill_var = NULL,
                           stat_type = "count",
                           position = "stack",
                           orientation = "vertical",
                           title = "",
                           subtitle = NULL,
                           x_label = NULL,
                           y_label = NULL,
                           color_palette = "Set2",
                           bar_alpha = 0.8,
                           bar_width = 0.8,
                           add_labels = FALSE,
                           label_size = 3,
                           label_color = "black",
                           theme_style = "minimal",
                           legend_position = "right",
                           facet_var = NULL,
                           facet_type = "wrap",
                           x_angle = 0,
                           y_limits = NULL,
                           reorder_bars = FALSE,
                           reorder_desc = TRUE,
                           add_error_bars = FALSE,
                           error_var = NULL,
                           custom_theme = NULL) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  
  # Set default labels
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) {
    if (is.null(y_var)) {
      y_label <- if (stat_type == "prop") "Proportion" else "Count"
    } else {
      y_label <- y_var
    }
  }
  
  # Reorder bars if requested
  if (reorder_bars && !is.null(y_var)) {
    if (reorder_desc) {
      data[[x_var]] <- reorder(data[[x_var]], -data[[y_var]])
    } else {
      data[[x_var]] <- reorder(data[[x_var]], data[[y_var]])
    }
  } else if (reorder_bars && is.null(y_var)) {
    # Reorder by count
    count_data <- data %>% count(.data[[x_var]], sort = reorder_desc)
    data[[x_var]] <- factor(data[[x_var]], levels = count_data[[x_var]])
  }
  
  # Create base aesthetic mapping
  if (is.null(y_var)) {
    # Count-based plot
    aes_mapping <- aes_string(x = x_var)
    if (!is.null(fill_var)) {
      aes_mapping$fill <- as.symbol(fill_var)
    }
  } else {
    # Value-based plot
    aes_mapping <- aes_string(x = x_var, y = y_var)
    if (!is.null(fill_var)) {
      aes_mapping$fill <- as.symbol(fill_var)
    }
  }
  
  # Create base plot
  p <- ggplot(data, aes_mapping)
  
  # Add bar layer
  if (is.null(y_var)) {
    # Use geom_bar for count data
    if (stat_type == "prop") {
      p <- p + geom_bar(position = position, alpha = bar_alpha, width = bar_width,
                        aes(y = after_stat(count/sum(count))))
    } else {
      p <- p + geom_bar(position = position, alpha = bar_alpha, width = bar_width)
    }
  } else {
    # Use geom_col for value data
    p <- p + geom_col(position = position, alpha = bar_alpha, width = bar_width)
  }
  
  # Add error bars if requested
  if (add_error_bars && !is.null(error_var) && !is.null(y_var)) {
    if (position == "dodge") {
      p <- p + geom_errorbar(aes_string(ymin = paste0(y_var, " - ", error_var),
                                        ymax = paste0(y_var, " + ", error_var)),
                             position = position_dodge(width = bar_width),
                             width = 0.2, color = "black")
    } else {
      p <- p + geom_errorbar(aes_string(ymin = paste0(y_var, " - ", error_var),
                                        ymax = paste0(y_var, " + ", error_var)),
                             width = 0.2, color = "black")
    }
  }
  
  # Add value labels if requested
  if (add_labels) {
    if (is.null(y_var)) {
      # For count data
      p <- p + geom_text(stat = "count", aes(label = after_stat(count)),
                         position = position_stack(vjust = 0.5),
                         size = label_size, color = label_color)
    } else {
      # For value data
      if (position == "stack") {
        p <- p + geom_text(aes_string(label = y_var),
                           position = position_stack(vjust = 0.5),
                           size = label_size, color = label_color)
      } else if (position == "dodge") {
        p <- p + geom_text(aes_string(label = y_var),
                           position = position_dodge(width = bar_width),
                           vjust = -0.2, size = label_size, color = label_color)
      } else {
        p <- p + geom_text(aes_string(label = y_var),
                           vjust = -0.2, size = label_size, color = label_color)
      }
    }
  }
  
  # Apply color palette
  if (!is.null(fill_var)) {
    if (color_palette == "viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (color_palette %in% c("Set1", "Set2", "Set3", "Dark2", "Paired")) {
      p <- p + scale_fill_brewer(type = "qual", palette = color_palette)
    } else if (is.vector(color_palette) && length(color_palette) > 1) {
      p <- p + scale_fill_manual(values = color_palette)
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
  
  # Flip coordinates for horizontal orientation
  if (orientation == "horizontal") {
    p <- p + coord_flip()
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
      axis.text.x = element_text(angle = x_angle, hjust = if(x_angle > 0) 1 else 0.5),
      legend.position = legend_position,
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
  }
  
  # Set y-axis limits if specified
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  return(p)
}

#' Quick Bar Plot Function
#'
#' A simplified wrapper around create_barplot() for rapid bar plot generation
#' with sensible defaults.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis. Default: NULL (count plot)
#' @param fill Character string specifying the column name for fill grouping. Default: NULL
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with bar plot
#'
#' @examples
#' # Quick count bar plot
#' quick_barplot(mtcars, x = "cyl", title = "Cylinder Count")
#'
#' # Quick grouped bar plot
#' quick_barplot(mtcars, x = "cyl", fill = "gear", title = "Cylinders by Gear")
#'
#' @export
quick_barplot <- function(data, x, y = NULL, fill = NULL, title = "") {
  create_barplot(data = data,
                 x_var = x,
                 y_var = y,
                 fill_var = fill,
                 title = title,
                 position = if(!is.null(fill)) "dodge" else "stack",
                 add_labels = TRUE,
                 reorder_bars = TRUE)
}

#' Create Customizable Ridge Plots with ggplot2
#'
#' This function generates ridge plots (also known as joy plots) using ggridges
#' with extensive customization options. Ridge plots are excellent for visualizing
#' distributions across multiple groups or time periods.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (continuous variable).
#'              Required parameter.
#' @param y_var Character string specifying the column name for the y-axis (categorical variable).
#'              This determines the different ridges. Required parameter.
#' @param fill_var Character string specifying the column name for fill grouping.
#'                 Default: NULL (uses y_var for fill)
#' @param scale Numeric value controlling the amount of vertical overlap between ridges.
#'              Higher values = more overlap. Default: 1.2
#' @param rel_min_height Numeric value (0-1) controlling the minimum height of ridges
#'                       relative to the tallest ridge. Default: 0.01
#' @param alpha Numeric value (0-1) controlling transparency of ridges. Default: 0.7
#' @param point_alpha Numeric value (0-1) controlling transparency of points (if shown). Default: 0.5
#' @param bandwidth Numeric value for kernel density bandwidth. NULL = automatic. Default: NULL
#' @param quantile_lines Numeric vector of quantile values to show as vertical lines.
#'                       Default: NULL (no quantile lines)
#' @param show_points Logical indicating whether to show individual data points. Default: FALSE
#' @param point_size Numeric value specifying size of individual points. Default: 0.5
#' @param jitter_height Numeric value controlling vertical jitter of points. Default: 0.1
#' @param title Character string for the plot title. Default: ""
#' @param subtitle Character string for the plot subtitle. Default: NULL
#' @param x_label Character string for the x-axis label. Default: NULL (uses x_var)
#' @param y_label Character string for the y-axis label. Default: NULL (uses y_var)
#' @param color_palette Character string or vector specifying color palette.
#'                     Options: "viridis", "Set1", "Set2", "Dark2", or custom vector.
#'                     Default: "viridis"
#' @param gradient_fill Logical indicating whether to use gradient fill based on x values.
#'                      Default: FALSE
#' @param theme_style Character string specifying ggplot2 theme: "minimal", "classic", 
#'                    "bw", "dark". Default: "minimal"
#' @param legend_position Character string specifying legend position: "top", "bottom",
#'                        "left", "right", or "none". Default: "right"
#' @param facet_var Character string specifying the column name for faceting. Default: NULL
#' @param facet_type Character string specifying facet type: "wrap" or "grid". Default: "wrap"
#' @param x_limits Numeric vector of length 2 specifying x-axis limits. Default: NULL
#' @param reverse_y Logical indicating whether to reverse the y-axis order. Default: FALSE
#' @param stat_type Character string specifying statistic: "density", "count", "binline".
#'                  Default: "density"
#' @param custom_theme Function specifying custom ggplot2 theme. Default: NULL
#'
#' @return A ggplot object with ridge plot visualization
#'
#' @examples
#' # Basic ridge plot
#' create_ridgeplot(iris, x_var = "Sepal.Length", y_var = "Species",
#'                  title = "Sepal Length Distribution by Species")
#'
#' # Ridge plot with points and quantile lines
#' create_ridgeplot(iris, x_var = "Petal.Width", y_var = "Species",
#'                  show_points = TRUE, quantile_lines = c(0.25, 0.5, 0.75),
#'                  title = "Petal Width with Quartiles")
#'
#' # Gradient fill ridge plot
#' create_ridgeplot(iris, x_var = "Sepal.Width", y_var = "Species",
#'                  gradient_fill = TRUE, color_palette = "plasma",
#'                  title = "Sepal Width with Gradient Fill")
#'
#' # Ridge plot with custom scaling and colors
#' create_ridgeplot(mtcars, x_var = "mpg", y_var = "factor(cyl)",
#'                  scale = 2, color_palette = c("#E31A1C", "#1F78B4", "#33A02C"),
#'                  title = "MPG Distribution by Cylinder Count")
#'
#' @details
#' This function creates publication-ready ridge plots with options for:
#' \itemize{
#'   \item Adjustable ridge scaling and overlap
#'   \item Multiple color schemes including gradients
#'   \item Individual data points overlay
#'   \item Quantile line annotations
#'   \item Various statistical representations
#'   \item Faceting for grouped comparisons
#' }
#'
#' Ridge plots are particularly effective for:
#' - Comparing distributions across multiple groups
#' - Time series of distributions
#' - Showing changes in distribution shape
#' - Large datasets where box plots become cluttered
#'
#' @note
#' Requires the ggridges package. Install with: install.packages("ggridges")
#'
#' @seealso \code{\link[ggridges]{geom_density_ridges}}, \code{\link[ggplot2]{geom_density}}
#' @import ggplot2 dplyr
#' @export
create_ridgeplot <- function(data,
                             x_var,
                             y_var,
                             fill_var = NULL,
                             scale = 1.2,
                             rel_min_height = 0.01,
                             alpha = 0.7,
                             point_alpha = 0.5,
                             bandwidth = NULL,
                             quantile_lines = NULL,
                             show_points = FALSE,
                             point_size = 0.5,
                             jitter_height = 0.1,
                             title = "",
                             subtitle = NULL,
                             x_label = NULL,
                             y_label = NULL,
                             color_palette = "viridis",
                             gradient_fill = FALSE,
                             theme_style = "minimal",
                             legend_position = "right",
                             facet_var = NULL,
                             facet_type = "wrap",
                             x_limits = NULL,
                             reverse_y = FALSE,
                             stat_type = "density",
                             custom_theme = NULL) {
  
  # Check if ggridges is available
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("ggridges package required for ridge plots. Install with: install.packages('ggridges')")
  }
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  require(ggridges)
  
  # Set default labels
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  if (is.null(fill_var)) fill_var <- y_var
  
  # Create base aesthetic mapping
  if (gradient_fill) {
    aes_mapping <- aes_string(x = x_var, y = y_var, fill = paste0("after_stat(x)"))
  } else {
    aes_mapping <- aes_string(x = x_var, y = y_var, fill = fill_var)
  }
  
  # Create base plot
  p <- ggplot(data, aes_mapping)
  
  # Add ridge layer based on stat_type
  if (stat_type == "density") {
    if (!is.null(quantile_lines)) {
      p <- p + ggridges::geom_density_ridges(scale = scale, 
                                             rel_min_height = rel_min_height,
                                             alpha = alpha,
                                             bandwidth = bandwidth,
                                             quantile_lines = TRUE,
                                             quantiles = quantile_lines)
    } else {
      p <- p + ggridges::geom_density_ridges(scale = scale,
                                             rel_min_height = rel_min_height,
                                             alpha = alpha,
                                             bandwidth = bandwidth)
    }
  } else if (stat_type == "count") {
    p <- p + ggridges::geom_density_ridges(stat = "count",
                                           scale = scale,
                                           rel_min_height = rel_min_height,
                                           alpha = alpha)
  } else if (stat_type == "binline") {
    p <- p + ggridges::geom_density_ridges(stat = "binline",
                                           scale = scale,
                                           rel_min_height = rel_min_height,
                                           alpha = alpha)
  }
  
  # Add points if requested
  if (show_points) {
    p <- p + ggridges::geom_jitter_strip(alpha = point_alpha,
                                         size = point_size,
                                         height = jitter_height)
  }
  
  # Apply color palette
  if (gradient_fill) {
    if (color_palette == "viridis") {
      p <- p + scale_fill_viridis_c(name = x_label)
    } else if (color_palette %in% c("plasma", "inferno", "magma", "cividis")) {
      p <- p + scale_fill_viridis_c(option = color_palette, name = x_label)
    } else {
      p <- p + scale_fill_gradient(low = color_palette[1], 
                                   high = color_palette[length(color_palette)],
                                   name = x_label)
    }
  } else {
    if (color_palette == "viridis") {
      p <- p + scale_fill_viridis_d()
    } else if (color_palette %in% c("Set1", "Set2", "Set3", "Dark2", "Paired")) {
      p <- p + scale_fill_brewer(type = "qual", palette = color_palette)
    } else if (is.vector(color_palette) && length(color_palette) > 1) {
      p <- p + scale_fill_manual(values = color_palette)
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
  
  # Reverse y-axis if requested
  if (reverse_y) {
    p <- p + scale_y_discrete(limits = rev)
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
  
  # Set x-axis limits if specified
  if (!is.null(x_limits)) {
    p <- p + xlim(x_limits)
  }
  
  return(p)
}

#' Quick Ridge Plot Function
#'
#' A simplified wrapper around create_ridgeplot() for rapid ridge plot generation
#' with sensible defaults.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis (continuous)
#' @param y Character string specifying the column name for the y-axis (categorical)
#' @param fill Character string specifying the column name for fill grouping. Default: NULL
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with ridge plot
#'
#' @examples
#' # Quick ridge plot
#' quick_ridgeplot(iris, x = "Sepal.Length", y = "Species", 
#'                 title = "Sepal Length by Species")
#'
#' # Quick ridge plot with custom fill
#' quick_ridgeplot(mtcars, x = "mpg", y = "factor(cyl)", 
#'                 title = "MPG Distribution by Cylinders")
#'
#' @export
quick_ridgeplot <- function(data, x, y, fill = NULL, title = "") {
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("ggridges package required for ridge plots. Install with: install.packages('ggridges')")
  }
  
  create_ridgeplot(data = data,
                   x_var = x,
                   y_var = y,
                   fill_var = fill,
                   title = title,
                   scale = 1.2,
                   alpha = 0.8,
                   color_palette = "Set2")
}

# Example usage demonstrations
if (FALSE) {
  # Bar plot examples
  library(dplyr)
  
  # Basic count bar plot
  p1 <- create_barplot(mtcars, x_var = "cyl", title = "Count by Cylinder")
  
  # Grouped bar plot
  p2 <- create_barplot(mtcars, x_var = "cyl", fill_var = "gear", 
                       position = "dodge", title = "Cylinders by Gear")
  
  # Value bar plot with labels
  mtcars_summary <- mtcars %>% 
    group_by(cyl) %>% 
    summarise(mean_mpg = mean(mpg), .groups = "drop")
  
  p3 <- create_barplot(mtcars_summary, x_var = "cyl", y_var = "mean_mpg",
                       add_labels = TRUE, title = "Mean MPG by Cylinder")
  
  # Ridge plot examples (requires ggridges)
  p4 <- create_ridgeplot(iris, x_var = "Sepal.Length", y_var = "Species",
                         title = "Sepal Length Distribution")
  
  p5 <- create_ridgeplot(iris, x_var = "Petal.Width", y_var = "Species",
                         show_points = TRUE, quantile_lines = c(0.25, 0.5, 0.75),
                         title = "Petal Width with Points and Quartiles")
  
  p6 <- create_ridgeplot(iris, x_var = "Sepal.Width", y_var = "Species",
                         gradient_fill = TRUE, color_palette = "plasma",
                         title = "Sepal Width with Gradient Fill")
  
  # Quick function examples
  quick_p1 <- quick_barplot(mtcars, x = "cyl", fill = "gear", 
                            title = "Quick Grouped Bar Plot")
  
  quick_p2 <- quick_ridgeplot(iris, x = "Sepal.Length", y = "Species",
                              title = "Quick Ridge Plot")
}

#' Custom Theme for Bar Plots
#'
#' Creates a clean, publication-ready theme specifically designed for bar plots.
#' Removes unnecessary grid lines while maintaining readability.
#'
#' @return A ggplot2 theme object
#' @export
barplot_clean_theme <- function() {
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

#' Custom Theme for Ridge Plots
#'
#' Creates a clean theme optimized for ridge plots with minimal distractions.
#'
#' @return A ggplot2 theme object
#' @export
ridgeplot_clean_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),
      text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
}

#' Create Stacked Percentage Bar Plot
#'
#' A specialized function for creating stacked bar plots showing percentages.
#' Useful for showing proportional composition across categories.
#'
#' @param data A data.frame containing the variables
#' @param x_var Character string for x-axis variable
#' @param fill_var Character string for fill variable
#' @param title Character string for title
#' @param show_percentages Logical, whether to show percentage labels
#' @return A ggplot object
#' @export
create_percentage_barplot <- function(data, x_var, fill_var, title = "", 
                                      show_percentages = TRUE) {
  
  # Calculate percentages
  perc_data <- data %>%
    group_by(.data[[x_var]], .data[[fill_var]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(.data[[x_var]]) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
  
  p <- ggplot(perc_data, aes_string(x = x_var, y = "count", fill = fill_var)) +
    geom_col(position = "fill", alpha = 0.8) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = title, y = "Percentage") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.title = element_blank()
    )
  
  if (show_percentages) {
    p <- p + geom_text(aes(label = paste0(percentage, "%")),
                       position = position_fill(vjust = 0.5),
                       color = "white", fontface = "bold", size = 3)
  }
  
  return(p)
}

#' Create Faceted Ridge Plot by Group
#'
#' A specialized function for creating ridge plots with automatic faceting
#' by a grouping variable. Useful for comparing distributions across multiple
#' conditions or time periods.
#'
#' @param data A data.frame containing the variables
#' @param x_var Character string for continuous variable
#' @param y_var Character string for categorical variable (ridges)
#' @param group_var Character string for grouping/faceting variable
#' @param title Character string for title
#' @param ncol Number of columns for faceting
#' @return A ggplot object
#' @export
create_faceted_ridgeplot <- function(data, x_var, y_var, group_var, 
                                     title = "", ncol = 2) {
  
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("ggridges package required. Install with: install.packages('ggridges')")
  }
  
  require(ggridges)
  
  ggplot(data, aes_string(x = x_var, y = y_var, fill = y_var)) +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 1.2) +
    facet_wrap(as.formula(paste("~", group_var)), ncol = ncol) +
    scale_fill_viridis_d(guide = "none") +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

#' Create Interactive Bar Plot (if plotly is available)
#'
#' Creates an interactive version of the bar plot using plotly if available.
#' Falls back to static ggplot if plotly is not installed.
#'
#' @param data A data.frame containing the variables
#' @param x_var Character string for x-axis variable
#' @param y_var Character string for y-axis variable (optional)
#' @param fill_var Character string for fill variable (optional)
#' @param title Character string for title
#' @return A plotly object or ggplot object
#' @export
create_interactive_barplot <- function(data, x_var, y_var = NULL, 
                                       fill_var = NULL, title = "") {
  
  # Create the base ggplot
  p <- create_barplot(data = data, x_var = x_var, y_var = y_var, 
                      fill_var = fill_var, title = title, 
                      add_labels = TRUE, position = "dodge")
  
  # Try to make it interactive with plotly
  if (requireNamespace("plotly", quietly = TRUE)) {
    return(plotly::ggplotly(p))
  } else {
    message("plotly not available. Returning static ggplot.")
    message("Install plotly for interactive plots: install.packages('plotly')")
    return(p)
  }
}


#' Calculate Percentage DataFrame
#'
#' A simple function to calculate percentages within groups and return a 
#' summary dataframe with counts, totals, and percentages. Perfect for 
#' preparing data for percentage stacked bar plots or summary tables.
#'
#' @param data A data.frame containing the variables
#' @param group_var Character string specifying the grouping variable (x-axis)
#' @param category_var Character string specifying the category variable (fill/stack)
#' @param weight_var Character string specifying weight variable (optional). 
#'                   If NULL, counts observations. Default: NULL
#' @param round_digits Number of decimal places for percentages. Default: 1
#'
#' @return A data.frame with columns: group_var, category_var, count, total, percentage
#'
#' @examples
#' # Basic usage with counts
#' calculate_percentage_df(mtcars, "cyl", "gear")
#'
#' # With weight variable  
#' calculate_percentage_df(sales_data, "region", "product", weight_var = "sales")
#'
#' # With custom rounding
#' calculate_percentage_df(iris, "Species", "Sepal.Length > 5", round_digits = 2)
#'
#' @export
calculate_percentage_df <- function(data, group_var, category_var, 
                                    weight_var = NULL, round_digits = 1) {
  
  require(dplyr)
  
  if (is.null(weight_var)) {
    # Count-based calculation
    result <- data %>%
      group_by(.data[[group_var]], .data[[category_var]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(.data[[group_var]]) %>%
      mutate(
        total = sum(count),
        percentage = round(count / total, round_digits)
      ) %>%
      ungroup()
  } else {
    # Weight-based calculation
    result <- data %>%
      group_by(.data[[group_var]], .data[[category_var]]) %>%
      summarise(count = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[group_var]]) %>%
      mutate(
        total = sum(count),
        percentage = round(count / totala, round_digits)
      ) %>%
      ungroup()
  }
  
  return(result)
}

#' Quick Percentage Summary
#'
#' Even simpler wrapper that just returns the key columns for quick viewing.
#'
#' @param data A data.frame containing the variables
#' @param group_var Character string specifying the grouping variable
#' @param category_var Character string specifying the category variable
#'
#' @return A data.frame with group, category, and percentage columns
#'
#' @examples
#' quick_percentage(mtcars, "cyl", "gear")
#' quick_percentage(iris, "Species", "Sepal.Length > 5")
#'
#' @export
quick_percentage <- function(data, group_var, category_var) {
  
  calculate_percentage_df(data, group_var, category_var) %>%
    select(all_of(c(group_var, category_var, "percentage")))
}

# Example usage
if (FALSE) {
  # Basic example with mtcars
  result1 <- calculate_percentage_df(mtcars, "cyl", "gear")
  print(result1)
  
  # Quick summary
  result2 <- quick_percentage(mtcars, "cyl", "gear")
  print(result2)
  
  # With iris data
  result3 <- calculate_percentage_df(iris, "Species", "Sepal.Length > 5")
  print(result3)
}





