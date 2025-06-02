# Required packages
library(ggplot2)
library(dplyr)

# Optional packages (with conditional loading)
if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
  message("ggbeeswarm package not installed. Beeswarm point style will not be available.")
  message("Install with: install.packages('ggbeeswarm')")
}

if (!requireNamespace("ggforce", quietly = TRUE)) {
  message("ggforce package not installed. Sina point style will not be available.")
  message("Install with: install.packages('ggforce')")
}

if (!requireNamespace("ggpubr", quietly = TRUE)) {
  message("ggpubr package not installed. Statistical comparisons will not be available.")
  message("Install with: install.packages('ggpubr')")
}


#' Create Customizable Boxplots with ggplot2
#'
#' This function generates boxplots using ggplot2 with extensive customization options.
#' It supports single boxplots, grouped boxplots, and boxplots with color grouping.
#' The function returns a ggplot object that can be further modified or displayed.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (categorical variable).
#'              If NULL, creates a single boxplot. Default: NULL
#' @param y_var Character string specifying the column name for the y-axis (continuous variable).
#'              Required parameter.
#' @param fill_var Character string specifying the column name for fill grouping.
#'                 Creates colored boxplots by group. Default: NULL
#' @param title Character string for the plot title. Default: "Boxplot"
#' @param x_label Character string for the x-axis label. If NULL, uses x_var name.
#'                Default: NULL
#' @param y_label Character string for the y-axis label. If NULL, uses y_var name.
#'                Default: NULL
#' @param theme_style Character string specifying ggplot2 theme. Options: "minimal",
#'                    "classic", "bw", "dark". Default: "minimal"
#' @param colors Named vector of colors for fill groups. Only used when fill_var is specified.
#'               Default: NULL (uses ggplot2 default colors)
#' @param show_points Logical indicating whether to overlay data points with jitter.
#'                    Default: FALSE
#' @param point_alpha Numeric value (0-1) controlling transparency of overlaid points.
#'                    Default: 0.3
#' @param outlier_shape Numeric value specifying the shape of outlier points.
#'                      Default: 19 (filled circle)
#' @param outlier_size Numeric value specifying the size of outlier points.
#'                     Default: 1.5
#'
#' @return A ggplot object that can be displayed, saved, or further customized
#'
#' @examples
#' # Basic single boxplot
#' create_boxplot(mtcars, y_var = "mpg", title = "Miles per Gallon Distribution")
#'
#' # Grouped boxplot by cylinder count
#' create_boxplot(mtcars, x_var = "cyl", y_var = "mpg", 
#'                title = "MPG by Number of Cylinders")
#'
#' # Boxplot with color grouping and data points
#' mtcars$gear_factor <- as.factor(mtcars$gear)
#' create_boxplot(mtcars, x_var = "cyl", y_var = "mpg", fill_var = "gear_factor",
#'                title = "MPG by Cylinders and Gears", show_points = TRUE)
#'
#' # Custom colors and theme
#' create_boxplot(iris, x_var = "Species", y_var = "Sepal.Length", 
#'                fill_var = "Species",
#'                colors = c("setosa" = "#FF6B6B", "versicolor" = "#4ECDC4", 
#'                          "virginica" = "#45B7D1"),
#'                title = "Sepal Length by Species",
#'                theme_style = "classic")
#'
#' @details
#' The function automatically handles different boxplot scenarios:
#' - Single boxplot: Only specify y_var
#' - Grouped boxplot: Specify both x_var and y_var
#' - Colored grouping: Add fill_var for color-coded groups
#' 
#' The returned ggplot object can be further customized with additional ggplot2 layers:
#' p <- create_boxplot(data, x_var = "group", y_var = "value")
#' p + geom_smooth() + facet_wrap(~category)
#'
#' @author Your Name
#' @seealso \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{ggplot}}
#' @import ggplot2 dplyr
#' @export
create_boxplot <- function(data, 
                           x_var = NULL, 
                           y_var, 
                           fill_var = NULL,
                           title = "Boxplot",
                           x_label = NULL,
                           y_label = NULL,
                           theme_style = "minimal",
                           colors = NULL,
                           show_points = FALSE,
                           point_alpha = 0.3,
                           outlier_shape = 19,
                           outlier_size = 1.5) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  
  # Set default labels if not provided
  if (is.null(x_label)) x_label <- ifelse(is.null(x_var), "", x_var)
  if (is.null(y_label)) y_label <- y_var
  
  # Create base plot
  if (is.null(x_var)) {
    # Single boxplot
    p <- ggplot(data, aes_string(x = '""', y = y_var))
  } else {
    # Grouped boxplot
    if (is.null(fill_var)) {
      p <- ggplot(data, aes_string(x = x_var, y = y_var))
    } else {
      p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var))
    }
  }
  
  # Add boxplot layer
  p <- p + geom_boxplot(outlier.shape = outlier_shape, 
                        outlier.size = outlier_size,
                        alpha = 0.7)
  
  # Add points if requested
  if (show_points) {
    if (is.null(x_var)) {
      p <- p + geom_jitter(width = 0.2, alpha = point_alpha)
    } else {
      p <- p + geom_jitter(width = 0.2, alpha = point_alpha)
    }
  }
  
  # Apply custom colors if provided
  if (!is.null(colors) && !is.null(fill_var)) {
    p <- p + scale_fill_manual(values = colors)
  }
  
  # Add labels and title
  p <- p + labs(
    title = title,
    x = x_label,
    y = y_label
  )
  
  # Apply theme
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
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
  
  return(p)
}

# Example usage:
# 
# # Basic boxplot
# create_boxplot(mtcars, y_var = "mpg", title = "Miles per Gallon Distribution")
# 
# # Grouped boxplot
# create_boxplot(mtcars, x_var = "cyl", y_var = "mpg", 
#                title = "MPG by Number of Cylinders")
# 
# # Boxplot with fill grouping
# mtcars$gear_factor <- as.factor(mtcars$gear)
# create_boxplot(mtcars, x_var = "cyl", y_var = "mpg", fill_var = "gear_factor",
#                title = "MPG by Cylinders and Gears", show_points = TRUE)
# 
# # Custom colors
# create_boxplot(iris, x_var = "Species", y_var = "Sepal.Length", 
#                fill_var = "Species",
#                colors = c("setosa" = "#FF6B6B", "versicolor" = "#4ECDC4", 
#                          "virginica" = "#45B7D1"),
#                title = "Sepal Length by Species")

#' Quick Boxplot Function
#'
#' A simplified wrapper around create_boxplot() for rapid boxplot generation
#' with sensible defaults. Always includes data points and uses minimal theme.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis (categorical variable).
#'          If NULL, creates a single boxplot. Default: NULL
#' @param y Character string specifying the column name for the y-axis (continuous variable).
#'          Required parameter.
#' @param fill Character string specifying the column name for fill grouping.
#'             Creates colored boxplots by group. Default: NULL
#' @param title Character string for the plot title. Default: "" (empty)
#'
#' @return A ggplot object with boxplot and overlaid jittered points
#'
#' @examples
#' # Quick single boxplot
#' quick_boxplot(mtcars, y = "mpg", title = "MPG Distribution")
#'
#' # Quick grouped boxplot
#' quick_boxplot(iris, x = "Species", y = "Sepal.Length", 
#'               fill = "Species", title = "Sepal Length by Species")
#'
#' @seealso \code{\link{create_boxplot}}
#' @export
quick_boxplot <- function(data, x = NULL, y, fill = NULL, title = "") {
  create_boxplot(data = data, 
                 x_var = x, 
                 y_var = y, 
                 fill_var = fill,
                 title = title,
                 show_points = TRUE,
                 theme_style = "minimal")
}



#' Custom Clean Theme for Boxplots
#'
#' Creates a clean, publication-ready theme for boxplots and other ggplot2 visualizations.
#' This theme removes grid lines and borders while adding directional arrows to axes,
#' creating a minimalist appearance suitable for scientific publications or presentations.
#'
#' @return A ggplot2 theme object that can be added to any ggplot
#'
#' @details
#' The theme applies the following modifications to the base theme_bw():
#' \itemize{
#'   \item Removes legend title for cleaner appearance
#'   \item Eliminates major and minor grid lines for minimal distraction
#'   \item Removes panel border for clean look
#'   \item Adds black axis lines with closed arrow endpoints (0.25 cm length)
#'   \item Sets all text elements to size 20 for better readability
#' }
#'
#' This theme is particularly effective for boxplots, scatter plots, and other
#' visualizations where clean axes and minimal visual clutter are desired.
#'
#' @examples
#' # Basic usage with boxplot
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_boxplot() +
#'   boxplot_theme()
#'
#'
#' @note
#' The arrow endpoints on axes may not display properly in all output formats
#' (e.g., some PDF viewers). Test your output format if arrows are critical
#' for your visualization.
#'
#' @seealso 
#' \code{\link[ggplot2]{theme_bw}}, \code{\link[ggplot2]{theme}}, 
#' \code{\link[ggplot2]{element_line}}, \code{\link[ggplot2]{arrow}}
#'
#' @author Your Name
#' @export
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






#' Create Complex Multi-layered Boxplots with Advanced Features
#'
#' This function generates sophisticated boxplots with multiple statistical layers,
#' annotations, and customization options. It supports faceting, statistical comparisons,
#' violin plots, notched boxplots, and publication-ready formatting.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x_var Character string specifying the column name for the x-axis (categorical variable).
#'              Required parameter.
#' @param y_var Character string specifying the column name for the y-axis (continuous variable).
#'              Required parameter.
#' @param fill_var Character string specifying the column name for fill grouping.
#'                 Default: NULL
#' @param facet_var Character string specifying the column name for faceting.
#'                  Default: NULL
#' @param facet_type Character string specifying facet type: "wrap" or "grid".
#'                   Default: "wrap"
#' @param add_violin Logical indicating whether to add violin plot layer underneath boxplots.
#'                   Default: FALSE
#' @param add_points Logical indicating whether to overlay individual data points.
#'                   Default: TRUE
#' @param point_style Character string specifying point style: "jitter", "beeswarm", or "sina".
#'                    Default: "jitter"
#' @param add_mean Logical indicating whether to add mean points and error bars.
#'                 Default: FALSE
#' @param error_type Character string specifying error bar type: "se" (standard error),
#'                   "sd" (standard deviation), or "ci" (95% confidence interval).
#'                   Default: "se"
#' @param notched Logical indicating whether to create notched boxplots for median comparison.
#'                Default: FALSE
#' @param add_n_labels Logical indicating whether to add sample size labels.
#'                     Default: FALSE
#' @param add_statistics Logical indicating whether to add statistical comparisons.
#'                       Default: FALSE
#' @param stat_method Character string specifying statistical test: "t.test", "wilcox.test".
#'                    Default: "wilcox.test"
#' @param stat_comparisons List of vectors specifying pairwise comparisons.
#'                         Default: NULL (all pairwise comparisons)
#' @param title Character string for the plot title. Default: ""
#' @param subtitle Character string for the plot subtitle. Default: NULL
#' @param x_label Character string for the x-axis label. Default: NULL
#' @param y_label Character string for the y-axis label. Default: NULL
#' @param color_palette Character string or vector specifying color palette.
#'                     Options: "viridis", "plasma", "Set1", "Set2", "Dark2", or custom vector.
#'                     Default: "Set2"
#' @param alpha_level Numeric value (0-1) controlling transparency of plot elements.
#'                    Default: 0.7
#' @param base_size Numeric value specifying base font size for theme.
#'                  Default: 12
#' @param legend_position Character string specifying legend position: "top", "bottom", 
#'                        "left", "right", or "none". Default: "right"
#' @param coord_flip Logical indicating whether to flip coordinates (horizontal boxplots).
#'                   Default: FALSE
#' @param y_limits Numeric vector of length 2 specifying y-axis limits. Default: NULL
#' @param custom_theme Function specifying custom ggplot2 theme. Default: NULL
#'
#' @return A ggplot object with complex boxplot visualization
#'
#' @examples
#' # Basic complex boxplot with violin layer and points
#' complex_boxplot(iris, x_var = "Species", y_var = "Sepal.Length",
#'                 add_violin = TRUE, add_points = TRUE, add_mean = TRUE)
#'
#' # Faceted boxplot with statistical comparisons
#' mtcars$cyl_factor <- as.factor(mtcars$cyl)
#' mtcars$gear_factor <- as.factor(mtcars$gear)
#' complex_boxplot(mtcars, x_var = "cyl_factor", y_var = "mpg", 
#'                 fill_var = "gear_factor", facet_var = "gear_factor",
#'                 add_statistics = TRUE, add_n_labels = TRUE)
#'
#' # Horizontal boxplot with custom styling
#' complex_boxplot(iris, x_var = "Species", y_var = "Petal.Width",
#'                 fill_var = "Species", coord_flip = TRUE, notched = TRUE,
#'                 color_palette = "viridis", point_style = "sina")
#'
#' @details
#' This function creates publication-ready boxplots with multiple layers:
#' \itemize{
#'   \item Violin plots for distribution shape visualization
#'   \item Individual data points with various display options
#'   \item Statistical comparisons with p-value annotations
#'   \item Sample size labels and mean/error indicators
#'   \item Flexible faceting and coordinate systems
#'   \item Custom color palettes and themes
#' }
#'
#' @note
#' Requires additional packages: ggbeeswarm (for beeswarm plots), 
#' ggforce (for sina plots), ggsignif (for statistical annotations).
#' Install with: install.packages(c("ggbeeswarm", "ggforce", "ggsignif"))
#'
#' @seealso \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{geom_violin}}
#' @import ggplot2 dplyr
#' @export
complex_boxplot <- function(data,
                            x_var,
                            y_var,
                            fill_var = NULL,
                            facet_var = NULL,
                            facet_type = "wrap",
                            add_violin = FALSE,
                            add_points = TRUE,
                            point_style = "jitter",
                            add_mean = FALSE,
                            error_type = "se",
                            notched = FALSE,
                            add_n_labels = FALSE,
                            add_statistics = FALSE,
                            stat_method = "wilcox.test",
                            stat_comparisons = NULL,
                            title = "",
                            subtitle = NULL,
                            x_label = NULL,
                            y_label = NULL,
                            color_palette = "Set2",
                            alpha_level = 0.7,
                            base_size = 12,
                            legend_position = "right",
                            coord_flip = FALSE,
                            y_limits = NULL,
                            custom_theme = NULL) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  
  # Check for optional packages
  violin_available <- requireNamespace("ggbeeswarm", quietly = TRUE)
  sina_available <- requireNamespace("ggforce", quietly = TRUE)
  
  # Set default labels
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Create base plot
  if (is.null(fill_var)) {
    p <- ggplot(data, aes_string(x = x_var, y = y_var))
  } else {
    p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var))
  }
  
  # Add violin layer if requested
  if (add_violin) {
    if (is.null(fill_var)) {
      p <- p + geom_violin(alpha = alpha_level * 0.5, trim = FALSE)
    } else {
      p <- p + geom_violin(alpha = alpha_level * 0.5, trim = FALSE,
                           position = position_dodge(0.8))
    }
  }
  
  # Add boxplot layer
  if (is.null(fill_var)) {
    p <- p + geom_boxplot(alpha = alpha_level, notch = notched, 
                          outlier.alpha = 0.6, width = 0.6)
  } else {
    p <- p + geom_boxplot(alpha = alpha_level, notch = notched,
                          position = position_dodge(0.8),
                          outlier.alpha = 0.6, width = 0.6)
  }
  
  # Add data points
  if (add_points) {
    if (point_style == "jitter") {
      if (is.null(fill_var)) {
        p <- p + geom_jitter(width = 0.15, alpha = alpha_level * 0.8, size = 1.2)
      } else {
        p <- p + geom_jitter(alpha = alpha_level * 0.8, size = 1.2,
                             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.15))
      }
    } else if (point_style == "beeswarm" && violin_available) {
      if (is.null(fill_var)) {
        p <- p + ggbeeswarm::geom_beeswarm(alpha = alpha_level * 0.8, size = 1.2)
      } else {
        p <- p + ggbeeswarm::geom_beeswarm(alpha = alpha_level * 0.8, size = 1.2,
                                           dodge.width = 0.8)
      }
    } else if (point_style == "sina" && sina_available) {
      if (is.null(fill_var)) {
        p <- p + ggforce::geom_sina(alpha = alpha_level * 0.8, size = 1.2)
      } else {
        p <- p + ggforce::geom_sina(alpha = alpha_level * 0.8, size = 1.2)
      }
    }
  }
  
  # Add mean and error bars
  if (add_mean) {
    if (error_type == "se") {
      stat_fun <- function(x) mean_se(x)
    } else if (error_type == "sd") {
      stat_fun <- function(x) mean_sdl(x, mult = 1)
    } else if (error_type == "ci") {
      stat_fun <- function(x) mean_cl_normal(x)
    }
    
    if (is.null(fill_var)) {
      p <- p + stat_summary(fun.data = stat_fun, geom = "errorbar", 
                            width = 0.2, color = "red", size = 1) +
        stat_summary(fun = mean, geom = "point", 
                     color = "red", size = 3, shape = 18)
    } else {
      p <- p + stat_summary(fun.data = stat_fun, geom = "errorbar", 
                            width = 0.2, color = "red", size = 1,
                            position = position_dodge(0.8)) +
        stat_summary(fun = mean, geom = "point", 
                     color = "red", size = 3, shape = 18,
                     position = position_dodge(0.8))
    }
  }
  
  # Add sample size labels
  if (add_n_labels) {
    if (is.null(fill_var)) {
      n_data <- data %>%
        group_by_at(x_var) %>%
        summarise(n = n(), 
                  y_pos = min(get(y_var), na.rm = TRUE) - 0.05 * diff(range(get(y_var), na.rm = TRUE)),
                  .groups = "drop")
    } else {
      n_data <- data %>%
        group_by_at(c(x_var, fill_var)) %>%
        summarise(n = n(),
                  y_pos = min(get(y_var), na.rm = TRUE) - 0.05 * diff(range(get(y_var), na.rm = TRUE)),
                  .groups = "drop")
    }
    
    p <- p + geom_text(data = n_data, 
                       aes_string(x = x_var, y = "y_pos", label = "paste('n =', n)"),
                       size = 3, inherit.aes = FALSE)
  }
  
  # Add statistical comparisons using ggpubr
  if (add_statistics) {
    # Ensure x_var is factor for proper grouping
    data[[x_var]] <- as.factor(data[[x_var]])
    
    # Get unique groups and remove any NA values
    unique_groups <- levels(data[[x_var]])
    unique_groups <- unique_groups[!is.na(unique_groups)]
    
    if (length(unique_groups) >= 2) {
      # Generate comparisons if not provided
      if (is.null(stat_comparisons)) {
        stat_comparisons <- combn(unique_groups, 2, simplify = FALSE)
      }
      
      # Validate comparisons
      valid_comparisons <- list()
      for (comp in stat_comparisons) {
        if (length(comp) == 2 && all(comp %in% unique_groups)) {
          # Check if both groups have sufficient data
          group1_data <- data[data[[x_var]] == comp[1] & !is.na(data[[y_var]]), y_var]
          group2_data <- data[data[[x_var]] == comp[2] & !is.na(data[[y_var]]), y_var]
          
          if (length(group1_data) >= 3 && length(group2_data) >= 3 &&
              var(group1_data) > 0 && var(group2_data) > 0) {
            valid_comparisons <- append(valid_comparisons, list(comp))
          }
        }
      }
      
      # Add statistical annotations using ggpubr
      if (length(valid_comparisons) > 0) {
        tryCatch({
          p <- p + ggpubr::stat_compare_means(
            comparisons = valid_comparisons,
            method = stat_method,
            label = "p.signif",
            step.increase = 0.1,
            tip.length = 0.01,
            hide.ns = FALSE,
            size = 3
          )
        }, error = function(e) {
          warning("Statistical testing with ggpubr failed: ", e$message)
          message("Make sure ggpubr is properly installed: install.packages('ggpubr')")
        })
      } else {
        warning("No valid comparisons found. Check that groups have sufficient data and variability.")
      }
    } else {
      warning("Need at least 2 groups for statistical comparisons.")
    }
  } else if (add_statistics) {
    warning("ggpubr package is required for statistical comparisons. Install with: install.packages('ggpubr')")
  }
  
  # Apply color palette
  if (!is.null(fill_var)) {
    if (color_palette == "viridis") {
      p <- p + scale_fill_viridis_d(alpha = alpha_level)
    } else if (color_palette == "plasma") {
      p <- p + scale_fill_viridis_d(option = "plasma", alpha = alpha_level)
    } else if (color_palette %in% c("Set1", "Set2", "Set3", "Dark2", "Paired")) {
      p <- p + scale_fill_brewer(type = "qual", palette = color_palette)
    } else if (is.vector(color_palette) && length(color_palette) > 1) {
      p <- p + scale_fill_manual(values = color_palette)
    }
  }
  
  # Add faceting
  if (!is.null(facet_var)) {
    if (facet_type == "wrap") {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y")
    } else if (facet_type == "grid") {
      p <- p + facet_grid(as.formula(paste("~", facet_var)), scales = "free_y")
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
    p <- p + theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(hjust = 0.5, size = base_size + 4, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = base_size + 1),
        axis.title = element_text(size = base_size + 2),
        axis.text = element_text(size = base_size),
        legend.position = legend_position,
        legend.title = element_text(size = base_size),
        legend.text = element_text(size = base_size - 1),
        strip.text = element_text(size = base_size, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  # Set y-axis limits if specified
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  # Flip coordinates if requested
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}

#' Quick Complex Boxplot with Sensible Defaults
#'
#' A simplified wrapper around complex_boxplot() for rapid generation of
#' feature-rich boxplots with commonly used options pre-configured.
#'
#' @param data A data.frame containing the variables to be plotted
#' @param x Character string specifying the column name for the x-axis
#' @param y Character string specifying the column name for the y-axis  
#' @param fill Character string specifying the column name for fill grouping. Default: NULL
#' @param facet Character string specifying the column name for faceting. Default: NULL
#' @param title Character string for the plot title. Default: ""
#'
#' @return A ggplot object with violin plots, boxplots, jittered points, and means
#'
#' @examples
#' # Quick complex visualization
#' quick_complex_boxplot(iris, x = "Species", y = "Sepal.Length", 
#'                       fill = "Species", title = "Sepal Length Distribution")
#'
#' @export
quick_complex_boxplot <- function(data, x, y, fill = NULL, facet = NULL, title = "") {
  complex_boxplot(data = data,
                  x_var = x,
                  y_var = y, 
                  fill_var = fill,
                  facet_var = facet,
                  add_violin = TRUE,
                  add_points = TRUE,
                  add_mean = TRUE,
                  add_n_labels = TRUE,
                  point_style = "jitter",
                  title = title,
                  color_palette = "Set2")
}




















