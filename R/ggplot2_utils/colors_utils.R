# Required packages
library(ggplot2)

# Optional packages (with conditional loading)
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  message("RColorBrewer package not installed. ColorBrewer palettes will not be available.")
  message("Install with: install.packages('RColorBrewer')")
}

if (!requireNamespace("viridis", quietly = TRUE)) {
  message("viridis package not installed. Viridis color palettes will not be available.")
  message("Install with: install.packages('viridis')")
}

if (!requireNamespace("randomcoloR", quietly = TRUE)) {
  message("randomcoloR package not installed. Random distinctive color generation will use fallback method.")
  message("Install with: install.packages('randomcoloR')")
}


# Functions for choosing color----

## categorical variables --------

### Selection of colors for variables -- general use -----------

#' Generate distinctive colors for categorical variables
#'
#' @param n Number of colors needed
#' @param method Color generation method. Options: "brewer", "viridis", "okabe_ito", 
#'               "random", "rainbow", "manual"
#' @param palette For brewer: palette name (e.g., "Set2", "Dark2", "Paired")
#'                For viridis: "viridis", "plasma", "inferno", "cividis"
#' @param colors For manual method: vector of color codes
#' @param colorblind_safe Logical, prioritize colorblind-friendly options
#' @param seed For random method: set seed for reproducibility
#' @return Vector of color codes
#' @examples
#' # Basic usage
#' get_categorical_colors(5)
#' get_categorical_colors(8, method = "brewer", palette = "Dark2")
#' get_categorical_colors(10, method = "viridis")
#' get_categorical_colors(3, method = "okabe_ito")

get_categorical_colors <- function(n, 
                                   method = "auto",
                                   palette = NULL,
                                   colors = NULL,
                                   colorblind_safe = FALSE,
                                   seed = 123) {
  
  # Load required libraries
  required_packages <- c("RColorBrewer", "viridis")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(paste("Package", pkg, "not available. Some methods may not work."))
    }
  }
  
  # Auto-select method based on number of colors and preferences
  if (method == "auto") {
    if (colorblind_safe && n <= 8) {
      method <- "okabe_ito"
    } else if (n <= 3) {
      method <- "brewer"
      palette <- "Set1"
    } else if (n <= 8) {
      method <- "brewer"
      palette <- "Set2"
    } else if (n <= 11) {
      method <- "brewer"
      palette <- "Paired"
    } else {
      method <- "random"
    }
  }
  
  # Generate colors based on method
  colors_out <- switch(method,
                       
                       # ColorBrewer palettes
                       "brewer" = {
                         if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
                           stop("RColorBrewer package required for brewer method")
                         }
                         
                         # Default palette selection
                         if (is.null(palette)) {
                           if (n <= 3) palette <- "Set1"
                           else if (n <= 8) palette <- "Set2"
                           else if (n <= 11) palette <- "Paired"
                           else stop("Too many colors for ColorBrewer. Try 'random' method.")
                         }
                         
                         # Check if palette can accommodate n colors
                         max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
                         if (n > max_colors) {
                           warning(paste("Palette", palette, "only supports", max_colors, "colors. Using maximum available."))
                           n <- max_colors
                         }
                         
                         RColorBrewer::brewer.pal(n, palette)
                       },
                       
                       # Viridis family
                       "viridis" = {
                         if (!requireNamespace("viridis", quietly = TRUE)) {
                           stop("viridis package required for viridis method")
                         }
                         
                         if (is.null(palette)) palette <- "viridis"
                         
                         switch(palette,
                                "viridis" = viridis::viridis_discrete(n),
                                "plasma" = viridis::plasma(n, discrete = TRUE),
                                "inferno" = viridis::inferno(n, discrete = TRUE),
                                "cividis" = viridis::cividis(n, discrete = TRUE),
                                viridis::viridis_discrete(n)
                         )
                       },
                       
                       # Okabe-Ito colorblind-safe palette
                       "okabe_ito" = {
                         okabe_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                           "#0072B2", "#D55E00", "#CC79A7", "#999999")
                         
                         if (n > length(okabe_colors)) {
                           warning(paste("Okabe-Ito palette only has", length(okabe_colors), 
                                         "colors. Recycling colors."))
                           rep(okabe_colors, length.out = n)
                         } else {
                           okabe_colors[1:n]
                         }
                       },
                       
                       # Random distinctive colors
                       "random" = {
                         if (requireNamespace("randomcoloR", quietly = TRUE)) {
                           set.seed(seed)
                           randomcoloR::distinctColorPalette(n)
                         } else {
                           # Fallback to rainbow with jittering
                           set.seed(seed)
                           hues <- seq(0, 1, length.out = n + 1)[1:n]
                           hues <- hues + runif(n, -0.1, 0.1) # Add some randomness
                           hues <- hues %% 1 # Keep in [0,1]
                           hsv(hues, s = runif(n, 0.6, 0.9), v = runif(n, 0.7, 0.9))
                         }
                       },
                       
                       # Rainbow colors
                       "rainbow" = {
                         rainbow(n, s = 0.7, v = 0.8)
                       },
                       
                       # Manual colors
                       "manual" = {
                         if (is.null(colors)) {
                           stop("colors argument required for manual method")
                         }
                         
                         if (length(colors) < n) {
                           warning("Not enough colors provided. Recycling colors.")
                           rep(colors, length.out = n)
                         } else {
                           colors[1:n]
                         }
                       },
                       
                       # Default fallback
                       {
                         warning("Unknown method. Using rainbow colors.")
                         rainbow(n, s = 0.7, v = 0.8)
                       }
  )
  
  return(colors_out)
}

#' Preview color palette
#' 
#' @param colors Vector of color codes
#' @param labels Optional labels for colors
#' @examples
#' colors <- get_categorical_colors(5, method = "brewer", palette = "Set2")
#' preview_colors(colors)

preview_colors <- function(colors, labels = NULL) {
  n <- length(colors)
  
  if (is.null(labels)) {
    labels <- paste("Color", 1:n)
  }
  
  # Create a simple bar plot to show colors
  par(mar = c(2, 8, 2, 1))
  barplot(rep(1, n), 
          col = colors, 
          horiz = TRUE, 
          names.arg = labels,
          las = 2,
          main = "Color Preview",
          axes = FALSE)
  
  # Print hex codes
  cat("Color codes:\n")
  for (i in 1:n) {
    cat(sprintf("%s: %s\n", labels[i], colors[i]))
  }
}

# Example usage and demonstrations
if (FALSE) {
  # Basic examples
  colors1 <- get_categorical_colors(5)
  colors2 <- get_categorical_colors(8, method = "brewer", palette = "Dark2")
  colors3 <- get_categorical_colors(6, method = "viridis", palette = "plasma")
  colors4 <- get_categorical_colors(4, colorblind_safe = TRUE)
  
  # Preview colors
  preview_colors(colors2, c("Group A", "Group B", "Group C", "Group D", "Group E", "Group F", "Group G", "Group H"))
  
  # Use with ggplot2
  library(ggplot2)
  my_colors <- get_categorical_colors(3, method = "okabe_ito")
  
  ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
    geom_point(size = 3) +
    scale_color_manual(values = my_colors) +
    theme_minimal()
}



### Selection of colors for variables -- advanced use -----------

#' Get publication-quality colors used in Cell/Nature journals
#'
#' @param n Number of colors needed (max 36)
#' @param shuffle Logical, whether to shuffle the color order
#' @param seed For shuffling: set seed for reproducibility
#' @return Vector of hex color codes
#' @examples
#' # Get first 8 publication colors
#' pub_colors <- get_publication_colors(8)
#' 
#' # Get shuffled colors for randomization
#' pub_colors <- get_publication_colors(12, shuffle = TRUE)

get_publication_colors <- function(n = 36, shuffle = FALSE, seed = 123) {
  
  # High-quality color palette commonly used in Cell, Nature, Science publications
  # These colors are carefully selected for:
  # - High contrast and distinctiveness
  # - Print and digital reproduction quality
  # - Professional appearance
  # - Good performance in colorblind accessibility
  
  pub_palette <- c(
    # Primary vibrant colors
    "#E31A1C",  # Red
    "#1F78B4",  # Blue  
    "#33A02C",  # Green
    "#FF7F00",  # Orange
    "#6A3D9A",  # Purple
    "#B15928",  # Brown
    "#A6CEE3",  # Light Blue
    "#B2DF8A",  # Light Green
    
    # Secondary distinctive colors
    "#FB9A99",  # Pink
    "#FDBF6F",  # Light Orange
    "#CAB2D6",  # Light Purple
    "#FFFF99",  # Yellow
    "#05188A",  # Navy Blue
    "#CEECF5",  # Pale Blue
    "#D0F5A9",  # Pale Green
    "#F9ED32",  # Bright Yellow
    
    # Tertiary professional colors
    "#F15A29",  # Vermillion
    "#006837",  # Dark Green
    "#A50026",  # Dark Red
    "#313695",  # Dark Blue
    "#74ADD1",  # Medium Blue
    "#ABD9E9",  # Sky Blue
    "#E0F3F8",  # Very Light Blue
    "#FEE090",  # Light Yellow
    
    # Additional distinctive colors
    "#FDAE61",  # Peach
    "#D73027",  # Bright Red
    "#4575B4",  # Steel Blue
    "#91BFDB",  # Powder Blue
    "#FC4E2A",  # Red Orange
    "#800026",  # Maroon
    "#41B6C4",  # Teal
    "#225EA8",  # Ocean Blue
    
    # Final accent colors
    "#253494",  # Indigo
    "#2C7FB8",  # Cerulean
    "#7FCDBB",  # Turquoise
    "#EDF8B1"   # Pale Yellow Green
  )
  
  # Validate input
  if (n > length(pub_palette)) {
    warning(paste("Maximum", length(pub_palette), "colors available. Returning all colors."))
    n <- length(pub_palette)
  }
  
  if (n <= 0) {
    stop("Number of colors must be positive")
  }
  
  # Select colors
  selected_colors <- pub_palette[1:n]
  
  # Shuffle if requested
  if (shuffle) {
    set.seed(seed)
    selected_colors <- sample(selected_colors)
  }
  
  return(selected_colors)
}

#' Create a publication-ready color scale for ggplot2
#'
#' @param n Number of colors needed
#' @param shuffle Logical, whether to shuffle the color order
#' @param discrete Logical, for discrete or continuous scale
#' @param ... Additional arguments passed to scale functions
#' @return ggplot2 scale function
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_publication() +
#'   theme_minimal()

scale_color_publication <- function(n = NULL, shuffle = FALSE, discrete = TRUE, ...) {
  if (discrete) {
    if (is.null(n)) {
      # Let ggplot2 determine the number of colors needed
      ggplot2::scale_color_manual(values = get_publication_colors(36, shuffle = shuffle), ...)
    } else {
      ggplot2::scale_color_manual(values = get_publication_colors(n, shuffle = shuffle), ...)
    }
  } else {
    # For continuous scales, use a subset of publication colors
    colors <- get_publication_colors(min(n %||% 6, 6), shuffle = shuffle)
    ggplot2::scale_color_gradientn(colors = colors, ...)
  }
}

#' Create a publication-ready fill scale for ggplot2
#'
#' @param n Number of colors needed
#' @param shuffle Logical, whether to shuffle the color order
#' @param discrete Logical, for discrete or continuous scale
#' @param ... Additional arguments passed to scale functions
#' @return ggplot2 scale function

scale_fill_publication <- function(n = NULL, shuffle = FALSE, discrete = TRUE, ...) {
  if (discrete) {
    if (is.null(n)) {
      ggplot2::scale_fill_manual(values = get_publication_colors(36, shuffle = shuffle), ...)
    } else {
      ggplot2::scale_fill_manual(values = get_publication_colors(n, shuffle = shuffle), ...)
    }
  } else {
    colors <- get_publication_colors(min(n %||% 6, 6), shuffle = shuffle)
    ggplot2::scale_fill_gradientn(colors = colors, ...)
  }
}

#' Show all publication colors with their hex codes
#'
#' @param ncol Number of columns for display
#' @examples
#' show_publication_palette()

show_publication_palette <- function(ncol = 6) {
  colors <- get_publication_colors(36)
  n <- length(colors)
  
  # Calculate layout
  nrow <- ceiling(n / ncol)
  
  # Create plot
  par(mar = c(1, 1, 3, 1), mfrow = c(1, 1))
  plot(1, type = "n", xlim = c(0, ncol), ylim = c(0, nrow), 
       xlab = "", ylab = "", axes = FALSE,
       main = "Publication Color Palette (Cell/Nature Style)")
  
  # Add color squares and labels
  for (i in 1:n) {
    row <- ceiling(i / ncol)
    col <- ((i - 1) %% ncol) + 1
    
    # Draw rectangle
    rect(col - 1, nrow - row, col, nrow - row + 1, 
         col = colors[i], border = "white", lwd = 2)
    
    # Add text
    text(col - 0.5, nrow - row + 0.3, colors[i], 
         cex = 0.7, col = "white", font = 2)
    text(col - 0.5, nrow - row + 0.7, paste0("#", i), 
         cex = 0.8, col = "white", font = 1)
  }
}



## continous variable ---------

### Selection of colors for variables -- general use ----------
#' Generate color palettes for continuous variables
#'
#' @param n Number of color steps/breaks (default: 100 for smooth gradients)
#' @param method Color palette method. Options: "viridis", "brewer_seq", "brewer_div", 
#'               "custom", "publication", "scientific", "heatmap"
#' @param palette Specific palette name within method
#' @param direction Direction of color scale: 1 (default) or -1 (reversed)
#' @param colors For custom method: vector of color codes to interpolate between
#' @param diverging Logical, whether to create a diverging palette (centered at midpoint)
#' @param midpoint For diverging palettes: the center value (default: 0)
#' @param colorblind_safe Logical, prioritize colorblind-friendly options
#' @return Vector of color codes
#' @examples
#' # Basic continuous palettes
#' get_continuous_colors(method = "viridis")
#' get_continuous_colors(method = "brewer_seq", palette = "Blues")
#' get_continuous_colors(method = "brewer_div", palette = "RdYlBu")
#' get_continuous_colors(method = "publication", palette = "nature_heatmap")

get_continuous_colors <- function(n = 100,
                                  method = "viridis",
                                  palette = NULL,
                                  direction = 1,
                                  colors = NULL,
                                  diverging = FALSE,
                                  midpoint = 0,
                                  colorblind_safe = FALSE) {
  
  # Load required libraries
  required_packages <- c("RColorBrewer", "viridis")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(paste("Package", pkg, "not available. Some methods may not work."))
    }
  }
  
  # Auto-adjust method based on preferences
  if (colorblind_safe && method == "auto") {
    method <- "viridis"
  }
  
  # Generate color palette based on method
  colors_out <- switch(method,
                       
                       # Viridis family (perceptually uniform, colorblind-friendly)
                       "viridis" = {
                         if (!requireNamespace("viridis", quietly = TRUE)) {
                           stop("viridis package required for viridis method")
                         }
                         
                         if (is.null(palette)) palette <- "viridis"
                         
                         palette_colors <- switch(palette,
                                                  "viridis" = viridis::viridis(n),
                                                  "plasma" = viridis::plasma(n),
                                                  "inferno" = viridis::inferno(n),
                                                  "magma" = viridis::magma(n),
                                                  "cividis" = viridis::cividis(n),
                                                  "rocket" = viridis::rocket(n),
                                                  "mako" = viridis::mako(n),
                                                  "turbo" = viridis::turbo(n),
                                                  viridis::viridis(n)
                         )
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # ColorBrewer sequential palettes
                       "brewer_seq" = {
                         if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
                           stop("RColorBrewer package required for brewer_seq method")
                         }
                         
                         if (is.null(palette)) palette <- "Blues"
                         
                         # Get maximum colors for the palette
                         max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
                         base_colors <- RColorBrewer::brewer.pal(max_colors, palette)
                         
                         # Interpolate to desired number of colors
                         palette_colors <- colorRampPalette(base_colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # ColorBrewer diverging palettes
                       "brewer_div" = {
                         if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
                           stop("RColorBrewer package required for brewer_div method")
                         }
                         
                         if (is.null(palette)) palette <- "RdYlBu"
                         
                         max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
                         base_colors <- RColorBrewer::brewer.pal(max_colors, palette)
                         
                         palette_colors <- colorRampPalette(base_colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # Custom color interpolation
                       "custom" = {
                         if (is.null(colors)) {
                           stop("colors argument required for custom method")
                         }
                         
                         palette_colors <- colorRampPalette(colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # Publication-quality continuous palettes
                       "publication" = {
                         if (is.null(palette)) palette <- "nature_heatmap"
                         
                         pub_palettes <- list(
                           # Nature/Cell style heatmaps
                           "nature_heatmap" = c("#000080", "#0000FF", "#00FFFF", "#FFFF00", "#FF0000", "#800000"),
                           "cell_heatmap" = c("#2166AC", "#4393C3", "#92C5DE", "#F7F7F7", "#FDBF6F", "#E31A1C", "#B2182B"),
                           
                           # Scientific gradients
                           "temperature" = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", 
                                             "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"),
                           "ocean" = c("#023858", "#045A8D", "#0570B0", "#3690C0", "#74A9CF", 
                                       "#A6BDDB", "#D0D1E6", "#ECE7F2", "#FFF7FB"),
                           "forest" = c("#00441B", "#006837", "#238B45", "#41AB5D", "#74C476", 
                                        "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5"),
                           
                           # Diverging scientific
                           "climate" = c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                                         "#F7F7F7", "#FDBF6F", "#FD8D3C", "#E31A1C", "#B10026"),
                           "geology" = c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", "#F5F5F5", 
                                         "#C7EAE5", "#80CDC1", "#35978F", "#01665E"),
                           
                           # Modern gradients
                           "sunset" = c("#2C1810", "#8B4513", "#CD853F", "#F4A460", "#FFE4B5", 
                                        "#FFF8DC", "#FFFFE0"),
                           "arctic" = c("#08306B", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", 
                                        "#C6DBEF", "#DEEBF7", "#F7FBFF")
                         )
                         
                         if (!palette %in% names(pub_palettes)) {
                           warning(paste("Unknown publication palette:", palette, ". Using nature_heatmap."))
                           palette <- "nature_heatmap"
                         }
                         
                         base_colors <- pub_palettes[[palette]]
                         palette_colors <- colorRampPalette(base_colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # Scientific color schemes
                       "scientific" = {
                         if (is.null(palette)) palette <- "parula"
                         
                         sci_palettes <- list(
                           "parula" = c("#352A87", "#0F5CDD", "#1481D6", "#06A7C6", "#38B99E", 
                                        "#92BF73", "#D9BA56", "#FCCE2E", "#F9FB0E"),
                           "jet" = c("#000080", "#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000", "#800000"),
                           "hot" = c("#000000", "#FF0000", "#FFFF00", "#FFFFFF"),
                           "cool" = c("#00FFFF", "#FF00FF"),
                           "spring" = c("#FF00FF", "#FFFF00"),
                           "winter" = c("#0000FF", "#00FF80"),
                           "autumn" = c("#FF0000", "#FFFF00"),
                           "summer" = c("#008000", "#FFFF00"),
                           "bone" = c("#000000", "#545474", "#A8A8BA", "#FFFFFF"),
                           "copper" = c("#000000", "#5C4033", "#B8814D", "#FF7F00", "#FFC78F")
                         )
                         
                         if (!palette %in% names(sci_palettes)) {
                           warning(paste("Unknown scientific palette:", palette, ". Using parula."))
                           palette <- "parula"
                         }
                         
                         base_colors <- sci_palettes[[palette]]
                         palette_colors <- colorRampPalette(base_colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # Specialized heatmap colors
                       "heatmap" = {
                         if (is.null(palette)) palette <- "red_white_blue"
                         
                         heatmap_palettes <- list(
                           "red_white_blue" = c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020"),
                           "blue_white_red" = c("#CA0020", "#F4A582", "#F7F7F7", "#92C5DE", "#0571B0"),
                           "green_black_red" = c("#00FF00", "#000000", "#FF0000"),
                           "blue_yellow_red" = c("#0000FF", "#FFFF00", "#FF0000"),
                           "spectral" = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", 
                                          "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
                         )
                         
                         if (!palette %in% names(heatmap_palettes)) {
                           warning(paste("Unknown heatmap palette:", palette, ". Using red_white_blue."))
                           palette <- "red_white_blue"
                         }
                         
                         base_colors <- heatmap_palettes[[palette]]
                         palette_colors <- colorRampPalette(base_colors)(n)
                         
                         if (direction == -1) rev(palette_colors) else palette_colors
                       },
                       
                       # Default fallback
                       {
                         warning("Unknown method. Using viridis colors.")
                         if (requireNamespace("viridis", quietly = TRUE)) {
                           viridis::viridis(n)
                         } else {
                           rainbow(n)
                         }
                       }
  )
  
  return(colors_out)
}

#' Create ggplot2 continuous color scales
#'
#' @param method Color palette method
#' @param palette Specific palette name
#' @param direction Direction of color scale (1 or -1)
#' @param ... Additional arguments passed to ggplot2 scale functions
#' @return ggplot2 scale function
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = wt, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_continuous_custom(method = "viridis", palette = "plasma")

scale_color_continuous_custom <- function(method = "viridis", 
                                          palette = NULL, 
                                          direction = 1, 
                                          n = 256,
                                          ...) {
  colors <- get_continuous_colors(n = n, method = method, palette = palette, direction = direction)
  ggplot2::scale_color_gradientn(colors = colors, ...)
}

#' Create ggplot2 continuous fill scales
#'
#' @param method Color palette method
#' @param palette Specific palette name
#' @param direction Direction of color scale (1 or -1)
#' @param ... Additional arguments passed to ggplot2 scale functions
#' @return ggplot2 scale function

scale_fill_continuous_custom <- function(method = "viridis", 
                                         palette = NULL, 
                                         direction = 1, 
                                         n = 256,
                                         ...) {
  colors <- get_continuous_colors(n = n, method = method, palette = palette, direction = direction)
  ggplot2::scale_fill_gradientn(colors = colors, ...)
}

#' Preview continuous color palette
#'
#' @param colors Vector of color codes or parameters for get_continuous_colors
#' @param values Optional vector of values to map colors to
#' @param title Plot title
#' @examples
#' # Preview a palette
#' colors <- get_continuous_colors(method = "publication", palette = "nature_heatmap")
#' preview_continuous_colors(colors)
#' 
#' # Preview with custom values
#' preview_continuous_colors(colors, values = seq(-1, 1, length.out = 100), 
#'                          title = "Diverging Scale")

preview_continuous_colors <- function(colors = NULL, 
                                      method = "viridis", 
                                      palette = NULL,
                                      values = NULL, 
                                      title = "Continuous Color Palette") {
  
  if (is.null(colors)) {
    colors <- get_continuous_colors(method = method, palette = palette)
  }
  
  n <- length(colors)
  
  if (is.null(values)) {
    values <- seq(0, 1, length.out = n)
  }
  
  # Create gradient plot
  par(mar = c(4, 1, 3, 1))
  image(matrix(values, nrow = 1), 
        col = colors, 
        axes = FALSE, 
        main = title,
        xlab = "Value Range")
  

  
  # Print palette info
  cat("\nPalette Information:\n")
  cat("Number of colors:", n, "\n")
  cat("Value range:", round(min(values), 3), "to", round(max(values), 3), "\n")
  cat("First color:", colors[1], "\n")
  cat("Last color:", colors[n], "\n")
}

#' Show available continuous palettes
#'
#' @param method Show palettes for specific method, or "all" for everything
#' @examples
#' show_continuous_palettes("publication")
#' show_continuous_palettes("all")

show_continuous_palettes <- function(method = "all") {
  
  palettes_info <- list(
    viridis = c("viridis", "plasma", "inferno", "magma", "cividis", "rocket", "mako", "turbo"),
    brewer_seq = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
                   "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", 
                   "YlGnBu", "YlOrBr", "YlOrRd"),
    brewer_div = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", 
                   "RdYlGn", "Spectral"),
    publication = c("nature_heatmap", "cell_heatmap", "temperature", "ocean", "forest", 
                    "climate", "geology", "sunset", "arctic"),
    scientific = c("parula", "jet", "hot", "cool", "spring", "winter", "autumn", 
                   "summer", "bone", "copper"),
    heatmap = c("red_white_blue", "blue_white_red", "green_black_red", 
                "blue_yellow_red", "spectral")
  )
  
  if (method == "all") {
    for (m in names(palettes_info)) {
      cat("\n", toupper(m), "palettes:\n")
      cat(paste(palettes_info[[m]], collapse = ", "), "\n")
    }
  } else if (method %in% names(palettes_info)) {
    cat(toupper(method), "palettes:\n")
    cat(paste(palettes_info[[method]], collapse = ", "), "\n")
  } else {
    cat("Available methods:", paste(names(palettes_info), collapse = ", "), "\n")
  }
}

# Example usage
if (FALSE) {
  # Basic continuous palettes
  colors1 <- get_continuous_colors(method = "viridis", palette = "plasma")
  colors2 <- get_continuous_colors(method = "brewer_seq", palette = "Blues")
  colors3 <- get_continuous_colors(method = "publication", palette = "nature_heatmap")
  
  # Preview palettes
  preview_continuous_colors(colors1, title = "Plasma Palette")
  preview_continuous_colors(method = "brewer_div", palette = "RdYlBu", 
                            title = "Diverging Palette")
  
  # Show available palettes
  show_continuous_palettes("publication")
  
  # Use with ggplot2
  library(ggplot2)
  
  # Heatmap example
  ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
    geom_point(size = 4) +
    scale_color_continuous_custom(method = "publication", palette = "temperature") +
    theme_minimal() +
    labs(title = "Publication-Quality Continuous Colors")
  
  # Fill example with custom colors
  custom_colors <- c("#000080", "#FFFFFF", "#FF0000")
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_tile() +
    scale_fill_continuous_custom(method = "custom", colors = custom_colors) +
    theme_minimal()
}


