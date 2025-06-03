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
  "Seurat"
)

suppressPackageStartupMessages({
  check_and_load_packages(required_packages)
})


#' Generate a Toy Seurat Object for Testing
#' 
#' Creates a realistic toy single-cell RNA-seq dataset with customizable parameters
#' for testing Seurat analysis pipelines. The generated data mimics real scRNA-seq
#' characteristics including sparse expression patterns, mitochondrial genes, and
#' realistic count distributions.
#' 
#' @param n_cells Integer. Number of cells to generate (default: 1000)
#' @param n_genes Integer. Total number of genes to generate (default: 2000)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' 
#' @return A Seurat object containing:
#'   \item{counts}{Sparse count matrix with realistic scRNA-seq expression patterns}
#'   \item{meta.data}{Basic cell metadata including nCount_RNA and nFeature_RNA}
#'   \item{features}{Gene names including mitochondrial genes (MT-*) for QC testing}
#' 
#' @details 
#' The function generates realistic single-cell data with the following characteristics:
#' \itemize{
#'   \item 50 mitochondrial genes (MT-*) with typically higher expression levels
#'   \item Variable gene expression probabilities (5-80% of cells per gene)
#'   \item Negative binomial distribution for count generation
#'   \item Sparse matrix structure typical of scRNA-seq data
#'   \item Realistic range of UMI counts and gene detection rates per cell
#' }
#' 
#' The generated object is suitable for testing:
#' \itemize{
#'   \item Quality control metrics and filtering functions
#'   \item Normalization and scaling procedures
#'   \item Dimensionality reduction and clustering algorithms
#'   \item Differential expression analysis
#' }
#' 
#' @examples
#' # Generate default toy dataset
#' toy_seurat <- generate_toy_seurat()
#' 
#' # Generate smaller dataset for quick testing
#' small_toy <- generate_toy_seurat(n_cells = 500, n_genes = 1000)
#' 
#' # Generate larger dataset with specific seed
#' large_toy <- generate_toy_seurat(n_cells = 2000, n_genes = 3000, seed = 456)
#' 
#' @seealso 
#' \code{\link[Seurat]{CreateSeuratObject}} for creating Seurat objects from real data
#' 
#' @export
generate_toy_seurat <- function(n_cells = 1000, n_genes = 2000, seed = 123) {
  set.seed(seed)
  
  # Create gene names (including mitochondrial genes)
  gene_names <- c(
    paste0("GENE", 1:(n_genes - 50)),  # Regular genes
    paste0("MT-", c("ND1", "ND2", "CO1", "CO2", "ATP8", "ATP6", "CO3", "ND3", 
                    "ND4L", "ND4", "ND5", "ND6", "CYB", "RNR1", "RNR2",
                    paste0("GENE", 1:35)))  # 50 mitochondrial genes
  )
  
  # Create cell barcodes
  cell_barcodes <- paste0("CELL_", sprintf("%04d", 1:n_cells))
  
  # Generate realistic count matrix
  # Most genes have low expression, some have high expression
  counts_matrix <- matrix(0, nrow = n_genes, ncol = n_cells)
  rownames(counts_matrix) <- gene_names
  colnames(counts_matrix) <- cell_barcodes
  
  # Fill matrix with realistic scRNA-seq data
  for (i in 1:n_genes) {
    # Probability that a gene is expressed in a cell (varies by gene)
    prob_expressed <- runif(1, 0.05, 0.8)
    
    # Which cells express this gene
    expressing_cells <- rbinom(n_cells, 1, prob_expressed) == 1
    
    if (sum(expressing_cells) > 0) {
      # Expression levels for expressing cells (negative binomial distribution)
      if (grepl("^MT-", gene_names[i])) {
        # Mitochondrial genes tend to have higher expression
        counts_matrix[i, expressing_cells] <- rnbinom(
          sum(expressing_cells), 
          size = 10, 
          mu = 20
        )
      } else {
        # Regular genes
        counts_matrix[i, expressing_cells] <- rnbinom(
          sum(expressing_cells), 
          size = 5, 
          mu = 8
        )
      }
    }
  }
  
  # Create Seurat object
  seurat_obj <- CreateSeuratObject(
    counts = counts_matrix,
    project = "ToyDataset",
    min.cells = 0,
    min.features = 0
  )
  
  return(seurat_obj)
}



#' Generate a Complex Toy Seurat Object with Real Gene Symbols
#' 
#' Creates a realistic complex single-cell RNA-seq dataset using real gene symbols
#' including mitochondrial genes, housekeeping genes, cell cycle genes, and cell-type
#' specific markers for stromal, immune, and epithelial cells. Optionally includes
#' batch effects simulation.
#' 
#' @param n_cells Integer. Total number of cells to generate (default: 2000)
#' @param n_stromal Integer. Number of stromal cells (default: 600)
#' @param n_immune Integer. Number of immune cells (default: 800) 
#' @param n_epithelial Integer. Number of epithelial cells (default: 600)
#' @param include_batch Logical. Whether to simulate batch effects (default: FALSE)
#' @param n_batches Integer. Number of batches if batch effects enabled (default: 3)
#' @param batch_effect_strength Numeric. Strength of batch effects 0-1 (default: 0.3)
#' @param noise_level Numeric. Technical noise level 0-1 (default: 0.2)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#' 
#' @return A Seurat object containing:
#'   \item{counts}{Sparse count matrix with realistic expression patterns}
#'   \item{meta.data}{Cell metadata including cell_type, batch, and QC metrics}
#'   \item{features}{Real gene symbols organized by functional categories}
#' 
#' @details 
#' The function generates realistic single-cell data with:
#' \itemize{
#'   \item Real gene symbols from major functional categories
#'   \item Cell-type specific expression patterns
#'   \item Mitochondrial genes with elevated expression
#'   \item Housekeeping genes with consistent expression
#'   \item Cell cycle genes with phase-specific patterns
#'   \item Optional batch effects with technical variation
#'   \item Realistic sparsity and count distributions
#' }
#' 
#' Cell types simulated:
#' \itemize{
#'   \item Stromal cells: High expression of COL1A1, VIM, ACTA2, etc.
#'   \item Immune cells: High expression of PTPRC, CD3E, CD68, etc.
#'   \item Epithelial cells: High expression of EPCAM, KRT18, CDH1, etc.
#' }
#' 
#' @examples
#' # Generate complex dataset without batch effects
#' complex_seurat <- generate_complex_toy_seurat()
#' 
#' # Generate with batch effects
#' batch_seurat <- generate_complex_toy_seurat(
#'   include_batch = TRUE, 
#'   n_batches = 3,
#'   batch_effect_strength = 0.4
#' )
#' 
#' # Custom cell type proportions
#' custom_seurat <- generate_complex_toy_seurat(
#'   n_stromal = 400, 
#'   n_immune = 1000, 
#'   n_epithelial = 600
#' )
#' 
#' @export
generate_complex_toy_seurat <- function(n_cells = 2000, 
                                       n_stromal = 600, 
                                       n_immune = 800, 
                                       n_epithelial = 600,
                                       include_batch = FALSE,
                                       n_batches = 3,
                                       batch_effect_strength = 0.3,
                                       noise_level = 0.2,
                                       seed = 123) {
  
  set.seed(seed)
  
  # Validate cell numbers
  if (n_stromal + n_immune + n_epithelial != n_cells) {
    warning("Sum of cell type numbers doesn't match n_cells. Adjusting proportionally.")
    total_specified <- n_stromal + n_immune + n_epithelial
    n_stromal <- round(n_stromal * n_cells / total_specified)
    n_immune <- round(n_immune * n_cells / total_specified)
    n_epithelial <- n_cells - n_stromal - n_immune
  }
  
  # Define real gene symbols by category
  mito_genes <- c(
    "MT-ND1", "MT-ND2", "MT-CO1", "MT-CO2", "MT-ATP8", "MT-ATP6", 
    "MT-CO3", "MT-ND3", "MT-ND4L", "MT-ND4", "MT-ND5", "MT-ND6", 
    "MT-CYB", "MT-RNR1", "MT-RNR2", "MT-TP", "MT-TF", "MT-TV"
  )
  
  housekeeping_genes <- c(
    "ACTB", "GAPDH", "TUBB", "RPL13A", "RPS18", "HPRT1", "TBP", 
    "YWHAZ", "B2M", "GUSB", "HMBS", "RPL32", "RPS13", "SDHA",
    "UBC", "PPIA", "RPL19", "RPLP0", "TFRC", "PGK1"
  )
  
  cell_cycle_genes <- c(
    # S phase
    "MCM2", "PCNA", "TYMS", "FEN1", "MCM7", "RPA2", "WDR76", 
    "SLBP", "CCNE2", "UBR7", "POLD3", "MSH2", "ATAD2", "RAD51AP1",
    # G2/M phase  
    "HMGB2", "CDK1", "NUSAP1", "UBE2C", "BIRC5", "TPX2", "TOP2A",
    "NDC80", "CKS2", "NUF2", "CKS1B", "MKI67", "TMPO", "CENPF"
  )
  
  stromal_markers <- c(
    "COL1A1", "COL1A2", "COL3A1", "VIM", "FN1", "ACTA2", "TAGLN",
    "MYH11", "CNN1", "MYLK", "PDGFRA", "PDGFRB", "DCN", "LUM",
    "SPARC", "BGN", "FSTL1", "MGP", "SERPINF1", "TIMP1"
  )
  
  immune_markers <- c(
    "PTPRC", "CD3E", "CD3D", "CD4", "CD8A", "CD68", "CD14", "CD16",
    "IL7R", "CCR7", "CD44", "CD69", "GZMB", "PRF1", "IFNG", "TNF",
    "IL2", "FOXP3", "CD25", "CTLA4", "PD1", "LAG3", "TIM3", "CD86"
  )
  
  epithelial_markers <- c(
    "EPCAM", "KRT18", "KRT19", "KRT8", "CDH1", "CLDN3", "CLDN4",
    "OCLN", "TJP1", "PKP3", "DSP", "JUP", "CTNNB1", "MUC1",
    "LGALS3", "S100A4", "KRT5", "KRT14", "TP63", "MYC"
  )
  
  # Additional background genes for complexity
  background_genes <- c(
    "MALAT1", "NEAT1", "XIST", "RPS4Y1", "EIF1AY", "DDX3Y", "UTY",
    "HSPA1A", "HSPA1B", "HSP90AA1", "CALM1", "CALM2", "CALM3",
    "FTL", "FTH1", "TMSB4X", "S100A6", "S100A11", "ANXA1", "ANXA2"
  )
  
  # Combine all genes
  all_genes <- c(mito_genes, housekeeping_genes, cell_cycle_genes, 
                 stromal_markers, immune_markers, epithelial_markers, 
                 background_genes)
  
  # Remove duplicates and create final gene list
  all_genes <- unique(all_genes)
  n_genes <- length(all_genes)
  
  cat("Total genes in dataset:", n_genes, "\n")
  
  # Create cell metadata
  cell_types <- c(
    rep("Stromal", n_stromal),
    rep("Immune", n_immune), 
    rep("Epithelial", n_epithelial)
  )
  
  # Create batch assignments if requested
  if (include_batch) {
    batches <- sample(paste0("Batch", 1:n_batches), n_cells, replace = TRUE)
  } else {
    batches <- rep("Batch1", n_cells)
  }
  
  # Create cell cycle phases
  cell_cycle_phases <- sample(c("G1", "S", "G2M"), n_cells, 
                             replace = TRUE, prob = c(0.6, 0.25, 0.15))
  
  # Generate cell barcodes
  cell_barcodes <- paste0("CELL_", sprintf("%04d", 1:n_cells))
  
  # Initialize count matrix
  counts_matrix <- matrix(0, nrow = n_genes, ncol = n_cells)
  rownames(counts_matrix) <- all_genes
  colnames(counts_matrix) <- cell_barcodes
  
  cat("Generating expression patterns...\n")
  
  # Generate expression for each gene category
  for (i in 1:n_genes) {
    gene <- all_genes[i]
    
    # Determine base expression parameters based on gene type
    if (gene %in% mito_genes) {
      base_prob <- 0.95
      base_mu <- 25
      base_size <- 8
    } else if (gene %in% housekeeping_genes) {
      base_prob <- 0.90
      base_mu <- 15
      base_size <- 10
    } else if (gene %in% cell_cycle_genes) {
      base_prob <- 0.3
      base_mu <- 8
      base_size <- 5
    } else if (gene %in% stromal_markers) {
      base_prob <- 0.4
      base_mu <- 12
      base_size <- 6
    } else if (gene %in% immune_markers) {
      base_prob <- 0.4
      base_mu <- 12  
      base_size <- 6
    } else if (gene %in% epithelial_markers) {
      base_prob <- 0.4
      base_mu <- 12
      base_size <- 6
    } else {
      base_prob <- 0.2
      base_mu <- 5
      base_size <- 3
    }
    
    # Cell-type specific modulation
    cell_specific_multiplier <- rep(1, n_cells)
    
    if (gene %in% stromal_markers) {
      cell_specific_multiplier[cell_types == "Stromal"] <- runif(n_stromal, 2, 5)
      cell_specific_multiplier[cell_types != "Stromal"] <- runif(n_cells - n_stromal, 0.1, 0.5)
    } else if (gene %in% immune_markers) {
      cell_specific_multiplier[cell_types == "Immune"] <- runif(n_immune, 2, 5)
      cell_specific_multiplier[cell_types != "Immune"] <- runif(n_cells - n_immune, 0.1, 0.5)
    } else if (gene %in% epithelial_markers) {
      cell_specific_multiplier[cell_types == "Epithelial"] <- runif(n_epithelial, 2, 5)
      cell_specific_multiplier[cell_types != "Epithelial"] <- runif(n_cells - n_epithelial, 0.1, 0.5)
    }
    
    # Cell cycle specific modulation
    if (gene %in% cell_cycle_genes) {
      s_genes <- c("MCM2", "PCNA", "TYMS", "FEN1", "MCM7", "RPA2", "WDR76")
      g2m_genes <- c("HMGB2", "CDK1", "NUSAP1", "UBE2C", "BIRC5", "TPX2", "TOP2A")
      
      if (gene %in% s_genes) {
        cell_specific_multiplier[cell_cycle_phases == "S"] <- 
          cell_specific_multiplier[cell_cycle_phases == "S"] * runif(sum(cell_cycle_phases == "S"), 3, 6)
      } else if (gene %in% g2m_genes) {
        cell_specific_multiplier[cell_cycle_phases == "G2M"] <- 
          cell_specific_multiplier[cell_cycle_phases == "G2M"] * runif(sum(cell_cycle_phases == "G2M"), 3, 6)
      }
    }
    
    # Batch effects if enabled
    batch_multiplier <- rep(1, n_cells)
    if (include_batch) {
      for (batch in unique(batches)) {
        batch_cells <- batches == batch
        batch_effect <- runif(1, 1 - batch_effect_strength, 1 + batch_effect_strength)
        batch_multiplier[batch_cells] <- batch_effect
      }
    }
    
    # Final expression calculation
    final_mu <- base_mu * cell_specific_multiplier * batch_multiplier
    final_prob <- pmin(base_prob * cell_specific_multiplier, 0.95)
    
    # Generate expression
    expressing_cells <- rbinom(n_cells, 1, final_prob) == 1
    
    if (sum(expressing_cells) > 0) {
      # Add technical noise
      noise_factor <- 1 + rnorm(sum(expressing_cells), 0, noise_level)
      noise_factor <- pmax(noise_factor, 0.1)  # Prevent negative values
      
      counts_matrix[i, expressing_cells] <- rnbinom(
        sum(expressing_cells),
        size = base_size,
        mu = final_mu[expressing_cells] * noise_factor
      )
    }
  }
  
  cat("Creating Seurat object...\n")
  
  # Create Seurat object
  seurat_obj <- CreateSeuratObject(
    counts = counts_matrix,
    project = "ComplexToyDataset",
    min.cells = 0,
    min.features = 0
  )
  
  # Add metadata
  seurat_obj$cell_type <- cell_types
  seurat_obj$batch <- batches
  seurat_obj$cell_cycle_phase <- cell_cycle_phases
  
  # Add gene categories to misc slot for reference
  seurat_obj@misc$gene_categories <- list(
    mitochondrial = mito_genes,
    housekeeping = housekeeping_genes,
    cell_cycle = cell_cycle_genes,
    stromal_markers = stromal_markers,
    immune_markers = immune_markers,
    epithelial_markers = epithelial_markers,
    background = background_genes
  )
  
  cat("Complex Seurat object created successfully!\n")
  cat("Cells by type:\n")
  print(table(seurat_obj$cell_type))
  if (include_batch) {
    cat("Cells by batch:\n")
    print(table(seurat_obj$batch))
  }
  cat("Cell cycle phases:\n")
  print(table(seurat_obj$cell_cycle_phase))
  
  return(seurat_obj)
}

# Example usage and testing
cat("=== Example Usage ===\n")

# Generate complex dataset without batch effects
complex_seurat <- generate_complex_toy_seurat(seed = 123)

# Generate with batch effects  
complex_seurat_batch <- generate_complex_toy_seurat(
  include_batch = TRUE,
  n_batches = 3, 
  batch_effect_strength = 0.4,
  seed = 456
)

cat("\nDataset without batch effects:\n")
cat("Dimensions:", dim(complex_seurat), "\n")

cat("\nDataset with batch effects:\n") 
cat("Dimensions:", dim(complex_seurat_batch), "\n")

# Quick visualization examples
if (require(ggplot2, quietly = TRUE)) {
  cat("\n=== Quick QC Plots ===\n")
  
  # Add basic QC metrics
  complex_seurat[["percent.mt"]] <- PercentageFeatureSet(complex_seurat, pattern = "^MT-")
  
  # Plot QC by cell type
  p1 <- VlnPlot(complex_seurat, features = "nCount_RNA", group.by = "cell_type")
  p2 <- VlnPlot(complex_seurat, features = "percent.mt", group.by = "cell_type")
  
  print(p1 / p2)
}