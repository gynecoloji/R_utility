# R Utility Functions Repository

A comprehensive collection of R utility functions for biolkogy data analysis, visualization, and statistical computing.(Especially for sequencing data analysis)

## Overview

This repository provides a curated set of R functions (summarizing the functions created by me from 2020) designed to streamline common workflows in computational biology and data science. The utilities cover the full spectrum from basic data visualization to advanced genomics analyses, making it easier to perform reproducible research. It is mostly used for my own research purpose. I would be very happy if others find it also useful for their research.

## Features

### 📊 Data Visualization
- Mainly focusing on basic graphs (scatterplot, boxplot, barplot and ridgeplot)
- Publication-ready plotting functions
- Add important stat-related information to plots
- Custom ggplot2 themes and color palettes
- Integrate pheatmap package for better visualization
- Multi-panel figure generation

### 🧬 Single Cell Data Analysis

#### Basic Single Cell Analysis
- Quality control metrics and filtering
- Normalization and scaling functions
- Dimensionality reduction (PCA, t-SNE, UMAP)
- Cell type annotation utilities
- Basic differential expression analysis

#### Advanced Single Cell Analysis
- Trajectory inference and pseudotime analysis
- Cell-cell communication analysis
- Integration of multiple datasets
- Advanced clustering algorithms
- Multi-modal data integration (RNA + ATAC)

### 🧪 Bulk RNA-seq Analysis
- Read count processing and normalization
- Differential gene expression analysis
- Gene set enrichment analysis
- Pathway analysis and visualization
- Alternative splicing analysis
- Co-expression network analysis

### 🔬 Epigenomics Analysis (ATAC-seq/ChIP-seq/Cut&Run-seq)
- Peak calling and annotation
- Differential accessibility/binding analysis
- Motif enrichment analysis
- ChromHMM state analysis
- Integration with transcriptomic data

### 📈 Statistical Analysis
- Advanced statistical testing functions
- Multiple testing correction utilities
- Power analysis and sample size calculations
- Survival analysis functions

## Installation

### Prerequisites
Ensure you have R (≥ 4.0.0) installed along with the following core packages:

```r
# Install required CRAN packages
install.packages(c(
  "tidyverse", "ggplot2", "dplyr", "data.table",
  "devtools", "BiocManager", "remotes"
))

# Install Bioconductor packages
BiocManager::install(c(
  "Seurat", "SingleCellExperiment", "DESeq2", 
  "edgeR", "limma", "GenomicRanges", "rtracklayer",
  "ChIPseeker", "DiffBind", "ATACseqQC"
))
```

### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/gynecoloji/R_utility.git
cd R_utility
```

2. Source the functions:
```r
# Source all utility functions
source("R/module_name.R") # module_name.R should be existent in R folder
```


## Examples and Tutorials

The `rmd/` directory contains comprehensive tutorials:

- `01_visualization_examples.Rmd` - Plotting and visualization
- `02_single_cell_basic.Rmd` - Single cell analysis basics
- `03_single_cell_advanced.Rmd` - Advanced single cell workflows
- `04_bulk_rnaseq_pipeline.Rmd` - RNA-seq analysis pipeline
- `05_epigenomics_analysis.Rmd` - ChIP-seq/ATAC-seq workflows
- `06_statistical_analysis.Rmd` - Advanced statistics


## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Workflow

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-function`)
3. Add your function with documentation and tests
4. Commit your changes (`git commit -am 'Add amazing function'`)
5. Push to the branch (`git push origin feature/amazing-function`)
6. Open a Pull Request

### Function Guidelines

- Follow consistent naming conventions (snake_case)
- Include comprehensive documentation with `@param` and `@return`
- Add unit tests for all functions
- Include example usage in documentation
- Ensure functions handle edge cases gracefully

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use these utility functions in your research, please cite:

```
@software{r_utility_functions,
  author = {Ji Wang},
  title = {R Utility Functions for Biology Data Analysis},
  url = {https://github.com/gynecoloji/R_utility},
  year = {2025}
}
```

## Support
- 📧 Contact: gynecoloji@gmail.com


## Acknowledgments

- The R Core Team for developing R
- The Bioconductor community for excellent genomics packages
- Hadley Wickham and the tidyverse team for data manipulation and visualization tools
- Pheatmap developers for enhanced heatmap visualizations
- Seurat team for single cell analysis tools
- DESeq2 and edgeR developers for RNA-seq analysis methods
- The broader R community for statistical computing tools
- All contributors who have helped improve this repository
---

**Note**: This repository is actively maintained. Please check for updates regularly and report any issues you encounter.