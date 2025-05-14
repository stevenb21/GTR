# GTR: Automated Exploratory Data Analysis in R

## Overview

GTR (Generalized Tabular Reporter) is an R package and Shiny app designed to streamline exploratory data analysis (EDA) for tabular datasets. The tool automatically generates informative summaries, diagnostic plots, and statistical tests with minimal user input, making it especially useful for quick, high-level data inspection.

This project was developed as a class final project to demonstrate comprehensive R package development, interactive data visualization, and statistical modeling.

## Features

- **Univariate Analysis:** Histograms, QQ-plots, and categorical bar plots for each feature.
- **Bivariate Analysis:** Boxplots, scatter plots, grouped bar plots, and correlation heatmaps.
- **Data Summarization:** Summary tables with skewness, kurtosis, missing values, and memory usage.
- **Feature Transformation:** Log, sqrt, standardization, and imputation pipelines.
- **Automated Statistical Testing:** 
  - Categorical DV: Chi-squared tests, ANOVA
  - Continuous DV: Linear, logistic, and multinomial regression
- **Interactive Web Application:** Shiny interface for uploading CSVs, setting categorical features, and viewing results across multiple tabs.

## Installation

Clone the repo and open the `.Rproj` file in RStudio:

```bash
git clone https://github.com/your-username/GTR.git
cd GTR
````

Install required dependencies:

```r
install.packages(c("shiny", "flextable", "patchwork", "moments", 
                   "ggplot2", "dplyr", "reshape2", "randomForest", 
                   "entropy", "nnet", "tidyverse"))
```

## Usage

Launch the Shiny app:

```r
source("R/gtr_shiny.R")
gtr_shiny()
```

Or use programmatically:

```r
library(GTR)
data(iris)
catvec <- c(0, 0, 0, 0, 1)
compute(iris, "Species", catvec)
```

## File Structure

* `R/` - Core R functions and helpers
* `inst/` - RMarkdown helpers rendered inside the Shiny UI
* `vignettes/` - Example use cases and package documentation
* `man/` - Function documentation (via roxygen2)
* `DESCRIPTION` - Package metadata
* `GTR.Rproj` - RStudio project file

## Example Input

* Input: CSV file with a mix of continuous and categorical variables
* User provides:

  * Vector of categorical column indices (e.g., `2,5,6`)
  * Name of dependent variable

## Example Output

* Flextable summary report
* Patchwork plots showing:

  * Distribution of features
  * Relationship to dependent variable
  * Correlation heatmap
* Statistical test results with significance markers

## Author

Steven Brooks
Indiana University
Contact: [stevbroo@iu.edu](mailto:stevbroo@iu.edu)

## License

MIT License

---

This project was submitted as a final deliverable for Computing for Biostatistics at Indiana University.

```
