# rankflex

**Flexible Framework for Multi-Source Ranking Aggregation and Visualization**
---

## Overview
`rankflex` is an R package that provides a general-purpose framework for **integrating, aggregating, and visualizing rankings from multiple sources**. It is designed for scenarios where you need to combine, standardize, and analyze ranking data from different platforms (e.g., university rankings, movie ratings, sports standings).

Key use cases include:
- Cleaning and standardizing multi-source ranking data
- Constructing personalized composite scores with custom weights
- Performing sensitivity analysis on weighting schemes
- Optimizing weights via linear programming
- Comparing custom rankings against original sources
- Visualizing results with static plots and animated multi-year bar charts

The package includes a sample dataset of **global university rankings (QS, Times Higher Education, and ARWU, 2023–2025)** as a demonstration use case.

---

## Installation
You can install the development version of `rankflex` from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install rankflex
devtools::install_github("shuiqiujian/rankflex")

# Load the package
library(rankflex)

```
---

## Quick Start Example
This example uses the included university ranking data to demonstrate the core workflow:

```r
library(rankflex)

# Load the sample dataset (QS, THE, ARWU rankings, 2023-2025)
data(rankings_sample)

# Step 1: Filter to a single source (QS) and choose indicators
# rankings_sample is long-format: each row is one university x one source x one year.
# Filter to one source before standardizing to avoid cross-source NA contamination.
qs_df <- rankings_sample[rankings_sample$source == "QS", ]

indicators <- c("score_overall", "score_academic_reputation",
                "score_employer_reputation", "score_citations_faculty")
indicators <- indicators[indicators %in% names(qs_df)]

# Step 2: Min-max normalize indicators to 0-100
qs_std <- standardized_scores(qs_df, indicators = indicators)

# Step 3: Construct composite rankings with custom weights
weights <- c(
  score_overall                = 0.40,
  score_academic_reputation    = 0.30,
  score_employer_reputation    = 0.20,
  score_citations_faculty      = 0.10
)
weights <- weights[names(weights) %in% indicators]

composite <- compute_composite(qs_std, weights = weights, years = 2025)

# Step 4: Generate a static ranking chart
plot_ranking_static(composite, year = 2025, top_n = 20)

# Step 5: Compare composite vs. original QS ranks
compared <- compare_rankings(composite, source_ranks = "rank")
head(compared[order(compared$diff_rank), 
              c("entity", "rank", "composite_rank", "diff_rank", "label_rank")], 10)
```
---

## Core Features
 
- Data Loading & Cleaning: Import ranking data from CSV/Excel with auto encoding detection; clean and standardize rank strings like "=1" or "101–150".
- Multi-Source Merging: Combine data from multiple sources into a wide-format table with merge_datasets().
- Composite Score Construction: Apply custom weights to build personalized rankings with compute_composite().
- Sensitivity Analysis: Test how stable rankings are under random weight perturbations with sensitivity_analysis().
- Weight Optimization: Find the best possible weighting scheme for a target entity via linear programming (optimize_weights()).
- Ranking Comparison: Identify over- and under-rated entities with compare_rankings().
- Visualization: Static horizontal bar charts (plot_ranking_static()) and animated racing bar charts (plot_ranking_dynamic()) for multi-year trends.
- Built-in Example Data: 13,000+ rows of cleaned 2023–2025 global university rankings (QS, THE, ARWU) included as rankings_sample.
 ---
 
## About the Author
 
Undergraduate student at CUHK-Shenzhen, passionate about data analytics, operations research, and building practical tools with R.
