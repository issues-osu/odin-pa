# Racial and Ethnic Differences in Drug Overdoses and Naloxone Administration in Pennsylvania

This repository contains the full data processing, spatial analysis, and modeling pipeline used in the study:

**Barboza, G. E., & Salerno, J. D.** (2020).  
*A descriptive study of racial and ethnic differences of drug overdoses and naloxone administration in Pennsylvania.*  
**International Journal of Drug Policy**, 83, 102718.  
[https://doi.org/10.1016/j.drugpo.2020.102718](https://doi.org/10.1016/j.drugpo.2020.102718)

---

## 🧭 Workflow Overview

### 1. Setup and Data Import
- Set global options (`options(warn = -1)`, `scipen = 999`).
- Load required packages (e.g., `ggplot2`, `dplyr`, `sf`, `tidycensus`, `sjPlot`).
- Import cleaned ODIN data from Pennsylvania Overdose Information Network (2018–2019).
- Standardize race/ethnicity classifications (NHWhite, NHBlack, Hispanic) and recode drug types, revival actions, and timestamps.

### 2. Preprocessing and Subsetting
- Generate subsets of:
  - **Unique incidents**
  - **Unique victims per incident**
  - **Victims with multiple suspected drugs**
- Extract incident-level variables including time of day, age group, and survival outcomes.

### 3. Population and Geographic Data
- Download race/ethnicity population denominators from the 2020 ACS 5-year estimates using `tidycensus`.
- Join OD data with population denominators to compute:
  - Race-specific overdose response rates per 100,000 (e.g., `nOD_white_rate`)
- Integrate with county-level shapefiles from `tigris`.
- Add rural/urban classification for stratified comparison.

### 4. Statistical Analysis
- Conduct descriptive statistics and chi-square tests comparing overdose and naloxone patterns by race/ethnicity.
- Estimate logistic regression models predicting survival, including:
  - Main effects for naloxone, drug type, and demographics
  - Interaction terms: `Naloxone_Admin * Race * Gender`
- Visualize effects using `sjPlot::plot_model()` and `effects::allEffects()`.

### 5. Spatial Analysis and Mapping
- Create quantile-binned overdose response rate variables:
  - `OD_all_quantiles`, `OD_white_quantiles`, `OD_black_quantiles`, `OD_hisp_quantiles`
- Join spatial `sf` geometries and overdose rates.
- Generate `ggplot2` choropleths for:
  - All races combined
  - NHWhite, NHBlack, and Hispanic separately
- Apply `viridis` fill scales, remove axis labels and gridlines for clean visualization.

### 6. Temporal Analysis
- Plot OD response frequencies by:
  - Hour of day
  - Day of week
  - Month of year
- Stratify by race/ethnicity.
- Calculate and compare mean doses and survival across time windows.

---

## 📁 Directory Structure

