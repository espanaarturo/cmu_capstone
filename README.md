# County-Level Risk Analysis of Healthcare Access and Racial Disparities in Preventable Hospitalizations

**Summer 2025 Capstone Project**  
UnitedHealth Group Bridges to Healthcare Technology Program at Carnegie Mellon University  
**Author:** Arturo Espana
---

## Overview

This project investigates how access to healthcare and structural determinants shape **racial disparities in preventable hospitalizations** across U.S. counties.

I developed a race-stratified modeling framework using:

- **Random Forests** to identify the most important predictors without assuming linearity
- **Negative Binomial Regression** for interpretable effect sizes and incidence rate ratios (IRRs)

My findings show that structural inequalities especially in socioeconomic conditions play a larger role in preventable hospitalizations among POC populations than clinical access alone. The results call for race-conscious interventions that address both medical and social determinants of health.

---

## Key Findings

- **High school completion**, **unemployment**, and **income inequality** ranked as top predictors of preventable hospitalizations for POC populations.
- **Rurality** and **provider supply** correlated paradoxically with hospitalization rates, suggesting overuse or systemic inefficiencies.
- Interventions focused solely on expanding health infrastructure may help White counties but are insufficient for POC communities unless upstream barriers are addressed.

---

## Methods

### Exploratory Data Analysis

- Examined variable distributions and racial disparities in preventable hospitalizations
- Visualized geographic clustering with choropleths and computed disparity ratios
- Evaluated multicollinearity using a correlation heatmap

### Modeling Approach

| Method                     | Purpose                                      |
|---------------------------|----------------------------------------------|
| **Random Forests**        | Ranked variable importance (flexible, nonparametric) |
| **Negative Binomial Regression** | Estimated effect sizes (IRRs) and compared predictors by race |

Models were stratified into **White** and **People of Color (POC)** populations and adjusted for population size using race-specific offsets.

---

## Tools & Technologies

- `R`
- Data from the **2025 County Health Rankings & Roadmaps**
- Modeling: Random Forests, Negative Binomial Regression

---

## Repository Structure

| Folder / File        | Description |
|----------------------|-------------|
| `final_report.html`  | Full technical report with visualizations, methods, and interpretation of findings |
| `poster.pdf`   | 	Capstone poster summarizing key results and recommendations |
| `dataPrep.R`              | Initial data loading, cleaning, and preprocessing pipeline |
| `EDA.R`              | Exploratory data analysis, including plots and descriptive summaries |
| `negBio.R`              | Negative Binomial regression modeling and diagnostics |
| `randomForest.R`              | 	Random Forest modeling, variable importance, and stratified subgroup analysis |

---

## Project Files

- [**Final Report (HTML)**](report/uhg_report.html)  
- [**Poster Presentation (PDF)**](poster/uhg_poster.pdf)  
- [**Code Repository**](https://github.com/espanaarturo/uhg-project)

---

## Summary of Contributions

- Built a robust, race-stratified model of preventable hospitalizations
- Applied both machine learning and statistical inference for dual insights
- Delivered a public-facing poster and technical report
- Presented results to UnitedHealth Group research advisors and academic mentors

---

## Recommendations

Based on the analysis, we propose:

1. **Race-conscious strategies** that address upstream barriers (education, income, employment)
2. Expansion of **Community Health Workers** and **preventive care access** in high-need areas
3. **Integrated care coordination models** for patients at risk of fragmented or overused hospital services
4. **More granular data collection** by race and geography to guide future interventions

---

## Acknowledgments

This research was conducted through the **UnitedHealth Group Bridges to Healthcare Technology Program**, hosted by **Carnegie Mellon University**.  

---

## Contact

**Arturo Espana**  
📧 [arturoespana@ucsb.edu](mailto:arturoespana@ucsb.edu)  
🔗 [LinkedIn](https://www.linkedin.com/in/arturo-espana)  

---
