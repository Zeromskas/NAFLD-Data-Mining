# Baseline clinical information and the benefit of model complexity in NAFLD mortality prediction

Code for the Data Mining course project by by **Albert Kušlevič**, **Pranas Žeromskas**, **Gabrielė Skirmantaitė** and **Vygintas Bartusevičius**.

The project asks whether more complex survival models like Cox model with splines and a random survival forest predict five-year mortality better than a linear Cox model in patients with non-alcoholic fatty liver disease (NAFLD). It also describes whether their advantage grows when more baseline clinical information is available.

## Data

The data are the `nafld1`, `nafld2` and `nafld3` tables distributed with the R package [`survival`](https://CRAN.R-project.org/package=survival): a 90% random sample of the Olmsted County NAFLD cohort (Allen et al., *Hepatology* 2018). The scripts load the data with `data(nafld, package = "survival")`.

## Code

`eda.R` contains the data analysis for the draft:

- builds the analysis sample of 3,015 NAFLD cases with recorded BMI;
- summarises baseline characteristics, follow-up and censoring;
- estimates five-year mortality by age, sex, BMI and number of metabolic conditions.

To run it, install the `survival` package version 3.8-12 and run `eda.R` in R or RStudio:

```r
install.packages("survival")
source("eda.R")
```

The code for the survival models will be added for the final paper.
