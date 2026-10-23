# Baseline clinical information and the benefit of model complexity in NAFLD mortality prediction

Code for the Data Mining course project by by **Albert Kušlevič**, **Pranas Žeromskas**, **Gabrielė Skirmantaitė** and **Vygintas Bartusevičius**.

The project asks whether more complex survival models like Cox model with splines and a random survival forest predict five-year mortality better than a linear Cox model in patients with non-alcoholic fatty liver disease (NAFLD). It also describes whether their advantage grows when more baseline clinical information is available.

## Data

The data are the `nafld1`, `nafld2` and `nafld3` tables distributed with the R package [`survival`](https://CRAN.R-project.org/package=survival): a 90% random sample of the Olmsted County NAFLD cohort (Allen et al., *Hepatology* 2018). The scripts load the data with `data(nafld, package = "survival")`. The same three tables can also be viewed and are described on Kaggle: https://www.kaggle.com/datasets/utkarshx27/non-alcohol-fatty-liver-disease/data.
