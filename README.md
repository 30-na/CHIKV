# CHIKV Thermal Response Functions

This repository organizes temperature-dependent functions used to study chikungunya transmission risk. The current work checks individual thermal response functions, compares sources from the parameter table, and builds a Mordecai-style baseline \(R_0(T)\) model for *Aedes aegypti* and *Aedes albopictus*.


## Baseline Model

The baseline reproduction number follows the Mordecai-style temperature-dependent form:

```math
R_0(T)=
\sqrt{
\frac{
a(T)^2 b(T)c(T)e^{-\mu(T)/PDR(T)}EFD(T)p_{EA}(T)MDR(T)
}{
N r \mu(T)^3
}
}
```

where:

- `a(T)`: biting rate
- `b(T)`: vector-to-human transmission probability
- `c(T)`: human-to-vector transmission probability
- `mu(T)`: adult mosquito mortality rate
- `PDR(T)`: parasite/virus development rate
- `EFD(T)`: eggs per female per day
- `pEA(T)`: egg-to-adult survival probability
- `MDR(T)`: mosquito development rate

## Current Contents

- Thermal response plots for each parameter.
- Mordecai baseline functions for both *Ae. aegypti* and *Ae. albopictus*.
- Interactive Shiny app for comparing \(R_0(T)\) across thermal-function choices.

## Project Structure

```text
R/         shared thermal response functions
scripts/   R Markdown analysis and helper scripts
interactive/ Shiny app for interactive R0 exploration
figures/   generated plots
docs/      project notes, parameter tables, and reference files
```

## Interactive Shiny App

[The CHIKV R0 Explorer](https://sinamokhtar.shinyapps.io/CHIKV/)

The app lets users choose mosquito species, temperature range, and one thermal function for each model parameter. It always shows the Mordecai baseline as a dashed comparison curve.

The plotted \(R_0(T)\) curves are standardized by the maximum raw \(R_0\) across all available function combinations for the selected species and temperature range. This makes curves comparable on a 0--1 scale.

To run locally:

```r
shiny::runApp("interactive")
```

Note: GitHub Pages cannot run Shiny apps directly. The online app link should point to a deployed Shiny service such as shinyapps.io or Posit Connect.

## Main Files

- `R/thermal_functions.R`: Shared thermal-response functions.
- `scripts/Temp_functions.Rmd`: Main thermal-function analysis.
- `interactive/app.R`: Shiny app for interactive \(R_0(T)\) exploration.
- `docs/Thermal_functions_table_6-22-26.docx`: Latest parameter/source table.
