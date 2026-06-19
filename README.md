# CHIKV Thermal Response Functions

This repository organizes temperature-dependent functions used to study chikungunya transmission risk. The current work checks individual thermal response functions, compares sources from the parameter table, and starts building the Mordecai-style baseline \(R_0(T)\) model for *Aedes aegypti* and *Aedes albopictus*.


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
- Mordecai default functions for both *Ae. aegypti* and *Ae. albopictus*.
- Separate relative \(R_0(T)\) plots for the two mosquito species.
- Source notes and parameter references from the latest thermal-functions table.

## Project Structure

```text
scripts/   R Markdown analysis and helper scripts
figures/   generated plots
output/    rendered reports and outputs
docs/      project notes, parameter tables, and reference files
```

## Interactive Plots

[The CHIKV R0 Explorer](https://30-na.github.io/CHIKV/)



It lets users choose mosquito species, temperature range, and one thermal function for each model parameter. The app recalculates the curve directly and reports zeros or non-finite values instead of silently fixing them.

## Main Files

- `scripts/Temp_functions.Rmd`: Main thermal-function analysis.
- `docs/Thermal_functions_table_6-12-26b.docx`: Latest parameter/source table.
