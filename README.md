## Bachelor Thesis

---

This repository hosts all code used to produce the models, graphs and calculations in my Bachelor Thesis **Margin Procyclicality during Covid 19 - Drivers, Impact and Solutions**

Last Edit: DATE
Submission: DATE
---

### Description of Files

- All proprietary functions used in the scripts are contained in :link: ![*functions*](functions.R)
- The scripts for each plot are located in :link: ![*Plots*](Plots/)
- All plots as .png files are located in  :link: ![*Plots/Output](Plots/Output)*
- All calculated procyclicality stats as reported in Chapter XX are located in the files :link: ![*measures.csv*](measures_short.csv) and ![*measures.csv*](measures_long.csv)
- All calculations performed to obtain the procyclicality stats are in the scripts :link: ![*procyclicality_calculations_long.r*](procyclicality_calculations_long.r) and ![*procyclicality_calculations_short.r*](procyclicality_calculations_short.r)
- All raw data used is located on  :link: ![*OneDrive*](https://1drv.ms/u/s!AoQRAZtdS9u4iZYwkNuHxv9e582O_g?e=1WXJlr)

### Margin Calculation

:one: All necessary arguments / static parameters which are needed as inputs for the Margin model must be stored in a list.

Necessary arguments:

- MPOR - Marign Period of Risk
- Factor - Robust upscaling Factor
- quantile - VaR quantile
- lambda - decay factor for the EWMA volatility calculation
- n_day - length of lookback period
- liq_group - liquidation group of the instrument for which margin is - calculated
- short - whether the margin is calculated for a long or short position

```
# store parameters 
params <- list(
    MPOR = 3, factor = 2, quantile = .95, 
    lambda = .95, n_day = 750, liq_group = "PEQ01",
    short = FALSE)
```

:two: the Margin Calculation functions draw their input data from a file which **must** be loaded into memory and assigned to the variable *master*. The file is located in :link: ![*Data/data_input.xlsx*](Data/data_input.xlsx). This file
contains the product names, their risk factors (such as returns) and other information such as stress dates for the floored margin calculation. The file contains all necessary information to calculate Margins for the products FESX/ FSMI/ FGBX/ FGBL.

Further products can easily be added to this file if you wish to calculate margin reuqirement for other futures.


In order to read the file into memory and assign it to the variable *master* please use the function `read_master()`

```
# load self-built functions 
source("functions.R")

# read and store master file
master <- read_master("Data/data_input.xlsx")
```

:three: Calculate Margin

The functions `calculate_margin()`, `calculate_fhs_margin()` and `calculate_sp_margin()` can be usded to calculate the Margins of a specific product contained in the master sheet between a start_date and and end_date. The Margin Model used the paramters as defined in params. Setting steps = TRUE will further include all intermediary calculation steps and used data in the resulting data frame (such as devaule factors, volatilities, log-returns etc.)
```
# load self-built functions 
source("functions.R")
# define start and end date for margin calculation 
start <- as.Date("2020-01-01")
end <- as.Date("2021-01-01")

# calculate filtered historical margin (unfloored) for FESX
out <- calculate_fhs_margin(product = "FESX", start = start_date, 
end = end_date, args = params, steps = FALSE)

# calculate floored margin (productive)
out <- calculate_margin(product = "FESX", start = start_date, 
end = end_date, args = params, steps = FALSE)

# calculate margin floor (stress period margin)
out <- calculate_sp_margin(product = "FESX", start = start_date, 
end = end_date, args = params, steps = FALSE)

# calculate procyclicality measures 
must include steps = TRUE!
```

### Warnings

The written functions do not have any error controls built in.
If you encounter an error, make sure to pass the demanded arguments in the required format to the functions. 
All functions were last tested with the data hosted in this repository on DATE using R VERSION

Also, make sure that you have all relevant packages installed which are needed to run the scripts 
- tidyverse
- runner
- lubridate
- readxl
-
-