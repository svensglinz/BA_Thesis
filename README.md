# BA_Thesis

This repository hosts all raw-data and code used to produce the models, graphs and calculations in my Bachelor Thesis **Margin Procyclicality during Covid 19 - Drivers, Impact and Solutions**

## Description of Functions 

## Initial Margin Calculation 

First, all necessary arguments / static parameters which are needed as inputs for the Margin model must be stored in a list. 

Read in the master file which contains information on the stress periods and all necessary risk factor returns of the front month future series 
It is important that the file is stored under the variable name *master* !

```
master <- read_master("Data/data_input.xlsx")
```

Necessary Parameters: 
MPOR - Marign Period of Risk 
Factor - Robust upscaling Factor 
quantile - VaR quantile 
lambda - decay factor for the EWMA volatility calculation 
n_day - length of lookback period 
liq_group - liquidation group of the instrument for which margin is calculated 
short - whether the margin is calculated for a long or short position

```
parameters <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, n_day = 750,
liq_group  = "PFI01", short = FALSE)
```

After that, we can calculate the Stress period margin, the filtered historical margin or the overall margin using the following functions between a start and an end date

```
#total margin
margin_calculator(product = "FESX", start = "01/01/2020", end = "01/01/2021", args = parameters)

#FHS_Margin 
...
#SP_Margin
...
```

