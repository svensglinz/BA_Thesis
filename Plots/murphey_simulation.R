# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# add font for plotting
font_add(
  family = "lmroman",
  regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define function paremeters
start_date <- as.Date("2006-01-01")
end_date <- as.Date("2021-01-01")
start_procyclicality <- as.Date("2020-01-01")
end_procyclicality <- as.Date("2020-12-31")


# parameters for the Kupiec Test of Failure Function
window <- 750
model_conf_level <- .99
test_conf_level <- .99

# store parameters needed for margin calculation
args_long_FESX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = NULL, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

# define lambdas to loop over
lambda_loop <- seq(0.9, 0.999, by = 0.01)

# pre-assign vectors
lambda <- vector("double", length(lambda_loop))
procyclicality <- vector("double", length(lambda_loop))
costs <- vector("double", length(lambda_loop))
max_shortfall <- vector("double", length(lambda_loop))
n_breaches <- vector("double", length(lambda_loop))
count <- 1

for (i in lambda_loop) {
  # assign lambda to the args list
  args_long_FESX$lambda <- i

  # calculate margins
  FESX_Margin <-
    calculate_fhs_margin(
      product = "FESX", start = start_date, end = end_date,
      args = args_long_FESX, steps = TRUE
    )

  # run Kupiec Test and discard those that do not meet test
  KPF_result <- kupiec_test(FESX_Margin,
    window = 750, model_conf_level = model_conf_level,
    test_conf_level = test_conf_level
  )

  # filter out observations in 2020 only for procyclicality measure
  # calculations
  FESX_Margin <- FESX_Margin |>
    filter(between(DATE, start_procyclicality, end_procyclicality))

  measures <-
    summary_stats(FESX_Margin, start = start_date, end = end_date)

  lambda[count] <- i
  costs[count] <- measures |>
    filter(type == "costs") |>
    pull(values)
  procyclicality[count] <- measures |>
    filter(type == "max_30d") |>
    pull(values)
  max_shortfall[count] <- measures |>
    filter(type == "max_shortfall") |>
    pull(values)
  n_breaches[count] <- measures |>
    filter(type == "n_breaches") |>
    pull(values)

  print(
    paste(
      "loop", as.character(count), "/",
      as.character(length(lambda_loop)), "finished",
      sep = " "
    )
  )
  count <- count + 1
}

results <- tibble(
  lambda = lambda,
  procyclicality = procyclicality,
  costs = costs,
  max_shortfall = max_shortfall,
  KPF_result = KPF_result,
  n_breaches = n_breaches
)

# do two separate graphs (average shortfall (including breaches) and then average costs!!!)

# assemble plot
results |>
  filter(KPF_result) |>
  ggplot(aes(y = procyclicality)) +
  geom_point(aes(x = costs, color = lambda, size = "Average Costs")) +
  geom_line(aes(x = max_shortfall * -1, linetype = "Max Shortfall")) +
  scale_color_gradientn(
    colors = c("white", "grey", "darkgrey", "black"),
    breaks = c(.9, .95, .99)
  ) +
  scale_x_continuous(labels = scales::label_percent()) +
  # ,sec.axis = sec_axis(trans = \(x) x)) + # not needed on this scale
  theme(
    text = element_text(family = "lmroman"),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey",
      linetype = 2,
      linewidth = 0.5
    ),
    panel.background = element_rect(color = "black", fill = "white"),
    legend.position = "bottom",
    plot.title = element_text(size = 10, face = "bold"),
    legend.key = element_rect(fill = "white"),
  ) +
  labs(
    title = "30-day Procyclicality - FHS Model",
    x = "Average Costs / Max Shortfall",
    y = "30-day Procylicality",
    color = "lambda"
  ) +
  guides(
    color = guide_colorbar(
      barwidth = 10,
      barheight = .5,
      title.position = "top"
    ),
    linetype = guide_legend(
      title.position = "top",
      title = ""
    )
  )

# for n breaches --> report in paper
# where the limit is for how many breaches
# over two years. Also report the max
# breaches for any lambda and the lowest
# breach and show that these are actually / hopefully
# quite close together!!!

# also check again the implementation
# that the returns are truly lagged
# (in the stats calculation)!!!!!!!!


# same for floored version!
args_long_FESX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = NULL, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

# define lambdas to loop over
lambda_loop <- seq(0.9, 0.999, by = 0.01)

# pre-assign vectors
lambda <- vector("double", length(lambda_loop))
procyclicality_30d <- vector("double", length(lambda_loop))
procyclicality_5d <- vector("double", length(lambda_loop))
costs <- vector("double", length(lambda_loop))
max_shortfall <- vector("double", length(lambda_loop))
n_breaches <- vector("double", length(lambda_loop))
count <- 1

for (i in lambda_loop) {
  # assign lambda to the args list
  args_long_FESX$lambda <- i

  # calculate margins
  FESX_Margin <-
    calculate_margin(
      product = "FESX", start = start_date, end = end_date,
      args = args_long_FESX, steps = TRUE
    )

  # run Kupiec Test and discard those that do not meet test
  KPF_result <- kupiec_test(FESX_Margin,
    window = 750, model_conf_level = model_conf_level,
    test_conf_level = test_conf_level
  )

  # filter out observations in 2020 only for procyclicality measure
  # calculations
  FESX_Margin <- FESX_Margin |>
    filter(between(DATE, start_procyclicality, end_procyclicality))

  measures <-
    summary_stats(FESX_Margin, start = start_date, end = end_date)

  lambda[count] <- i
  costs[count] <- measures |>
    filter(type == "costs") |>
    pull(values)
  procyclicality_30d[count] <- measures |>
    filter(type == "max_30d") |>
    pull(values)
  procyclicality_5d[count] <- measures |>
    filter(type == "max_5d") |>
    pull(values)
  max_shortfall[count] <- measures |>
    filter(type == "max_shortfall") |>
    pull(values)
  n_breaches[count] <- measures |>
    filter(type == "n_breaches") |>
    pull(values)

  print(
    paste(
      "loop", as.character(count), "/",
      as.character(length(lambda_loop)), "finished",
      sep = " "
    )
  )
  count <- count + 1
}

results <- tibble(
  lambda = lambda,
  procyclicality = procyclicality,
  costs = costs,
  max_shortfall = max_shortfall,
  KPF_result = KPF_result,
  n_breaches = n_breaches
)

# do two separate graphs (average shortfall (including breaches) and then average costs!!!)

# assemble plot
results |>
  filter(KPF_result) |>
  ggplot(aes(y = procyclicality_30d)) +
  geom_point(aes(x = costs, color = lambda)) +
  scale_color_gradientn(
    colors = c("white", "darkgrey", "black"),
    breaks = c(.9, .95, .99)
  ) +
  scale_x_continuous(labels = scales::label_percent()) +
  # ,sec.axis = sec_axis(trans = \(x) x)) + # not needed on this scale
  theme(
    text = element_text(family = "lmroman"),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey",
      linetype = 2,
      linewidth = 0.5
    ),
    panel.background = element_rect(color = "black", fill = "white"),
    legend.position = "bottom",
    plot.title = element_text(size = 10, face = "bold"),
    legend.key = element_rect(fill = "white"),
  ) +
  labs(
    title = "30-day Procyclicality - FHS Model",
    x = "Average Costs",
    y = "30-day Procylicality",
    color = "lambda"
  ) +
  guides(
    color = guide_colorbar(
      barwidth = 10,
      barheight = .5,
      title.position = "top",
      title.hjust = .5
    )
  )

# assemble plot for max shortfall and number of breaches in 2020
results |>
  filter(KPF_result) |>
  ggplot(aes(x = max_shortfall * -1, y = procyclicality_30d)) +
  geom_line()

# for the paper, before doing this, show baseline models wihtout lambda calibration!!!
# also in the lambda charts, highlight in color (or size or point type)
# which is the calibration used by eurex!

# explain the Kupiec Proportion of Failure test

# same with 25% floor!


# same with capped margin!

# same with speed limits!


# any other stuff???

# report short portfolio and portfolio for
# fixed income instruments in the appendix and just
# refer to it during the paper!!!

# also justify why we do average costs and not
# excess costs to baseline model as done in other papers
# --> Client centric view!
