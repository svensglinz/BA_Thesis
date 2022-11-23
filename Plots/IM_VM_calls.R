# load relevant packages
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggsci)
library(ggExtra)
library(ggforce)
library(patchwork)
library(latex2exp)

df <- read_csv("Data/Eurex_Data/VM_IM_Calls.csv",
  col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter IM requirements from Option expiries!

df |>
  filter(TYP == "EOD") |>
  pivot_wider(names_from = TYPE, values_from = DEBIT_EUR) |>
  rename(VM = `VAR MARGIN PAID`, IM = `MARGIN CALL`) |>
  mutate(RATIO = VM / IM) |>
  ggplot(aes(x = FACT_DATE)) +
  geom_line(aes(y = IM))

# filter out day with large option & index future expiries!
test <- df |>
  filter(TYP == "EOD") |>
  pivot_wider(names_from = TYPE, values_from = DEBIT_EUR) |>
  rename(VM = `VAR MARGIN PAID`, IM = `MARGIN CALL`) |>
  mutate(
    RATIO = VM / IM,
    day = wday(FACT_DATE, label = T),
    week = stringi::stri_datetime_fields(FACT_DATE)$WeekOfMonth,
    month = month(FACT_DATE),
    expiry = ifelse((day == "Fri" & week == 3 & month %in% c(3, 6, 9, 12)), T, F)
  ) |>
  arrange(desc(IM))

test |>
  ggplot(aes(x = FACT_DATE, y = IM)) +
  geom_line() +
  geom_point(data = test |> filter(expiry == T), aes(y = IM), color = "red", size = 2)


# VM vs IM calls! --> Also do a graph for the FESX future itself and not only for eurex
# clearing at an aggregate level!!!
test |>
  filter(expiry == F & between(FACT_DATE, start_date, end_date)) |>
  ggplot(aes(x = FACT_DATE, y = IM)) +
  geom_line() +
  geom_line(aes(y = VM), color = "red")
