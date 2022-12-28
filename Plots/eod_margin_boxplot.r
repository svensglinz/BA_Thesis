# load relevant packages
library(lubridate)
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

df <- read_csv("Data/Eurex_Data/VM_IM_Calls.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter out day with large option & index future expiries!
test <- df |>
    filter(TYP == "EOD", between(FACT_DATE, start_date, end_date)) |>
    mutate(
        DAY = wday(FACT_DATE),
        WEEK = stringi::stri_datetime_fields(FACT_DATE)$WeekOfMonth,
        MONTH = month(FACT_DATE),
        EXPIRY = ifelse((DAY == 5 & WEEK == 3 & MONTH %in% c(3, 6, 9, 12)),
            TRUE, FALSE
        )
    )

test |>
    filter(!EXPIRY) |>
    ggplot(
        aes(
            x = as.factor(MONTH),
            y = DEBIT_EUR, group = interaction(MONTH, TYPE), color = TYPE
        )
    ) +
    geom_boxplot()
