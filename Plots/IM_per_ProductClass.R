# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# define parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

# load data
df <- read_csv("Data/Eurex_Data/IM per Product Class.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter & clean data
df <- df |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    mutate(PRODUCT_GROUP = ifelse(PRODUCT_GROUP %in% c(
        "EQUITY DERIVATIVES",
        "FIXED INCOME DERIVATIVES", "OTC IRS"
    ), PRODUCT_GROUP, "OTHER")) |>
    group_by(FACT_DATE, PRODUCT_GROUP) |>
    summarize(IM_EUR = sum(IM_EUR))

# create plot
out <-
    df |>
    ggplot(aes(x = FACT_DATE, y = IM_EUR / 10^9, fill = PRODUCT_GROUP)) +
    geom_area(position = "stack") +
    scale_y_continuous(breaks = seq(from = 0, to = 80, by = 20),
    expand = expansion(mult = c(.01,.05))) +
    scale_x_date(breaks = seq.Date(
        from = start_date,
        to = end_date, by = "month"
    ), labels = scales::label_date(format = "%b"),
    expand = expansion(mult = c(.005,.005))) +
labs(
    title = "Initial Margin per Asset Class (in Bio EUR)",
    x = NULL,
    y = NULL,
    fill = NULL
) +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, "cm"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks = element_line(color = "black")
    ) +
    scale_fill_jama(labels = c(
        "Equity Derivatives",
        "FI Derivatives", "OTC IRS", "Other"
    ))

# save plot
ggsave("Plots/Output/IM_per_asset.png",
    plot = out,
    device = "png", dpi = 350, height = 8.5,
    width = 15.9, units = "cm"
)



# write this as separate script 
# evaluate excess collateral across entire eurex clearing group!

# TMR vs Collateral per Security Type (SUM)
TMR <-
    read_csv("Data/Eurex_Data/TMR.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

TMR <-
    TMR |>
    group_by(FACT_DATE) |>
    summarize(total_tmr = sum(TMR_EUR)) |> 
    select(FACT_DATE, total_tmr)

IM <-
    read_csv("Data/Eurex_Data/IM per Product Class.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

IM <-
    IM |>
    group_by(FACT_DATE) |>
    summarize(total_im = sum(IM_EUR)) |> 
    select(FACT_DATE, total_im)

collateral <-
    read_csv("Data/Eurex_Data/Collateral per Security Type.csv",
        col_types = cols(
            FACT_DATE = col_date(format = "%d/%m/%Y"),
            SECURITY_TYPE = readr::col_factor(),
            COLLATERAL_TYPE = readr::col_factor()
        )
    )

collateral <-
    collateral |>
    group_by(FACT_DATE) |>
    summarize(total_collateral = sum(COLLATERAL_VALUE_EUR)) |> 
    select(FACT_DATE, total_collateral)

defaultfund <-
    read_csv("Data/Eurex_Data/Default Fund Requirement by Date.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

defaultfund <-
    defaultfund |>
    rename(total_defaultfund = REQUIREMENT_EOP) |>
    select(FACT_DATE, total_defaultfund)

joined <- defaultfund |>
    left_join(TMR, by = c("FACT_DATE")) |>
    left_join(IM, by = c("FACT_DATE")) |>
    left_join(collateral, by = c("FACT_DATE")) |> 
    mutate("tmr_and_default" = total_tmr + total_defaultfund,
    ratio = total_collateral / tmr_and_default)

joined_long <- joined |> pivot_longer(-FACT_DATE)

joined |>
    ggplot(aes(x = FACT_DATE, ratio)) +
    geom_line()
# which one shows which actually!! (does it include the default fund or not for example???)

#https://www.blackrock.com/corporate/literature/publication/bcbs-cpmi-iosco-margin-report-blackrock-response.pdf
#opinion by blackrock on overcollaterlaization during covid 

#large overcoll could also mean that companies already insure against massive rises by proactively posting  more margin!!!