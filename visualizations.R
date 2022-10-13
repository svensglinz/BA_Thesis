#load libraries
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggsci)
library(ggExtra)

source("functions_redone.R")
####################
#EWMA VOLATILITY CALCULATION
######################

vol_FESX <- calculate_vola(product = "FESX", start = start, end = end, lambda = lambda, n_day = n_day) |> 
  rename(date = returns, FESX = vola)
vol_FSMI <- calculate_vola(product = "FSMI", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FSMI = vola)
vol_FGBX <- calculate_vola(product = "FGBX", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FGBX = vola)
vol_FGBL <- calculate_vola(product = "FGBL", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FGBL = vola)

combined <- vol_FESX |> 
  full_join(vol_FSMI) |>
  full_join(vol_FGBX) |>
  full_join(vol_FGBL) |> 
  na.omit() |> 
  pivot_longer(cols = 2:5, names_to = "product", values_to = "vola")

#PLOT the volatility graph
#this plot should show that if we put it in larger context, this was not an extreme outlier 
#especially if we compare it to the financial crisis 
# --> What has changed then, was it clearing volumes or what is it ???!!!

#could also calculate margin backwards to these days and show our effects --> Large backesting!

vol_plot <- 
  combined |> 
  ggplot(aes(x = date, y = vola, group= product, color = product))+
  geom_line()+
  theme_minimal()+
  labs(title = "Daily Exponentially weighted Volatility of Returns",
       x = NULL,
       y = NULL, 
       color = NULL)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(color = "grey", linetype = 2, size = 0.5),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = 0.01, to = 0.06, by = 0.01), labels = scales::label_percent())+
  expand_limits(y = 0.06)+
  scale_color_jama()

ggsave("graphs/volplot.png", plot = vol_plot, device = "png", dpi = 300, height = 6.03, width = 15.9, units = "cm")


##############################################
#MARGIN CALCULATIONS
##############################################


FESX_Margin_long <- margin_calculator(product = "FESX", start = start, end = end, args = args_long)
FESX_Margin_short <- margin_calculator(product = "FESX", start = start, end = end, args = args_short)

FESX_Margin <- FESX_Margin_long |> full_join(FESX_Margin_short, by = c("dates")) |> 
  rename(MARGIN_LONG = MARGIN.x, MARGIN_SHORT = MARGIN.y) |> 
  mutate(MARGIN_SHORT = -MARGIN_SHORT) |> 
  pivot_longer(2:3, names_to = "margin")

FESX_Margin |>
  ggplot(aes(x = dates, y = value, groups = margin, color = margin))+
  geom_line()+
  scale_y_continuous(breaks = seq(from = -0.25, to = 0.25, by = 0.05),
                     labels = scales::label_percent())+
theme_minimal()+
  labs(title = "Daily Exponentially weighted Volatility of Returns",
       x = NULL,
       y = NULL, 
       color = NULL)+
  theme(panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_color_jama()

#####################################################################
#check out how this was done in previous data file!
IMC <-  read_csv("Data/Eurex_Data/IMC_Redone.csv")
IMC <- IMC |>
  mutate(Description = as.Date(Description, format = "%Y-%m-%d"))

IMC |> filter(between(Description, as.Date("2020-03-01"), as.Date("2020-04-01"))) |> 
  ggplot(aes(x = Description, y = N.Calls, color = Typpe))+
  geom_histogram(position = "stack", stat = "identity")

#IMC VOLUME 
IMC |> 
  ggplot(aes(x = Description, y = Volume, group = Typpe, color = Typpe))+
  geom_line()


#eligible securities 
securities <- read_csv("Data/Eurex_Data/eligible_securities.csv",
                       col_types = cols(SECURITY_EVALUATION_FACTOR = col_double(),
                                        FACT_DATE = col_date(format = "%d/%m/%Y"),
                                        SECURITY_TYPE = readr::col_factor()))

#ALSO INCLUDE A SUMMARY TABLE WHICH SHOWS HOW MANY BONDS THIS DATA SET INCLUDES!!!!!!!

#also here in title provide average number of observations per single date  ????

securities_plot <- securities |> group_by(SECURITY_TYPE, FACT_DATE) |> 
  filter(SECURITY_TYPE %in% c("BANK BONDS", "CORPORATE BONDS",
                              "SOVEREIGN GOVERNMENT BONDS", "STATE AGENCIES",
                              "STATE/MUNICIPAL BONDS", "STOCKS")) |> 
  summarize(avg_haircut = mean(SECURITY_EVALUATION_FACTOR, na.rm = T)) |> 
  ggplot(aes(x = FACT_DATE, y = avg_haircut))+
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", size = 2, alpha = .2)+
  geom_line()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid = element_line(color = "grey"),
        strip.background = element_rect(color = "grey", fill = "grey"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6))+
  labs(title = "sfsf",
       x = "",
       y = "")+
  facet_wrap(~SECURITY_TYPE)

ggsave("graphs/securities_plot.png", plot = securities_plot, device = "png", dpi = 300, height = 8.5, width = 15.9, units = "cm")


#collateral value analysis during the crisis!

collateral <- read_csv("Data/Eurex_Data/Collateral per Security Type.csv",
                       col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"),
                                        SECURITY_TYPE = readr::col_factor(),
                                        COLLATERAL_TYPE = readr::col_factor()))

collateral |> 
  filter(SECURITY_TYPE %in% c("BANK BONDS", "CORPORATE BONDS", "SOVEREIGN GOVERNMENT BONDS",
                              "STATE AGENCIES", "STOCKS", "STATE/MUNICIPAL BONDS")) |> 
  ggplot(aes(x = FACT_DATE, y = COLLATERAL_VALUE_EUR, fill = SECURITY_TYPE))+
  geom_area(position = "stack")+
  scale_fill_jama()

#percentage contribution in collateral 
grouped_daily <- 
  collateral |>
  group_by(FACT_DATE) |>
  summarize(total = sum(MARKET_VALUE_EUR))

collateral <- collateral |>
  left_join(grouped_daily, by = c("FACT_DATE")) |> 
  mutate(perc = MARKET_VALUE_EUR / total)

col_plot <- 
  collateral |> 
  filter(SECURITY_TYPE %in% c("BANK BONDS", "CORPORATE BONDS", "SOVEREIGN GOVERNMENT BONDS",
                              "STATE AGENCIES", "STOCKS", "STATE/MUNICIPAL BONDS", "null")) |> 
  ggplot(aes(x = FACT_DATE, y = perc))+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", size = 2, alpha = .2)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "",
       y = "",
       x = "",
       color = "")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid = element_line(color = "grey"),
        strip.background = element_rect(color = "grey", fill = "grey"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 6))+
  facet_wrap(~SECURITY_TYPE, scales = "free_y")

ggsave("graphs/collateral.png", plot = col_plot, device = "png", dpi = 300, height = 8.5, width = 15.9, units = "cm")


#default fund 
default_fund <- read_csv("Data/Eurex_Data/Default Fund Requirement by Date.csv",
                         col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y")))

default_fund |> 
  ggplot(aes(x = FACT_DATE, y = REQUIREMENT_EOP/10^9))+
  geom_line()+
  theme_light()+
  theme(panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"))+
  labs(title = "Default Fund Requirement in Bio. USD",
       y = "",
       x = "")

#create Brownian Motion  paths 
start <- c(100, rnorm(n = 99, mean = 0, sd = 1))

paths <- matrix(data = NA, nrow = 400, ncol = 10)
for (i in 1:10) paths[,i] <- c(start, rnorm(n = 300, mean = 0, sd = 1))
paths <- paths |>
  as.data.frame() |>
  sapply(cumsum) |>
  as.data.frame()

paths$index <- 1:400
paths$index <- as.factor(paths$index)
paths$col <- c(rep("black", 99), rep("darkgrey", 301))

paths <- paths |> pivot_longer(-index) |> as.data.frame(exp(paths))

IM_graph <- 
  paths |> 
  ggplot(aes(x = index, y = value, group = name))+
  geom_point(size = 0, color = "white")+
  geom_line(col = paths$col)+
  geom_vline(xintercept = 100)+
  geom_segment(aes(y = paths$value[paths$index == 100][1], x = 100, 
                   yend = paths$value[paths$index == 100][1], xend = 400), color = "red")+
  geom_vline(xintercept = 200, linetype = 2)+
  geom_vline(xintercept = 300, linetype = 2)+
  labs(x = "Time",
       y = "Value",
       title = "Concept of Initial Margin")+
  theme_minimal()+
  scale_y_continuous(breaks = seq(from = 50, to = 130, by = 10))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold", margin = margin(0,0,0,0)))+
  scale_x_discrete(breaks = c(1,100, 200, 300, 350, 400),
                   labels=c("1" = "t-1", "100" = "t",
                            "200" = "t+1", "300" = "t+2",
                            "350" = "...", "400" = "t+n"))

IM_graph <- ggMarginal(IM_graph, type = "density", margins = "y")
  
ggsave("graphs/IM_graph.png", plot = IM_graph, device = "png", dpi = 300, height = 6.23, width = 12.9, units = "cm")

df <- tibble(NULL)

empty_CCP <- 
  df |> 
  ggplot(aes(NULL))+
  theme(panel.border = element_rect(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold"))
  labs(title = "CCP Clearing")

ggsave("graphs/empty_CCP.png", plot = empty_CCP, device = "png", dpi = 300, height = 6.63, width = 7.34, units = "cm")

empty_bilateral <- 
  df |> 
  ggplot(aes(NULL))+
  theme(panel.border = element_rect(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold"))+
  labs(title = "Bilateral Clearing")

ggsave("graphs/empty_bilateral.png", plot = empty_bilateral, device = "png", dpi = 300, height = 6.63, width = 7.34, units = "cm")


#plot of baseline scenario for FESX and FGBX (or should we take FGBL as well ??? Check out website 
#to see whether they are still there!!!)

args_long <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
             n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
             short = FALSE)

args_short <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                   n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
                   short = TRUE)

FESX_long <- margin_calculator(product = "FESX", start = "01/01/2000", end = "01/01/2021", args = args_long)
FESX_short <- margin_calculator(product = "FESX", start = "01/01/2000", end = "01/01/2021", args = args_short)
FESX_long$test <- -FESX_long$test
FESX_long$type <- "long"
FESX_short$type <- "short"

FESX_margin <- bind_rows(FESX_long, FESX_short)

FESX <- master$returns |> filter(INST == "FESX") |> select(DATE, LOG_RET) |> 
  arrange(DATE)
FESX$MPOR_RET <- rollapply(FESX$LOG_RET, 3, FUN = function(x) exp(sum(x))-1,
                        by.column = F, align = "left", fill = NA)

FESX_plot <- FESX_margin |> left_join(FESX, by = c("DATE"))

FESX_long <- FESX_plot |> filter(type == "long") |> 
  mutate(breach = ifelse(MPOR_RET < test, "red", "black"))

FESX_short <- FESX_plot |> filter(type == "short") |> 
  mutate(breach = ifelse(MPOR_RET > test, "red", "black"))

FESX_plot <-  bind_rows(FESX_short, FESX_long)
FESX_plot$size <- ifelse(FESX_plot$breach == "red", 2, 0.75)
FESX_plot <- FESX_plot |> arrange(breach)

FESX_out <- FESX_plot |> 
  ggplot(aes(x = DATE))+
  geom_line(aes(y = test, group = type, color = type))+
  geom_point(aes(y = MPOR_RET), color =FESX_plot$breach, size = FESX_plot$size)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.05),
                     labels = scales::percent_format())+
  scale_x_date(breaks = seq.Date(from = as.Date("2006-01-01"),
                                 to = as.Date("2023-01-01"), by = "2 years"),
               labels = date_format("%Y"))+
  labs(title = "Margin Requirment FESX",
       y = "Margin in %",
       x = "Date",
       color = "")+
  scale_color_jama()
  
ggsave("graphs/FESX_plot.png", plot = FESX_out, device = "png", dpi = 300, height = 8.54, width = 16.1, units = "cm")


#####do plot for FGBL 
args_long <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                  n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PFI01",
                  short = FALSE)

args_short <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                   n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PFI01",
                   short = TRUE)

FGBL_long <- margin_calculator(product = "FGBL", start = "01/01/2000",
                               end = "01/01/2021", args = args_long) |> 
  mutate(type = "long", 
         test = -test)

FGBL_short <- margin_calculator(product = "FGBL", start = "01/01/2000",
                                end = "01/01/2021", args = args_short) |> 
  mutate(type = "short")

FGBL <- master$returns |> filter(INST == "FGBL") |> select(DATE, LOG_RET) |> 
  arrange(DATE)

FGBL$MPOR_RET <- rollapply(FGBL$LOG_RET, 3, FUN = function(x) exp(sum(x))-1,
                           by.column = F, align = "left", fill = NA)

FGBL_short <- FGBL_short |> left_join(FGBL, by = c("DATE")) |> 
  mutate(breach = ifelse(MPOR_RET > test, "red", "black"))

FGBL_long <- FGBL_long |> left_join(FGBL, by = c("DATE")) |> 
  mutate(breach = ifelse(MPOR_RET < test, "red", "black"))

FGBL_plot <-  bind_rows(FGBL_short, FGBL_long)
FGBL_plot$size <- ifelse(FGBL_plot$breach == "red", 3, 1.5)
FGBL_plot <- FGBL_plot |> arrange(breach)
FGBL_out <- FGBL_plot |> 
  ggplot(aes(x = DATE))+
  geom_line(aes(y = test, group = type, color = type))+
  geom_point(aes(y = MPOR_RET), color =FGBL_plot$breach, size = FGBL_plot$size)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = -0.04, to = 0.4, by = 0.01),
                     labels = scales::percent_format())+
  scale_x_date(breaks = seq.Date(from = as.Date("2006-01-01"),
                                 to = as.Date("2023-01-01"), by = "2 years"),
               labels = scales::date_format("%Y"))+
  labs(title = "Margin Requirment FGBL",
       y = "Margin in %",
       x = "Date",
       color = "")+
  scale_color_jama()

ggsave("graphs/FGBL_plot.png", plot = FGBL_out, device = "png", dpi = 300, height = 8.54, width = 16.1, units = "cm")

#####do plot for FSMI 

#!! be careful lambda is different for fixed income vs. equity and also the quantile!!!!!!
args_long <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                  n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
                  short = FALSE)

args_short <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                   n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
                   short = TRUE)

FSMI_long <- margin_calculator(product = "FSMI", start = "01/01/2000",
                               end = "01/01/2021", args = args_long) |> 
  mutate(type = "long", 
         test = -test)

FSMI_short <- margin_calculator(product = "FSMI", start = "01/01/2000",
                                end = "01/01/2021", args = args_short) |> 
  mutate(type = "short")

FSMI <- master$returns |> filter(INST == "FSMI") |> select(DATE, LOG_RET) |> 
  arrange(DATE)

FSMI$MPOR_RET <- rollapply(FSMI$LOG_RET, 3, FUN = function(x) exp(sum(x))-1,
                           by.column = F, align = "left", fill = NA)

FSMI_short <- FSMI_short |> left_join(FSMI, by = c("DATE")) |> 
  mutate(breach = ifelse(MPOR_RET > test, "red", "black"))

FSMI_long <- FSMI_long |> left_join(FSMI, by = c("DATE")) |> 
  mutate(breach = ifelse(MPOR_RET < test, "red", "black"))

FSMI_plot <-  bind_rows(FSMI_short, FSMI_long)
FSMI_plot$size <- ifelse(FSMI_plot$breach == "red", 3, 1.5)
FSMI_plot <- FSMI_plot |> arrange(breach)
FSMI_out <- FSMI_plot |> 
  ggplot(aes(x = DATE))+
  geom_line(aes(y = test, group = type, color = type))+
  geom_point(aes(y = MPOR_RET), color =FSMI_plot$breach, size = FSMI_plot$size)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.05),
                     labels = scales::percent_format())+
  scale_x_date(breaks = seq.Date(from = as.Date("2006-01-01"),
                                 to = as.Date("2023-01-01"), by = "2 years"),
               labels = scales::date_format("%Y"))+
  labs(title = "Margin Requirment FSMI",
       y = "Margin in %",
       x = "Date",
       color = "")+
  scale_color_jama()

ggsave("graphs/FSMI_plot.png", plot = FSMI_out, device = "png", dpi = 300, height = 8.54, width = 16.1, units = "cm")


##for FGBX 
args_long <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                  n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PFI01",
                  short = FALSE)

args_short <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
                   n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PFI01",
                   short = TRUE)

FESX_long <- margin_calculator(product = "FGBX", start = "01/01/2000", end = "01/01/2021", args = args_long)
FESX_short <- margin_calculator(product = "FGBX", start = "01/01/2000", end = "01/01/2021", args = args_short)
FESX_long$test <- -FESX_long$test
FESX_long$type <- "long"
FESX_short$type <- "short"

FESX_margin <- bind_rows(FESX_long, FESX_short)

FESX <- master$returns |> filter(INST == "FGBX") |> select(DATE, LOG_RET) |> 
  arrange(DATE)
FESX$MPOR_RET <- rollapply(FESX$LOG_RET, 3, FUN = function(x) exp(sum(x))-1,
                           by.column = F, align = "left", fill = NA)

FESX_plot <- FESX_margin |> left_join(FESX, by = c("DATE"))

FESX_long <- FESX_plot |> filter(type == "long") |> 
  mutate(breach = ifelse(MPOR_RET < test, "red", "black"))

FESX_short <- FESX_plot |> filter(type == "short") |> 
  mutate(breach = ifelse(MPOR_RET > test, "red", "black"))

FESX_plot <-  bind_rows(FESX_short, FESX_long)
FESX_plot$size <- ifelse(FESX_plot$breach == "red", 3, 1.5)

FESX_out <- FESX_plot |> 
  ggplot(aes(x = DATE))+
  geom_line(aes(y = test, group = type, color = type))+
  geom_point(aes(y = MPOR_RET), color =FESX_plot$breach, size = FESX_plot$size)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.05),
                     labels = scales::percent_format())+
  scale_x_date(breaks = seq.Date(from = as.Date("2006-01-01"),
                                 to = as.Date("2023-01-01"), by = "2 years"),
               labels = date_format("%Y"))+
  labs(title = "Margin Requirment FESX",
       y = "Margin in %",
       x = "Date",
       color = "")+
  scale_color_jama()

ggsave("graphs/FESX_plot.png", plot = FESX_out, device = "png", dpi = 300, height = 8.54, width = 16.1, units = "cm")
