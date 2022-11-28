
# default fund
default_fund <- read_csv("Data/Eurex_Data/Default Fund Requirement by Date.csv",
  col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

default_fund |>
  ggplot(aes(x = FACT_DATE, y = REQUIREMENT_EOP / 10^9)) +
  geom_line() +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(color = "black"),
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Default Fund Requirement in Bio. USD",
    y = "",
    x = ""
  )

empty <- tibble(NULL)

empty_CCP <-
  empty |>
  ggplot(aes(NULL)) +
  theme(
    panel.border = element_rect(color = "black"),
    axis.title = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )
labs(title = "CCP Clearing")

ggsave("graphs/empty_CCP.png",
  plot = empty_CCP, device = "png",
  dpi = 300, height = 6.63, width = 7.34, units = "cm"
)

empty_bilateral <-
  empty |>
  ggplot(aes(NULL)) +
  theme(
    panel.border = element_rect(color = "black"),
    axis.title = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(title = "Bilateral Clearing")

ggsave("graphs/empty_bilateral.png",
  plot = empty_bilateral, device = "png",
  dpi = 300, height = 6.63, width = 7.34, units = "cm"
)

empty_Closeout <-
  empty |>
  ggplot(aes(NULL)) +
  theme(
    panel.border = element_rect(color = "black"),
    axis.title = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(title = "Position Close Out")

ggsave("graphs/empty_Closeout.png",
  plot = empty_Closeout, device = "png", dpi = 300,
  height = 5.97, width = 8.41, units = "cm"
)

empty_Porting <-
  empty |>
  ggplot(aes(NULL)) +
  theme(
    panel.border = element_rect(color = "black"),
    axis.title = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  labs(title = "Client Position Porting")

ggsave("graphs/empty_Porting.png",
  plot = empty_Porting, device = "png",
  dpi = 300, height = 5.97, width = 7.52, units = "cm"
)
