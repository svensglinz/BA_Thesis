# load necessary packages
library(tidyverse)
library(showtext)

# creation of empty picture frames with titles

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman10_math.otf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Bilateral Clearing
bilateral_clearing <- NULL |>
    ggplot() +
    labs(title = "Bilateral Clearing") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(color = "black", fill = "transparent")
    )

# save plot
ggsave(
    "Plots/Output/Empty_Bilateral_Clearing.png", bilateral_clearing,
    width = 6.2, height = 6.2, unit = "cm", dpi = 300
)

# CCP Clearing
ccp_clearing <- NULL |>
    ggplot() +
    labs(title = "CCP Clearing") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(color = "black", fill = "transparent")
    )

# save plot
ggsave(
    "Plots/Output/Empty_CCP_Clearing.png", ccp_clearing,
    width = 6.2, height = 6.2, unit = "cm", dpi = 300
)

# Client Position Porting
client_porting <- NULL |>
    ggplot() +
    labs(title = "Client Position Porting") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(color = "black", fill = "transparent")
    )

# save plot
ggsave(
    "Plots/Output/Empty_Client_Porting.png", client_porting,
    width = 7.1, height = 5.8, unit = "cm", dpi = 300
)

# Closing out of Direct Member
closing_out <- NULL |>
    ggplot() +
    labs(title = "Closing out of Direct Member") +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(color = "black", fill = "transparent")
    )

# save plot
ggsave(
    "Plots/Output/Empty_Closing_Out.png", closing_out,
    width = 7.1, height = 5.8, unit = "cm", dpi = 300
)
