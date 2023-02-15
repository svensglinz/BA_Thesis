## Bachelor Thesis

---

This repository hosts all code used to produce the models, graphs and calculations for my Bachelor Thesis **Margin Procyclicality during Covid 19 - Drivers, Impact and Solutions**

---

### Description of Files

- All proprietary functions used in the scripts as well as a short description on their functionality are contained in :link: ![*functions*](functions.r)
- The scripts to generate each plot are located in :link: ![*Plots*](Plots/)
- All plots as .svg files are located in  :link: ![*Plots/Output*](Plots/Output/)
- All calculated procyclicality stats are located in the files :link: ![*calculations_fesx_long.csv*](Data/calculations_fesx_long.csv) and ![*calculations_fesx_short.csv*](Data/calculations_fesx_short.csv)
- All calculations performed to obtain the procyclicality stats are in the scripts :link: ![*calculations_fesx_long.r*](calculations_fesx_long.r) and ![*calculations_fesx_short.r*](calculations_fesx_short.r)
- All files that are referred to in the code as being in the folder Data/Eurex_Data are located on  :link: ![*OneDrive*](https://1drv.ms/u/s!AoQRAZtdS9u4iZYwkNuHxv9e582O_g?e=1WXJlr.com) for confidentiality.

---

### Data Sources

- daily returns in file `Data/data_input`: Eurex Clearing (from 2005 on), Bloomberg (until 2005)
- stress period dates in file `Data/data_input`: Eurex Clearing
- all data in `Data/Eurex_Data`: Eurex Clearing

---

### Dependencies

All code was last checked on 14/02/2023 in the following environment and with the following packages loaded and/or attached:

```r
sessionInfo()

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22621)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_Switzerland.utf8  LC_CTYPE=English_Switzerland.utf8   
# [3] LC_MONETARY=English_Switzerland.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_Switzerland.utf8
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
# 
# other attached packages:
#  [1] lubridate_1.9.0  timechange_0.1.1 ggh4x_0.2.3      runner_0.4.2    
#  [5] zoo_1.8-11       readxl_1.4.1     ggsci_2.9        latex2exp_0.9.6
#  [9] showtext_0.9-5   showtextdb_3.0   sysfonts_0.8.8   scales_1.2.1
# [13] glue_1.6.2       ggrepel_0.9.2    forcats_0.5.2    stringr_1.5.0
# [17] dplyr_1.0.10     purrr_1.0.0      readr_2.1.3      tidyr_1.2.1
# [21] tibble_3.1.8     ggplot2_3.4.0    tidyverse_1.3.2 
# 
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.9          lattice_0.20-45     svglite_2.1.0
#  [4] assertthat_0.2.1    utf8_1.2.2          R6_2.5.1
#  [7] cellranger_1.1.0    backports_1.4.1     reprex_2.0.2
# [10] httr_1.4.4          pillar_1.8.1        rlang_1.0.6        
# [13] googlesheets4_1.0.1 textshaping_0.3.6   labeling_0.4.2
# [16] googledrive_2.0.0   bit_4.0.5           munsell_0.5.0
# [19] broom_1.0.2         compiler_4.2.2      modelr_0.1.10
# [22] pkgconfig_2.0.3     systemfonts_1.0.4   tidyselect_1.2.0
# [25] fansi_1.0.3         later_1.3.0         crayon_1.5.2       
# [28] tzdb_0.3.0          dbplyr_2.2.1        withr_2.5.0
# [31] grid_4.2.2          jsonlite_1.8.0      gtable_0.3.1
# [34] lifecycle_1.0.3     DBI_1.1.3           magrittr_2.0.3
# [37] cli_3.4.1           stringi_1.7.8       vroom_1.6.0
# [40] farver_2.1.1        fs_1.5.2            xml2_1.3.3
# [43] ellipsis_0.3.2      ragg_1.2.4          generics_0.1.3     
# [46] vctrs_0.5.1         tools_4.2.2         httpgd_1.3.0
# [49] bit64_4.0.5         hms_1.1.2           parallel_4.2.2
# [52] colorspace_2.0-3    gargle_1.2.1        rvest_1.0.3
# [55] haven_2.5.1
```
