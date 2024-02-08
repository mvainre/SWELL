################################################################################-
#
# SWELL Vainre et al 2024 BMJ Mental Health
# Colour scheme
# Written by: Maris Vainre, 2023
#
################################################################################-


cols_Report1 <- data.frame(col_name    = "SWELL",
                           col_theme   = "#06d6a0",
                           col_accent1 = "#06d6a0",
                           col_accent2 = "#ffd166",
                           col_accent3 = "#ef476f", 
                           col_accent4 = "#726da8",
                           col_accent5 = "#73877b",
                           col_dark    = "#584b53",
                           col_neu     = "#616161",
                           #col_neu     = "#ede3e4", 
                           col_white   = "#f6f2f2")

cols_Report2  <- data.frame(col_name    = "MRC",
                            col_theme   = "#008AAD", 
                            col_accent1 = "#00BED5",
                            col_accent2 = "#FF6300", 
                            col_accent3 = "#FBBB10",
                            col_accent4 = "#70AD47",
                            col_accent5 = "#00A78B",
                            col_dark    = "#2E2C61",
                            col_neu     = "#616161", 
                            col_white   = "#FFFFFF")


cols_Report = bind_rows(cols_Report1, cols_Report2)
rm(cols_Report1, cols_Report2)
