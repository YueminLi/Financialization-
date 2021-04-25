# figure 1 
if (!require("pacman")) install.packages("pacman")
p_load(ggplot2)

df <- read.csv("../data_processing/financialization_df.csv")
df$X <- NULL
df <- df %>%
  filter(Year >= 1960) 

