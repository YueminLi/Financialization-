library(pacman)
p_load(ggpubr, ggtext, tidyverse, reshape2)
load("./data_model/model.RData")

#library(ggplot2)
f1 <- select(df_model, 
             Year, Country, DV_VA, DV_nfc_ls, DV_hh_ls)
f1 <- na.omit(f1)
f1 <- f1[order(f1$Country, f1$Year),]

# merge f1 with the ADF test results
variety <- read_rds("../table_and_figure/variety.rds")
f1 <- merge(f1, variety, by.x = "Country", by.y = "country", all.x = T)

#financialization <- (1/2-as.numeric(f1$stationarity=="non-stationarity"))*(-2)

# Solution 1
# This solution attributes countries of financialization with positive values 
# and other countries with negative.
## This solution of visualization is not good because the color change is not obvious when midpoint has to be 0
# ggplot(data=f1, aes(x =Year, y = Country)) +
#   geom_tile(aes(fill = log(DV_VA)*financialization)) +
#   scale_fill_gradient2(midpoint=0,
#                        low="#4D908E", mid = "#F9C74F",high="#F3722C") +
#   # scale_colour_manual(values = c("white",NA)) +
#   theme_bw()


# Solution 2
f1$order <- row_number(f1$Country)
f1$valogic <- f1$va=="non-stationarity"
f1$nfclogic <- f1$nfc == "non-stationarity"
f1$hhlogic <- f1$hh == "non-stationarity"

library(ggtext)

f1 %>%
  mutate(va.label = paste0("<span style = 'color: ",
                         ifelse(valogic==TRUE, "#131516", "#A8AFB3"),  # #131516 is darker, #A8AFB3 is lighter
                         ";'>",
                         Country,
                         "</span>", sep = "") ,
        va.label = fct_reorder(va.label, order)) %>%
  ggplot(aes(x =Year, y = va.label)) +
  geom_tile(aes(fill = log(DV_VA))) +
  scale_fill_gradient2(midpoint=10,
                       low="#FFEBEF", mid = "#FF708D",high="#F50031") +
  theme_bw()  +
  theme(axis.text.y = element_markdown()) +
  xlab("Year") + 
  ylab("Values Added in Financial Activities (log)") +  
  labs(fill = "") -> figure1a

f1 %>%
  mutate(nfc.label = paste0("<span style = 'color: ",
                            ifelse(nfclogic==TRUE, "#131516", "#A8AFB3"),  # #131516 is darker, #A8AFB3 is lighter
                           ";'>",
                           Country,
                           "</span>", sep = "") ,
         nfc.label = fct_reorder(nfc.label, order)) %>%
  ggplot(aes(x =Year, y = nfc.label)) +
  geom_tile(aes(fill = log(DV_nfc_ls))) +
  scale_fill_gradient2(midpoint=6,
                       low="#aecdc2", mid = "#6aaa96",high="#488f31") +
  theme_bw()  +
  theme(axis.text.y = element_markdown()) + 
  xlab("Year") + 
  ylab("Nonfinancial Corporate Debts (log)") +  
  labs(fill = "") -> figure1b



f1 %>%
  mutate(hh.label = paste0("<span style = 'color: ",
                           ifelse(hhlogic==TRUE, "#131516", "#A8AFB3"),   # #131516 is darker, #A8AFB3 is lighter
                            ";'>",
                            Country,
                            "</span>", sep = "") ,
         hh.label = fct_reorder(hh.label, order)) %>%
  ggplot(aes(x =Year, y = hh.label)) +
  geom_tile(aes(fill = log(DV_hh_ls))) +
  scale_fill_gradient2(midpoint=6,
                       low="#c1e7ff", mid = "#6996b3",high="#004c6d") +
  theme_bw()  +
  theme(axis.text.y = element_markdown()) + 
  xlab("Year") + 
  ylab("Household Debts (log)") +  
  labs(fill = "") -> figure1c


library(ggpubr)
ggarrange(figure1a, figure1b, figure1c, 
          #labels = c("", "", ""),
          ncol = 3, nrow = 1,
          legend = "bottom") -> figure1


ggsave(
  "figure1.png",
  plot = last_plot(),
  device = NULL,
  path = "../table_and_figure",
  scale = 1,
  width = 14.50,
  height = 8,
  units = c("in", "cm", "mm"),
  dpi = 320)



## Correlations
va_cor <- va_df %>%
  select(- Year, -Country, -DV_VA_pc)
colnames(va_cor) = c("DV lag",
                     "GNL", "GNL lag",
                     "GE", "GE lag",
                     "NTB", "NTB lag",
                     "FNO", "FNO lag",
                     "REER", "GDP", "CPI", "GINI")
va_cormat <- round(cor(va_cor),2)
melted_va <- melt(va_cormat)
corf_va <- ggplot(data = melted_va, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint=0,
                       low="#2dacb8", mid = "#ffffff",high="#fb7d5f") +
  xlab("") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))



nfc_cor <- nfc_df %>%
  select(- Year, -Country, -DV_nfc_ls_pc)
colnames(nfc_cor) = c("NFC lag",
                      "GNL", "GNL lag",
                      "GE", "GE lag",
                      "NTB", "NTB lag",
                      "FNO", "FNO lag",
                      "REER", "GDP", "CPI", "GINI")
nfc_cormat <- round(cor(nfc_cor),2)
melted_nfc <- melt(nfc_cormat)
corf_nfc <- ggplot(data = melted_nfc, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint=0,
                       low="#2dacb8", mid = "#ffffff",high="#fb7d5f")+
  xlab("") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))



hh_cor <- hh_df %>%
  select(- Year, -Country, -DV_hh_ls_pc)
colnames(hh_cor) = c("HD lag",
                     "GNL", "GNL lag",
                     "GE", "GE lag",
                     "NTB", "NTB lag",
                     "FNO", "FNO lag",
                     "REER", "GDP", "CPI", "GINI")
hh_cormat <- round(cor(hh_cor),2)
melted_hh <- melt(hh_cormat)
corf_hh <- ggplot(data = melted_hh, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint=0,
                       low="#2dacb8", mid = "#ffffff",high="#fb7d5f")+
  xlab("") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))


#library(ggpubr)
corf <- ggarrange(corf_va, corf_nfc, corf_hh, 
                  labels = c("", "", ""),
                  ncol = 3, nrow = 1,
                  legend = "bottom") 
corf

ggsave(
  "corf.png",
  plot = last_plot(),
  device = NULL,
  path = "./table_and_figure",
  scale = 1,
  width = 11.5,
  height = 4.5,
  units = c("in", "cm", "mm"),
  dpi = 320)



