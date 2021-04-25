library(pacman)
p_load(ggpubr, ggtext, tidyverse)
setwd("~/Downloads/GitHub/Hegemony_Pitfall_or_Statecraft_Toolkit")
df <- read.csv("./data_processing/financialization_df.csv")
df$X <- NULL
df <- df %>%
  filter(Year >= 1960) 
colnames(df)
df[df==0] <- NA
df$Country[duplicated(cbind(df$Country, df$Year))]
df <- df[!duplicated(cbind(df$Country, df$Year)),]
df <- df[order(df$Country, df$Year),]

#library(ggplot2)
f1 <- select(df, 
             Year, Country, DV_VA, DV_nfc_ls, DV_hh_ls)
f1 <- na.omit(f1)
f1 <- f1[order(f1$Country, f1$Year),]

# merge f1 with the ADF test results
variety <- read_rds("variety.rds")
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

#library(ggtext)

f1 %>%
  mutate(va.label = paste0("<span style = 'color: ",
                         ifelse(valogic==TRUE, "#de425b", "#488f31"),  # #488f31 is green, #de425b is red
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
                           ifelse(nfclogic==TRUE, "#de425b", "#488f31"),  # #488f31 is green, #de425b is red
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
                            ifelse(hhlogic==TRUE, "#de425b", "#488f31"),  # #488f31 is green, #de425b is red
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


#library(ggpubr)
ggarrange(figure1a, figure1b, figure1c, 
          #labels = c("", "", ""),
          ncol = 3, nrow = 1,
          legend = "bottom")



