library(tidyverse)

wiid <- read_csv("./data_source/wiid.csv")

wiid$quality <- factor(wiid$quality,
                     levels = c("High","Average","Low","Not Known"))
wiid$source <- factor(wiid$source, 
                    levels = c("United Nations", "World Bank", "OECD Other international organizations", "Eurostat","SEDLAC", 
                               "Luxembourg Income Study", "National statistical authority", "Research study"))

wiid$resource <- factor(wiid$resource,
                      levels = c("Income (net)", "Income (net/gross)", "Income (gross)", "Earnings", "Consumption"))

wiid$scale <- factor(wiid$scale,
                   levels = c("Per capita", "No adjustment", "Equivalized"))

wiid$popcovr <- factor(wiid$popcovr,
                     levels = c("All", "Economically active", "Specific categories"))

wiid$sharing_unit <- factor(wiid$sharing_unit,
                   levels = c("Household", "Family", "Tax unit", "Person"))


## strict version
## In this version, the variables of resource, scale, sharing_unit, areacovr, popcovr must fit pre-determined required.
## Duplicated rows are selected by quality and source

wiid <- wiid[order(wiid$country, wiid$year, wiid$quality, wiid$source),]

select_wiid <- wiid %>%
  filter(resource == "Income (net)",
         scale == "Per capita",
         sharing_unit == "Household",
         areacovr == "All",
         popcovr == "All")

strict_gini <- select_wiid[duplicated(cbind(select_wiid$country, select_wiid$year))==F,] 
### strict_gini has 1183 obs.



## relaxed version
## in this version, only arecovr is required to be "All",
## for other variables, we look for the second or third best if the best data are not available.

wiid <- wiid[order(wiid$country, wiid$year, wiid$quality, wiid$scale, wiid$resource, wiid$source, wiid$popcovr, wiid$sharing_unit),]
select_wiid <- wiid %>%
  filter(areacovr == "All")
relaxed_gini <- select_wiid[duplicated(cbind(select_wiid$country, select_wiid$year))==F,] 
## relaxed_gini has 3548 obs

rm(select_wiid)


