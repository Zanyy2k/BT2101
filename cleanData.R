library(dplyr)
library(data.table)

setwd("/Users/benjaminong/Desktop/Shiny/SIA.dashboard")

followees = setDT(read.csv("followees.csv", as.is = T))
country = read.csv("followers_by_country.csv", as.is = T)
country = setDT(country[which(country$count >= 30),])

country$percent.popn = country$count/country$pop * 100
country$percent.total = round(country$count / sum(country$count) * 100, 3)
country = country %>% arrange(-count)
write.csv(country, "followers_country_count.csv", row.names = F)
country$percent.popn = NULL
country$percent.total = NULL


most.countries = setDT(read.csv("most_countries_top_100.csv", as.is = T))
country.top.100 = setDT(read.csv("top_100_per_country.csv", as.is = T))
overall.top.500 = setDT(read.csv("top_500_overall.csv", as.is = T))

colnames(followees)[1] = "followee"
colnames(overall.top.500)[1] = "followee"

setkey(followees, followee)
setkey(country, country)

setkey(most.countries, followee)
setkey(country.top.100, followee)
setkey(country.top.100, country)
setkey(overall.top.500, followee)

most.countries = followees[most.countries]
overall.top.500 = followees[overall.top.500]

country.top.100 = merge(country.top.100, followees, by = "followee")
country.top.100 = merge(country.top.100, country, by = "country")
country.top.100$percent = round(country.top.100$count.x / country.top.100$count.y, 4)*100
colnames(country.top.100)[3] = "count"

most.countries$followee = NULL
overall.top.500$followee = NULL
country.top.100$followee = NULL

write.csv(most.countries %>% arrange(-count), "most_countries.csv", row.names = F)
write.csv(overall.top.500, "overall_top_500.csv", row.names = F)
write.csv(country.top.100 %>% arrange(-percent), "country_top_100.csv", row.names = F)

rm(list = ls())