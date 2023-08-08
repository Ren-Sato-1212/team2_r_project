
data2021 <-  read.csv("year2021.csv",  header = TRUE)
View(data2021)

data2020 <-  read.csv("year2020.csv",  header = TRUE)
View(data2020)

data2019 <-  read.csv("year2019.csv",  header = TRUE)
View(data2019)

data2018 <-  read.csv("year2018.csv",  header = TRUE)
View(data2018)

data2017 <-  read.csv("year2017.csv",  header = TRUE)
View(data2017)

data2016 <-  read.csv("year2016.csv",  header = TRUE)
View(data2016)

data2015 <-  read.csv("year2015.csv",  header = TRUE)
View(data2015)

data2014 <-  read.csv("year2014.csv",  header = TRUE)
View(data2014)

data2013 <-  read.csv("year2013.csv",  header = TRUE)
View(data2013)

data2010 <-  read.csv("year2010.csv",  header = TRUE)
View(data2010)

colnames(data2015) <- colnames(data2010)
colnames(data2016) <- colnames(data2010)
colnames(data2017) <- colnames(data2010)

data <- rbind(data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019, data2020, data2021)

View(data)

library(tidyverse)

temp <- data %>% 
  subset(PURPOSECODE == 11182 | PURPOSECODE == 12182 | PURPOSECODE == 23182 | PURPOSECODE == 31182 | PURPOSECODE == 31282 | PURPOSECODE == 31382 | PURPOSECODE == 32182 | PURPOSECODE == 41082 | PURPOSECODE == 43082 | PURPOSECODE == 22010 | PURPOSECODE == 22020 | PURPOSECODE == 22030 | PURPOSECODE == 22040) %>%
  group_by(RECIPIENTCODE, RECIPIENTNAME) %>%
  summarise(USD_DISBURSEMENT_DEFL_SUM = sum(USD_DISBURSEMENT_DEFL, na.rm = T))

temp <- sort(temp)

View(temp)

sort(unique(temp$PURPOSECODE))

data[data$RECIPIENTCODE == 55, 'RECIPIENTNAME'] <- 'Turkiye'
data[data$RECIPIENTCODE == 55, 'RECIPIENTNAME']

temp2 <- data %>% 
  subset(PURPOSECODE == 11182 | PURPOSECODE == 12182 | PURPOSECODE == 23182 | PURPOSECODE == 31182 | PURPOSECODE == 31282 | PURPOSECODE == 31382 | PURPOSECODE == 32182 | PURPOSECODE == 41082 | PURPOSECODE == 43082 | PURPOSECODE == 22010 | PURPOSECODE == 22020 | PURPOSECODE == 22030 | PURPOSECODE == 22040) %>%
  group_by(YEAR, RECIPIENTCODE, RECIPIENTNAME) %>%
  summarise(USD_DISBURSEMENT_DEFL_SUM = sum(USD_DISBURSEMENT_DEFL, na.rm = T)) %>%
  subset(RECIPIENTCODE == 55 | RECIPIENTCODE == 425 | RECIPIENTCODE == 635 | RECIPIENTCODE == 645 | RECIPIENTCODE == 730 | RECIPIENTCODE == 769 | RECIPIENTCODE == 139 | RECIPIENTCODE == 238 | RECIPIENTCODE == 431 | RECIPIENTCODE == 248 | RECIPIENTCODE == 282 | RECIPIENTCODE == 738 | RECIPIENTCODE == 136 | RECIPIENTCODE == 798 | RECIPIENTCODE == 666)

View(temp2)

rd <- read.csv('R&D지표_종속변수_2010_2021.csv', header = T, encoding = 'utf-8')
ict <- read.csv('인터넷지표_종속변수_2010_2021.csv', header = T, encoding = 'utf-8')
rea <- read.csv('연구자수_종속변수_2010_2021.csv', header = T, encoding = 'utf-8')

unique(rd$Country)

View(rd)
view(ict)
view(rea)

lo <- data %>% # 하위
  subset(PURPOSECODE == 11182 | PURPOSECODE == 12182 | PURPOSECODE == 23182 | PURPOSECODE == 31182 | PURPOSECODE == 31282 | PURPOSECODE == 31382 | PURPOSECODE == 32182 | PURPOSECODE == 41082 | PURPOSECODE == 43082 | PURPOSECODE == 22010 | PURPOSECODE == 22020 | PURPOSECODE == 22030 | PURPOSECODE == 22040) %>%
  group_by(YEAR, RECIPIENTCODE, RECIPIENTNAME) %>%
  summarise(USD_DISBURSEMENT_DEFL_SUM = sum(USD_DISBURSEMENT_DEFL, na.rm = T)) %>%
  subset(RECIPIENTCODE == 540 | RECIPIENTCODE == 351 | RECIPIENTCODE == 451 | RECIPIENTCODE == 613 | RECIPIENTCODE == 434 | RECIPIENTCODE == 347)

hi <- data %>% # 상위
  subset(PURPOSECODE == 11182 | PURPOSECODE == 12182 | PURPOSECODE == 23182 | PURPOSECODE == 31182 | PURPOSECODE == 31282 | PURPOSECODE == 31382 | PURPOSECODE == 32182 | PURPOSECODE == 41082 | PURPOSECODE == 43082 | PURPOSECODE == 22010 | PURPOSECODE == 22020 | PURPOSECODE == 22030 | PURPOSECODE == 22040) %>%
  group_by(YEAR, RECIPIENTCODE, RECIPIENTNAME) %>%
  summarise(USD_DISBURSEMENT_DEFL_SUM = sum(USD_DISBURSEMENT_DEFL, na.rm = T)) %>%
  subset(RECIPIENTCODE == 55 | RECIPIENTCODE == 425 | RECIPIENTCODE == 730 | RECIPIENTCODE == 139 | RECIPIENTCODE == 358 | RECIPIENTCODE == 63)
  
  
View(lo)
View(hi)

rd[rd$LOCATION == 'TUR', 'Country'] <- 'Turkiye'

lo.rd <- rd %>% 
  select(Country, TIME, Value) %>%
  group_by(Country, TIME) %>%
  subset(Country == 'Iran (Islamic Republic of)' | Country == 'Honduras' | Country == 'Paraguay' | Country == 'Kazakhstan' | Country == 'Chile' | Country == 'Guatemala')

hi.rd <- rd %>% 
  select(Country, TIME, Value) %>%
  group_by(Country, TIME) %>%
  subset(Country == 'Turkiye' | Country == 'Argentina' | Country == 'China' | Country == 'Tunisia' | Country == 'Mexico' | Country == 'Serbia')

view(lo.rd)
View(hi.rd)

str(ict)
head(ict)

ict[ict$entityIso == 'TUR', 'entityName'] <- 'Turkiye'

lo.ict <- ict %>% 
  select(seriesUnits, entityName, dataValue, dataYear) %>%
  group_by(entityName, dataYear) %>%
  subset(entityName == 'Iran (Islamic Republic of)' | entityName == 'Honduras' | entityName == 'Paraguay' | entityName == 'Kazakhstan' | entityName == 'Chile' | entityName == 'Guatemala') %>%
  subset(seriesUnits == 'Mbit/s') %>%
  select(entityName, dataValue, dataYear)

hi.ict <- ict %>% 
  select(seriesUnits, entityName, dataValue, dataYear) %>%
  group_by(entityName, dataYear) %>%
  subset(entityName == 'Turkiye' | entityName == 'Argentina' | entityName == 'China' | entityName == 'Tunisia' | entityName == 'Mexico' | entityName == 'Serbia') %>%
  subset(seriesUnits == 'Mbit/s') %>%
  select(entityName, dataValue, dataYear)

view(lo.ict)
view(hi.ict)

str(rea)

rea[rea$LOCATION == 'TUR', 'Country'] <- 'Turkiye'

lo.rea <- rea %>% 
  select(Country, TIME, Value) %>%
  group_by(Country, TIME) %>%
  subset(Country == 'Iran (Islamic Republic of)' | Country == 'Honduras' | Country == 'Paraguay' | Country == 'Kazakhstan' | Country == 'Chile' | Country == 'Guatemala')

hi.rea <- rea %>% 
  select(Country, TIME, Value) %>%
  group_by(Country, TIME) %>%
  subset(Country == 'Turkiye' | Country == 'Argentina' | Country == 'China' | Country == 'Tunisia' | Country == 'Mexico' | Country == 'Serbia')

view(lo.rea)
view(hi.rea)

str(lo.ict)
lo.ict <- rbind(lo.ict, data.frame(entityName = 'Paraguay', dataValue = 83000, dataYear = 2017))
lo.ict <- rbind(lo.ict, data.frame(entityName = 'Paraguay', dataValue = 86000, dataYear = 2018))
lo.ict <- rbind(lo.ict, data.frame(entityName = 'Paraguay', dataValue = 89000, dataYear = 2019))
lo.ict <- rbind(lo.ict, data.frame(entityName = 'Paraguay', dataValue = 92000, dataYear = 2020))

view(lo.ict)

write.csv(hi, 'hi.csv', row.names = F)
write.csv(lo, 'lo.csv', row.names = F)
write.csv(hi.ict, 'hi_ict.csv', row.names = F)
write.csv(hi.rd, 'hi_rd.csv', row.names = F)
write.csv(hi.rea, 'hi_rea.csv', row.names = F)
write.csv(lo.ict, 'lo_ict.csv', row.names = F)
write.csv(lo.rd, 'lo_rd.csv', row.names = F)
write.csv(lo.rea, 'lo_rea.csv', row.names = F)

getwd()
