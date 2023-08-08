# data.2012.raw <- read.table('./project/CRS_2012_data.txt', header = T, encoding = 'utf-8', sep = '|')

source('./project/data.load.R')

data.2012 <- data.load('./project/CRS_2012_data.txt')
data.2013 <- data.load('./project/CRS_2013_data.txt')
data.2014 <- data.load('./project/CRS_2014_data.txt')
data.2015 <- data.load('./project/CRS_2015_data.txt')
data.2016 <- data.load('./project/CRS_2016_data.txt')
data.2017 <- data.load('./project/CRS_2017_data.txt')
data.2018 <- data.load('./project/CRS_2018_data.txt')
data.2019 <- data.load('./project/CRS_2019_data.txt')
data.2020 <- data.load('./project/CRS_2020_data.txt')
data.2021 <- data.load('./project/CRS_2021_data.txt') # 파일 읽기

str(data.2012)
View(data.2012)

data.2015.arrange <- data.2015 %>%
  select(Year, DonorCode, DonorName, AgencyCode, AgencyName, ProjectNumber, RecipientCode, RecipientName, RegionCode, RegionName, USD_Disbursement, USD_Disbursement_Defl, GrantEquiv, USD_GrantEquiv, PurposeCode, PurposeName)
data.2016.arrange <- data.2016 %>%
  select(Year, DonorCode, DonorName, AgencyCode, AgencyName, ProjectNumber, RecipientCode, RecipientName, RegionCode, RegionName, USD_Disbursement, USD_Disbursement_Defl, GrantEquiv, USD_GrantEquiv, PurposeCode, PurposeName)
data.2017.arrange <- data.2017 %>%
  select(Year, DonorCode, DonorName, AgencyCode, AgencyName, ProjectNumber, RecipientCode, RecipientName, RegionCode, RegionName, USD_Disbursement, USD_Disbursement_Defl, GrantEquiv, USD_GrantEquiv, PurposeCode, PurposeName)

View(data.2015.arrange)
View(data.2016.arrange)
View(data.2017.arrange)

unique(data.2015.arrange$DonorName)
unique(data.2015.arrange$AgencyName)
unique(data.2015.arrange$RecipientName)
unique(data.2015.arrange$RegionName)
unique(data.2015.arrange$PurposeName)

write.csv(data.2015.arrange, "./test/year2015.csv", row.names = F)
write.csv(data.2016.arrange, "./test/year2016.csv", row.names = F)
write.csv(data.2017.arrange, "./test/year2017.csv", row.names = F)


