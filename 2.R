lo <-  read.csv("final_lo.csv",  header = TRUE)
View(lo)

hi <-  read.csv("final_hi.csv",  header = TRUE)
View(hi)

data <- rbind(lo, hi)

library(tidyverse)

str(lo)

# 하위 연도별 기부금 추이
lo %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_oda, col = RECIPIENTNAME)) +
    geom_line() +
    geom_point() +
    ggtitle("기부 금액 수취 하위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_ict, col = RECIPIENTNAME)) + 
    geom_line() + 
    geom_point() +
    ggtitle("ICT 하위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_rd, col = RECIPIENTNAME)) + 
    geom_line() + 
    geom_point() +
    ggtitle("R&D 하위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_rea, col = RECIPIENTNAME)) +
    geom_line() +
    geom_point() +
    ggtitle("연구자수 증감 하위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  subset(RECIPIENTNAME == 'Iran') %>%
  ggplot() + 
    geom_line(aes(x = YEAR, y = Value_oda)) + 
    geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
    geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
    geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
    ggtitle("이란의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
    theme(plot.title = element_text(size = 15, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1)) +
    labs(color = "지표",  x = "연도", y = "값")


lo %>%
  subset(RECIPIENTNAME == 'Iran') %>%
  summarise(Value_ict_cor = cor(Value_oda, Value_ict), Value_rea_cor = cor(Value_oda, Value_rea), Value_rea_rd = cor(Value_oda, Value_rd))

lo %>%
  subset(RECIPIENTNAME == 'Honduras') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("온두라스의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  subset(RECIPIENTNAME == 'Chile') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("칠레의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  subset(RECIPIENTNAME == 'Guatemala') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("과테말라의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  subset(RECIPIENTNAME == 'Kazakhstan') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("카자흐스탄의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

lo %>%
  subset(RECIPIENTNAME == 'Paraguay') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("파라과이의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_oda, col = RECIPIENTNAME)) + 
  geom_line() + 
  geom_point() +
  ggtitle("기부금액수취 상위그룹") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_ict, col = RECIPIENTNAME)) +
    geom_line() +
    geom_point() +
    ggtitle("ICT 상위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_rea, col = RECIPIENTNAME)) +
    geom_line() + 
    geom_point() +
    ggtitle("R&D 상위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  group_by(RECIPIENTNAME) %>%
  ggplot(aes(x = YEAR, y = Value_rd, col = RECIPIENTNAME)) + 
    geom_line() + 
    geom_point() +
    ggtitle("연구자수 증감 상위그룹") + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == 'Turkiye') %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("터키의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == "China (People's Republic of)") %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("중국의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == "Mexico") %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("멕시코의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == "Tunisia") %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("튀니지지의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == "Serbia") %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("세르비아의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

hi %>%
  subset(RECIPIENTNAME == "Argentina") %>%
  ggplot() + 
  geom_line(aes(x = YEAR, y = Value_oda)) + 
  geom_line(aes(x = YEAR, y = Value_ict), color = 'red') + 
  geom_line(aes(x = YEAR, y = Value_rea), color = 'blue') + 
  geom_line(aes(x = YEAR, y = Value_rd), color = 'green') +
  ggtitle("아르헨티나의 과학기술지표 연도별 추이(ICT, 연구자수, R&D)") + 
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1))

pairs( hi[,4:7])
pairs( lo[,4:7])
