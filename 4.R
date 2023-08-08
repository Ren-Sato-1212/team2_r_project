hi %>%
  group_by(RECIPIENTCODE) %>%
  summarise(cor_ict = cor(Value_oda, Value_ict), cor_rd = cor(Value_oda, Value_rd), cor_rea = cor(Value_oda, Value_rea))

lo %>%
  group_by(RECIPIENTCODE) %>%
  summarise(cor_ict = cor(Value_oda, Value_ict), cor_rd = cor(Value_oda, Value_rd), cor_rea = cor(Value_oda, Value_rea))

library(psych)

str(hi)

hi %>%
  subset(RECIPIENTCODE == 63) %>%
  select(YEAR, RECIPIENTCODE, Value_oda, Value_ict, Value_rd, Value_rea) %>%
  ggplot(aes(x = YEAR)) + geom_line(aes(y = Value_oda), color = "blue") + geom_line(aes(y = Value_ict), color = 'red') + geom_line(aes(y = Value_rea), color = 'green') + geom_line(aes(y = Value_rd), color = 'black')


temp <- hi %>%
  group_by(RECIPIENTCODE, YEAR)

as.data.frame(temp)
