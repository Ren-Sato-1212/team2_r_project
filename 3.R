lo.mean <- lo %>%
  group_by(YEAR) %>%
  summarise(Value_oda_mean = mean(Value_oda), Value_ict_mean = mean(Value_ict), Value_rea_mean = mean(Value_rea), Value_rd_mean = mean(Value_rd))

hi.mean <- hi %>%
  group_by(YEAR) %>%
  summarise(Value_oda_mean = mean(Value_oda), Value_ict_mean = mean(Value_ict), Value_rea_mean = mean(Value_rea), Value_rd_mean = mean(Value_rd))

lo.mean <- as.data.frame(lo.mean)
hi.mean <- as.data.frame(hi.mean)

lo.mean.s <- as.data.frame(scale(lo.mean))
hi.mean.s <- as.data.frame(scale(hi.mean))

View(lo.mean.s)

ggplot() + geom_line(data = lo.mean, aes(x = YEAR, y = Value_oda_mean), color = 'red') + geom_line(data = hi.mean, aes(x = YEAR, y = Value_oda_mean)) + geom_point(data = lo.mean, aes(x = YEAR, y = Value_oda_mean), color = 'red') + geom_line(data = lo.mean, aes(x = YEAR, y = Value_oda_mean), color = 'red') + geom_point(data = hi.mean, aes(x = YEAR, y = Value_oda_mean))
ggplot() + geom_line(data = lo.mean, aes(x = YEAR, y = Value_ict_mean), color = 'red') + geom_line(data = hi.mean, aes(x = YEAR, y = Value_ict_mean)) + geom_point(data = lo.mean, aes(x = YEAR, y = Value_ict_mean), color = 'red') + geom_line(data = lo.mean, aes(x = YEAR, y = Value_ict_mean), color = 'red') + geom_point(data = hi.mean, aes(x = YEAR, y = Value_ict_mean))
ggplot() + geom_line(data = lo.mean, aes(x = YEAR, y = Value_rea_mean), color = 'red') + geom_line(data = hi.mean, aes(x = YEAR, y = Value_rea_mean)) + geom_point(data = lo.mean, aes(x = YEAR, y = Value_rea_mean), color = 'red') + geom_line(data = lo.mean, aes(x = YEAR, y = Value_rea_mean), color = 'red') + geom_point(data = hi.mean, aes(x = YEAR, y = Value_rea_mean))
ggplot() + geom_line(data = lo.mean, aes(x = YEAR, y = Value_rd_mean), color = 'red') + geom_line(data = hi.mean, aes(x = YEAR, y = Value_rd_mean)) + geom_point(data = lo.mean, aes(x = YEAR, y = Value_rd_mean), color = 'red') + geom_line(data = lo.mean, aes(x = YEAR, y = Value_rd_mean), color = 'red') + geom_point(data = hi.mean, aes(x = YEAR, y = Value_rd_mean))


cor(lo.mean$Value_oda_mean, lo.mean$Value_ict_mean) # -0.594179
cor(lo.mean$Value_oda_mean, lo.mean$Value_rea_mean) # -0.570527
cor(lo.mean$Value_oda_mean, lo.mean$Value_rd_mean) # -0.5241686

ggplot(data = lo.mean, aes(x = Value_oda_mean, y = Value_ict_mean)) + geom_point()
ggplot(data = lo.mean, aes(x = Value_oda_mean, y = Value_rea_mean)) + geom_point()
ggplot(data = lo.mean, aes(x = Value_oda_mean, y = Value_rd_mean)) + geom_point()

cor(hi.mean$Value_oda_mean, hi.mean$Value_ict_mean) # -0.3780562
cor(hi.mean$Value_oda_mean, hi.mean$Value_rea_mean) # -0.08215799
cor(hi.mean$Value_oda_mean, hi.mean$Value_rd_mean) # -0.27525

ggplot(data = hi.mean, aes(x = Value_oda_mean, y = Value_ict_mean)) + geom_point()
ggplot(data = hi.mean, aes(x = Value_oda_mean, y = Value_rea_mean)) + geom_point()
ggplot(data = hi.mean, aes(x = Value_oda_mean, y = Value_rd_mean)) + geom_point()

str(lo.mean)

lo$Value_oda <- scale(lo$Value_oda)
lo$Value_ict <- scale(lo$Value_ict)
lo$Value_rea <- scale(lo$Value_rd)
lo$Value_rd <- scale(lo$Value_rd)

hi$Value_oda <- scale(hi$Value_oda)
hi$Value_ict <- scale(hi$Value_ict)
hi$Value_rea <- scale(hi$Value_rd)
hi$Value_rd <- scale(hi$Value_rd)

pairs.panels(hi[, 4:7])
