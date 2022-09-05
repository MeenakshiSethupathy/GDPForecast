rm(list = ls())
# Install and load required packages if not already installed
if (!require("pacman")) {
  install.packages("pacman")
}
p_load(quantmod, mFilter, zoo, dynlm, sandwich, lmtest, readxl, writexl)


# Pull data from FRED
#getSymbols(series_name,src=source)
getSymbols("UNRATE",src="FRED")
mydata<-UNRATE

ur=ts(mydata$UNRATE, start=c(1947,1),freq=4)
ur<- 100 * log(ur)
ur<-window(ur, c(1947,1),c(2019,4))
ur.hp<-hpfilter(ur)
plot(ur.hp)

hamilton_end <- c(2019, 4)
lags <- c(8, 9, 10, 11)

ham <- function(ur) {
  data.merge <- ts(merge.zoo(ur,
                             lag(ur, -lags[1]),
                             lag(ur, -lags[2]),
                             lag(ur, -lags[3]),
                             lag(ur, -lags[4])),start = c(1947,1), end= hamilton_end, frequency=4)
  data.subset <- window(data.merge, start=c(1949,4))
  colnames(data.subset) <- c("ylead8", "y", "ylag1", "ylag2", "ylag3")
  ham_reg <- lm(ylead8 ~., data = data.subset)
  summary(ham_reg)
  trend <- predict(ham_reg)
  names(trend) <- NULL
  trend <- ts(trend,
              start =c(1949,4),
              end = hamilton_end,
              freq = 4)
  cycle <- ur - trend
  
  ham <- merge.zoo(ur, trend, cycle)
  return(ham)
}
#using the hamilton filter
gdp.ur <- ham(ur)

plot(cycle, col=2, main="Hamilton filter UR vs Hodrick Prescott Filter")
lines(ur.hp$cycle, col =3)
legend("topright",legend=c("Hamilton filter UR", "HP"), col=2:3, lty=rep(1,3), ncol=1)
