
# Install and load required packages if not already installed
if (!require("pacman")) {
  install.packages("pacman")
}
p_load(quantmod, mFilter, zoo, dynlm, sandwich, lmtest, readxl, writexl)


# Pull data from FRED
#getSymbols(series_name,src=source)
getSymbols("GDPC1",src="FRED")
mydata<-GDPC1
# Convert the numeric data to time series data sets
#gdp=ts(mydata[,(1)], start.ts, end.ts,freq=freq.ts)
gdp<-ts(mydata$GDPC1, start=c(1947,1),freq=4)
gdp <- 100 * log(gdp)
gdp<-window(gdp, c(1947,1),c(2019,4))


gdp.hp<-hpfilter(gdp)
plot(gdp.hp)
hamilton_end <- c(2019, 4)
lags <- c(8, 9, 10, 11)

ham <- function(gdp) {
  data.merge <- ts(merge.zoo(gdp,
                             lag(gdp, -lags[1]),
                             lag(gdp, -lags[2]),
                             lag(gdp, -lags[3]),
                             lag(gdp, -lags[4])),start = c(1947,1), end= hamilton_end, frequency=4)
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
  cycle <- gdp - trend
  
  ham <- merge.zoo(gdp, trend, cycle)
  return(ham)
}
#using the hamilton filter
gdp.ham <- ham(gdp)

plot(cycle, col=2, main="Hamilton filter vs Hodrick Prescott Filter")
lines(gdp.hp$cycle, col =3)
legend("topright",legend=c("Hamilton filter", "HP"), col=2:3, lty=rep(1,3), ncol=1)
