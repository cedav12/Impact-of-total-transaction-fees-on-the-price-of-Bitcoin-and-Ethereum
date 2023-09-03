library("tseries")
library("xts")
library("zoo")
library("Hmisc")
library("systemfit")
library("AER")
library("broom")

library("foreign")
library("xtable")
library("stargazer")

setwd("~/Impact-of-total-transaction-fees-on-the-price-of-Bitcoin-and-Ethereum")

btc_data = read.csv("data/btc_data1.csv")
btc_data$total_addresses = btc_data$active_addresses
btc_data$mean_fees = btc_data$total_fees / btc_data$total_transactions

eth_data = read.csv("data/eth_data1.csv")
eth_data$total_addresses = eth_data$active_addresses
eth_data$mean_fees = eth_data$total_fees / eth_data$total_transactions

# Compute lagged_price.
lagged = function(df, colname="close"){
  lagged_val = c(NA, df[1:nrow(df) - 1, colname])
  new_name = paste0("lagged_", colname)
  df[new_name] = lagged_val
  return(df)
}
btc_data = lagged(btc_data, "close")
btc_data = lagged(btc_data, "hash_rate_mean")
eth_data = lagged(eth_data, "close")
eth_data = lagged(eth_data, "hash_rate_mean")

# We will check correlation of potential instruments with endogeneous variables.
btc_cor = cor(as.matrix(btc_data[2:nrow(btc_data), 3:ncol(btc_data)]))
eth_cor = cor(as.matrix(eth_data[2:nrow(eth_data), 3:ncol(eth_data)]))

# test whather instruments are significant.

# Instruments for Price.
summary(lm(log(close) ~ log(sp500), data = btc_data))
summary(lm(log(close) ~ log(gold), data = btc_data))
summary(lm(log(close) ~ log(vix), data = btc_data))
summary(lm(log(close) ~ log(lagged_close), data = btc_data))
summary(lm(log(close) ~ total_supply, data = btc_data))
summary(lm(log(close) ~ log(transfers_med), data = btc_data))
summary(lm(log(close) ~ log(lagged_hash_rate_mean), data = btc_data))

# Instruments for Searches.
summary(lm(log(google_trends) ~ log(sp500), data = btc_data))
summary(lm(log(google_trends) ~ log(gold), data = btc_data))
summary(lm(log(google_trends) ~ log(vix), data = btc_data))
summary(lm(log(google_trends) ~ log(lagged_close), data = btc_data))
summary(lm(log(google_trends) ~ total_supply, data = btc_data))
summary(lm(log(google_trends) ~ log(transfers_med), data = btc_data))
summary(lm(log(google_trends) ~ log(lagged_hash_rate_mean), data = btc_data))

# Instruments for Hashrate.
summary(lm(log(hash_rate_mean) ~ log(sp500), data = btc_data))
summary(lm(log(hash_rate_mean) ~ log(gold), data = btc_data))
summary(lm(log(hash_rate_mean) ~ log(vix), data = btc_data))
summary(lm(log(hash_rate_mean) ~ log(lagged_close), data = btc_data))
summary(lm(log(hash_rate_mean) ~ total_supply, data = btc_data))
summary(lm(log(hash_rate_mean) ~ log(transfers_med), data = btc_data))
summary(lm(log(hash_rate_mean) ~ log(lagged_hash_rate_mean), data = btc_data))

# Instruments for Fees.
summary(lm(log(total_fees) ~ log(sp500), data = btc_data))
summary(lm(log(total_fees) ~ log(gold), data = btc_data))
summary(lm(log(total_fees) ~ log(vix), data = btc_data))
summary(lm(log(total_fees) ~ log(lagged_close), data = btc_data))
summary(lm(log(total_fees) ~ total_supply, data = btc_data))
summary(lm(log(total_fees) ~ log(transfers_med), data = btc_data))
summary(lm(log(total_fees) ~ log(lagged_hash_rate_mean), data = btc_data))


# We will build model of simultaneous equations. One equation for Price,
# Second for Total transaction fees.

price_equation = log(close) ~ log(total_fees) + log(hash_rate_mean) + 
  log(google_trends) + log(sigma) + log(sp500) + log(total_addresses)
fee_equation = log(total_fees) ~ log(close) + log(sigma) + log(total_addresses) +
  log(google_trends)
  
# Define system of equation as list, then put it into systemfit function.
system = list(price=price_equation, fees=fee_equation)

# Define instruments, for both equations
instruments = ~ log(sp500) + log(gold) + log(vix) + 
  total_supply + log(transfers_med) + log(sigma) +
  log(total_addresses)

btc_2sls = systemfit(system, inst=instruments, method="2SLS", data=btc_data)
# TODO: nemame cely system - nedava tolik smysl pouzit 3sls
btc_3sls = systemfit(system, inst=instruments, method="3SLS", method3sls="IV",
                     data=btc_data)
btc_ols = systemfit(system, method="OLS", data=btc_data)
summary(btc_2sls)
summary(btc_3sls)
summary(btc_ols)

# Hausman Test
# hausman.systemfit(btc_2sls,btc_3sls)
hausman.systemfit(btc_2sls,btc_ols) # zamitneme consistenci ols


close_eq = ivreg(log(close) ~ log(total_fees) + log(hash_rate_mean) + 
                log(google_trends) + log(sp500) |
  log(sp500) + log(gold) + log(vix) + 
  total_supply + log(transfers_med), data = btc_data)

fee_eq = ivreg(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)
                  | log(sp500) + log(gold) + log(vix) + 
                 total_supply + log(transfers_med) + log(sigma) +
                 log(total_addresses), data = btc_data)

# Test endogeneity - Durbin-Wu-Hausman
summary(close_eq, diagnostic=TRUE)
summary(fee_eq, diagnostic=TRUE)

## Test unit root of residuals
# Augmented Dickey-Fuller test

#H_0 = unit_root
adf.test(close_eq$residuals)
adf.test(fee_eq$residuals)

# KPSS test
kpss.test(close_eq$residuals)
kpss.test(fee_eq$residuals)

## Test heteroscedasticity

# Breusch-Pagan test
bptest(close_eq, data = btc_data)
bptest(fee_eq, data = btc_data)
# heteroscedasticity detected - we should use heteroscedasticity robust errors.


## Test Autocorelation

# Durbin-Watson test
dwtest(close_eq, data = btc_data)
dwtest(fee_eq, data = btc_data)


# Test normality of residuals
shapiro.test(close_eq$residuals)
shapiro.test(fee_eq$residuals)


# heteroscedasticity and autocorrelation errors.
coeftest(close_eq, vcov = vcovHAC(close_eq, type="HC3"))
coeftest(fee_eq, vcov = vcovHAC(fee_eq, type="HC3"))


### ETHEREUM
eth_data$btc = btc_data$close

# There arrise a problem with numerical stability in case of google_trends
# (the minimum = 0), thus we should will rewrite minimum by the second smallest value
eth_data$google_trends[which.min(eth_data$google_trends)] = sort(eth_data$google_trends)[2]

# the minimum of sigma is 0, which arise when the OHLC are all equal.
# check:
eth_data[which.min(eth_data$sigma), c("open", "high", "low", "close")]
# We use the same approach as for sigma:
eth_data$sigma[which.min(eth_data$sigma)] = sort(eth_data$sigma)[2]

close_eq_eth = ivreg(log(close) ~ log(total_fees) + log(btc) + 
                   log(google_trends) + log(sigma) + log(total_addresses) | log(gold) + log(vix) + 
                   total_supply + log(transfers_med) + log(sigma) + log(btc) +
                   log(total_addresses), data = eth_data)

fee_eq_eth = ivreg(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)
                   | log(sp500) + log(gold) + log(vix) + log(total_addresses) +
                 total_supply + log(sigma), data = eth_data)
summary(close_eq_eth, diagnostic=TRUE)
summary(fee_eq_eth, diagnostic=TRUE)

# Wu-Hausman test indicates, that total_fees and close are realy endogeneous.

## Test unit root of residuals
# Augmented Dickey-Fuller test

#nulova hypothesa = unit_root
adf.test(close_eq_eth$residuals)
adf.test(fee_eq_eth$residuals)

# KPSS test
kpss.test(close_eq_eth$residuals)
kpss.test(fee_eq_eth$residuals)


## Test heteroscedasticity

# Breusch-Pagan test
bptest(close_eq_eth, data = eth_data)
bptest(fee_eq_eth, data = eth_data)
# heteroscedasticity detected - we should use heteroscedasticity robust errors.


## Test Autocorelation

# Durbin-Watson test
dwtest(close_eq_eth, data = eth_data)
dwtest(fee_eq_eth, data = eth_data)


# Test normality of residuals
shapiro.test(close_eq_eth$residuals)
shapiro.test(fee_eq_eth$residuals)


# TODO: heteroscedasticity and autocorrelation errors.
coeftest(close_eq_eth, vcov = vcovHAC(close_eq_eth, type="HC3"))
coeftest(fee_eq_eth, vcov = vcovHAC(fee_eq_eth, type="HC3"))


# In ETH there is visible positive trend between fees and close. On the other 
# hand, in case of BTC it seems that several subtrends occur, In order to explore
# this phenomenom, we will divide the whole interval into several subintervals

ggplot(data=btc_data, aes(X, log(close))) +
  geom_point() +
  geom_vline(xintercept = c(750, 1500), color = "red")

### BTC subintervals

# define default formulas
eth_cf = as.formula(log(close) ~ log(total_fees) + log(hash_rate_mean) + 
  log(google_trends) + log(sigma) + log(sp500) + log(total_addresses) +
  log(btc)| log(sp500) + log(gold) + log(vix) + 
  total_supply + log(transfers_med) + log(sigma) +
  log(total_addresses) + log(btc))

btc_cf = as.formula(log(close) ~ log(total_fees) + log(hash_rate_mean) + 
  log(google_trends) + log(sigma) + log(sp500) + log(total_addresses) | 
  log(sp500) + log(gold) + log(vix) + 
  total_supply + log(transfers_med) + log(sigma) +
  log(total_addresses))

fee_f = as.formula(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses) +
  log(google_trends) | log(sp500) + log(gold) + log(vix) + 
  total_supply + log(transfers_med) + log(sigma) +
  log(total_addresses))

# Define function, which creates model
analyze_subinterval = function(data, lower, upper, eq_close, eq_fee){
  result = list()
  if (data == "eth"){
    new_data = eth_data[lower:upper, ]
  } else {
    new_data = btc_data[lower:upper, ]
  }
  
  if (data == "eth"){
    close_eq = ivreg(eq_close, data=new_data)
    
  } else {
    close_eq = ivreg(eq_close, data = new_data)
  }
  fee_eq = ivreg(eq_fee, data = new_data)
  result$close_eq = close_eq
  result$fee_eq = fee_eq
  result$data = new_data
  return(result)
}

dsummary = function(model){
  print(summary(model, diagnostic=TRUE))
}

##### BTC 1 
btc_1_cf = as.formula(log(close) ~ log(total_fees) + log(hash_rate_mean)+
                        log(google_trends) | log(sp500) + log(sigma)+
                        log(gold) + log(vix) + 
                        total_supply + log(transfers_med))
btc_1_fee = as.formula(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)|
                         log(sp500) + log(gold) + log(vix) + 
                         total_supply + log(transfers_med) + log(sigma) +
                         log(total_addresses))


sub_btc_1 = analyze_subinterval("btc", 1, 750, btc_1_cf, btc_1_fee)
dsummary(sub_btc_1$close_eq)
dsummary(sub_btc_1$fee_eq)

adf.test(sub_btc_1$close_eq$residuals)
adf.test(sub_btc_1$fee_eq$residuals)

kpss.test(sub_btc_1$close_eq$residuals)
kpss.test(sub_btc_1$fee_eq$residuals)

bptest(sub_btc_1$close_eq, data = sub_btc_1$data)
bptest(sub_btc_1$fee_eq, data = sub_btc_1$data)

dwtest(sub_btc_1$close_eq, data = sub_btc_1$data)
dwtest(sub_btc_1$fee_eq, data = sub_btc_1$data)

shapiro.test(sub_btc_1$close_eq$residuals)
shapiro.test(sub_btc_1$fee_eq$residuals)


coeftest(sub_btc_1$close_eq, vcov = vcovHAC(sub_btc_1$close_eq, type="HC3"))
coeftest(sub_btc_1$fee_eq, vcov = vcovHAC(sub_btc_1$fee_eq, type="HC3"))

##### BTC 2

btc_2_fee = as.formula(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)|
                         log(gold) + log(vix) + log(transfers_med)+
                          log(sigma) +
                         log(total_addresses))
btc_2_cf = as.formula(log(close) ~ log(total_fees) + 
                        log(google_trends) + log(sigma) + log(sp500)| 
                        log(sp500) + log(gold) + log(vix) + 
                        total_supply + log(transfers_med) + log(sigma) +
                        log(total_addresses))


sub_btc_2 = analyze_subinterval("btc", 751, 1500, btc_2_cf, btc_2_fee)
dsummary(sub_btc_2$close_eq)
dsummary(sub_btc_2$fee_eq)

##R2 coef
cor(predict(sub_btc_2$close_eq), sub_btc_2$data$close)^2


adf.test(sub_btc_2$close_eq$residuals)
adf.test(sub_btc_2$fee_eq$residuals)

kpss.test(sub_btc_2$close_eq$residuals)
kpss.test(sub_btc_2$fee_eq$residuals)

bptest(sub_btc_2$close_eq, data = sub_btc_2$data)
bptest(sub_btc_2$fee_eq, data = sub_btc_2$data)

dwtest(sub_btc_2$close_eq, data = sub_btc_2$data)
dwtest(sub_btc_2$fee_eq, data = sub_btc_2$data)

shapiro.test(sub_btc_2$close_eq$residuals)
shapiro.test(sub_btc_2$fee_eq$residuals)

coeftest(sub_btc_2$close_eq, vcov = vcovHAC(sub_btc_2$close_eq, type="HC3"))
coeftest(sub_btc_2$fee_eq, vcov = vcovHAC(sub_btc_2$fee_eq, type="HC3"))

##### BTC 3
btc_3_fee = as.formula(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)|
                         log(gold) + log(vix) + log(transfers_med)+
                         log(sigma) +
                         log(total_addresses))
btc_3_cf = as.formula(log(close) ~ log(total_fees) + log(hash_rate_mean) + 
                        log(google_trends) + log(sp500) + log(total_addresses) | 
                        log(sp500) + log(gold) + log(vix) + 
                        total_supply + log(transfers_med) + log(sigma) +
                        log(total_addresses))
sub_btc_3 = analyze_subinterval("btc", 1501, nrow(btc_data), btc_3_cf, btc_3_fee)
dsummary(sub_btc_3$close_eq)
dsummary(sub_btc_3$fee_eq)

adf.test(sub_btc_3$close_eq$residuals)
adf.test(sub_btc_3$fee_eq$residuals)

kpss.test(sub_btc_3$close_eq$residuals)
kpss.test(sub_btc_3$fee_eq$residuals)

bptest(sub_btc_3$close_eq, data = sub_btc_3$data)
bptest(sub_btc_3$fee_eq, data = sub_btc_3$data)

dwtest(sub_btc_3$close_eq, data = sub_btc_3$data)
dwtest(sub_btc_3$fee_eq, data = sub_btc_3$data)

shapiro.test(sub_btc_3$close_eq$residuals)
shapiro.test(sub_btc_3$fee_eq$residuals)

coeftest(sub_btc_3$close_eq, vcov = vcovHAC(sub_btc_3$close_eq, type="HC3"))
coeftest(sub_btc_3$fee_eq, vcov = vcovHAC(sub_btc_3$fee_eq, type="HC3"))


### ETH
ggplot(data=eth_data, aes(X, log(close))) +
  geom_point() +
  geom_vline(xintercept = c(750, 1500), color = "red")

##### ETH 1

eth_1_cf = as.formula(log(close) ~ log(total_fees) + 
                        log(google_trends) + log(sigma) |  log(gold) + log(vix) + 
                        total_supply + log(transfers_med) + log(sigma) +
                        log(total_addresses) + log(btc))

sub_eth_1 = analyze_subinterval("eth", 1, 750, eth_1_cf, fee_f)
dsummary(sub_eth_1$close_eq)
dsummary(sub_eth_1$fee_eq)

adf.test(sub_eth_1$close_eq$residuals)
adf.test(sub_eth_1$fee_eq$residuals)

kpss.test(sub_eth_1$close_eq$residuals)
kpss.test(sub_eth_1$fee_eq$residuals)

bptest(sub_eth_1$close_eq, data = sub_eth_1$data)
bptest(sub_eth_1$fee_eq, data = sub_eth_1$data)

dwtest(sub_eth_1$close_eq, data = sub_eth_1$data)
dwtest(sub_eth_1$fee_eq, data = sub_eth_1$data)

shapiro.test(sub_eth_1$close_eq$residuals)
shapiro.test(sub_eth_1$fee_eq$residuals)

coeftest(sub_eth_1$close_eq, vcov = vcovHAC(sub_eth_1$close_eq, type="HC3"))
coeftest(sub_eth_1$fee_eq, vcov = vcovHAC(sub_eth_1$fee_eq, type="HC3"))

##### ETH 2
eth_2_cf = as.formula(log(close) ~ log(total_fees) + log(sigma) + log(hash_rate_mean) +
                        log(google_trends) +  log(total_addresses)| log(sigma) + log(gold) + log(vix) + 
                        total_supply + log(transfers_med) + 
                        log(total_addresses))

eth_2_fee = as.formula(log(total_fees) ~ log(close) + log(sigma) + log(total_addresses)|
                         log(sp500) + log(gold) + log(vix) + 
                         total_supply + log(transfers_med) + log(sigma) +
                         log(total_addresses))

sub_eth_2 = analyze_subinterval("eth", 751, 1500, eth_2_cf, eth_2_fee)

dsummary(sub_eth_2$close_eq)
dsummary(sub_eth_2$fee_eq)

adf.test(sub_eth_2$close_eq$residuals)
adf.test(sub_eth_2$fee_eq$residuals)

kpss.test(sub_eth_2$close_eq$residuals)
kpss.test(sub_eth_2$fee_eq$residuals)

bptest(sub_eth_2$close_eq, data = sub_eth_2$data)
bptest(sub_eth_2$fee_eq, data = sub_eth_2$data)

dwtest(sub_eth_2$close_eq, data = sub_eth_2$data)
dwtest(sub_eth_2$fee_eq, data = sub_eth_2$data)

shapiro.test(sub_eth_2$close_eq$residuals)
shapiro.test(sub_eth_2$fee_eq$residuals)

coeftest(sub_eth_2$close_eq, vcov = vcovHAC(sub_eth_2$close_eq, type="HC3"))
coeftest(sub_eth_2$fee_eq, vcov = vcovHAC(sub_eth_2$fee_eq, type="HC3"))


##### ETH 3 

eth_3_cf = as.formula(log(close) ~ log(total_fees) + log(hash_rate_mean) + 
                      log(sigma) + log(sp500) +
                      log(btc)| log(sp500) + log(gold) + log(vix) + 
                      total_supply + log(transfers_med) + log(sigma) +
                      log(total_addresses) + log(btc))

sub_eth_3 = analyze_subinterval("eth", 1501, nrow(eth_data), eth_3_cf, fee_f)
dsummary(sub_eth_3$close_eq)
dsummary(sub_eth_3$fee_eq)

##R2 coef
cor(predict(sub_eth_3$fee_eq), sub_eth_3$data$total_fees)^2

adf.test(sub_eth_3$close_eq$residuals)
adf.test(sub_eth_3$fee_eq$residuals)

kpss.test(sub_eth_3$close_eq$residuals)
kpss.test(sub_eth_3$fee_eq$residuals)



bptest(sub_eth_3$close_eq, data = sub_eth_3$data)
bptest(sub_eth_3$fee_eq, data = sub_eth_3$data)

dwtest(sub_eth_3$close_eq, data = sub_eth_3$data)
dwtest(sub_eth_3$fee_eq, data = sub_eth_3$data)

shapiro.test(sub_eth_3$close_eq$residuals)
shapiro.test(sub_eth_3$fee_eq$residuals)

coeftest(sub_eth_3$close_eq, vcov = vcovHAC(sub_eth_3$close_eq, type="HC3"))
coeftest(sub_eth_3$fee_eq, vcov = vcovHAC(sub_eth_3$fee_eq, type="HC3"))


############ cryptoasset Market intervals characteristics
log_ret = function(price){
  log_diff = log(price[2:length(price)] / price[1:length(price)-1])
  return(log_diff)
}
## 1) 1.1.2016 - 21.1.2018
btc_data$close[750] / btc_data$close[1]
eth_data$close[750] / eth_data$close[1]
mean(btc_data$sigma[1:750])
mean(eth_data$sigma[1:750])

## 2) 22.1.2018 - 10.2.2020
btc_data$close[1500] / btc_data$close[751]
eth_data$close[1500] / eth_data$close[751]
mean(btc_data$sigma[751:1500])
mean(eth_data$sigma[751:1500])

## 3) 11.2.2020 - 1.9.2022
btc_data$close[dim(btc_data)[1]] / btc_data$close[1501]
eth_data$close[dim(btc_data)[1]] / eth_data$close[1501]
mean(btc_data$sigma[751:dim(btc_data)[1]])
mean(eth_data$sigma[751:dim(btc_data)[1]])


### Mean google trend effect accross intervals
mean(btc_data$google_trends[1:750])
mean(btc_data$google_trends[751:1500])
mean(btc_data$google_trends[1501:dim(btc_data)[1]])

#sd
sd(btc_data$google_trends[1:750])
sd(btc_data$google_trends[751:1500])
sd(btc_data$google_trends[1501:dim(btc_data)[1]])

#max
max(btc_data$google_trends[1:750])
max(btc_data$google_trends[751:1500])
max(btc_data$google_trends[1501:dim(btc_data)[1]])
plot(btc_data$google_trends)
