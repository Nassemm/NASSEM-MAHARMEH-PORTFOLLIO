library(stargazer)
library(ggplot2)
library(car)
library(lubridate)
library(lmtest)
library(modelsummary)

#73.82217

remove(list=ls())
setwd('/Users/nassemmaharmeh/Documents')
gas <- read.csv("/Users/nassemmaharmeh/Documents/gasdata1.csv")
head(gas)
sink('project1_dataverify_NassemMaharmeh.txt')

dim(gas)
str(gas)
head(gas)
tail(gas)
stargazer(gas,type = 'text')

obs = 607
start = "1975-01-01"
gas$date = seq(as.Date(start), by = "months", length.out = obs)
head(gas)
tail(gas)

pce2000_avg <- gas %>%
  filter(year(xdate) == 2000) %>%
  summarize(avg = mean(pce2017, na.rm = TRUE)) %>%
  pull(avg)

stopifnot(is.finite(pce2000_avg), pce2000_avg > 0)
gas <- gas %>%
  mutate(
    pce00  = pce2017 / pce2000_avg,      # NEW deflator: avg(2000)=1.0
    # 2) Per-capita gallons
    qpc    = Quantgas / pop,             # NOTE: if pop is in 1,000s and Quantgas in 1,000s, the ratio is fine.
    # 3) Real price of gasoline (2000$)
    rpgas  = Pricegas / pce00,
    # 4) Real disposable income per capita (2000$)
    rdpipc = Disp_inc / pce00
  )

# Helper: safe log (handles non-positive gracefully)
safe_log <- function(x) ifelse(x > 0, log(x), NA_real_)

# 5) Logs required
gas <- gas %>%
  mutate(
    mon       = month(xdate),
    ln_qpc    = safe_log(qpc),
    ln_rpgas  = safe_log(rpgas),
    ln_rdpipc = safe_log(rdpipc),
    # convert % to decimals before logs
    unemp_dec = unrate / 100,
    i1_dec    = tb1yr / 100,
    i10_dec   = tb10yr / 100,
    infl_dec  = inflation_p / 100,
    ln_unemp  = safe_log(unemp_dec),
    ln_i1     = safe_log(i1_dec),
    ln_i10    = safe_log(i10_dec),
    ln_infl   = safe_log(infl_dec)
  )

# ============== Subperiods ==============
gas1 <- subset(gas, xdate >= as.Date("1975-11-01") & xdate <= as.Date("1980-11-01"))
gas2 <- subset(gas, xdate >= as.Date("2001-03-01") & xdate <= as.Date("2006-03-01"))
gas3 <- subset(gas, xdate >= as.Date("2015-02-01") & xdate <= as.Date("2020-02-01"))
gas4 <- subset(gas, xdate >= as.Date("2020-07-01") & xdate <= as.Date("2025-07-01"))

# ============== Table 1 model ==============
# Log per-capita consumption on log real price, log real income pc, and logs of
# unemployment, short rate (1y), long rate (10y), and inflation + month FE.
FORMULA <- ln_qpc ~ ln_rpgas + ln_rdpipc + ln_unemp + ln_i1 + ln_i10 + ln_infl + factor(mon)

m1 <- lm(FORMULA, data = gas1)
m2 <- lm(FORMULA, data = gas2)
m3 <- lm(FORMULA, data = gas3)
m4 <- lm(FORMULA, data = gas4)

models <- list(
  "Nov 1975 – Nov 1980" = m1,
  "Mar 2001 – Mar 2006" = m2,
  "Feb 2015 – Feb 2020" = m3,
  "Jul 2020 – Jul 2025" = m4
)

# ============== Durbin–Watson (order 1) ==============
dw_stat <- sapply(models, function(mod) unname(dwtest(mod)$statistic[["DW"]]))
dw_pval <- sapply(models, function(mod) unname(dwtest(mod)$p.value))

extra_rows <- data.frame(
  term = c("Durbin–Watson", "DW p-value"),
  t(rbind(dw_stat, dw_pval)),
  check.names = FALSE
)
names(extra_rows)[-1] <- names(models)

# ============== Print nicely ==============
msummary(
  models,
  stars = TRUE,
  fmt = 3,
  gof_omit = "IC|Log|F$",        # optional
  add_rows = extra_rows,
  coef_omit = "^factor\\(mon\\)" # hide month dummies (optional)
)

gas1 = subset(gas, date >='1975-11-01' & date <='1980-11-01')
gas2 = subset(gas, date >='2001-03-01' & date <='2006-03-01')
gas3 = subset(gas, date >='2015-02-01' & date <='2020-02-01')
gas4 = subset(gas, date >='2020-07-01' & date <= '2025-07-01')

stargazer(gas1, type = 'text', title = "Nov 1975 - Nov 1980")
stargazer(gas2, type = 'text', title = "Mar 2001 - Mar 2006")
stargazer(gas3, type = 'text', title = "Feb 2015 - Feb 2020")
stargazer(gas4, type = 'text', title = "Jul 2020 - Jul 2025")
gas$pce00 = gas$pce/94.6
gas$rpgas = gas$pgas/gas$pce00
gas$lnrp = log(gas$rpgas)


library(lubridate)
gas$mon = month(gas$date)

lm(y ~ x + as.factor(month), data = gas)
gas$d1 = ifelse(gas$mon==1,1,0)
gas$d2 = ifelse(gas$mon==2,1,0)
gas$d3 = ifelse(gas$mon==3,1,0)
gas$d4 = ifelse(gas$mon==4,1,0)
gas$d5 = ifelse(gas$mon==5,1,0)
gas$d6 = ifelse(gas$mon==6,1,0)
gas$d7 = ifelse(gas$mon==7,1,0)
gas$d8 = ifelse(gas$mon==8,1,0)
gas$d9 = ifelse(gas$mon==9,1,0)
gas$d10 = ifelse(gas$mon==10,1,0)
gas$d11 = ifelse(gas$mon==11,1,0)

lm(y~x + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11, data = gas)


gas1 = subset(gas, date>="1975-11-01" & date <= "1980-11-01")
gas2 = subset(gas, date>="2001-03-01" & date <= "2006-03-01")
gas3 = subset(gas, date>="2015-02-01" & date <= "2020-02-01")
gas4 = subset(gas, date>="2020-07-01" & date <= "2025-07-01")


myvars=c('var1','var2','var3','var4')
stargazer(gas1[myvars], type='text')
stargazer(gas2[myvars], type='text')
stargazer(gas3[myvars], type='text')
stargazer(gas4[myvars], type='text')

par(mar = c(0.5,4,4,4))
plot(gas$date,gas$rpgas, Main = 'Real price and gas consumption',
    ylim = c(0,5), xlab ='Date', ylab ='rpgas', type='1', col= 2)
par(new = TRUE)
#graph1
plot(gas$date,gas$qpc, type='1', col = 3, axes = FALSE, xlab ='', ylab = '')
axis(side = 4, at = pretty(range(gas$qpc)))
mtext('consumption', side = 4, line = 2)
legend('topleft',
       c('real price', 'PC consumption'), lty= 1, col = 2.3)
#graph2
par(mar = c(0.5,3,3,3))
plot(gas$date,gas$rdpipc, main = 'Real disposable income Pc',
     xlab= 'Date',ylab = '"real income PC', type ='1', col = 2)

#graph3
gas1$obsn = 1:nrow(gas1)
gas2$obsn = 1:nrow(gas2)
gas3$obsn = 1:nrow(gas3)
gas4$obsn = 1:nrow(gas4)

ggplot(NULL, aes(obsn, rpgas)) + 
  geom_line(data = gas1, aes(color = 'red')) +
  geom_line(data = gas2, aes(color = 'blue')) +
  geom_line(data = gas3, aes(color = 'black')) +
  geom_line(data = gas4, aes(color = 'green')) +
  scale_color_identity(name = 'subperiods',
                       breaks = c('red', 'blue', 'black', 'green'),
                       labels = c("Nov1975-Nov1980", 'Mar2001-Mar2006',
                                  "Fev2015 - Feb2020", 'Jul2020 - Jul2025'),
                       guide = 'legend')


  









sink()