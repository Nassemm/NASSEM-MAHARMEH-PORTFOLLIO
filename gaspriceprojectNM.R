############################################################
#  ECON Project 1 – Gasoline Demand (Full Code)
#  Author: Nassem Maharmeh
############################################################

## --- Libraries ---
library(dplyr)
library(lubridate)
library(stargazer)
library(lmtest)
library(ggplot2)

## --- Load data ---
setwd('/Users/nassemmaharmeh/Documents')
gas <- read.csv("gasdata1.csv", stringsAsFactors = FALSE)
gas$xdate <- as.Date(gas$xdate)
gas$date  <- gas$xdate

## --- Rebase PCE to 2000 = 1 ---
pce2000_avg <- gas %>%
  filter(year(xdate) == 2000) %>%
  summarise(avg = mean(pce2017, na.rm = TRUE)) %>% pull(avg)
gas <- gas %>%
  mutate(pce00  = pce2017 / pce2000_avg,
         qpc    = Quantgas / pop,
         rpgas  = Pricegas / pce00,
         rdpipc = Disp_inc / pce00)

## --- Logs and monthly dummy base ---
safe_log <- function(x) ifelse(x > 0, log(x), NA_real_)
gas <- gas %>%
  mutate(mon       = month(xdate),
         ln_qpc    = safe_log(qpc),
         ln_rpgas  = safe_log(rpgas),
         ln_rdpipc = safe_log(rdpipc),
         ln_unemp  = safe_log(unrate/100),
         ln_i1     = safe_log(tb1yr/100),
         ln_i10    = safe_log(tb10yr/100),
         ln_infl   = safe_log(inflation_p/100))

## --- Sub-periods ---
gas1 <- subset(gas, xdate >= "1975-11-01" & xdate <= "1980-11-01")
gas2 <- subset(gas, xdate >= "2001-03-01" & xdate <= "2006-03-01")
gas3 <- subset(gas, xdate >= "2015-02-01" & xdate <= "2020-02-01")
gas4 <- subset(gas, xdate >= "2020-07-01" & xdate <= "2025-07-01")

## =========================================================
## ==============   TABLE 1 : BASE MODEL   =================
## =========================================================
FORM_T1 <- ln_qpc ~ ln_rpgas + ln_rdpipc + ln_unemp +
  ln_i1 + ln_i10 + ln_infl + factor(mon)

m1 <- lm(FORM_T1, data = gas1)
m2 <- lm(FORM_T1, data = gas2)
m3 <- lm(FORM_T1, data = gas3)
m4 <- lm(FORM_T1, data = gas4)

dw1 <- dwtest(m1); dw2 <- dwtest(m2); dw3 <- dwtest(m3); dw4 <- dwtest(m4)

stargazer(m1, m2, m3, m4, type="text",
          title="Table 1: Gasoline Demand Models (Four Periods)",
          column.labels=c("Nov75-Nov80","Mar01-Mar06",
                          "Feb15-Feb20","Jul20-Jul25"),
          dep.var.labels="ln(Qpc)",
          covariate.labels=c("ln(Real Price)","ln(Real Income PC)",
                             "ln(Unemployment)","ln(1-yr Rate)",
                             "ln(10-yr Rate)","ln(Inflation)"),
          omit="factor\\(mon\\)", omit.labels="Monthly FE",
          omit.stat=c("f","ser","adj.rsq"), digits=3)
cat("\nDurbin–Watson Statistics (order 1):\n")
cat(sprintf("P1 %.3f (p=%.3f)\n", unname(dw1$statistic), dw1$p.value))
cat(sprintf("P2 %.3f (p=%.3f)\n", unname(dw2$statistic), dw2$p.value))
cat(sprintf("P3 %.3f (p=%.3f)\n", unname(dw3$statistic), dw3$p.value))
cat(sprintf("P4 %.3f (p=%.3f)\n", unname(dw4$statistic), dw4$p.value))

## =========================================================
## ==============   DESCRIPTIVE STATISTICS   ===============
## =========================================================
vars <- c("qpc","rpgas","rdpipc","unrate","tb1yr","inflation_p")

d1 <- as.data.frame(gas1[, vars])
d2 <- as.data.frame(gas2[, vars])
d3 <- as.data.frame(gas3[, vars])
d4 <- as.data.frame(gas4[, vars])

stargazer(d1, type="text", title="Descriptive Stats 1975–80")
stargazer(d2, type="text", title="Descriptive Stats 2001–06")
stargazer(d3, type="text", title="Descriptive Stats 2015–20")
stargazer(d4, type="text", title="Descriptive Stats 2020–25")

## =========================================================
## ==============   GRAPHS (FIG 1–3)   =====================
## =========================================================
par(mar=c(4,4,2,4))
plot(gas$date, gas$rpgas, type="l", col=2,
     main="Real Price (left) & Per-Capita Consumption (right)",
     xlab="Date", ylab="Real Price (2000$)")
par(new=TRUE)
plot(gas$date, gas$qpc, type="l", col=3, axes=FALSE, xlab="", ylab="")
axis(4, at=pretty(range(gas$qpc, na.rm=TRUE)))
mtext("Consumption (gal per cap)", side=4, line=2)
legend("topleft", legend=c("Real Price","Consumption"),
       col=c(2,3), lty=1, bty="n")

par(mar=c(4,4,2,2))
plot(gas$date, gas$rdpipc, type="l", col=2,
     main="Real Disposable Income Per Capita (2000$)",
     xlab="Date", ylab="Dollars (2000$)")

gas1$obsn<-seq_len(nrow(gas1)); gas2$obsn<-seq_len(nrow(gas2))
gas3$obsn<-seq_len(nrow(gas3)); gas4$obsn<-seq_len(nrow(gas4))
ggplot(NULL,aes(obsn,rpgas))+
  geom_line(data=gas1,aes(color="red"))+
  geom_line(data=gas2,aes(color="blue"))+
  geom_line(data=gas3,aes(color="black"))+
  geom_line(data=gas4,aes(color="green"))+
  scale_color_identity(name="Subperiods",
                       breaks=c("red","blue","black","green"),
                       labels=c("Nov75-Nov80","Mar01-Mar06","Feb15-Feb20","Jul20-Jul25"),
                       guide="legend")+
  labs(title="Real Gasoline Price by Subperiod",
       x="Within-period month index",y="Real price (2000$)")

## =========================================================
## ==============   TABLE 4 (W MACROS) + F-TESTS   =========
## =========================================================
FORM_D <- ln_qpc ~ ln_rpgas + ln_rdpipc + factor(mon)
FORM_E <- ln_qpc ~ ln_rpgas + ln_rdpipc +
  ln_unemp + ln_i1 + ln_infl + factor(mon)

m1_D <- lm(FORM_D, data=gas1); m1_E <- lm(FORM_E, data=gas1)
m3_D <- lm(FORM_D, data=gas3); m3_E <- lm(FORM_E, data=gas3)
m4_D <- lm(FORM_D, data=gas4); m4_E <- lm(FORM_E, data=gas4)

stargazer(m1_D,m1_E,m3_D,m3_E,m4_D,m4_E,
          type="text",
          title="Table 4: Models With/Without Macro Variables",
          column.labels=c("75-80 Base","75-80 Macro",
                          "15-20 Base","15-20 Macro",
                          "20-25 Base","20-25 Macro"),
          omit="factor\\(mon\\)", omit.labels="Monthly FE",
          omit.stat=c("f","ser","adj.rsq"), digits=3)

cat("\nF-tests for joint significance of macro vars:\n")
print(anova(m1_D,m1_E))
print(anova(m3_D,m3_E))
print(anova(m4_D,m4_E))

## ====== PREDICTION ($0.25 TAX) — FIXED ======
base4 <- gas4 %>% arrange(xdate) %>% slice_tail(n = 1)

# Apply $0.25 to the nominal price, then rebuild real price and income
new_nom_price <- base4$Pricegas + 0.25
new_rpgas     <- new_nom_price / base4$pce00
new_rdpipc    <- base4$rdpipc * 1.014  # +1.4%

# Make sure 'mon' in newdata has the same factor levels as in the training data
mon_levels <- sort(unique(gas4$mon))

newx <- data.frame(
  ln_rpgas = log(new_rpgas),
  ln_rdpipc = log(new_rdpipc),
  ln_unemp = log(base4$unrate/100),
  ln_i1    = log(base4$tb1yr/100),
  ln_infl  = log(ifelse(base4$inflation_p <= 0, 0.001, base4$inflation_p)/100),
  mon      = factor(base4$mon, levels = mon_levels)
)

# Predict for the two Period-4 models (with and without macro vars)
# For the base (no-macro) model, only pass the needed columns:
newx_D <- newx[, c("ln_rpgas","ln_rdpipc","mon")]
pred_D <- predict(m4_D, newdata = newx_D, interval = "prediction", se.fit = TRUE)
pred_E <- predict(m4_E, newdata = newx,   interval = "prediction", se.fit = TRUE)

# pred_* are lists; the fitted values & intervals are in $fit (a matrix: fit/lwr/upr)
pred_D_ln <- as.data.frame(pred_D$fit)  # columns: fit, lwr, upr
pred_E_ln <- as.data.frame(pred_E$fit)

# Convert ln(qpc) back to levels
pred_D_lvl <- exp(pred_D_ln)
pred_E_lvl <- exp(pred_E_ln)

cat("\nPredicted ln(qpc) under $0.25 tax (Period 4):\n")
print(round(pred_D_ln, 3))
print(round(pred_E_ln, 3))

cat("\nPredicted qpc (levels):\n")
print(round(pred_D_lvl, 3))
print(round(pred_E_lvl, 3))

cat("\n--- End of Project 1 Script ---\n")


