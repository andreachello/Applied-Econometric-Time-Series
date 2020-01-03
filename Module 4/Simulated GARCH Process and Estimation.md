---
title: "Simulated GARCH Process and Estimation"
output: html_notebook
---

It is not often possible for GARCH proceeses to fit real world data in a perfect way. Therefore, it is important to test the estimation methods on simulated data (fictitious data constructed solely for the purpose of fitting our model of interest).

*Method used:* garchSim method from the fGarch package to create the data from our model.

**AR(3)-GARCH(2,2)**

$y_t = 0.5 y_{t-1} + 0.2y_{t-2} - 0.1y_{t-3} + u_t$

$u_t = \epsilon_t\sqrt{h_t}$

$\epsilon_t \sim N(0,1)$

$h_t = 0.001 + 0.3u^2_{t-1} + 0.2u^2_{t-2} + 0.2h_{t-1} + 0.1h_{t-2}$

```{r}
library(fGarch)
library(forecast)
# generate simulated process
spec = garchSpec(model = list(omega = 0.001, ar = c(0.5, 0.2, -0.1),
alpha = c(0.3, 0.2),
beta = c(0.2, 0.1)))
process = garchSim(spec , n = 500)
plot(process)

```

**Main Findings:** 
There is persistence in the series although it is not trending not wandering arbitrarily.
There are periods of high volatilty followed by periods of tranquillity.

**Autocorrelation Function:**

```{r}
acf(process)
acf(process^2)

```

Both show significant auto-correlations for several lags although the squared process has less persistence than the real world model has.

# Fitting Models

```{r}
model1 <- garchFit(formula ~ garch(3,0), data = process, trace = FALSE)
summary(model1)
```

While this model is sufficient to capture the time series properties of the squared residuals (GARCH effects), the levels of the residuals are still auto-correlated. The residuals test as normal.

## Fitting the Correct Model (AR(3)-GARCH(2,2))

```{r}
model2 <- garchFit(formula ~ arma(3,0) + garch(2,2), data = process, trace = F)
summary(model2)
```

In this case all diagnostic tests look good, i.e. normal and without autocorrelation in either level or square.

The **AR** coefficients are significant and almost within two standard deviations of their true values.

**GARCH** coefficients **ALPHAS** are significant but statistically significantly different from their true values.

**GARCH** coefficients **BETAS** are nowhere near significant.

This often happens in situations where there are both AR and MA effects in either the conditional expectation or condition variance. Maybe a simpler model is able to capture the same time series properties more parsimoniously. There may exist many different but equivalent representations of the same process.

Fitting an **AR(3) - ARCH(2)** model yields the best results as can be seen below.

```{r}
model3 <- garchFit(formula ~ arma(3,0) + garch(2,0), data = process, trace = F)
summary(model3)
```
