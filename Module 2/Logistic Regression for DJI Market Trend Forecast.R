

# ===================================================================
# -------------------------------------------------------------------

#                   Dow Jones Index(DJI) Trend

#                 From WQU Econometrics: Module 2

# -------------------------------------------------------------------
# ===================================================================

## Is predicted using logistic regression 

"
-----------------------------------------------
Algorithm:
-----------------------------------------------

1. DJI data is extracted from Yahoo Finance

2. Different indicators are calculated:
  
  - Moving Average
  - Standard Deviation
  - Relative Strength Index (RSI)
  - MACD
  - Bollinger Band

3. Create variable direction:

  - Up(1) or down(0) [modelled as a logistic regression so
    dependednt variable Y is 1 if the markets are up and 0
    if the markets are down]

  - Current price > 20 days previous price --> Up Direction

  - Current price < 20 days previous price --> Down Direction

4. Data is divided in two parts:

  - In-sample data -> Model Building Process

  - Out-sample data -> Evaluation

  - In-sample and out-sample start and end dates are indicated

5. Data is standardized in order rto avoid the higher scaled
   variables which have a higher impact on the results

  - Standardized data = (X - mean) / Std(X)

  - Mean and SD is calculated for each column

6. Logistic Regression is implemented"

install.packages("quantmod")
library(quantmod)

# ---------------------------------------------

# 1. DJI data is extracted from Yahoo Finance

# ---------------------------------------------

# Get the data from Yahoo! for DJI
getSymbols("^DJI", src = "yahoo")

dow_jones <- DJI[,"DJI.Close"]


# ---------------------------------------------

# 2. Different indicators are calculated

# ---------------------------------------------

# Moving average calculations

## Moving average 10
average_10 <- rollapply(dow_jones, 10, mean)

## Moving average 20
average_20 <- rollapply(dow_jones, 20, mean)

# Standard Deviation calculations

std_10 <- rollapply(dow_jones, 10, sd)
std_20 <- rollapply(dow_jones, 20, sd)

# Relative Strength Index calculations

rsi5 <- RSI(dow_jones, 5, "SMA")
rsi14 <- RSI(dow_jones, 14, "SMA")

# MACD calculations

macd12269 <- MACD(dow_jones, 12, 26, 9, "SMA")
macd7205 <- MACD(dow_jones, 7, 20, 5, "SMA")

# Bollinger Bands calculations

bollinger_bands <- BBands(dow_jones, 20, "SMA", 20)

# ---------------------------------------------

# 3. Create variable direction

# ---------------------------------------------

## No initial direction (initialising the direction)
direction <- NULL

## Up and down direction (compare DJI with its lagged 20 day price)
### replace all up comparisons with a 1, down with a 0

direction[dow_jones > lag(dow_jones, 20)] <- 1

direction[dow_jones < lag(dow_jones, 20)] <- 0

## Bind all the variables into same dataframe

dow_jones <- cbind(dow_jones, average_10, average_20, std_10, std_20, 
                   rsi5, rsi14, macd12269, macd7205, bollinger_bands, 
                   direction)

dimension <- dim(dow_jones)

# ---------------------------------------------

# 4. Divide data into two parts

# ---------------------------------------------

# In-sample dates

is_start_date <- "2010-01-01" #Initial date
is_end_date <- "2014-12-31" #End date

# Out-sample dates

os_start_date <- "2015-01-01"
os_end_date <- "2015-12-1-31"

# Get the index (dates) of rows between the in sample start and end dates
insample_row <- which(index(dow_jones) >= is_start_date & index(dow_jones) <= is_end_date)

outsample_row <- which(index(dow_jones) >= os_start_date & index(dow_jones) <= os_end_date)

# Filter the dji by insample and outsample indices

insample_dji <- dow_jones[insample_row, ]
outsample_dji <- dow_jones[outsample_row, ]

# ---------------------------------------------

# 5. Standardize the data

# ---------------------------------------------

# Mean of the insample data
insample_mean <- apply(insample_dji,2,mean)

# SD of the insample data
insample_std <- apply(insample_dji, 2, sd)

## Create a matrix of 1s from the insample data
## Dimensions are (rxc) so [1] = row and [2] = col

insample_identity <- matrix(1, dim(insample_dji)[1], dim(insample_dji)[2])

# Normalise: z_i = (x_i - mean_i) / (sd_i)
"since:

x = [       ]    mean(col vector) = [   ]
    [       ]                       [   ]
    [       ]                       [   ]  
    
   1258 x 16                        1258 x 1  


isme = [              ]

            1 x 16
            
isidn = [       ]   t(isidn) = 16 x 1258
        [       ]   isme * t(isidn) = 1 x 1258
        [       ]   t(isme*t(isidn)) = 1258 x 1
        
        1258 x 16
"

normalised_insample_dji <- (insample_dji - t(insample_mean*t(insample_identity)) /
                             t(insample_std*t(insample_identity)))
dm <- dim(insample_dji)

# replace the normalised directions with the old directions
## directions[in the range of the insample dates index]
normalised_insample_dji[, dm[2]] <- direction[insample_row]

# ---------------------------------------------

# 5. Apply Logistic Regression

# ---------------------------------------------

formula <- paste("direction ~ .", sep = "")

model <- glm(direction ~ ., family = "binomial", normalised_insample_dji)

plot(model)
