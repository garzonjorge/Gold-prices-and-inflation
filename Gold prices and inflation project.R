install.packages("readxl")
library("readxl")
library("dplyr")

##reading the required file

df <- read_excel(file.choose())

str(df)

###Taking the natural log

df <- df %>%
  mutate(`logGold Price US dollar (USD)` = log(`Gold Price US dollar (USD)`, base = exp(1)),
         `logCPI (inflation)` = log(`CPI (inflation)`, base = exp(1)),
         `logDollar_Index (USD)` = log(`Dollar_Index (USD)`, base = exp(1)),
         `logtotal_unemployment_rate` = log(`total_unemployment_rate`, base = exp(1)),
         `logClose S&P 500 (USD)` = log(`Close S&P 500 (USD)`, base = exp(1)))


##bivariate analysis gold vs inflation

model_3 <- lm(`logGold Price US dollar (USD)` ~ `logCPI (inflation)`,df)

summary(model_3)

##model 4 controlling for all the other variables

model_4 <- lm(`logGold Price US dollar (USD)` ~ `logCPI (inflation)` + 
                `logDollar_Index (USD)` + 
                `logtotal_unemployment_rate` +
                `logClose S&P 500 (USD)`,df)

summary(model_4)

###model 5 controlling for all the other variables including the interaction term between inflation and S&P 500

model_5 <- lm(`logGold Price US dollar (USD)` ~ `logCPI (inflation)` + 
                `logDollar_Index (USD)` + 
                `logtotal_unemployment_rate` +
                `logClose S&P 500 (USD)`  +
                `logCPI (inflation)`*`logClose S&P 500 (USD)` 
              ,df)

summary(model_5)


