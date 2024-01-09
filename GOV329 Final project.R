library(readr)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(caret)
library(tidyverse)
require(MASS)
library(factoextra)
query_2 <- read_csv("Desktop/2023:2024 Wesleyan/GOV329/query_2.csv")
query_2 <- query_2[query_2$year >= 1975, ]
query_2cleaned <- na.omit(query_2[, c("country", "year", "system_DPI", "yrsoffc_DPI","finittrm_DPI","yrcurnt_DPI","multpl_DPI","prtyin_DPI","tensys_strict_DPI", "edu_gdp_ES", "gdp_WDI_PW" )])

summary(query_2cleaned$system_DPI)
summary(query_2cleaned$edu_gdp_ES)

##system_DPI -> political electoral system in a given nation (main IV)
          # 0 is Presidential, 1 is Assembly-elected president, 2 is parliamentary
##edu_gdp_ES -> government expenditure on education as % of GDP (%) (main DV)
#yrsoffc_DPI -> Chief Executive Years in Office
#finittrm_DPI -> Finite Term in Office (1 if yes, O if no)
#yrcurnt_DPI -> Years Left in Current Term 
#multpl_DPI -> Can Chief Executive Serve Multiple Terms (1 = yes, 0 = no)
#prtyin_DPI -> Party of Chief Executive Length of Time in Office
#tensys_strict_DPI -> How long has the country been autocratic or democratic, respectively?
#gdp_WDI_PW -> GDP (constant 2005 US$)

test.aov <-aov(edu_gdp_ES ~ system_DPI, data = query_2cleaned)
summary(test.aov) # P value of <2e-16, significant.

query_2cleaned <- query_2cleaned %>%
  mutate_all(~replace(., . == -999, NA))

query_2cleaned <- na.omit(query_2cleaned)

model1<- lm(edu_gdp_ES ~ system_DPI, data = query_2cleaned)
summary(model1) #significant (p-value of <2e-16), AIC: 10,584

model2<- lm(edu_gdp_ES ~ system_DPI + yrsoffc_DPI + finittrm_DPI + yrcurnt_DPI + multpl_DPI + prtyin_DPI + tensys_strict_DPI + gdp_WDI_PW, data = query_2cleaned)
summary(model2) #most vars significant (except finitterm_DPI, prtyin_DPI), AIC: 10,363

model1AIC <- AIC(model1) #10,586 - super high, not good
model2AIC <- AIC(model2) #10,362 - Better, but still, not that good

modelling<- tab_model(model1, model2, dv.labels = c('model1', 'model2'), show.aic = TRUE)
modelling


#Univariate Plot: Average Education Expenditure as % of GDP across time
ggplot(query_2cleaned, aes(x = year, y = edu_gdp_ES)) +
  geom_bar(na.rm = TRUE, stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Average Education Expenditure Across Time", x = "Year ", y = "Education Expenditure as % of GDP")


#Univariate plot: Distribution of Electoral System Across Time
query_2cleaned$system_DPI <- as.factor(query_2cleaned$system_DPI)
ggplot(data = query_2cleaned, aes(x = year, fill = system_DPI)) +geom_bar(na.rm = TRUE) + scale_fill_discrete(name = "Electoral System", labels = c("Presidential", "Parliamentary-elected President", "Parliamentary")) +
  labs(title = "Distribution of Electoral System Across Time", x = "Year", y = "Count", fill = "Electoral System")




univariategraph<- ggplot(data = query_2cleaned, aes(x = edu_gdp_ES, fill = as.factor(system_DPI))) +
  geom_boxplot(na.rm = TRUE) + scale_fill_discrete(name = "Electoral System", labels = c("Presidential", "Parliamentary-elected President", "Parliamentary")) +
  labs(title = "Univariate Box Plot", x = "Education Expenditure (% of GDP)", y = "Electoral System", fill = "Electoral System")

univariategraph + 
  theme(axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank())



