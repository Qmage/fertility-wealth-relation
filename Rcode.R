library(xlsx)
library(reshape2)
library(dplyr)
library(ggplot2)

gdp <- read.xlsx("Consolidated.xlsx", sheetName = "GDP Per Capita") 
birth1000 <- read.xlsx("Consolidated.xlsx", sheetName = "Birth Per 1000 Per Year")
birthwoman <- read.xlsx("Consolidated.xlsx", sheetName = "Birth Per Woman")
countries <- read.xlsx("Consolidated.xlsx", sheetName = "Countries Metadata")

#remove unused columns
gdp$Indicator.Name <- NULL
gdp$Indicator.Code <- NULL
birth1000$Country.Name <- NULL
birth1000$Indicator.Name <- NULL
birth1000$Indicator.Code <- NULL
birthwoman$Country.Name <- NULL
birthwoman$Indicator.Name <- NULL
birthwoman$Indicator.Code <- NULL


gdp_clean <- melt(gdp, 
            id.vars = c("Country.Name", "Country.Code"),
            variable.name = "Year", 
            value.name = "GDP.Per.Capita")

birth1000_clean <- melt(birth1000, 
                  id.vars = c("Country.Code"),
                  variable.name = "Year", 
                  value.name = "Birth.Per.1000.Per.Year")

birthwoman_clean <- melt(birthwoman, 
                        id.vars = c("Country.Code"),
                        variable.name = "Year", 
                        value.name = "Birth.Per.Woman")


gdp_clean$Year <- gsub("X", "", gdp_clean$Year)
birth1000_clean$Year <- gsub("X", "", birth1000_clean$Year)
birthwoman_clean$Year <- gsub("X", "", birthwoman_clean$Year)

gdp_clean$Year <- as.numeric(gdp_clean$Year)
birth1000_clean$Year <- as.numeric(birth1000_clean$Year)
birthwoman_clean$Year <- as.numeric(birthwoman_clean$Year)

gdp_clean <- gdp_clean[!is.na(gdp_clean$GDP.Per.Capita), ]
birth1000_clean <- birth1000_clean[!is.na(birth1000_clean$Birth.Per.1000.Per.Year), ]
birthwoman_clean <- birthwoman_clean[!is.na(birthwoman_clean$Birth.Per.Woman), ]

max_gdp_year <- gdp_clean %>%
  group_by(Country.Name, Country.Code) %>%
  select(Year) %>%
  summarise(
    Year = max(Year)
  )
max_birth1000_year <- birth1000_clean %>%
  group_by(Country.Code) %>%
  select(Year) %>%
  summarise(
    Year = max(Year)
  )
max_birthwoman_year <- birthwoman_clean %>%
  group_by(Country.Code) %>%
  select(Year) %>%
  summarise(
    Year = max(Year)
  )

latest_gdp = merge(x = gdp_clean, y = max_gdp_year, by = c("Country.Name", "Country.Code", "Year"))
latest_gdp <- rename(latest_gdp,GDP.Year=Year)
latest_birth1000 = merge(x = birth1000_clean, y = max_birth1000_year, by = c("Country.Code", "Year"))
latest_birth1000 <- rename(latest_birth1000,birth1000.Year=Year)
latest_birthwoman = merge(x = birthwoman_clean, y = max_birthwoman_year, by = c("Country.Code", "Year"))
latest_birthwoman <- rename(latest_birthwoman,birthwoman.Year=Year)


Final_DF <- merge(x = latest_gdp, 
                   y = countries[,c('Country.Code','Region','IncomeGroup')], 
                   by = c("Country.Code"), all.x = TRUE)
Final_DF <- merge(x = Final_DF, 
                 y = latest_birth1000, 
                 by = c("Country.Code"), all.x = TRUE)
Final_DF <- merge(x = Final_DF, 
                 y = latest_birthwoman, 
                 by = c("Country.Code"), all.x = TRUE)
Final_DF$IncomeGroup <- factor(Final_DF$IncomeGroup,
                               levels=c('Low income',
                                        'Lower middle income',
                                        'Upper middle income',
                                        'High income: nonOECD',
                                        'High income: OECD'))


lm_eqn <- function(y, x, df){
  m <- lm(y ~ x, df)
  eq <- substitute( italic(r)^2~"="~r2, list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
  }

ggplot(Final_DF, aes(x=GDP.Per.Capita, y=Birth.Per.1000.Per.Year)) +
  geom_point(shape=1) +
  geom_smooth() + labs(y = "Birth per 1000 per year", x = "GDP per capita (US Dollar $)") + 
  geom_text(x = 100000, y = 40, 
            label = lm_eqn(Final_DF$Birth.Per.1000.Per.Year, Final_DF$GDP.Per.Capita, Final_DF), 
            parse = TRUE)

ggplot(Final_DF, aes(x=log(GDP.Per.Capita), y=Birth.Per.1000.Per.Year)) +
  geom_point(shape=1) +
  geom_smooth() + labs(y = "Birth per 1000 per year", x = "Log of GDP per capita (US Dollar $)") + 
  geom_text(x = 10, y = 40, 
            label = lm_eqn(Final_DF$Birth.Per.1000.Per.Year, log(Final_DF$GDP.Per.Capita), Final_DF), 
            parse = TRUE)
  
  
ggplot(Final_DF, aes(x=GDP.Per.Capita, y=Birth.Per.Woman)) +
  geom_point(shape=1) +
  geom_smooth() + labs(y = "Birth per Woman", x = "GDP per capita (US Dollar $)") + 
  geom_text(x = 100000, y = 6, 
            label = lm_eqn(Final_DF$Birth.Per.Woman, log(Final_DF$GDP.Per.Capita), Final_DF), 
            parse = TRUE)

ggplot(Final_DF, aes(x=log(GDP.Per.Capita), y=Birth.Per.Woman)) +
  geom_point(shape=1) +
  geom_smooth() + labs(y = "Birth per Woman", x = "Log of GDP per capita (US Dollar $)") + 
  geom_text(x = 10, y = 6, 
            label = lm_eqn(Final_DF$Birth.Per.Woman, log(Final_DF$GDP.Per.Capita), Final_DF), 
            parse = TRUE)


ggplot(Final_DF[!is.na(Final_DF$IncomeGroup),], 
       aes(x=IncomeGroup, y=Birth.Per.1000.Per.Year, fill=IncomeGroup)) + 
  geom_boxplot() + labs(y = "Birth per 1000 per year", x = "Income Group") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  guides(fill=guide_legend(title="Income Group"))

ggplot(Final_DF[!is.na(Final_DF$IncomeGroup),], 
       aes(x=IncomeGroup, y=Birth.Per.Woman, fill=IncomeGroup)) + 
  geom_boxplot() + labs(y = "Birth per Woman", x = "Income Group") +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  guides(fill=guide_legend(title="Income Group"))

write.csv(Final_DF, file = "final_table.csv")

install.packages('Hmisc')
library(Hmisc)
final_matrix <- as.matrix(Final_DF[,c('GDP.Per.Capita',
                                      'Birth.Per.1000.Per.Year',
                                      'Birth.Per.Woman')])

rcorr(final_matrix, type="pearson")
rcorr(final_matrix, type="spearman")

cor.test(Final_DF$GDP.Per.Capita,
         Final_DF$Birth.Per.1000.Per.Year,
         method="pearson")

cor.test(log(Final_DF$GDP.Per.Capita),
         Final_DF$Birth.Per.1000.Per.Year,
         method="spearman")

cor.test(Final_DF$GDP.Per.Capita,
         Final_DF$Birth.Per.1000.Per.Year,
         method="kendall")

cor.test(Final_DF$GDP.Per.Capita,
         Final_DF$Birth.Per.Woman,
         method="pearson")

cor.test(Final_DF$GDP.Per.Capita,
         Final_DF$Birth.Per.Woman,
         method="spearman")

cor.test(Final_DF$GDP.Per.Capita,
         Final_DF$Birth.Per.Woman,
         method="kendall")
