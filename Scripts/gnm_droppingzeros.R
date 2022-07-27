library(tidyverse)
library(tidylog)
library(ludridate)
library(gnm)
library(dlnm)


data(chicagoNMMAPS)
db <- chicagoNMMAPS

## defining strata
db <-
    db %>% 
    mutate(stratum = factor(paste(year, month, dow, sep='_')))

# no zero deaths
db %>% 
    group_by(stratum) %>% 
    summarise(n=sum(death)) %>% 
    count(n==0)

db %>% 
    group_by(stratum) %>% 
    summarise(n=sum(cvd)) %>% 
    count(n==0)

db %>% 
    group_by(stratum) %>% 
    summarise(n=sum(resp)) %>% 
    count(n==0)

# creating zero strata

strata_id <- unique(db$stratum) # 1176 strata

strata_id_zero_200 <- sample_n(as_tibble(strata_id), 200)

db_200 <-
    db %>% 
    mutate(death = ifelse(stratum %in% strata_id_zero_200$value, 0, death),
           pm10 = pm10/10)

## gnm poisson

# keeping zeros
mp_all <- gnm(death ~ pm10,   
    family=poisson(), data=db_200,  eliminate=stratum)


# taking out zeros
mp_zero <- gnm(death ~ pm10,   
              family=poisson(), data=db_200 %>% filter(!stratum %in% strata_id_zero$value),  eliminate=stratum)


# quasi
# keeping zeros
mqp_all <- gnm(death ~ pm10,   
              family=quasipoisson(), data=db_200,  eliminate=stratum)


# taking out zeros
mqp_zero <- gnm(death ~ pm10,   
               family=quasipoisson(), data=db_200 %>% filter(!stratum %in% strata_id_zero$value),  eliminate=stratum)

summary <- rbind(se(mp_all), se(mp_zero), se(mqp_all), se(mqp_zero))
summary <- cbind(summary, c('Poisson with zeros','Poisson without zeros',
            'QuasiPoisson with zeros','Quasi Poisson without zeros'))
names(summary) <- c('estimate', 'se', 'model')
summary <- summary %>% as_tibble() %>% select(model, estimate, se)
summary

# now less strata to be dropped
    
strata_id_zero50 <- sample_n(as_tibble(strata_id), 50)

db_50 <-
    db %>% 
    mutate(death = ifelse(stratum %in% strata_id_zero50$value, 0, death),
           pm10 = pm10/10)

## gnm poisson

# keeping zeros
mp_all <- gnm(death ~ pm10,   
              family=poisson(), data=db_50,  eliminate=stratum)


# taking out zeros
mp_zero <- gnm(death ~ pm10,   
               family=poisson(), data=db_50 %>% filter(!stratum %in% strata_id_zero$value),  eliminate=stratum)


# quasi
# keeping zeros
mqp_all <- gnm(death ~ pm10,   
               family=quasipoisson(), data=db_50,  eliminate=stratum)


# taking out zeros
mqp_zero <- gnm(death ~ pm10,   
                family=quasipoisson(), data=db_50 %>% filter(!stratum %in% strata_id_zero$value),  eliminate=stratum)

summary <- rbind(se(mp_all), se(mp_zero), se(mqp_all), se(mqp_zero))
summary <- cbind(summary, c('Poisson with zeros','Poisson without zeros',
                            'QuasiPoisson with zeros','Quasi Poisson without zeros'))
names(summary) <- c('estimate', 'se', 'model')
summary <- summary %>% as_tibble() %>% select(model, estimate, se)
summary

