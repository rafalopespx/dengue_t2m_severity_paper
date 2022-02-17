is.capital<-function(x = NULL, metro_capitals = FALSE){
  require(geobr)
  # library(MASS)
  require(parallel)
  require(magrittr)
  capitals_code<-c(4314902, 4205407, 4106902, #Regiao Sul 
                   3550308, 3304557, 3106200, 3205309, # Regiao Sudeste
                   5103403, 5002704, 5208707, 5300108, # Regiao Centro-Oeste
                   1200401, 1302603, 1501402, 1400100, 1600303, 1100205, 1721000, # Regiao Norte
                   2111300, 2211001, 2408102, 2611606, 2507507, 2704302, 2800308, 2927408, 2304400 # Regiao Nordeste
  )
  if(metro_capitals){
    metro_area<-geobr::read_metro_area(year = 2018, simplified = T, showProgress = F)
    
    capitals_metro<-metro_area %>% 
      dplyr::filter(code_muni %in% capitals_code)
    
    capitals_metro <- metro_area %>% 
      dplyr::filter(name_metro %in% capitals_metro$name_metro)
    
    capitals_and_metro<-c(capitals_metro$code_muni, capitals_code[!capitals_code %in% capitals_metro$code_muni])
    
    lookup_fun<-function(y){
      city<-geobr::lookup_muni(code_muni = y)
    }
    numcores<-parallel::detectCores()-1
    capitals_and_metro_list<-parallel::mclapply(capitals_and_metro, lookup_fun, mc.cores = numcores)
    capitals_and_metro_list<-dplyr::bind_rows(capitals_and_metro_list)
    capitals_and_metro_list$code_muni_six_digits<-substr(capitals_and_metro_list$code_muni, 1, 6)
    return(capitals_and_metro_list)
    
  }
  
  lookup_fun<-function(y){
    city<-geobr::lookup_muni(code_muni = y)
  }
  numcores<-parallel::detectCores()-1
  capitals_list<-parallel::mclapply(capitals_code, lookup_fun, mc.cores = numcores)
  capitals_list<-dplyr::bind_rows(capitals_list)
  capitals_list$code_muni_six_digits<-substr(capitals_list$code_muni, 1, 6)
  return(capitals_list)
}

regiao<- function(x, english = FALSE){
  x<-as.data.frame(x)
  if(missing(english) | english == FALSE){
    x$regiao <- NA
    x[x$abbrev_state %in% c("RS", "SC", "PR"), "regiao"] <- "Sul"
    x[x$abbrev_state %in% c("AM", "AC", "AP", "PA", "RO", "RR", "TO"), "regiao"] <- "Norte"
    x[x$abbrev_state %in% c("MS", "MT", "GO", "DF"), "regiao"] <- "Centro-Oeste"
    x[x$abbrev_state %in% c("SP", "RJ", "MG", "ES"), "regiao"] <- "Sudeste"
    x[x$abbrev_state %in% c("BA", "PE", "PB", "MA", "CE", "RN", "AL", "SE", "PI"), "regiao"] <- "Nordeste"
  } else {
    x$region <- NA
    x[x$abbrev_state %in% c("RS", "SC", "PR"), "region"] <- "South"
    x[x$abbrev_state %in% c("AM", "AC", "AP", "PA", "RO", "RR", "TO"), "region"] <- "North"
    x[x$abbrev_state %in% c("MS", "MT", "GO", "DF"), "region"] <- "Center-West"
    x[x$abbrev_state %in% c("SP", "RJ", "MG", "ES"), "region"] <- "Southeast"
    x[x$abbrev_state %in% c("BA", "PE", "PB", "MA", "CE", "RN", "AL", "SE", "PI"), "region"] <- "Northeast"
  }
  return(x)
}

# br.cities<-function(x = NULL){
#   library(geobr)
#   library(MASS)
#   library(parallel)
#   library(magrittr)
#   # capitals_code<-c(4314902, 4205407, 4106902, #Regiao Sul 
#   #                  3550308, 3304557, 3106200, 3205309, # Regiao Sudeste
#   #                  5103403, 5002704, 5208707, 5300108, # Regiao Centro-Oeste
#   #                  1200401, 1302603, 1501402, 1400100, 1600303, 1100205, 1721000, # Regiao Norte
#   #                  2111300, 2211001, 2408102, 2611606, 2507507, 2704302, 2800308, 2927408, 2304400 # Regiao Nordeste
#   # )
#   lookup_fun<-function(y){
#     city<-lookup_muni(code_muni = y)
#   }
#   numcores<-detectCores()-1
#   cities_list<-mclapply(cities, lookup_fun, mc.cores = numcores)
#   cities_list<-bind_rows(cities_list)
#   cities_list$code_muni_six_digits<-substr(cities_list$code_muni, 1, 6)
#   return(cities_list)
# }
