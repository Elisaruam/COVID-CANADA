library(dplyr)
library(canadacovid)
library(lubridate)

datosCovidCanada <- function(fecha){
  reports<-get_reports(split = c( "province"))
  
  reportesFilter<- reports %>%
    filter(date> ymd(as.Date(fecha)-month(6)) )
  
  reportesAgrupados<- reportesFilter %>%
    select(province, date, change_cases, change_vaccinations) %>%
    group_by(province) %>%
    summarise(casos_medios = mean(change_cases), vacunas_medias= mean(change_vaccinations))
  
  provinces<-get_provinces(geo_only = TRUE)
  
  reportesAgrupados_<- merge(reportesAgrupados, provinces, by.x = "province", by.y = "code", all.x = TRUE)
  reportesAgrupados_$tasa_casos_medios<- reportesAgrupados_$casos_medios/ reportesAgrupados_$population
  reportesAgrupados_$tasa_vacunas_medios<- reportesAgrupados_$vacunas_medias/ reportesAgrupados_$population
  
  ProvinciaMayorTasaCasos<- reportesAgrupados_ %>%
    filter(tasa_casos_medios == max(tasa_casos_medios)) %>%
    select(name)
  ProvinciaMayorTasaVacunas<- reportesAgrupados_ %>%
    filter(tasa_vacunas_medios == max(tasa_vacunas_medios)) %>%
    select(name)
  return( print(paste("La provincia con mayor tasa de contagios es", ProvinciaMayorTasaCasos, "La provincia con mayor tasa de vacunas diarias es", ProvinciaMayorTasaVacunas)))

  
}

datosCovidCanada("2021-06-30")
