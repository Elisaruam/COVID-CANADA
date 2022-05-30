#3.	A través de la librería canadacovid genera una query que permita conocer quien ha sido la provincia 
#con mayor tasa de contagios y con mayor tasa de vacunación diaria en los últimos seis meses
library(dplyr)
library(canadacovid)

provinces<-get_provinces(geo_only = TRUE)
reports<-get_reports(split = c( "province"))

reportesFilter<- reports %>%
  filter(date> "2021-11-30" )

reportesAgrupados<- reportesFilter %>%
  select(province, date, change_cases, change_vaccinations) %>%
  group_by(province) %>%
  summarise(casos_medios = mean(change_cases), vacunas_medias= mean(change_vaccinations))

reportesAgrupados_<- merge(reportesAgrupados, provinces, by.x = "province", by.y = "code", all.x = TRUE)
reportesAgrupados_$tasa_casos_medios<- reportesAgrupados_$casos_medios/ reportesAgrupados_$population
reportesAgrupados_$tasa_vacunas_medios<- reportesAgrupados_$vacunas_medias/ reportesAgrupados_$population

ProvinciaMayorTasaCasos<- reportesAgrupados_ %>%
  filter(tasa_casos_medios == max(tasa_casos_medios)) %>%
  select(name)
ProvinciaMayorTasaVacunas<- reportesAgrupados_ %>%
  filter(tasa_vacunas_medios == max(tasa_vacunas_medios)) %>%
  select(name)

print(paste("La provincia con mayor tasa de contagios es", ProvinciaMayorTasaCasos))
print(paste("La provincia con mayor tasa de vacunas diarias es", ProvinciaMayorTasaVacunas))
