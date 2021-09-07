
# libraries ---------------------------------------------------------------

library(rio)
library(dplyr)


# favelasShape -----------------------------------------------------------

PathFile="./Limite_Favelas_2016/Limite_Favelas_2016.shp"
shape=readOGR(PathFile)
data_shape=shape@data

## Cleaning_favelasShape -------------------------------------------------
data_shape<- data_shape %>% dplyr::rename(upp = UPP)
data_shape$upp<- ifelse(data_shape$upp=="Batan", "Batam", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Camarista Meier", "Camarista Méier", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Cerro Corá", "Cerro-Corá", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Fallet / Fogueteiro - Coroa", "Coroa / Fallet / Fogueteiro", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Manguinhos - Arará/Mandela", "Arará / Mandela", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Pavão-Pavãozinho / Cantagalo","Pavão-Pavãozinho", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Prazeres / Escondidinho","Escondidinho / Prazeres", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Vila Proletária","Parque Proletário", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="São João","São João / Matriz / Queto", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Tabajaras / Cabritos","Tabajaras", data_shape$upp)
data_shape$upp<- ifelse(data_shape$upp=="Vidigal / Chácara do Céu","Vidigal", data_shape$upp)
data_shape$upp<- ifelse(data_shape$Nome=="Mangueirinha","Mangueirinha", data_shape$upp)


# treatment01 -------------------------------------------------------------
data_treatment<- data_shape %>%
  filter(upp != "N")%>%dplyr::select(  Nome,
                                       Complexo,
                                       upp)

#Sources
#https://noticias.uol.com.br/cotidiano/ultimas-noticias/2014/03/30/forcas-policiais-do-rio-comecam-a-ocupar-favelas-do-complexo-da-mare.htm
#https://medium.com/@brasilobserver/mare-complex-occupied-b9fabbffdc46
#https://www.arcgis.com/apps/MapJournal/index.html?appid=2910ea44348f49cea4ab130f2f7f8cd5
#http://www.ispdados.rj.gov.br/?

datesUPP<- read_excel("./Dependencies/UppDatasDeOcupacaoeInstalacao.xlsx")
##UppDatasDeOcupacaoeInstalacao.xlsx has UPP's info (deployment)

data_treatment<- merge(data_treatment , datesUPP , by = c("upp"), sort = TRUE, all=T)

## Cleaning_treatment01 -------------------------------------------------

data_treatment$data_ocupacao<- ifelse(data_treatment$upp == "Maré", "2014-03-30", 
                             as.character(data_treatment$data_ocupacao))
data_treatment=data_treatment%>%dplyr::select(Nome,Complexo,
                       upp,data_ocupacao,data_fim)


# merge_dataShape_treatment01 ---------------------------------------------

ShapeTreatment <- merge(data_shape , data_treatment , by = c("Nome","upp","Complexo"), sort = TRUE, all=T)

ShapeTreatment$Treatment <- ifelse(is.na(ShapeTreatment$data_ocupacao), 0,1)

## clean_data_final ---------------------------------------------

ShapeTreatment<- ShapeTreatment %>%
  dplyr::mutate(year.occupation = lubridate::year(data_ocupacao), 
                month.occupation = lubridate::month(data_ocupacao), 
                day.occupation = lubridate::day(data_ocupacao))

ShapeTreatment<- ShapeTreatment %>%
  dplyr::mutate(year.fim = lubridate::year(data_fim), 
                month.fim = lubridate::month(data_fim), 
                day.fim = lubridate::day(data_fim))


# Merge_PolygonsLights_data_final ---------------------------------------------

PolygonsLights<-import("PolygonsLights.xlsx")

data_analysis07232021=PolygonsLights%>%
  mutate(year=substr(date,1,4),
         month=as.numeric(substr(date,5,7)))%>%
  left_join(ShapeTreatment, by="Codfavela")%>%
  mutate(treatment2=ifelse(Treatment==1&year>=year.occupation,1,0))%>%
  mutate(treatment3=ifelse(year==year.occupation&month<month.occupation,1,0))%>%
  mutate(treatment_final=ifelse(treatment2==1&treatment3==0,1,0))%>%
  dplyr::select(-c("treatment2","treatment3"))

save(data_analysis07232021,ShapeTreatment,datesUPP, file="SetsAnalysis07232021.RData")
