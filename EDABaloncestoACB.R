# 1step Carga de datos -------------------------------------------------------------------


# Borramos el global environment para quitar residuos
rm(list = ls())

#Instalamos paquetes esenciales
install.packages("remotes")
pacman::p_load(tm,SnowballC,tidyverse, wordcloud, lubridate,plotly)

# Descargamos un paquete de descargas via github
# descargando el repositorio de github de bziarkowski
remotes::install_github("bziarkowski/euRobasket") #descarga repo github

#cargamos libreria de la api
library(euRobasket)

# Si lo de antes no ha funcionado porbar con:
install.packages("devtools")
devtools::install_github('bziarkowski/euRobasket')



# Cargamos todos los datos entre wl 29 de septiembre de 2003 hasta hoy
cs1 <- get_boxscores_realgm(start_date = '2003-9-29', end_date = '2020-12-4', league = 'Spanish ACB')


# para saber cuales son las ligas disponibles
get_realgm_league_names()
# Ejemplo: cq <-get_players_stats_realgm(league = 'German BBL', season = 2019, type = 'Totals', position = 'PG')


# Sabemos que en 'c' se almacena una lista de datasets entonces para tener un solo dataset con todos 
# los partidos y jugadores primero hay que juntar 2 de ellos para luego hacer un for que vaya añadiendo
# dataset a dataset

finalDF = rbind(cs1[[1]],cs1[[2]]) #este comando me junta los dos primeros datsets

# Un for para append todos
for (i in 3:length(cs1)) {
  finalDF <-  rbind(cs1[[i]],finalDF)
}


# La carga de los datos es muy lenta por lo que si no quiere cargarlo cada vez que se reinicie R o borre accidentalmente
# finalDF es muy util guardar los datos en un RDS de esta forma si se resetea R solo hay que llamar a RDS.
saveRDS(finalDF,'acb17years.rds')

# Si hemos guardado los datos en un RDS se puede cargar el data set con el siguiente comando
finalDF <- readRDS('acb17years.rds')
finalDF2 <- readRDS('acb17years.rds')

# Primeras manipulaciones (No tener en cuenta) -------------------------------------------------

Bases<- finalDF %>% filter(str_detect(position, "PG"))
mean(Bases$blocks)
sd(Bases$blocks)

escoltas<- finalDF %>% filter(str_detect(position, "SG"))
mean(escoltas$blocks)
sd(escoltas$blocks)

aleros<- finalDF %>% filter(str_detect(position, "SF"))
mean(aleros$blocks)
sd(aleros$blocks)

alas<- finalDF %>% filter(str_detect(position, "PF"))
mean(alas$blocks)
sd(alas$blocks)

pivots<- finalDF %>% filter(str_detect(position, "C"))
mean(pivots$blocks)
sd(pivots$blocks)

# Formateando el dataset (parte ETL) -----------------------------------------------------

# Evitamos posiciones mal puestas
finalDF<- finalDF %>% filter(str_detect(position, "C|PF|SF|SG|PG"))

# Eliminamos la columna team_name_short ya que no nos aporta valor
finalDF <-select(finalDF, -c(team_name_short))

# Renombramos columnas por el '3' que nos obligaba a jugar con las comillas
finalDF <- rename(finalDF,threesmade=`3pm`)
finalDF <- rename(finalDF,threesattemp=`3pa`)

#convertimos los char en numerical
finalDF[,6:20] <- sapply(finalDF[,6:20],as.numeric)

# Añadimos nuevas varibles

# Para agrupar por mes
finalDF$fechaMes <- format(as.Date(finalDF$match_date, "%Y-%m-%d"), "%Y-%m")
finalDF$fechaYear <- format(as.Date(finalDF$match_date, "%Y-%m-%d"), "%Y")


# Porcentaje de acierto de los triples
finalDF$PercTrhees <- (finalDF$threesmade/finalDF$threesattemp)*100
# Segunda froma: finalDF <- ( mutate(finalDF, PercTrhees = (threesmade / threesattemp)*100))

finalDF <- ( mutate(finalDF, PercallShoots = (fgm/fga)*100))


# Evidamos los Nan que hacenq ue no se pueda graficar y se rellena con un 0 
finalDF$PercTrhees[is.na(finalDF$PercTrhees)] <- 0

# 2step Empezamos con los gráficos y filtros --------------------------------------------------


# Gráficos de dispersión (puntos) cuyo eje x son las asistencias, el eje y lo rebotes ofensivos y luego se clasifica
# por colores según la posición. De etsa forma podemos ver las carateristicas de las posiciones según el numero de rebotes
# y asistencias.
asis_rebound_pos <- finalDF %>% ggplot(aes(x=assists, y=off_rebounds,color=position))+geom_point()+
  ggtitle("Rebotes ofensivos y asistencias según la posición")
asis_rebound_pos 
# Mini conclusión: se ve como los que mas rebotes ofensivos cogen son verdes o marrones pivots o alas
# se ve que los que mas asisten son bases. no es normal que un pivot de >10asistencias, huele a que tiene alma de base

# Gráfico practicamente igual que el anterior pero en este caso queremos ver según la posición la comparación de bloqueos 
# hechos y triples metidos.
dl <- finalDF %>% ggplot(aes(x=threesmade, y=blocks,color=position))+geom_point()
dl
# Poca información poco visible

# Enmpezamos a filtrar
# MuchosBloqueos <- quantile(finalDF$blocks)
afiltro1v <- finalDF %>% filter(`blocks`>=4)
dp <- afiltro1v %>% ggplot(aes(x=threesmade, y=blocks, color=position))+geom_point(aes(shape=position, size=off_rebounds))
dp



# Graficos utiles y tablas nuevas--------------------------------------------------


gr0 <- finalDF %>% ggplot(aes(x=PercTrhees, y=blocks,color=position,size=threesmade))+geom_point(aes(shape=position)) +
  xlab("%triples") + ylab("Numero tapones") +
  ggtitle("Distribución numero de tapones con respecto al % de triples, y ordenado por posicion") + theme_bw()
gr0


muchosblocs <- finalDF%>% filter(blocks>= 4)#partidos que han superado los 4blocs

# Porcentaje de bloqueos por jugador
porcBloqJug <- as.data.frame(prop.table(table(muchosblocs$position))*100)
colnames(porcBloqJug) <- c("position" , "frecuencia")
# Perfil del jugador que más bloqueos hace
freq_Max_Bloqueos <- max(as.data.frame(prop.table(table(muchosblocs$position))*100)[2])
maximoBloqueador <- (porcBloqJug %>% filter(frecuencia == freq_Max_Bloqueos))[1]



# Creamos una tabla donde telaranaPivVSBasesure las posiciones, el status y la frecuencia de aparición
# De este modo podemos separar por el status las posiciones que más bloqueos hacen
tab.5 <- as.data.frame(prop.table(table(muchosblocs$position, muchosblocs$status), 2)*100)
colnames(tab.5) <- c("position", "status", "Pct")

gr1 <- ggplot(tab.5, aes(x=position, y=Pct))  +
  geom_bar(stat="identity", width=0.5, colour="blue", fill = "red") +
  xlab("posicion") + ylab("% de jugadores con >=4blocks") +
  ggtitle("Distribución de los jugadores mas taponadores por posicion (%)") + theme_bw()
gr1 <- gr1 + facet_grid(status ~.)
gr1



# Redondeamos
porcBloqJugRed <-  rapply(object = porcBloqJug, f = round, classes = "numeric" , how = "replace" , digit = 1)
# Graficamos funcion tarta (donut)
donut_Bloq_Pos <- ggplot(porcBloqJugRed,aes(x=2,y=frecuencia, fill=position))+
  geom_bar(stat = "identity",
           color="black")+
  geom_text(aes(label=frecuencia),
            position=position_stack(vjust=0.5),color="black",size=3)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","purple"))+
  theme_void()+
  labs(title="% posicion de jugadores con más de 4 tapones")+ xlim(0.5,2.5)
donut_Bloq_Pos



# triples3quartil <- quantile(finalDF$threesmade)
muchostriples<- finalDF%>% filter(threesmade>= 5)#partidos que han superado los 4blocs
masde5triples <- as.data.frame(prop.table(table(muchostriples$position))*100)


masde5triples <- rapply(object = masde5triples, f = round, classes = "numeric", how = "replace", digits = 1)
colnames(masde5triples) <- c("position", "percentage")

muchostriples_prop_Pos_Barras <- ggplot(masde5triples, aes(x=position, y = percentage)) +
  geom_bar(stat="identity", width=0.5, colour="blue", fill = "red") +
  xlab("position") + ylab("% de jugadores con >=8 triples") +
  ggtitle("% posicion de jugadores con más de 8 triples ") + theme_bw()
muchostriples_prop_Pos_Barras

muchostriples_prop_Pos_donut <- ggplot(masde5triples,aes(x=2,y=percentage, fill=position))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percentage),
            position=position_stack(vjust=0.5),color="white",size=3)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
  theme_void()+
  labs(title="% posicion de jugadores con más de 8 triples")+ xlim(0.5,2.5)
muchostriples_prop_Pos_donut




muchasAsistencias<- finalDF%>% filter(assists>=4 )#partidos que han superado los 4blocs
con4OmasAssist <- as.data.frame(prop.table(table(muchasAsistencias$position))*100)
df2 <- rapply(object = con4OmasAssist, f = round, classes = "numeric", how = "replace", digits = 1)
colnames(df2) <- c("position", "percentage")
gr9 <- ggplot(df2, aes(x=position, y = percentage)) +
  geom_bar(stat="identity", width=0.5, colour="green", fill = "red") +
  xlab("position") + ylab("% de jugadores con >=4 asistencias") +
  ggtitle("% posicion de jugadores con 4 asistencias o más  ") + theme_bw()
gr9#
gr10 <- ggplot(df2,aes(x=2,y=percentage, fill=position))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percentage),
            position=position_stack(vjust=0.5),color="white",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","purple"))+
  theme_void()+
  labs(title="% posicion de jugadores con 4 asistencias o más ")+ xlim(0.5,2.5)
gr10

muchosrobos<- finalDF%>% filter(steals>=2 )#partidos que han superado los 4blocs
con2OmasRobos <- as.data.frame(prop.table(table(muchosrobos$position))*100)
df2 <- rapply(object = con2OmasRobos, f = round, classes = "numeric", how = "replace", digits = 1)
colnames(df2) <- c("position", "percentage")
gr11 <- ggplot(df2, aes(x=position, y = percentage)) +
  geom_bar(stat="identity", width=0.5, colour="green", fill = "red") +
  xlab("position") + ylab("% de jugadores con >=2 robos") +
  ggtitle("% posicion de jugadores con 2 robos o más  ") + theme_bw()
gr11#
gr12 <- ggplot(df2,aes(x=2,y=percentage, fill=position))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percentage),
            position=position_stack(vjust=0.5),color="white",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","purple"))+
  theme_void()+
  labs(title="% posicion de jugadores con 2 robos o más ")+ xlim(0.5,2.5)
gr12



muchosrebotesofensivos<- finalDF%>% filter(off_rebounds>=3 )#partidos que han superado los 4blocs
con3OmasRebotes <- as.data.frame(prop.table(table(muchosrebotesofensivos$position))*100)
df2 <- rapply(object = con3OmasRebotes, f = round, classes = "numeric", how = "replace", digits = 1)
colnames(df2) <- c("position", "percentage")
gr13 <- ggplot(df2, aes(x=position, y = percentage)) +
  geom_bar(stat="identity", width=0.5, colour="green", fill = "red") +
  xlab("position") + ylab("% de jugadores con >=2 robos") +
  ggtitle("% posicion de jugadores con 3 rebotes o más  ") + theme_bw()
gr13#
gr14 <- ggplot(df2,aes(x=2,y=percentage, fill=position))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percentage),
            position=position_stack(vjust=0.5),color="white",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange","gray","purple"))+
  theme_void()+
  labs(title="% posicion de jugadores con 3 rebotes ofensivos o más ")+ xlim(0.5,2.5)
gr14



new_frame2<- finalDF%>% filter(  tot_rebounds%in% (0:9))
tab.4 <- as.data.frame(prop.table(table(new_frame2$tot_rebounds, new_frame2$position), 2)*100)
colnames(tab.4) <- c("tot_rebounds", "position", "Pct")
gr17 <- ggplot(tab.4, aes(x=tot_rebounds, y=Pct))+
  geom_bar(stat="identity", width=0.5, colour="blue", fill = "red")+
  xlab("rebotes") + ylab("%total rebotes por posicion") +
  ggtitle("Distribución del % total de rebotes ofensivos  para cada posición") + theme_bw()
# gr5 no hacer caso no entiendo aun pero si entiendo y sirve el siguiente
gr18 <-  gr17+ facet_grid(position ~.)
gr18

gr19 <- ggplot(tab.4, aes(x=tot_rebounds, y=Pct))+
  geom_col(size = 1, color = "green", fill = "white") +
  geom_line(size = 0.8, color="red", group = 1) +
  xlab("rebotes") + ylab("%total rebotes por posicion") +
  ggtitle("Distribución del % total de rebotes ofensivos  para cada posición") + theme_bw()
gr20 <-  gr19+ facet_grid(position ~.)
gr20

mean(finalDF$blocks)
equipostop<- finalDF %>% filter(str_detect(team_name_long, "Real Madrid|Barca|TD Systems Baskonia|Valencia Basket|Movistar Estudiantes|
RETAbet Bilbao Basket|Iberostar Tenerife|Club Joventut Badalona|Herbalife Gran Canaria"))
finalDF[is.na(finalDF)] = 0


# Mejores gráficos --------------------------------------------------------

# Filtramos 
Bases<- finalDF %>% filter(str_detect(position, "PG"))
escoltas<- finalDF %>% filter(str_detect(position, "SG"))
Pivots<- finalDF %>% filter(str_detect(position, "C"))
Tavares<- finalDF %>% filter(str_detect(player, "Edy Tavares"))
MarcGasol<- finalDF %>% filter(str_detect(player, "Marc Gasol"))
Jaycee_Carroll <-  finalDF %>% filter(str_detect(player, "Jaycee Carroll"))
Alex_Abrines <-  finalDF %>% filter(str_detect(player, "Alex Abrines"))
Alex_Abrines_ultimosYear <-  Alex_Abrines %>% filter(Alex_Abrines$fechaYear>2015)

library(plotly)	

#GRAFICO TELARAÑA BASES VS PIVOTS
telaranaPivVSBases <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) 
telaranaPivVSBases <- telaranaPivVSBases %>%
  add_trace(
    r = c(mean(Bases$blocks), mean(Bases$threesmade), mean(Bases$off_rebounds), mean(Bases$steals), mean(Bases$assists), mean(Bases$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'Bases'
  )
telaranaPivVSBases <- telaranaPivVSBases %>%
  add_trace(
    r = c(mean(Pivots$blocks), mean(Pivots$threesmade), mean(Pivots$off_rebounds), mean(Pivots$steals), mean(Pivots$assists), mean(Pivots$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'Pivots'
  ) 

telaranaPivVSBases <- telaranaPivVSBases %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T
      )
    )
  )

telaranaPivVSBases



#GRAFICO TELARAÑA TAVARES VS PIVOTS VS BASES
telaranaPivVSBasesTavsC <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) 
telaranaPivVSBasesTavsC <- telaranaPivVSBasesTavsC %>%
  add_trace(
    r = c(mean(Tavares$blocks), mean(Tavares$threesmade), mean(Tavares$off_rebounds), mean(Tavares$steals), mean(Tavares$assists), mean(Tavares$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'MediaTavares'
  )
telaranaPivVSBasesTavsC <- telaranaPivVSBasesTavsC %>%
  add_trace(
    r = c(mean(Pivots$blocks), mean(Pivots$threesmade), mean(Pivots$off_rebounds), mean(Pivots$steals), mean(Pivots$assists), mean(Pivots$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'MediaPivots'
  ) 
telaranaPivVSBasesTavsC <- telaranaPivVSBasesTavsC %>%
  add_trace(
    r = c(mean(Bases$blocks), mean(Bases$threesmade), mean(Bases$off_rebounds), mean(Bases$steals), mean(Bases$assists), mean(Bases$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'Bases'
  )
telaranaPivVSBasesTavsC <- telaranaPivVSBasesTavsC %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T
      )
    )
  )

telaranaPivVSBasesTavsC


#GRAFICO TELARAÑA Marc Gasol VS PIVOTS (su posición)
telaranaPivVSBasesmarcvsC <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) 
telaranaPivVSBasesmarcvsC <- telaranaPivVSBasesmarcvsC %>%
  add_trace(
    r = c(mean(MarcGasol$blocks), mean(MarcGasol$threesmade), mean(MarcGasol$off_rebounds), mean(MarcGasol$steals), mean(MarcGasol$assists), mean(MarcGasol$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'MediaMarcGasol'
  )
telaranaPivVSBasesmarcvsC <- telaranaPivVSBasesmarcvsC %>%
  add_trace(
    r = c(mean(Pivots$blocks), mean(Pivots$threesmade), mean(Pivots$off_rebounds), mean(Pivots$steals), mean(Pivots$assists), mean(Pivots$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'MediaPivots'
  ) 

telaranaPivVSBasesmarcvsC <- telaranaPivVSBasesmarcvsC %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,2.5)
      )
    )
  )

telaranaPivVSBasesmarcvsC


# Comparación Jaycee Carrolly Alex Abrines
compJayceeAlex <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) 
compJayceeAlex <- compJayceeAlex %>%
  add_trace(
    r = c(mean(Jaycee_Carroll$blocks), mean(Jaycee_Carroll$threesmade), mean(Jaycee_Carroll$off_rebounds), mean(Jaycee_Carroll$steals), mean(Jaycee_Carroll$assists), mean(Jaycee_Carroll$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'Jaycee Carroll'
  )
compJayceeAlex <- compJayceeAlex %>%
  add_trace(
    r = c(mean(Alex_Abrines$blocks), mean(Alex_Abrines$threesmade), mean(Alex_Abrines$off_rebounds), mean(Alex_Abrines$steals), mean(Alex_Abrines$assists), mean(Alex_Abrines$turnovers)),
    theta = c('MedBlocks','Medtripl','MedRebOff', 'MedSteals', 'MedAssist', 'MedTO'),
    name = 'Alex Abrines'
  ) 

compJayceeAlex <- compJayceeAlex %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T
      )
    )
  )

compJayceeAlex