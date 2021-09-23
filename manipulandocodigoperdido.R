
library(euRobasket)
pacman::p_load(tm,SnowballC,tidyverse, wordcloud, lubridate,plotly)
cs1 <- get_boxscores_realgm('2021-6-24','2021-9-24', league = 'Spanish ACB')
finalDF22 = rbind(cs1[[1]],cs1[[2]]) #este comando me junta los dos primeros datsets
# Un for para append todos
for (i in 3:length(cs1)) {
  finalDF22 <-  rbind(cs1[[i]],finalDF22)
}
#finalDF22=acb17years
mrgg <- get_scores_realgm('2021-6-24','2021-9-24', 'Spanish ACB')
mrgg1=mrgg
mrgg <- rename(mrgg,match_date=`date`)
mrgg <- rename(mrgg,team_name_long=home_team)
mrgg[,5:5] <- sapply(mrgg[,5:5],as.character)
#mrgg1 <- get_scores_realgm('2003-9-29','2021-4-4', 'Spanish ACB')
mrgg1 <- rename(mrgg1,match_date=`date`)
mrgg1 <- rename(mrgg1,team_name_long=away_team)
mrgg1[,5:5] <- sapply(mrgg1[,5:5],as.character)
# dff <- merge (acb17years, mrgg, by = "match_date")
# by = c("match_date", "team_name_long"
mrgg <- na.omit(mrgg)
mrgg1 <- na.omit(mrgg1)
mrDatosTesTotal21 = merge(mrgg1,finalDF22,by = c("match_date", "team_name_long"))
mrDatosTesTotal22 = merge(mrgg,finalDF22,by = c("match_date", "team_name_long"))
mrDatosTesTotal22$team<-mrDatosTesTotal22$team_name_long
mrDatosTesTotal21$team<-mrDatosTesTotal21$team_name_long
mrDatosTesTotal22$rival<-mrDatosTesTotal22$away_team
mrDatosTesTotal21$rival<-mrDatosTesTotal21$home_team
v <- c(1:length(mrDatosTesTotal21))
i <- length(v)
ty <- v[length(v)]
while (i>1) {
  v[i]<- v[i-1]
  i <- i-1
}
v[1] <- ty
mrDatosTesTotal21 = subset (mrDatosTesTotal21, select=v)
# mrDatosTesTotal22 = subset (mrDatosTesTotal21, select=v)
mrDatosTesTotal21 <- rename(mrDatosTesTotal21,away_team=`team_name_long`)
mrDatosTesTotal22 <- rename(mrDatosTesTotal22,home_team=`team_name_long`)
mrDatosTesTotal22 = mrDatosTesTotal22 [ , c(names(mrDatosTesTotal21))]
# mrdfFin <-  rbind(mrDatosTesTotal21,mrDatosTesTotal22)
# mh1<- mrdfFin %>% filter(str_detect(mrdfFin$match_date, "2020-11-22"))
# mrdfFin$Win_Lose <- (mrdfFin$home_score/mrdfFin$away_team)
#poniendo vic/der para los locales
li <- c(1:nrow(mrDatosTesTotal22))
for (i in 1:length(li)) {
  if(( mrDatosTesTotal22[i,]$home_score/mrDatosTesTotal22[i,]$away_score) > 1) {
    li[i]<-"Win"
  } else {
    li[i]<-"Lose"
  }
}
mrDatosTesTotal22$Win_Lose<- li
v <- c(1:length(mrDatosTesTotal22))
i <- length(v)
ty <- v[length(v)]
while (i>1) {
  v[i]<- v[i-1]
  i <- i-1
}
v[1] <- ty
#poniendo vic/der para los visitantes
lis <- c(1:nrow(mrDatosTesTotal21))
for (i in 1:length(lis)) {
  if(( mrDatosTesTotal21[i,]$home_score/mrDatosTesTotal21[i,]$away_score) > 1) {
    lis[i]<-"Lose"
  } else {
    lis[i]<-"Win"
  }
}
mrDatosTesTotal21$Win_Lose<- lis
s <- c(1:length(mrDatosTesTotal21))
i <- length(s)
ty <- s[length(s)]
tq <- s[length(s)-1]
while (i>2) {
  s[i]<- s[i-2]
  i <- i-1
}
s[1] <- ty
s[2] <- tq
mrDatosTesTotal21 = subset (mrDatosTesTotal21, select=s)
mrDatosTesTotal22 = subset (mrDatosTesTotal22, select=s)
# save()
mrdfFin <-  rbind(mrDatosTesTotal21,mrDatosTesTotal22)
mrdfFin <-select(mrdfFin, -c(team_name_short))
mrdfFin <- rename(mrdfFin,threesmade=`3pm`)
mrdfFin <- rename(mrdfFin,threesattemp=`3pa`)
mrdfFin[,13:29] <- sapply(mrdfFin[,13:29],as.numeric)
mrdfFin$PercTrhees <- (mrdfFin$threesmade/mrdfFin$threesattemp)*100
mrdfFin$PercallShoots <- (mrdfFin$fgm/mrdfFin$fga)*100
view(mrdfFin)

saveRDS(finalPLUSPLUS,'~/R/rcosass/finalPLUSPLUS_SEPTIEMBRE')


#mrdfFin1 <- mrdfFin %>%  filter(mrdfFin$match_date>=2021-01-25)
finalPLUSPLUS <- readRDS('~/R/rcosass/finalPLUSPLUS')#salvado
finalPLUSPLUS = rbind(mrdfFin,finalPLUSPLUS)
finalPLUSPLUScopy=finalPLUSPLUS
#=finalPLUSPLUS1


#finalPLUSPLUS=mrdfFin
View(finalPLUSPLUS)
pr <- finalPLUSPLUS %>% filter(str_detect(player, "Melo"))
View(pr)
jugadores <- distinct(finalPLUSPLUS, player)#solo eslista de los jugadores  checky
conteoJV <- table (WINS$player)#cuenta la frecuencia d ejugadores distintos en el dataframe de victorias. CUENTA LAS WINS por jugador
conteoWINS <- as.data.frame(conteoJV)
conteoJL <- table (LOSES$player)
WINS <-  filter(finalPLUSPLUS,finalPLUSPLUS$Win_Lose=='Win')
LOSES <-  filter(finalPLUSPLUS,finalPLUSPLUS$Win_Lose=='Lose')
jugadores <- distinct(finalPLUSPLUS, player)#solo eslista de los jugadores  checky
conteoJV <- table (WINS$player)#cuenta la frecuencia d ejugadores distintos en el dataframe de victorias. CUENTA LAS WINS por jugador
conteoWINS <- as.data.frame(conteoJV)
conteoJL <- table (LOSES$player)
conteoLOSES <- as.data.frame(conteoJL)
VyL <- merge (conteoWINS, conteoLOSES, by = "Var1")# une A con B
VyL <- rename(VyL,WINS=Freq.x)
VyL <- rename(VyL,LOSES=Freq.y)
VyL$percWin_Los <- (VyL$WINS/(VyL$WINS+VyL$LOSES))*100
VyL <- rename(VyL,Name=Var1)
afiltro1v <- VyL %>% filter(percWin_Los>=81.0)
aaafiltro1v <- VyL %>% filter((WINS+LOSES)>=100)
aPercVICBIEN <- aaafiltro1v %>% filter(percWin_Los>=81.0)
conteoJ <- table (WINS$team)#cuenta la frecuencia d ejugadores distintos en el dataframe de victorias. CUENTA LAS WINS por jugador
conteoWINSteams <- as.data.frame(conteoJ)
AAlistaJugadores <- list()
AAListaStatsJugadores <- list()
sizet2 <-list(count(VyL))
#AAFAKEListaJugadores[[1]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==VyL["Name"][1,])
for (i in 1:sizet2[[1]][["n"]]) {
  AAlistaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==VyL["Name"][i,])
  AAListaStatsJugadores[[i]] <- list( mean(AAlistaJugadores[[i]]$minutes), mean(AAlistaJugadores[[i]]$points), mean(AAlistaJugadores[[i]]$tot_rebounds),mean(AAlistaJugadores[[i]]$assists) ,sum(AAlistaJugadores[[i]]$points) )
}
AAstats_Totales_Jugador <- data.frame(minutos=numeric(), puntos=numeric(), rebotes=numeric(), asistenicas=numeric(), sumaTotalPuntos=numeric())
for (i in 1:sizet2[[1]][["n"]]) {
  AAstats_Totales_Jugador[i, ] <- AAListaStatsJugadores[[i]]
}
AAstats_Totales_Jugador$Indice <- c(1:sizet2[[1]][["n"]])
VyL$Indice <- c(1:sizet2[[1]][["n"]])
TablaJugadores = merge(VyL,AAstats_Totales_Jugador,by = "Indice")

TablaJugadores$PartidosTotales <- (TablaJugadores$WINS+TablaJugadores$LOSES)

View(TablaJugadores)
saveRDS(TablaJugadores,'~/R/rcosass/sec_TablaJugadoresSeptiembre23')
TablaJugadores_anterior <- readRDS('~/R/rcosass/sec_TablaJugadoresSeptiembre23')#salvado


pr2 <- TablaJugadores %>% filter(str_detect(Name, "Gasol"))
View(pr2)
pr2 <- TablaJugadores %>% filter(str_detect(Name, "Llull"))
View(pr2)

#aquin acabaaaa










pr2 <- finalPLUSPLUS %>% filter(str_detect(player, "Bolmaro"))
pr22 <- pr2 %>% filter(pr2$Win_Lose=="Win")
#count de jugadorsws

sizet <-list(count(jugadores))
listaJugadores <- list()
ListaStatsJugadores <- list()
for (i in 1:sizet[[1]][["n"]]) {
  ListaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[i,])
  ListaStatsJugadores[[i]] <- list( mean(ListaJugadores[[i]]$minutes), mean(ListaJugadores[[i]]$points), mean(ListaJugadores[[i]]$tot_rebounds), mean(ListaJugadores[[i]]$assists) )
}
#creando el dataframe con las stats totales de cada jugador
ListaJugadores <- list()
#ListaJugadores[[1]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[1,])
ListaStatsJugadores<-list()
stats_Totales_Jugador <- data.frame(minutos=numeric(), puntos=numeric(), rebotes=numeric(), asistenicas=numeric())
for (i in 1:sizet[[1]][["n"]]) {
  ListaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[i,])
  ListaStatsJugadores[[i]] <- list( mean(ListaJugadores[[i]]$minutes), mean(ListaJugadores[[i]]$points), mean(ListaJugadores[[i]]$tot_rebounds), mean(ListaJugadores[[i]]$assists) )
}
#creando el dataframe con las stats totales de cada jugador
#ListaJugadores <- list()
#ListaJugadores[[1]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[1,])
#ListaStatsJugadores<-list()
stats_Totales_Jugador <- data.frame(minutos=numeric(), puntos=numeric(), rebotes=numeric(), asistenicas=numeric())
for (i in 1:sizet[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
stats_Totales_Jugador$Indice <- c(1:sizet[[1]][["n"]])
for (i in 1:sizet[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
#count de jugadorsws
sizet <-list(count(jugadores))
for (i in 1:sizet[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
View(sizet)
for (i in 1:sizet2[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
for (i in 1:sizet[[1]][["n"]]) {
  ListaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[i,])
  ListaStatsJugadores[[i]] <- list( mean(ListaJugadores[[i]]$minutes), mean(ListaJugadores[[i]]$points), mean(ListaJugadores[[i]]$tot_rebounds), mean(ListaJugadores[[i]]$assists) )
}
for (i in 1:sizet2[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
#count de jugadorsws
sizet <-list(count(jugadores))
#creando el dataframe con las stats totales de cada jugador
ListaJugadores <- list()
#ListaJugadores[[1]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[1,])
ListaStatsJugadores<-list()
stats_Totales_Jugador <- data.frame(minutos=numeric(), puntos=numeric(), rebotes=numeric(), asistenicas=numeric())
for (i in 1:sizet[[1]][["n"]]) {
  ListaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==jugadores[i,])
  ListaStatsJugadores[[i]] <- list( mean(ListaJugadores[[i]]$minutes), mean(ListaJugadores[[i]]$points), mean(ListaJugadores[[i]]$tot_rebounds), mean(ListaJugadores[[i]]$assists) )
}
for (i in 1:sizet2[[1]][["n"]]) {
  stats_Totales_Jugador[i, ] <- ListaStatsJugadores[[i]]
}
stats_Totales_Jugador$Indice <- c(1:sizet[[1]][["n"]])
stats_Totales_Jugador$Indice <- c(1:sizet2[[1]][["n"]])
VyL$Indice <- c(1:sizet2[[1]][["n"]])
TablaJugadores = merge(stats_Totales_Jugador,VyL,by = index)
View(TablaJugadores)
TablaJugadores =inner_join(stats_Totales_Jugador,VyL)
View(TablaJugadores)
AAlistaJugadores <- list()
AAListaStatsJugadores <- list()
sizet2 <-list(count(VyL))
#AAFAKEListaJugadores[[1]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==VyL["Name"][1,])
for (i in 1:sizet2[[1]][["n"]]) {
  AAlistaJugadores[[i]] <-  filter(finalPLUSPLUS,finalPLUSPLUS$player==VyL["Name"][i,])
  AAListaStatsJugadores[[i]] <- list( mean(AAlistaJugadores[[i]]$minutes), mean(AAlistaJugadores[[i]]$points), mean(AAlistaJugadores[[i]]$tot_rebounds),mean(AAlistaJugadores[[i]]$assists) ,sum(AAlistaJugadores[[i]]$points) )
}
AAstats_Totales_Jugador <- data.frame(minutos=numeric(), puntos=numeric(), rebotes=numeric(), asistenicas=numeric(), sumaTotalPuntos=numeric())
for (i in 1:sizet2[[1]][["n"]]) {
  AAstats_Totales_Jugador[i, ] <- AAListaStatsJugadores[[i]]
}
TablaJugadores <- list()
AAstats_Totales_Jugador$Indice <- c(1:sizet2[[1]][["n"]])
VyL$Indice <- c(1:sizet2[[1]][["n"]])
TablaJugadores = merge(VyL,AAstats_Totales_Jugador,by = "Indice")
View(TablaJugadores)
saveRDS(TablaJugadores,'TablaJugadoresACBStats2Marzo')
pr2 <- TablaJugadores %>% filter(str_detect(Name, "Bolmaro"))
View(pr2)
pr2 <- TablaJugadores %>% filter(str_detect(Name, "Mirotic"))
View(pr2)
