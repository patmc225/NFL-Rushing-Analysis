

pbp = load_pbp(2022)

na <- pbp |>
  filter(rush == 1, is.na(run_location)) |>
  select(posteam, defteam, desc, down, ydstogo, yards_gained, run_gap, run_location)


orushes <- pbp |>
  filter(rush == 1) |>
  group_by(posteam) |>
  summarize(total = n())
drushes <- pbp |>
  filter(rush == 1) |>
  group_by(defteam) |>
  summarize(total = n())


le <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "left")
  average = mean(le$epa)
  sd = sd(le$epa)
  le <- le |>
  group_by(team = defteam, type = "le") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  le$attper <- (le$attempts / le$total)*100.0
  le$percentile <- 1 - pnorm((le$epa-average)/sd)
  le$rank<-rank(le$epa)
 
  
lt <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "left")
  average = mean(lt$epa)
  sd = sd(lt$epa)
  lt <- lt |>
  group_by(team = defteam, type = "lt") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  lt$attper <- (lt$attempts / lt$total)*100.0
  lt$percentile <- 1 - pnorm((lt$epa-average)/sd)
  lt$rank<- rank(lt$epa)

lg <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "left")
  average = mean(lg$epa)
  sd = sd(lg$epa)
  lg <- lg |>
  group_by(team = defteam, type = "lg") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  lg$attper <- (lg$attempts / lg$total)*100.0
  lg$percentile <- 1 - pnorm((lg$epa-average)/sd)
  lg$rank<-rank(lg$epa)

c <- pbp |>
  filter(rush == 1, !is.na(epa), is.na(run_gap), run_location == "middle")
  average = mean(c$epa)
  sd = sd(c$epa)
  c <- c |>
  group_by(team = defteam, type = "c") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  c$attper <- (c$attempts / c$total)*100.0
  c$percentile <- 1 - pnorm((c$epa-average)/sd)
  c$rank<- rank(c$epa)

rg <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "right")
  average = mean(rg$epa)
  sd = sd(rg$epa)
  rg <- rg |>
  group_by(team = defteam, type = "rg") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  rg$attper <- (rg$attempts / rg$total)*100.0
  rg$percentile <- 1 - pnorm((rg$epa-average)/sd)
  rg$rank<-rank(rg$epa)

rt <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "right")
  average = mean(rt$epa)
  sd = sd(rt$epa)
  rt <- rt |>
  group_by(team = defteam, type = "rt") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  rt$attper <- (rt$attempts / rt$total)*100.0
  rt$percentile <- 1 - pnorm((rt$epa-average)/sd)
  rt$rank<- rank(rt$epa)

re <- pbp |>
  filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "right")
  average = mean(re$epa)
  sd = sd(re$epa)
  re <- re |>
  group_by(team = defteam, type = "re") |>
  summarize(epa = mean(epa), attempts = n()) |>
  left_join(rushes, by = c("team"="defteam"))
  re$attper <- (re$attempts / re$total)*100.0
  re$percentile <- 1 - pnorm((re$epa-average)/sd)
  re$rank<-rank(re$epa)

ovrD <- pbp |>
  filter(rush == 1, !is.na(epa)) 
  average = mean(ovrD$epa)
  sd = sd(ovrD$epa)
  ovrD <- ovrD |>
  group_by(team = defteam, type = "ovr") |>
  summarize(epa = mean(epa), attempts = n())
  ovrD$total <- ovrD$attempts
  ovrD$attper <- (ovrD$attempts / ovrD$attempts)*100.0
  ovrD$percentile <- 1 - pnorm((ovrD$epa-average)/sd)
  ovrD$rank<-rank(ovrD$epa)

  ovrD <- rbind(ovrD, re)
  ovrD <- rbind(ovrD, rt)
  ovrD <- rbind(ovrD, rg)
  ovrD <- rbind(ovrD, c)
  ovrD <- rbind(ovrD, lg)
  ovrD <- rbind(ovrD, lt)
  ovrD <- rbind(ovrD, le)
  
  
  
  rushes <- pbp |>
    filter(rush == 1) |>
    group_by(posteam) |>
    summarize(total = n())
  
  le <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "left")
  average = mean(le$epa)
  sd = sd(le$epa)
  le <- le |>
    group_by(team = posteam, type = "le") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  le$attper <- (le$attempts / le$total)*100.0
  le$percentile <- pnorm((le$epa-average)/sd)
  le$rank<-(32-rank(le$epa))+1
  
  
  lt <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "left")
  average = mean(lt$epa)
  sd = sd(lt$epa)
  lt <- lt |>
    group_by(team = posteam, type = "lt") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  lt$attper <- (lt$attempts / lt$total)*100.0
  lt$percentile <- pnorm((lt$epa-average)/sd)
  lt$rank<-(32-rank(lt$epa))+1
  
  lg <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "left")
  average = mean(lg$epa)
  sd = sd(lg$epa)
  lg <- lg |>
    group_by(team = posteam, type = "lg") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  lg$attper <- (lg$attempts / lg$total)*100.0
  lg$percentile <- pnorm((lg$epa-average)/sd)
  lg$rank<-(32-rank(lg$epa))+1
  
  c <- pbp |>
    filter(rush == 1, !is.na(epa), is.na(run_gap), run_location == "middle")
  average = mean(c$epa)
  sd = sd(c$epa)
  c <- c |>
    group_by(team = posteam, type = "c") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  c$attper <- (c$attempts / c$total)*100.0
  c$percentile <- pnorm((c$epa-average)/sd)
  c$rank<-(32-rank(c$epa))+1
  
  rg <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "right")
  average = mean(rg$epa)
  sd = sd(rg$epa)
  rg <- rg |>
    group_by(team = posteam, type = "rg") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  rg$attper <- (rg$attempts / rg$total)*100.0
  rg$percentile <- pnorm((rg$epa-average)/sd)
  rg$rank<-(32-rank(rg$epa))+1
  
  rt <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "right")
  average = mean(rt$epa)
  sd = sd(rt$epa)
  rt <- rt |>
    group_by(team = posteam, type = "rt") |>
    summarize(epa = mean(epa), attempts = n()) |>
    left_join(rushes, by = c("team"="posteam"))
  rt$attper <- (rt$attempts / rt$total)*100.0
  rt$percentile <- pnorm((rt$epa-average)/sd)
  rt$rank<-(32-rank(rt$epa))+1
  
  ore <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "right")
  average = mean(ore$epa)
  sd = sd(ore$epa)
  ore <- ore |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OREattper <- round( 100*(ore$attempts / ore$total) , 1)
  re$OREpercentile <- round( 100*pnorm((ore$epa-average)/sd) , 1)
  re$ORErank<-(32-rank(ore$epa))+1
  ore <- ore[,-2:-4]
  
  ort <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "right")
  average = mean(ort$epa)
  sd = sd(ort$epa)
  ort <- ort |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$ORTattper <- round( 100*(ort$attempts / ort$total) , 1)
  re$ORTpercentile <- round( 100*pnorm((ort$epa-average)/sd) , 1)
  re$ORTrank<-(32-rank(ort$epa))+1
  ort <- ort[,-2:-4]
  
  org <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "right")
  average = mean(org$epa)
  sd = sd(org$epa)
  org <- org |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$ORGattper <- round( 100*(org$attempts / org$total) , 1)
  re$ORGpercentile <- round( 100*pnorm((org$epa-average)/sd) , 1)
  re$ORGrank<-(32-rank(org$epa))+1
  org <- org[,-2:-4]
  
  oc <- pbp |>
    filter(rush == 1, !is.na(epa), is.na(run_gap), run_location == "middle")
  average = mean(oc$epa)
  sd = sd(oc$epa)
  oc <- oc |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OCattper <- round( 100*(oc$attempts / oc$total) , 1)
  re$OCpercentile <- round( 100*pnorm((oc$epa-average)/sd) , 1)
  re$OCrank<-(32-rank(oc$epa))+1
  oc <- oc[,-2:-4]
  
  olg <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "left")
  average = mean(olg$epa)
  sd = sd(olg$epa)
  olg <- olg |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OLGattper <- round( 100*(olg$attempts / olg$total) , 1)
  re$OLGpercentile <- round( 100*pnorm((olg$epa-average)/sd) , 1)
  re$OLGrank<-(32-rank(olg$epa))+1
  olg <- olg[,-2:-4]
  
  olt <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "left")
  average = mean(olt$epa)
  sd = sd(olt$epa)
  olt <- olt |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OLTattper <- round( 100*(olt$attempts / olt$total) , 1)
  re$OLTpercentile <- round( 100*pnorm((olt$epa-average)/sd) , 1)
  re$OLTrank<-(32-rank(olt$epa))+1
  olt <- olt[,-2:-4]
  
  ole <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "left")
  average = mean(ole$epa)
  sd = sd(ole$epa)
  ole <- ole |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OLEattper <- round( 100*(ole$attempts / ole$total) , 1)
  re$OLEpercentile <- round( 100*pnorm((ole$epa-average)/sd) , 1)
  re$OLErank<-(32-rank(ole$epa))+1
  ole <- ole[,-2:-4]
  
  oovr <- pbp |>
    filter(rush == 1, !is.na(epa))
  average = mean(oovr$epa)
  sd = sd(oovr$epa)
  oovr <- oovr |>
    group_by(team = posteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(orushes, by = c("team"="posteam"))
  re$OOVRatt <- oovr$attempts
  re$OOVRpercentile <- round( 100*pnorm((oovr$epa-average)/sd) , 1)
  re$OOVRrank<-(32-rank(oovr$epa))+1
  oovr <- oovr[,-2:-4]
  
  dre <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "right")
  average = mean(dre$epa)
  sd = sd(dre$epa)
  dre <- dre |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DREattper <- round( 100*(dre$attempts / dre$total) , 1)
  re$DREpercentile <- 100-round( 100*pnorm((dre$epa-average)/sd) , 1)
  re$DRErank<-rank(dre$epa)
  dre <- dre[,-2:-4]
  
  drt <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "right")
  average = mean(drt$epa)
  sd = sd(drt$epa)
  drt <- drt |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DRTattper <- round( 100*(drt$attempts / drt$total) , 1)
  re$DRTpercentile <- 100-round( 100*pnorm((drt$epa-average)/sd) , 1)
  re$DRTrank<-rank(drt$epa)
  drt <- drt[,-2:-4]
  
  drg <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "right")
  average = mean(drg$epa)
  sd = sd(drg$epa)
  drg <- drg |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DRGattper <- round( 100*(drg$attempts / drg$total) , 1)
  re$DRGpercentile <- 100- round( 100*pnorm((drg$epa-average)/sd) , 1)
  re$DRGrank<-rank(drg$epa)
  drg <- drg[,-2:-4]
  
  dc <- pbp |>
    filter(rush == 1, !is.na(epa), is.na(run_gap), run_location == "middle")
  average = mean(dc$epa)
  sd = sd(dc$epa)
  dc <- dc |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DCattper <- round( 100*(dc$attempts / dc$total) , 1)
  re$DCpercentile <- 100-round( 100*pnorm((dc$epa-average)/sd) , 1)
  re$DCrank<-rank(dc$epa)
  dc <- dc[,-2:-4]
  
  dlg <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "guard", run_location == "left")
  average = mean(dlg$epa)
  sd = sd(dlg$epa)
  dlg <- dlg |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DLGattper <- round( 100*(dlg$attempts / dlg$total) , 1)
  re$DLGpercentile <- 100-round( 100*pnorm((dlg$epa-average)/sd) , 1)
  re$DLGrank<-rank(dlg$epa)
  dlg <- dlg[,-2:-4]
  
  dlt <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "tackle", run_location == "left")
  average = mean(dlt$epa)
  sd = sd(dlt$epa)
  dlt <- dlt |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DLTattper <- round( 100*(dlt$attempts / dlt$total) , 1)
  re$DLTpercentile <- 100- round( 100*pnorm((dlt$epa-average)/sd) , 1)
  re$DLTrank<- rank(dlt$epa)
  dlt <- dlt[,-2:-4]
  
  dle <- pbp |>
    filter(rush == 1, !is.na(epa), run_gap == "end", run_location == "left")
  average = mean(dle$epa)
  sd = sd(dle$epa)
  dle <- dle |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DLEattper <- round( 100*(dle$attempts / dle$total) , 1)
  re$DLEpercentile <- 100-round( 100*pnorm((dle$epa-average)/sd) , 1)
  re$DLErank<-rank(dle$epa)
  dle <- dle[,-2:-4]
  
  dovr <- pbp |>
    filter(rush == 1, !is.na(epa))
  average = mean(dovr$epa)
  sd = sd(dovr$epa)
  dovr <- dovr |>
    group_by(team = defteam) |>
    summarize(epa = mean(epa),attempts = n()) |>
    left_join(drushes, by = c("team"="defteam"))
  re$DOVRatt <- dovr$attempts
  re$DOVRpercentile <- 100 - round( 100*pnorm((dovr$epa-average)/sd) , 1)
  re$DOVRrank<- rank(dovr$epa)
  dovr <- dovr[,-2:-4]
  re <- re[-re$DRTatt]
  
  write.csv(re, "final.csv", row.names=TRUE)
  
  
  
  ovr <- pbp |>
    filter(rush == 1, !is.na(epa)) 
  average = mean(ovr$epa)
  sd = sd(ovr$epa)
  ovr <- ovr |>
    group_by(team = posteam, type = "ovr") |>
    summarize(epa = mean(epa), attempts = n())
  ovr$total <- ovr$attempts
  ovr$attper <- (ovr$attempts / ovr$attempts)*100.0
  ovr$percentile <- pnorm((ovr$epa-average)/sd)
  ovr$rank<-(32-rank(ovr$epa))+1
  
  ovr <- rbind(ovr, re)
  ovr <- rbind(ovr, rt)
  ovr <- rbind(ovr, rg)
  ovr <- rbind(ovr, c)
  ovr <- rbind(ovr, lg)
  ovr <- rbind(ovr, lt)
  ovr <- rbind(ovr, le)

  write.xml(ovr, "ovr.csv", row.names=TRUE)
  