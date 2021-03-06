# LICENCE ----
#
# Copyright 2018 Clement POIRET.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Initial setup ----

rm(list=ls())

library(readr)
library(tidyverse)
library(fmsb)
library(data.table)

setwd('~/Documents/Data/Université/Ultimate-R-Report')
numbers_of_players = 18

# Reading and cleaning data ----

df <- read.csv('Data/ultimate.csv', header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1", sep = ";")

cnames <- c('Laps',
            'Player',
            'Length',
            'Distance',
            'VMax',
            'Nb.Acc',
            'Amax',
            'FC.Moy',
            'FC.Max',
            'Nb.Sol',
            'Nb.Sauts',
            'Nb.Chocs',
            'DA1',
            'DA2',
            'Bat',
            'Signal',
            'Nb.Dec',
            'Nb.Chg.Dir',
            'TZV1',
            'TZV2',
            'TZV3',
            'TZV4',
            'TZV5',
            'DPZV1',
            'DPZV2',
            'DPZV3',
            'DPZV4',
            'DPZV5',
            'Var.RR.HF.Moy',
            'Var.RR.BR.Moy',
            'HF.BF.Moy',
            'D.GPS',
            'Nb.Chocs2',
            'D.Seance',
            'D.Data',
            'D.Cardio',
            'Perte',
            'V.Bat',
            'D.Moy.Acc',
            'H.Moy.Moy',
            'H.Max.Sauts',
            'NA')
colnames(df) <- cnames
rm(cnames)

df.team <- data.table(Equipe = c("Orange", "Orange", "Orange", "Orange", "Orange", "Orange", "Bleu", "Bleu", "Bleu", "Bleu", "Bleu", "Bleu", "Jaune", "Jaune", "Jaune", "Jaune", "Jaune", "Jaune"),
                        Player = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15", "16", "17", "18"))

# Make data uniform (cf. df[,1])
i <- 1
while (i <= nrow(df)) {
  print(i)
  min = i
  max = i + numbers_of_players + 1
  df[min:max,1] <- df[min,1]
  i = i + numbers_of_players + 2
}

# Remove unneeded multiple headers
i <- 1
while (i < nrow(df)) {
  df <- df[-i,]
  i = i + numbers_of_players + 1
}

# Some minor changes 
god <- df[1,"Player"]
for (i in 1:nrow(df)) {
  if (df[i,"Player"] != god) {
    df[i,"Player"] = str_extract_all(df[i,"Player"], "[0-9]+")[[1]]
  } else {
    df[i,"Player"] = 'Mean'
  }
}

df <- left_join(df, df.team)
df$Length = as.POSIXct(df$Length, format = "%H:%M:%S")
df$Length = (minute(df$Length)*60+second(df$Length))

# Clean up
rm(i, max, min, god)

# Creating new df with only relevant laps ----

real_laps = c('Laps 3',
              'Laps 5',
              'Laps 7',
              'Laps 9',
              'Laps 11',
              'Laps 13',
              'Laps 15',
              'Laps 17',
              'Laps 19',
              'Laps 21',
              'Laps 23',
              'Laps 25')

df.real <- NULL
for (i in 1:length(real_laps)) {
  df.real <- rbind(df.real, df[df$Laps %in% real_laps[i],])
}

rm(i, real_laps)

# Summarise ----

# Summarized df using Dplyr
df.total <- df.real[df.real$Player!='Mean',] %>%
  group_by(Player) %>%
  summarise(TotalDistance = sum(as.numeric(as.character(Distance))),
            TotalTime = sum(Length),
            MeanDistance = mean(as.numeric(as.character(Distance))),
            Nb.Acc = sum(as.numeric(as.character(Nb.Acc))),
            DPZV1 = sum(as.numeric(as.character(DPZV1))),
            DPZV2 = sum(as.numeric(as.character(DPZV2))),
            DPZV3 = sum(as.numeric(as.character(DPZV3))),
            DPZV4 = sum(as.numeric(as.character(DPZV4))),
            TZV1 = sum(as.numeric(as.character(TZV1))),
            TZV2 = sum(as.numeric(as.character(TZV2))),
            TZV3 = sum(as.numeric(as.character(TZV3))),
            TZV4 = sum(as.numeric(as.character(TZV4))), 
            Team = unique(as.character(Equipe))) %>%
  arrange(Player)

# Comparing Player 17 (Best), with max/min values and Player 8
max = data.frame(cbind('Max', max(df.total$DPZV1),
            max(df.total$DPZV2),
            max(df.total$DPZV3),
            max(df.total$DPZV4)), stringsAsFactors = F)

min = data.frame(cbind('Min', min(df.total$DPZV1),
            min(df.total$DPZV2),
            min(df.total$DPZV3),
            min(df.total$DPZV4)), stringsAsFactors = F)

mean.dpzv <- data.frame(cbind('Mean',
                   mean(df.total$DPZV1),
                   mean(df.total$DPZV2),
                   mean(df.total$DPZV3),
                   mean(df.total$DPZV4)), stringsAsFactors = F)

prof17 <- data.frame(df.total[17,c(1,6:9)], stringsAsFactors = F)
prof8 <- data.frame(df.total[8,c(1,6:9)], stringsAsFactors = F)

colnames(mean.dpzv) <- colnames(prof17)
colnames(max) <- colnames(prof17)
colnames(min) <- colnames(prof17)

df.profile.17 <- rbind(max, min, prof17, prof8, mean.dpzv)
rownames(df.profile.17) <- c('Max', 'Min', '17', '8', 'Mean')
df.profile.17 <- as.matrix(df.profile.17[,-1])
class(df.profile.17) <- "numeric"

rm(max, mean.dpzv, min, prof17, prof8)

# Exporting df to use other softwares if needed ----

write.csv(df, 'clean.csv')
write.csv(df.profile.17, 'player17vs8.csv')
write.csv(df.real, 'playedlaps.csv')
write.csv(df.total, 'summarized.csv')

# Plotting WIP ----

radarchart(as.data.frame(df.profile.17), plwd=1, axistype=2,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8)

ggplot(data = df.total,
       aes(x = Player, y = TotalDistance)) +
  geom_point() +
  geom_hline(yintercept = mean(df.total$TotalDistance), linetype="dashed", color = "red")

colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( as.data.frame(df.profile.17)  , axistype=1 ,
            #custom polygon
            pcol=colors_in , pfcol=colors_in , plwd=0.1, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,600,150), cglwd=0.8,
            #custom labels
            vlcex=0.8
)

tmp <- t(df.profile.17)
write.csv(tmp, 'player17vs8_2.csv')
