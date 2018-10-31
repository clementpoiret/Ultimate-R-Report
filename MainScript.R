# Initial setup ----

rm(list=ls())

library(readr)
library(tidyverse)
library(fmsb)

setwd('~/Documents/Data/Université/Ultimate')
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

df.real <- df[df$Laps %in% real_laps[i],]

df.real <- NULL
for (i in 1:length(real_laps)) {
  df.real <- rbind(df.real, df[df$Laps %in% real_laps[i],])
}

# Summarise ----

# Summarized df using Dplyr
df.total <- df.real[df.real$Player!='Mean',] %>%
  group_by(Player) %>%
  summarise(TotalDistance = sum(as.numeric(as.character(Distance))),
            MeanDistance = mean(as.numeric(as.character(Distance))),
            Nb.Acc = sum(as.numeric(as.character(Nb.Acc))),
            DPZV1 = sum(as.numeric(as.character(DPZV1))),
            DPZV2 = sum(as.numeric(as.character(DPZV2))),
            DPZV3 = sum(as.numeric(as.character(DPZV3))),
            DPZV4 = sum(as.numeric(as.character(DPZV4)))) %>%
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

prof17 <- data.frame(df.total[17,c(1,5:8)], stringsAsFactors = F)
prof8 <- data.frame(df.total[8,c(1,5:8)], stringsAsFactors = F)

colnames(mean.dpzv) <- colnames(prof17)
colnames(max) <- colnames(prof17)
colnames(min) <- colnames(prof17)

df.profile.17 <- rbind(max, min, prof17, prof8, mean.dpzv)
rownames(df.profile.17) <- c('Max', 'Min', '17', '8', 'Mean')
df.profile.17 <- as.matrix(df.profile.17[,-1])
class(df.profile.17) <- "numeric"

# Plotting ----

radarchart(as.data.frame(df.profile.17))

ggplot(data = df.total,
       aes(x = Player, y = TotalDistance)) +
  geom_point() +
  geom_hline(yintercept = mean(df.total$TotalDistance), linetype="dashed", color = "red")

# Plot 2: Same plot with custom features
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( as.data.frame(df.profile.17)  , axistype=1 ,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,600,150), cglwd=0.8,
            #custom labels
            vlcex=0.8
)
legend(x=1, y=1, legend = rownames(as.data.frame(df.profile.17)[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", pt.cex=3)
