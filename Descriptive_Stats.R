##Input and Tidy Data

library(tidyverse)
setwd("~/R/Peru_trees")

#USE THIS ONE - ALL CLEAN AND CORRECT - added 'class' column and omitted Transects EVR6 and EVR5
trees <- read.table(file = "treeswclass.csv", sep = ",", header = T)



summary(trees)
str(trees)




#####Summary Stats DBH and Height by Site  - Nice Table******
library(psych)
stats <- describeBy(DBH + Height ~ hab + dis, data = trees)
stats #cannot change to a data frame or tibble


#HABITAT

#Upland - 320 + 1047 = 1367 stems
#Flooded - 197+402 = 599
#White Sand - 181+791 = 972
1367+599+972
#Total: 2938 stems and 46 NAs (from DBH column) = 2984

# % Habitat Stems
1367/2984 * 100 #45.81% Upland - aka 46% of the trees surveyed were upland trees
599/2984 * 100 #20.07% Flooded
972/2984 * 100 # 32.57% White Sand
46/2984 * 100 #1.54% NA



#DISTURBANCE

#Disturbed = 197+320+181 = 698
#Primary = 402+1047+791 = 2240
698+2240
#Total: 2938 stems and 46 NAs

# % Disturbed and % Primary Stems
698/2984 *100 #23.39% Disturbed
2240/2984*100 #75.07% Primary
46/2984 #0.0154% NA

# % Class Stems (data from stats object)
197/2984 * 100 # 6.60% FD
320/2984 * 100 # 10.72% UD
181/2984 *100 # 6.07% WSD
402/2984 * 100 # 13.47% FP
1047/2984 *100 # 35.09% UP
791/2984 * 100 # 26.51% WSP
46/2984 * 100 #1.54 NA

#Descriptive Stats DBH and Height
summary(trees$DBH) #mean DBH 8.393cm
summary(trees$Height) #mean height 6.645cm

# Smallest and Largest DBH
which.min(trees$DBH)
trees[1835,] #Primary Upland, Transect DZ2, Vantanea guianensis, DBH = 2.1

which.max(trees$DBH)
trees[148,]
#Primary Flooded Transect JH-02, Sapium marmieri, DBH = 90


# Tallest and shortest Height
which.min(trees$Height)
trees[1569,] #Primary Flooded Transect AR1, Rudgea verticillata, Height = 1.5m

which.max(trees$Height)
trees[1530,] #Primary Flooded Transect AR1, Sommera sabiceoides, Height = 25m. 

#The tallest and shortest trees are in the same transect, and the largest DBH is in the same class type.


# HISTOGRAMS
library(ggplot2)

#Simplest Histogram
histdbhsimp <- ggplot(trees) +
  geom_histogram(aes(x = DBH, fill = dis), binwidth = 5) + 
  facet_wrap(~ hab, scales = "fixed") +
  theme(legend.title = element_blank()) +
  labs( x = "DBH (cm)", y = "Frequency")
histdbhsimp



# Histogram of DBH per Hab AND Dis
dbh_hab_dis <- ggplot(trees) +
  geom_histogram(aes(x = DBH, fill = dis), binwidth  = 5) + 
  facet_grid(dis ~ hab, scale = "free_y") +
  
  labs( x = "DBH (cm)", y = "Frequency") +
  
  theme(strip.background = element_rect(colour = "black", fill = "white")) + 
  theme(legend.title = element_blank())
print(dbh_hab_dis)