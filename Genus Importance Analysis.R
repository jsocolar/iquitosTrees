#Input Data

library(tidyverse)
library(dplyr)
setwd("~/R/Peru_trees")

trees <- read.table(file = "treeswclass.csv", sep = ",", header = T)

#Trimmed and arranged data set
trees %>% 
  select(ID, Site, Transect, Point, Genus, N, DBH, hab, dis, class) %>% 
  arrange(class) -> trees.g.ivi #Note - arranged by class so transects all pooled

#Basal Area added (grouped by class)
trees.g.ivi %>% 
  group_by(class) %>% 
  mutate(Basal.Area = pi * ((DBH/2)^2)) -> trees.g.ivi #Now have Basal Areas! Same as family


# -- Computing Relative Freq of Genuses
# Rel Freq of each genus in Upland Primary
trees.g.ivi.up <- filter(trees.g.ivi, hab == "Upland", dis == "Primary")
rfg.up <- table(trees.g.ivi.up$Genus)/length(trees.g.ivi.up$Genus)
as.data.frame(rfg.up) -> rfg.up
rfg.up <- rename(rfg.up, Genus = Var1)
rfg.up$class <- 'Primary.Upland'

# Rel Freq of each genus in Upland Disturbed
trees.g.ivi.ud <- filter(trees.g.ivi, hab == "Upland", dis == "Disturbed")
rfg.ud <- table(trees.g.ivi.ud$Genus)/length(trees.g.ivi.ud$Genus)
as.data.frame(rfg.ud) -> rfg.ud
rfg.ud <- rename(rfg.ud, Genus = Var1)
rfg.ud$class <- 'Disturbed.Upland'

# Rel Freq of each genus in Flooded Disturbed
trees.g.ivi.fd <- filter(trees.g.ivi, hab == "Flooded", dis == "Disturbed")
rfg.fd <- table(trees.g.ivi.fd$Genus)/length(trees.g.ivi.fd$Genus)
as.data.frame(rfg.fd) -> rfg.fd
rfg.fd <- rename(rfg.fd, Genus = Var1)
rfg.fd$class <- 'Disturbed.Flooded'


# Rel Freq of each genus in Flooded Primary
trees.g.ivi.fp <- filter(trees.g.ivi, hab == "Flooded", dis == "Primary")
rfg.fp <- table(trees.g.ivi.fp$Genus)/length(trees.g.ivi.fp$Genus)
as.data.frame(rfg.fp) -> rfg.fp
rfg.fp <- rename(rfg.fp, Genus = Var1)
rfg.fp$class <- 'Primary.Flooded'

# Rel Freq of each genus in White.Sand Disturbed
trees.g.ivi.wd <- filter(trees.g.ivi, hab == "White.Sand", dis == "Disturbed")
rfg.wd <- table(trees.g.ivi.wd$Genus)/length(trees.g.ivi.wd$Genus)
as.data.frame(rfg.wd) -> rfg.wd
rfg.wd <- rename(rfg.wd, Genus = Var1)
rfg.wd$class <- 'Disturbed.White.Sand'


# Rel Freq of each genus in White.Sand Primary
trees.g.ivi.wp <- filter(trees.g.ivi, hab == "White.Sand", dis == "Primary")
rfg.wp <- table(trees.g.ivi.wp$Genus)/length(trees.g.ivi.wp$Genus)
as.data.frame(rfg.wp) -> rfg.wp
rfg.wp <- rename(rfg.wp, Genus = Var1)
rfg.wp$class <- 'Primary.White.Sand'


# All Rel Frequencies*****
rel.freq.tot.g <- rbind(rfg.fd, rfg.ud, rfg.wd, rfg.fp, rfg.up, rfg.wp)
rel.freq.tot.g


## -- Computing Relative Dominance of Genuses

# RD = sum(Basal Area of 1 Tree Genus/ Basal Area of all tree Genus) PER CLASS

##Genus basal area per class
trees.g.ivi %>% 
  group_by(class, Genus) %>% 
  mutate(G.BA = sum(Basal.Area)) -> trees.g.ivi #each family has its own basal area per class
trees.g.ivi[is.na(trees.g.ivi)] = 0 #convert NA to 0 for math

##Total Basal Area (per class)
sum(trees.ivi$Basal.Area) #Total BA is 327081.6
trees.g.ivi %>% 
  group_by(class) %>% 
  mutate(Tot.BA = sum(Basal.Area)) -> trees.g.ivi # now total BA per class


#Relative Dominance - Fam BA/Tot BA
trees.g.ivi$Rel.Dom <- trees.g.ivi$G.BA / trees.g.ivi$Tot.BA


#Now I  to filter out the excess so there is just one entry for each genus in each class

trees.g.ivi %>% 
  select(Genus, class, G.BA, Tot.BA, Rel.Dom) %>% 
  unique() -> trees.g.ivi.dom

trees.g.ivi.dom <- with(trees.g.ivi.dom, trees.g.ivi.dom[order(class, Genus) , ])

#check
subset(trees.g.ivi.dom, class == "Primary.Upland") -> g
sum(g$Rel.Dom) # = 1 :)

#Now should bind with rel.freq.tot
cbind(rel.freq.tot.g, trees.g.ivi.dom) -> gen.importance #classes and genuses aligned! now filter excess columns

#Get rid of duplicate columns
gen.importance <- gen.importance[!duplicated(as.list(gen.importance))]

gen.importance %>% 
  select(Genus, Freq, class, G.BA, Tot.BA, Rel.Dom) -> gen.importance #duplicate columns now removed


## -- Computing Importance Value
# IV = Rel. Freq + Rel. Dom
gen.importance$IV <- gen.importance$Freq + gen.importance$Rel.Dom



## FINAL TABLES
#flip class and Freq column order
gen.importance <- gen.importance[, c(1,3,2,4,5,6,7)]

##Make separate objects for each class and descending IV column
#DF Final
gen.imp.df <- subset(gen.importance, class == "Disturbed.Flooded")
gen.imp.df[with(gen.imp.df, order(-IV)), ] -> gen.imp.df

#DU Final
gen.imp.du <- subset(gen.importance, class == "Disturbed.Upland")
gen.imp.du[with(gen.imp.du, order(-IV)), ] -> gen.imp.du

#DWS Final
gen.imp.dw <- subset(gen.importance, class == "Disturbed.White.Sand")
gen.imp.dw[with(gen.imp.dw, order(-IV)), ] -> gen.imp.dw

#PF Final
gen.imp.pf <- subset(gen.importance, class == "Primary.Flooded")
gen.imp.pf[with(gen.imp.pf, order(-IV)), ] -> gen.imp.pf

#PU Final
gen.imp.pu <- subset(gen.importance, class == "Primary.Upland")
gen.imp.pu[with(gen.imp.pu, order(-IV)), ] -> gen.imp.pu

#PWS Final
gen.imp.pw <- subset(gen.importance, class == "Primary.White.Sand")
gen.imp.pw[with(gen.imp.pw, order(-IV)), ] -> gen.imp.pw