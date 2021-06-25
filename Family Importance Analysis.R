#Input Data

library(tidyverse)
library(dplyr)
setwd("~/R/Peru_trees")

trees <- read.table(file = "treeswclass.csv", sep = ",", header = T)


#Trimmed and arranged data set
trees %>% 
  select(ID, Site, Transect, Point, Family, N, DBH, hab, dis, class) %>% 
  arrange(class) -> trees.ivi #Note - arranged by class so transects all pooled


#Basal Area added (grouped by class)
trees.ivi %>% 
  group_by(class) %>% 
  mutate(Basal.Area = pi * ((DBH/2)^2)) -> trees.ivi #Now have Basal Areas!



#-----Computing Relative Frequency of Families

# Rel Freq of each family in Upland Primary
trees.ivi.up <- filter(trees.ivi, hab == "Upland", dis == "Primary")
rf.up <- table(trees.ivi.up$Family)/length(trees.ivi.up$Family)
as.data.frame(rf.up) -> rf.up
rf.up <- rename(rf.up, Family = Var1)
rf.up$class <- 'Primary.Upland'

# Rel Freq of each family in Upland Disturbed
trees.ivi.ud <- filter(trees.ivi, hab == "Upland", dis == "Disturbed")
rf.ud <- table(trees.ivi.ud$Family)/length(trees.ivi.ud$Family)
as.data.frame(rf.ud) -> rf.ud
rf.ud <- rename(rf.ud, Family = Var1)
rf.ud$class <- 'Disturbed.Upland'

# Rel Freq of each family in Flooded Disturbed
trees.ivi.fd <- filter(trees.ivi, hab == "Flooded", dis == "Disturbed")
rf.fd <- table(trees.ivi.fd$Family)/length(trees.ivi.fd$Family)
as.data.frame(rf.fd) -> rf.fd
rf.fd <- rename(rf.fd, Family = Var1)
rf.fd$class <- 'Disturbed.Flooded'


# Rel Freq of each family in Flooded Primary
trees.ivi.fp <- filter(trees.ivi, hab == "Flooded", dis == "Primary")
rf.fp <- table(trees.ivi.fp$Family)/length(trees.ivi.fp$Family)
as.data.frame(rf.fp) -> rf.fp
rf.fp <- rename(rf.fp, Family = Var1)
rf.fp$class <- 'Primary.Flooded'

# Rel Freq of each family in White.Sand Disturbed
trees.ivi.wd <- filter(trees.ivi, hab == "White.Sand", dis == "Disturbed")
rf.wd <- table(trees.ivi.wd$Family)/length(trees.ivi.wd$Family)
as.data.frame(rf.wd) -> rf.wd
rf.wd <- rename(rf.wd, Family = Var1)
rf.wd$class <- 'Disturbed.White.Sand'


# Rel Freq of each family in White.Sand Primary
trees.ivi.wp <- filter(trees.ivi, hab == "White.Sand", dis == "Primary")
rf.wp <- table(trees.ivi.wp$Family)/length(trees.ivi.wp$Family)
as.data.frame(rf.wp) -> rf.wp
rf.wp <- rename(rf.wp, Family = Var1)
rf.wp$class <- 'Primary.White.Sand'


# All Rel Frequencies*****
rel.freq.tot <- rbind(rf.fd, rf.ud, rf.wd, rf.fp, rf.up, rf.wp)
rel.freq.tot
#242 Families because per class, not in general (75 families in general)



# -- Computing Relative Dominance of Families
# RD = sum(Basal Area of 1 Tree Fam/ Basal Area of all tree fams) PER CLASS

##Family basal area per class
trees.ivi %>% 
  group_by(class, Family) %>% 
  mutate(F.BA = sum(Basal.Area)) -> trees.ivi #each family has its own basal area per class
trees.ivi[is.na(trees.ivi)] = 0 #convert NA to 0 for math


##Total Basal Area (per class)
sum(trees.ivi$Basal.Area) #Total BA is 327081.6
trees.ivi %>% 
  group_by(class) %>% 
  mutate(Tot.BA = sum(Basal.Area)) -> trees.ivi # now total BA per class

  ####CHECK FOR TOTAL BA per class
#total basal area for DF
trees.ivi %>% 
  subset(class == "Disturbed.Flooded") -> DF.Tot.BA
DF.Tot.BA %>% mutate(Tot.BA = sum(DF.Tot.BA$Basal.Area)) -> DF.Tot.BA #class total = 13645.56

#total DU
DU.Tot.BA <- subset(trees.ivi, class == "Disturbed.Upland")
DU.Tot.BA %>% mutate(Tot.BA = sum(DU.Tot.BA$Basal.Area)) -> DU.Tot.BA #class total = 25246.81

#total DWS
DWS.Tot.BA <- subset(trees.ivi, class == "Disturbed.White.Sand")
DWS.Tot.BA %>% mutate(Tot.BA = sum(DWS.Tot.BA$Basal.Area)) -> DWS.Tot.BA #class total = 8059.302

#total PF
PF.Tot.BA <- subset(trees.ivi, class == "Primary.Flooded")
PF.Tot.BA %>% mutate(Tot.BA = sum(PF.Tot.BA$Basal.Area)) -> PF.Tot.BA #class total = 82457.74

#total PU
PU.Tot.BA <- subset(trees.ivi, class == "Primary.Upland")
PU.Tot.BA %>% mutate(Tot.BA = sum(PU.Tot.BA$Basal.Area)) -> PU.Tot.BA #class total = 145492

#total PWS
PWS.Tot.BA <- subset(trees.ivi, class == "Primary.White.Sand")
PWS.Tot.BA %>% mutate(Tot.BA = sum(PWS.Tot.BA$Basal.Area)) -> PWS.Tot.BA #class total = 52180.13

#check to add to 327081.6 (all classes total)
13645.56 + 25246.81 + 8059.302 + 82457.74 + 145492 + 52180.13




# Relative Dominance - Fam BA/Tot BA
trees.ivi$Rel.Dom <- trees.ivi$F.BA / trees.ivi$Tot.BA

#Now filter out the excess so there is just one entry for each family in each class, so there should be 272 at the end

trees.ivi %>% 
  select(Family, class, F.BA, Tot.BA, Rel.Dom) %>% 
  unique() -> trees.ivi.dom

trees.ivi.dom <- with(trees.ivi.dom, trees.ivi.dom[order(class, Family) , ])

#check
subset(trees.ivi.dom, class == "Primary.Upland") -> g
sum(g$Rel.Dom) # = 1 :)

#Now  bind with rel.freq.tot
cbind(rel.freq.tot, trees.ivi.dom) -> fam.importance #classes and families aligned! now filter excess columns

#Get rid of duplicate columns
fam.importance <- fam.importance[!duplicated(as.list(fam.importance))]

fam.importance %>% 
  select(Family, Freq, class, F.BA, Tot.BA, Rel.Dom) -> fam.importance #duplicate columns now removed


## -- Computing Importance Value

# IV = Rel. Freq + Rel. Dom
fam.importance$IV <- fam.importance$Freq + fam.importance$Rel.Dom


#Final Tables

#flip class and Freq column order
fam.importance <- fam.importance[, c(1,3,2,4,5,6,7)]

##Make separate objects for each class and descending IV column

#DF Final
fam.imp.df <- subset(fam.importance, class == "Disturbed.Flooded")
fam.imp.df[with(fam.imp.df, order(-IV)), ] -> fam.imp.df

#DU Final
fam.imp.du <- subset(fam.importance, class == "Disturbed.Upland")
fam.imp.du[with(fam.imp.du, order(-IV)), ] -> fam.imp.du

#DWS Final
fam.imp.dw <- subset(fam.importance, class == "Disturbed.White.Sand")
fam.imp.dw[with(fam.imp.dw, order(-IV)), ] -> fam.imp.dw

#PF Final
fam.imp.pf <- subset(fam.importance, class == "Primary.Flooded")
fam.imp.pf[with(fam.imp.pf, order(-IV)), ] -> fam.imp.pf

#PU Final
fam.imp.pu <- subset(fam.importance, class == "Primary.Upland")
fam.imp.pu[with(fam.imp.pu, order(-IV)), ] -> fam.imp.pu

#PWS Final
fam.imp.pw <- subset(fam.importance, class == "Primary.White.Sand")
fam.imp.pw[with(fam.imp.pw, order(-IV)), ] -> fam.imp.pw


#check if makes sense with Primary Upland example
subset(trees.ivi.up, Family == "Myristicaceae") -> pu.check1 
subset(trees.ivi.up, Family == "Fabaceae") -> pu.check2

#according to the final table, Myristicaceae is the most 
#important family. So to check, I subsetted the original 
#subset of the class. There were 131 cases (freq) and 
#several trees with large basal areas (rel.dom). Compared 
#with the number 2 family, Fabaceae, Fab had only 88 cases 
#(freq), and not nearly as many trees with large basal areas.
#So, final table makes sense. 
