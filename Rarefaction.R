# Data Import and Cleaning

setwd("~/R/Peru_trees")

trees <- read.table(file = "Peru_trees.csv", sep = ",", header = T)

#Clean the data
length(unique(trees$Site)) #20 sites
trees$Site <- gsub("El dorado", "El Dorado", trees$Site)


#Change names (and checked) #Maybe underscore for future
trees$hab <- factor(trees$hab,
                    labels = c("Upland", "Flooded", "White Sand"))

trees$dis <- factor(trees$dis,
                    labels = c("Disturbed", "Primary"))


# Packages 
#install.packages("iNEXT")

## install the latest version from github
#install.packages("devtools")
library(devtools)
library(usethis)
#install.packages("httr")
library(httr)
#install_github('JohnsonHsieh/iNEXT')
#install_github('AnneChao/iNEXT')
library(iNEXT)
library(ggplot2)


#For example:
#iNEXT(x, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, nboot=50)




## Abundance-based rarefaction per disturbance class (flipping Jacob's figure)
# Abundance matrix for trees
tsp <- unique(trees$N)
tst <- unique(trees$point)
tpt.matrix <- as.data.frame(matrix(data = NA, nrow=length(tst), ncol=2+length(tsp)))
names(tpt.matrix)[1] <- "Hab"
names(tpt.matrix)[2] <- "Dis"
names(tpt.matrix)[3:(length(tsp)+2)] <- as.character(tsp)
row.names(tpt.matrix) <- tst


#loop to fill the abundance matrix
for(i in 1:length(tst)){
  tpt.matrix$Hab[i] <- unique(trees$hab[which(trees$point==tst[i])])
  tpt.matrix$Dis[i] <- unique(trees$dis[which(trees$point==tst[i])])
  for(j in 1:length(tsp)){
    tpt.matrix[i,(j+2)] <- dim(trees[which(trees$point==tst[i] & trees$N==tsp[j]),])[1]
  }
}


tpt.matrix <- tpt.matrix[ , -which(names(tpt.matrix)==" ")]
rownames(tpt.matrix) <- gsub("-", "", rownames(tpt.matrix))
rownames(tpt.matrix) <- gsub("0", "", rownames(tpt.matrix))
rownames(tpt.matrix) <- gsub("-", "", rownames(tpt.matrix))
rownames(tpt.matrix) <- gsub("_", ".", rownames(tpt.matrix))



# Intact habitat
ptmatrix <- tpt.matrix[which(tpt.matrix$Dis==2), -2]
# Disturbed habitat
dtmatrix <- tpt.matrix[which(tpt.matrix$Dis==1), -2]





# Abundance-based rarefaction for trees in Primary sites
u.pt.mat <- ptmatrix[ptmatrix[,1]==1,2:dim(ptmatrix)[2]]
f.pt.mat <- ptmatrix[ptmatrix[,1]==2,2:dim(ptmatrix)[2]]
w.pt.mat <- ptmatrix[ptmatrix[,1]==3,2:dim(ptmatrix)[2]]


ab.site.p <- list()
ab.site.p$Upland <- colSums(u.pt.mat)
ab.site.p$Flooded <- colSums(f.pt.mat)
ab.site.p$White.Sand <- colSums(w.pt.mat)
ab.site.p.trees <- iNEXT(ab.site.p, datatype = "abundance") 
ab.site.p.trees.plot <- ggiNEXT(ab.site.p.trees, color.var = "site")
ab.site.p.trees.plot + ggtitle('Primary.Ab-Based')
ab.site.p.trees.plot <- ab.site.p.trees.plot + ggtitle('Primary.Ab-Based')




# Abundance-based rarefaction for trees in Disturbed sites
u.dt.mat <- dtmatrix[dtmatrix[,1]==1,2:dim(dtmatrix)[2]]
f.dt.mat <- dtmatrix[dtmatrix[,1]==2,2:dim(dtmatrix)[2]]
w.dt.mat <- dtmatrix[dtmatrix[,1]==3,2:dim(dtmatrix)[2]]


ab.site.d <- list()
ab.site.d$Upland <- colSums(u.dt.mat)
ab.site.d$Flooded <- colSums(f.dt.mat)
ab.site.d$White.Sand <- colSums(w.dt.mat)
ab.site.d.trees <- iNEXT(ab.site.d, datatype = "abundance") 
ab.site.d.trees.plot <- ggiNEXT(ab.site.d.trees, color.var = "site")
ab.site.d.trees.plot + ggtitle('Disturbed.Ab-Based') 
ab.site.d.trees.plot <- ab.site.d.trees.plot + ggtitle('Disturbed.Ab-Based') 



# Together
require(gridExtra)
plot1 <- ab.site.p.trees.plot
plot2 <- ab.site.d.trees.plot
grid.arrange(plot1, plot2, ncol=2)



## Individual-based abundance per disturbance class

# Abundance matrix for trees
tspp <- unique(trees$N)
tstt <- unique(trees$Transect)
ttr.matrix <- as.data.frame(matrix(data = NA, nrow=length(tstt), ncol=2+length(tspp)))
names(ttr.matrix)[1] <- "Hab"
names(ttr.matrix)[2] <- "Dis"
names(ttr.matrix)[3:(length(tspp)+2)] <- as.character(tspp)
row.names(ttr.matrix) <- tstt


#loop to fill the matrix
for(i in 1:length(tstt)){
  ttr.matrix$Hab[i] <- unique(trees$hab[which(trees$Transect==tstt[i])])
  ttr.matrix$Dis[i] <- unique(trees$dis[which(trees$Transect==tstt[i])])
  for(j in 1:length(tspp)){
    ttr.matrix[i,(j+2)] <- dim(trees[which(trees$Transect==tstt[i] & trees$N==tspp[j]),])[1]
  }
}


# Intact habitat
pttmatrix <- ttr.matrix[which(ttr.matrix$Dis==2), -2]
# Disturbed habitat
dttmatrix <- ttr.matrix[which(ttr.matrix$Dis==1), -2]



# Individual-based rarefaction for trees in Primary sites
u.pt.mat.ib <- pttmatrix[pttmatrix[,1]==1,2:dim(pttmatrix)[2]]
f.pt.mat.ib <- pttmatrix[pttmatrix[,1]==2,2:dim(pttmatrix)[2]]
w.pt.mat.ib <- pttmatrix[pttmatrix[,1]==3,2:dim(pttmatrix)[2]]


ab.site.p.ib <- list()
ab.site.p.ib$Upland <- colSums(u.pt.mat.ib)
ab.site.p.ib$Flooded <- colSums(f.pt.mat.ib)
ab.site.p.ib$White.Sand <- colSums(w.pt.mat.ib)
ab.site.p.trees.ib <- iNEXT(ab.site.p.ib, datatype = "abundance", q = 0, size = c(7, 2000), knots= 4) 
ab.site.p.trees.plot.ib <- ggiNEXT(ab.site.p.trees.ib, color.var = "site")
ab.site.p.trees.plot.ib + ggtitle('Primary Ind-Based')
ab.site.p.trees.plot.ib <- ab.site.p.trees.plot.ib + ggtitle('Primary Ind-Based')



# Individual-based rarefaction for trees in Disturbed sites
u.dt.mat.ib <- dttmatrix[dttmatrix[,1]==1,2:dim(dttmatrix)[2]]
f.dt.mat.ib <- dttmatrix[dttmatrix[,1]==2,2:dim(dttmatrix)[2]]
w.dt.mat.ib <- dttmatrix[dttmatrix[,1]==3,2:dim(dttmatrix)[2]]

ab.site.d.ib <- list()
ab.site.d.ib$Upland <- colSums(u.dt.mat.ib)
ab.site.d.ib$Flooded <- colSums(f.dt.mat.ib)
ab.site.d.ib$White.Sand <- colSums(w.dt.mat.ib)
ab.site.d.trees.ib <- iNEXT(ab.site.d.ib, datatype = "abundance", q = 0, size = c(7, 600), knots = 40) 
ab.site.d.trees.plot.ib <- ggiNEXT(ab.site.d.trees.ib, color.var = "site")
ab.site.d.trees.plot.ib + ggtitle('Disturbance Ind-Based')

ab.site.d.trees.plot.ib <- ab.site.d.trees.plot.ib + ggtitle('Disturbance Ind-Based')


# Together
require(gridExtra)
plot3 <- ab.site.p.trees.plot.ib
plot4 <- ab.site.d.trees.plot.ib
grid.arrange(plot3, plot4, ncol=2)


# All of them 
grid.arrange(plot1, plot2, plot3, plot4, ncol=4)

#Export PNG 1600x500 to see
