### Constructing palette objects from image repo using image pal 

library(devtools)
install.packages("imgpalr")
install_github("andreacirilloac/paletter")
library(imgpalr)
library(tidyverse)

# palette images 
PalPath <- "PalPics/"
PalPics <- list.files(PalPath) %>% as.data.frame() %>% 
  filter(!str_detect(., "Key")) %>% mutate_at(vars(.), as.character) %>% 
  pull(.) #remove the master image 

PalNames <- str_remove(PalPics, ".png") #Palette Names 

Darks <- c("HeartToRemember", "CosmicHeartbeat", "DontEatAzaleas", "CautionIsNotColdness", "SnakesEyeView", 
           "MelodyOfFreefall", "DragonEgg", "Omens") # pals with dark hues to set separate params 
Brights <- setdiff(PalNames, Darks)

PalPaths <- paste0(PalPath, PalPics)
PalPathsDark <- paste0(PalPath, Darks, ".png")
PalPathsBright <- paste0(PalPath, Brights, ".png")

QualVibesBright <- lapply(1:length(PalPathsBright), function(a) { 
  seed <- 1
  pal<- image_pal(PalPathsBright[[a]], type="qual",k=5, n=5, bw=c(0.3, .9), brightness=c(0.20,0.9), plot=TRUE)
  return(pal) 
}) 
names(QualVibesBright) <- Brights

QualVibesDark <- lapply(1:length(PalPathsDark), function(a) { 
  set.seed(1)
  pal<- image_pal(PalPathsDark[[a]], type="qual", k=5, n=5, bw=c(0.02, .7), brightness=c(0.1,0.9), plot=TRUE)
  return(pal) 
}) 
names(QualVibesDark) <- Darks 

QualVibes <-  c(QualVibesBright, QualVibesDark) %>% as.list() 
QualVibes <- QualVibes[order(names(QualVibes))]

SeqVibes <- lapply(1:length(PalPaths), function(a) { 
  set.seed(1)
  pal<- image_pal(PalPaths[[a]], type="seq", k=5, n=8, seq_by="hsv", 
                  bw=c(0.1,0.9), 
                  brightness=c(0.10,0.90), saturation=c(0.2, 0.9), 
                  plot=TRUE)
  return(pal) 
}) 
names(SeqVibes) <- PalNames

DivVibes <- lapply(1:length(PalPaths), function(a) { 
  set.seed(1)
  pal<- image_pal(PalPaths[[a]], type="div", n=9, seq_by="hsv", 
                  bw=c(0.1,0.9), 
                  brightness=c(0.40,0.9), saturation=c(0.4, 0.9), 
                  plot=TRUE)
  return(pal) 
}) 
names(DivVibes) <- PalNames

saveRDS(QualVibes, file="QualVibes.rds")
saveRDS(SeqVibes, file="SeqVibes.rds")
saveRDS(DivVibes, file="DivVibes.rds")
