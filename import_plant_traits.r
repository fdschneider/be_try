
plant_traits_raw <- read.csv("data/plants_traits_TRY/2181.txt", sep = "\t" )
try_datasets <- read.csv("data/plants_traits_TRY/author_data.csv")
try_datasets_refs <- read.csv("data/plants_traits_TRY/dataset_reference.csv")

# original file: 2181.txt as downloaded on 01.08.2016 from TRY database following data request #2181. Traits requested: ... , plant IDs requested: ...)  

plant_trait_datasets <- unique(plant_traits_raw$Dataset)
plant_trait_authors <- unique(paste(plant_traits_raw$FirstName, plant_traits_raw$LastName))
plant_trait_species <- levels(plant_traits_raw$AccSpeciesName)

plant_traits_raw <- subset(plant_traits_raw, !is.na(TraitID))
plant_traits_raw$ID <- 1:length(plant_traits_raw$LastName)


## set StdValue for Nitrogen Fixation as binary

#levels(plant_traits_raw$OrigValueStr[plant_traits_raw$TraitID == 8, drop = TRUE])

nfixing_1 = c("1", "yes", "Yes", "High", "N-FIXER", "N2 fixing", "y", "Y", "yes, an N fixer", "Medium")
nfixing_0 = c("0", "no", "Low", "n", "N", "No", "NO-N-FIXER", "no, not an N fixer", "not N2 fixing")

plant_traits_raw$StdValue[plant_traits_raw$TraitID == 8] <- plant_traits_raw$OrigValueStr[plant_traits_raw$TraitID == 8] %in% nfixing_1


## set StdValue for Plant palatability

levels(plant_traits_raw$OrigValueStr[plant_traits_raw$TraitID == 679, drop = TRUE])
unique(plant_traits_raw$SpeciesName[plant_traits_raw$TraitID == 679])

palatable_1 = c("high", "High", "Yes")
palatable_05 = c("Medium", "Moderate", "Slight", "variable (e.g. young plants palatable but adult plant not)")
palatable_0 = c("low", "Low", "No", "None")

plant_traits_raw$StdValue[plant_traits_raw$TraitID == 679 & plant_traits_raw$OrigValueStr %in% palatable_1] <- 1
plant_traits_raw$StdValue[plant_traits_raw$TraitID == 679 & plant_traits_raw$OrigValueStr %in% palatable_05] <- 0.5
plant_traits_raw$StdValue[plant_traits_raw$TraitID == 679 & plant_traits_raw$OrigValueStr %in% palatable_0] <- 0

## set StdValue for pollination syndrome 

#levels(plant_traits_raw$OrigValueStr[plant_traits_raw$TraitID == 29, drop = TRUE])
#pollvec <- plant_traits_raw$OrigValueStr[plant_traits_raw$TraitID == 29, drop = TRUE]

#levels(pollvec) <- c(NA, NA, "wind", "")


#trait_observations <- ddply(plant_traits_raw[, c("AccSpeciesID","AccSpeciesName", "TraitID", "TraitName", "StdValue")], .(TraitID,AccSpeciesID), summarise,  AccSpeciesName = unique(AccSpeciesName)[1], observations = sum(StdValue/StdValue, na.rm = TRUE) )
#subset(trait_observations, observations > 100 )

#par(mfrow = c(2,1), mar = c(0,2,1,1), bty = "n")
#boxplot(log(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == "Dactylis glomerata")$StdValue), horizontal = TRUE, xaxt = "n") -> hdat
#par(mar = c(3,2,0,1))
#hist(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == "Dactylis glomerata")$StdValue, main = NA)
#lines(exp(hdat$stats[c(1,5)]), c(10,10), lwd = 1)
#lines(exp(hdat$stats[c(2,4)]), c(10,10), lwd = 5)
#lines(exp(hdat$stats[c(3,3)]), c(9,11), lwd = 3, col = "white")

## rectify data by excluding extreme observations (outliers outside 95% inner quantile on log-transformed values, Chambers et al 1983, based on function hist() in R)

## identify outliers
clean_out <- lapply(levels(plant_traits_raw$AccSpeciesName), function(x) {
  outlier <- boxplot.stats(log(subset(plant_traits_raw, TraitID == 1 & AccSpeciesName == x)$StdValue))$out 
  subset(plant_traits_raw, AccSpeciesName == x  & log(StdValue) %in% outlier)[,c("ID", "Dataset", "LastName","FirstName", "AccSpeciesName","StdValue")]
})
clean_out <- plyr::ldply(clean_out, data.frame)

## remove outliers
plant_traits_clean <- plant_traits_raw[-clean_out$ID,] 


plant_traits_clean$TraitName_short <- plant_traits_clean$TraitName
levels(plant_traits_clean$TraitName_short) <- c("","leaf_area", "SLA","leaf_drymass","LDMC", "leaf_N",  "leaf_P", "leaf_thickness","height","Nfixation","palatability","pollination","root_depth","seedmass","SSD")


# crunching average trait values per plant

## step 1: take average value per dataset (per author)

plant_traits_per_dataset <- plyr::ddply(plant_traits_clean, .(Dataset, AccSpeciesID, TraitID), summarise, StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), UnitName = unique(UnitName)[1], ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  TraitName = unique(TraitName)[1] ,  AccSpeciesName = unique(AccSpeciesName)[1],  TraitName_short = unique(TraitName_short)[1] )


# plant_traits_perauthor <- ddply(plant_traits_clean[, c("LastName", "AccSpeciesName", "AccSpeciesID", "TraitID", "TraitName", "StdValue", "UnitName", "ErrorRisk")], .(LastName, AccSpeciesID, TraitID), summarise, StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), UnitName = unique(UnitName)[1], ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  TraitName = unique(TraitName)[1] ,  AccSpeciesName = unique(AccSpeciesName)[1] )

##  eliminate datasets 

target_traits <- c("leaf_area", "SLA", "leaf_drymass","LDMC", "leaf_N", "leaf_P", "leaf_thickness","height", "root_depth","seedmass","SSD")

plant_traits_per_dataset <- subset(plant_traits_per_dataset, TraitName_short %in% target_traits)

#remove tall plants, i.e. trees and shrubs

trees_and_shrubs <- unique(subset(plant_traits_per_dataset, TraitName_short == "height" & StdValue >= 2.0)$AccSpeciesName)

plant_traits_per_dataset <- subset(plant_traits_per_dataset, !(AccSpeciesName %in% trees_and_shrubs)  )



outlandish <- c("California Coastal Grassland Database", "Chinese Leaf Traits Database", "Cedar Creek Savanna SLA, C, N Database", "Chinese Traits", "Cold Tolerance, Seed Size and Height of North American Forest Tree Species", "Leaf Ash Content in China's Terrestrial Plants", "Leaf Traits Mount Hutt, New Zealand", "Midwestern and Southern US Herbaceous Species Trait Database", "New South Wales Plant Traits Database", "Overton/Wright New Zealand Database", "Fonseca/Wright New South Wales Database", "Plant Traits for Grassland Species (Konza Prairie, Kansas, USA)", "Plant Traits for Pinus and Juniperus Forests in Arizona", "Maximum Height of Chinese Tree Species (From Silva Sinica)")

outlandish <- factor(outlandish, levels = levels(try_datasets$Dataset))

coauthorship <- unique(subset(try_datasets, Permitted == "yes" & CoauthorshipRequested == "yes")$Dataset)

## percentage of data for which coauthorship is required: 
sum(subset(try_datasets, Permitted == "yes" & CoauthorshipRequested == "yes")$Count)/sum(try_datasets$Count)


## blame for outliers

contribution <-  plyr::ddply(plant_traits_raw, .(Dataset), summarize, measurements = length(ID))

blame <- plyr::ddply(clean_out, .(Dataset), summarize, outlier = length(ID))

blame$rate <- blame$outlier/contribution$measurements[match( blame$Dataset, contribution$Dataset)]
blame$out <- blame$rate >= 0.05

blame <- blame[order(-blame$rate),]


## step 2: compare influence of questioned datasets

plant_traits_full <- ddply(plant_traits_per_dataset, 
                      .(AccSpeciesID, TraitID), 
                      summarise, 
                        StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), 
                        UnitName = unique(UnitName)[1], 
                        ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  
                        TraitName = unique(TraitName)[1] ,  
                        AccSpeciesName = unique(AccSpeciesName)[1],  
                        TraitName_short = unique(TraitName_short)[1] 
                      )

## build plant -- trait matrix 
plant_trait_matrix_full <- reshape2::dcast(plant_traits_full, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )



### compare with and without outlandish data 

without_outlandish <- ddply(subset(plant_traits_per_dataset, !Dataset %in% outlandish), 
                            .(AccSpeciesID, TraitID), summarize, 
                            StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), 
                            UnitName = unique(UnitName)[1], 
                            ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  
                            TraitName = unique(TraitName)[1] ,  
                            AccSpeciesName = unique(AccSpeciesName)[1],  
                            TraitName_short = unique(TraitName_short)[1] 
                            )

#### build plant -- trait matrix 
plant_trait_matrix_outlandish <- reshape2::dcast(without_outlandish, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )
plant_trait_matrix_outlandish <- plant_trait_matrix_outlandish[match(plant_trait_matrix_full$AccSpeciesID, plant_trait_matrix_outlandish$AccSpeciesID),]  #match rows to plant_trait_matrix_full



par(mfrow = c(4,3), mar = c(2,2,3,0), oma = c(3,4,0,0))
for(i in target_traits) {
  plot(plant_trait_matrix_full[,i], plant_trait_matrix_outlandish[,i], log = "xy", pch = 20, main = i)
  mtext(
    paste0(
      length(na.omit(plant_trait_matrix_full[,i]))-length(na.omit(plant_trait_matrix_outlandish[,i])),
      "/", 
      length(na.omit(plant_trait_matrix_full[,i])),
      " R²: ", 
      1-round(summary(lm(log(plant_trait_matrix_full[,i])~log(plant_trait_matrix_outlandish[,i])))[]$adj.r.squared, 2)
    ), 
    line = -1, cex = 0.7)
  abline(a = 0, b = 1)
}
mtext("trait values with outlandish data", side = 1, line = 1, outer = TRUE)
mtext("trait values without outlandish data", side = 2, line = 1, outer = TRUE)


### compare with and without blamed data

without_blamed <- ddply(subset(plant_traits_per_dataset, !Dataset %in% blame$Dataset[blame$out]), 
                            .(AccSpeciesID, TraitID), summarize, 
                            StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), 
                            UnitName = unique(UnitName)[1], 
                            ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  
                            TraitName = unique(TraitName)[1] ,  
                            AccSpeciesName = unique(AccSpeciesName)[1],  
                            TraitName_short = unique(TraitName_short)[1] 
)

#### build plant -- trait matrix 
plant_trait_matrix_blamed <- reshape2::dcast(without_blamed, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )
plant_trait_matrix_blamed <- plant_trait_matrix_blamed[match(plant_trait_matrix_full$AccSpeciesID, plant_trait_matrix_blamed$AccSpeciesID),]  #match rows to plant_trait_matrix_full


par(mfrow = c(4,3), mar = c(2,2,3,0), oma = c(3,4,0,0))
for(i in target_traits) {
  plot(plant_trait_matrix_full[,i], plant_trait_matrix_blamed[,i], log = "xy", pch = 20, main = i)
  mtext(
    paste0(
      length(na.omit(plant_trait_matrix_full[,i]))-length(na.omit(plant_trait_matrix_blamed[,i])),
      "/", 
      length(na.omit(plant_trait_matrix_full[,i])),
      " R²: ", 
      1-round(summary(lm(log(plant_trait_matrix_full[,i])~log(plant_trait_matrix_blamed[,i])))[]$adj.r.squared, 2)
    ), 
    line = -1, cex = 0.7)
  abline(a = 0, b = 1)
}
mtext("trait values with blamed data", side = 1, line = 1, outer = TRUE)
mtext("trait values without blamed data", side = 2, line = 1, outer = TRUE)


### compare with and without coauthorship data

without_coauthorship <- ddply(subset(plant_traits_per_dataset, !Dataset %in% coauthorship), 
                        .(AccSpeciesID, TraitID), summarize, 
                        StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), 
                        UnitName = unique(UnitName)[1], 
                        ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  
                        TraitName = unique(TraitName)[1] ,  
                        AccSpeciesName = unique(AccSpeciesName)[1],  
                        TraitName_short = unique(TraitName_short)[1] 
)

#### build plant -- trait matrix 
plant_trait_matrix_coauthorship <- reshape2::dcast(without_coauthorship, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )
plant_trait_matrix_coauthorship <- plant_trait_matrix_coauthorship[match(plant_trait_matrix_full$AccSpeciesID, plant_trait_matrix_coauthorship$AccSpeciesID),]  #match rows to plant_trait_matrix_full

par(mfrow = c(4,3), mar = c(2,2,3,0), oma = c(3,4,0,0))
for(i in target_traits) {
  plot(plant_trait_matrix_full[,i], plant_trait_matrix_coauthorship[,i], log = "xy", pch = 20, main = i)
  mtext(
    paste0(
      length(na.omit(plant_trait_matrix_full[,i]))-length(na.omit(plant_trait_matrix_coauthorship[,i])),
      "/", 
      length(na.omit(plant_trait_matrix_full[,i])),
      " R²: ", 
      1-round(summary(lm(log(plant_trait_matrix_full[,i])~log(plant_trait_matrix_coauthorship[,i])))[]$adj.r.squared, 2)
    ), 
    line = -1, cex = 0.7)
  abline(a = 0, b = 1)
}
mtext("trait values with coauthorship data", side = 1, line = 1, outer = TRUE)
mtext("trait values without coauthorship data", side = 2, line = 1, outer = TRUE)


### step 3: compile final dataset

plant_traits <- ddply(subset(plant_traits_per_dataset, !Dataset %in% unique(c(as.character(coauthorship), as.character(blame$Dataset[blame$out])))), 
                      .(AccSpeciesID, TraitID), 
                      summarise, 
                      StdValue = round(exp(mean(log(StdValue), na.rm = TRUE)), 3), 
                      UnitName = unique(UnitName)[1], 
                      ErrorRisk = round(mean(ErrorRisk, na.rm = TRUE),3),  
                      TraitName = unique(TraitName)[1] ,  
                      AccSpeciesName = unique(AccSpeciesName)[1],  
                      TraitName_short = unique(TraitName_short)[1] 
)


#### build plant -- trait matrix 
plant_trait_matrix <- reshape2::dcast(plant_traits, AccSpeciesID ~ TraitName_short, value.var = "StdValue" )
plant_trait_matrix <- plant_trait_matrix[match(plant_trait_matrix_full$AccSpeciesID, plant_trait_matrix$AccSpeciesID),]  #match rows to plant_trait_matrix


par(mfrow = c(4,3), mar = c(2,2,3,0), oma = c(3,4,0,0))
for(i in target_traits) {
  plot(plant_trait_matrix_full[,i], plant_trait_matrix[,i], log = "xy", pch = 20, main = i)
  mtext(
    paste0(
      length(na.omit(plant_trait_matrix_full[,i]))-length(na.omit(plant_trait_matrix[,i])),
      "/", 
      length(na.omit(plant_trait_matrix_full[,i])),
      " R²: ", 
      1-round(summary(lm(log(plant_trait_matrix_full[,i])~log(plant_trait_matrix[,i])))[]$adj.r.squared, 2)
    ), 
    line = -1, cex = 0.7)
  abline(a = 0, b = 1)
}
mtext("trait values with omitted data", side = 1, line = 1, outer = TRUE)
mtext("trait values without omitted data", side = 2, line = 1, outer = TRUE)



## step 4: get reference list of datasets used. 


used_datasets <- unique(subset(plant_traits_per_dataset, !Dataset %in% unique(c(as.character(coauthorship), as.character(blame$Dataset[blame$out]))))$Dataset) 

reflist <- subset(try_datasets_refs, Dataset %in% used_datasets)


## step 5: derived trait values

## leaf mass per area

plant_trait_matrix$LMA <- with(plant_trait_matrix, leaf_drymass/leaf_area)



## step 6: clean up and save file

save(plant_trait_matrix, file = "data/plant_trait_matrix.rData")
save(reflist, file = "data/reflist_TRY.rData")

if(FALSE){
  dd_0 <- na.omit(plant_trait_matrix[,c("SLA", "leaf_N", "leaf_area", "LMA", "leaf_area", "seedmass","height", "stem_drymass")]) 
  
  pca_dd <- rda(dd_0, scale = TRUE)
  eig_rel <- round(eigenvals(pca_dd)/sum(eigenvals(pca_dd))*100,1)
  biplot(pca_dd, scaling = -1, 
         xlab = paste0("PC1 (", eig_rel[1], "%)"),
         ylab = paste0("PC2 (", eig_rel[2], "%)"),
         col = c("#000000", "red")) -> fig
  points(fig, "sites", pch = 20, col = "#000000")
  text(fig, "species", col="red", cex= 1)
}

rm(plant_traits, clean_out, plant_traits_raw, plant_trait_datasets, plant_trait_authors, plant_trait_species, plant_traits_perauthor, plant_traits_clean, blame, contribution, plant_trait_matrix_blamed, plant_trait_matrix_coauthorship, plant_trait_matrix_full, plant_trait_matrix_outlandish, plant_traits_final, plant_traits_per_dataset, reflist, try_datasets, try_datasets_refs, without_outlandish, without_blamed, without_coauthorship, palatable_0, palatable_05, palatable_1, nfixing_0, nfixing_1, outlandish, coauthorship, target_traits, trees_and_shrubs, used_datasets, i, plant_traits_full, plant_trait_matrix)

