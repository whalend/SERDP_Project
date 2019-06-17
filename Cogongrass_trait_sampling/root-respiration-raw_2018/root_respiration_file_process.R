#### Read and Process root respiration LI-COR files ###
#
# Made 8/14/18
#

#root_type <- ifelse( grep('*fine*', unique(root_output[[i]]$fname)) > 0, "Fine root", "Rhizome")

base_path <- "/Users/tess/Documents/work/SERDP_Project/root-respiration-raw/"
cogongrass_biomass <- read.csv("/Users/tess/Downloads/Cogongrass_root_respiration_biomass\ -\ Sheet1.csv", header = TRUE)

flist <- list.files(base_path)
root_output <- list()
root_clean <- list()

skip <- c(1,2, 3, 4, 28, 69, 68, 37,38, 36)
for ( i in seq_along(flist)){
  print(i)
  if( i %in% skip){ next }
  
 root_output[[i]] <- PEcAn.photosynthesis::read_Licor(paste(base_path, flist[i], sep = ""))

plot(root_output[[i]]$FTime, (root_output[[i]]$CO2R), main = unique(root_output[[i]]$fname, ylim = c(400, 600)), ylab = "Measured Sample CO2 custom axis", xlab = "Time")
lines(root_output[[i]]$FTime, (root_output[[i]]$CO2S), main = unique(root_output[[i]]$fname))

}

## Fine roots seem to have consistent slopes, just sometimes in the wrong direction

rhiz_number <- length(cogongrass_biomass[cogongrass_biomass$Root.Type == "rhizome",]$Root.Number)
rhiz <- rep(NA, rhiz_number)

for (i in seq_along(root_output)){
  if( i %in% skip){ next }
  
  name <- unique(root_output[[i]]$fname)
  
  if(!is_empty(grep("rhiz", as.character(name)))){
    rhiz[i] <- mean(root_output[[i]]$CO2S)
  }
  
}
rhiz <- as.data.frame(rhiz)
ggplot(rhiz) +
  geom_boxplot(aes(x = rhiz))

plot(root_output[[i]]$FTime, (root_output[[i]]$CO2R), main = unique(root_output[[i]]$fname))
lines(root_output[[i]]$FTime, (root_output[[i]]$CO2S), main = unique(root_output[[i]]$fname))


## Unsure what I was trying to do below

if( !is.error(root_output[[i]]) ){
  type <- what_root_type(root_output[[i]]$fname)
  index <- what_sample_index()
  
  if(type == "rhiz"){
    
  }
  
  
}