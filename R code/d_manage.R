###############################
## Data management functions ##
###############################

#' Function preparing the village dictionnary
#' 
#' @param data Raw data from the village dictionnary
#' @return A clean dictionnary with unique names of villages, unique IDs, and lat long coordinates (decimal) for some locations

clean_village_file <- function(data=villages){
	note("\nRearranging the columns...\n")
	temp <- data[,1:3]
	temp[2:(nrow(temp)+1),] <- temp
	temp[1,] <- colnames(temp)
	colnames(temp) <- c("village", "lat", "long")
	note("Keeping only coordinates...\n")
	temp$lat[grepl("*[a-z]", temp$lat)] <- NA
	temp[,2:3] <- apply(temp[,2:3], 2, as.numeric)
	note("Looking for duplicates...\n")
	note(paste("	Number of potential duplicates found:",(nrow(temp)-length(unique(temp$village))),"!\n", sep=" "))
	note("Attibuting ID villages for anonimity...\n")
	temp$loc_ID[temp$village=="Otjitanda"] <- 101
	temp$loc_ID[temp$village=="Etengwa"] <- 102
	temp$loc_ID[!(temp$loc_ID %in% c(101,102))] <- 200:(199+length(temp$loc_ID[!(temp$loc_ID %in% c(101,102))]))
	return(temp)
}

#' Basic function to get rid of the "NA" and turn them into actual NA
#' 
#' @param X A vector with character data
#' @return A clean vector with actual NA 

NA_clean <- function(X){
	return(ifelse(X=="NA", NA, X))
}

#' Cleaning the village names based on the survey of the locations and the potential homonymies
#' 
#' @param X A vector with village names
#' @return A vector 

village_name_clean <- function(X){
	X[X=="Orokatouo"] <- "Orokatuwo"
	X[X=="Orokatuo"] <- "Orokatuwo"
	X[grepl("Ombujaondume", X)] <- "Ombwanjandume"
	X[X=="Okanjandi"] <- "Okanyandi"
	X[X=="Okauua"] <- "Okaua"
	X[X=="Etengua"] <- "Etengwa"
	X[grepl("Angola", X)] <- "Abroad"
	X[X=="Otjizi"] <- "Otjizu"
	X[X=="Okahua"] <- "Okauwa"
	X[X=="Ozia"] <- "Ozija"
	X[X=="Okakuyo"] <- "Okakuyu"
	X[X=="Omatjivingo"] <- "Omuatjivingo"
	X[X=="Ovikotorungo"] <- "Ovikotorongo"
	X[X=="Embuende"] <- "Embwende"
	X[X=="Otjinungwe"] <- "Otjinungwa"
	X[X=="Ongondavembari"] <- "Ongondjonambari"
	X[X=="Okangwati"] <- "Okuanguati"
	X[X=="Ekaranjuwo"] <- "Ekarwondjiwo"
	X[X=="Omatjivingo"] <- "Omuatjivingo"
	X[X==""] <- NA
	return(X)
}
