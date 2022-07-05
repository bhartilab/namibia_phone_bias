#' Data management to prepare the data for network analysis
#' 
#' @param data Processed data of the survey and the children
#' @param verbose TRUE/FALSE option. If TRUE a count of the children with links to an ID not present in the survey of the same year and the ID of the children will be generated.
#' @return A clean dictionnary with unique names of villages, unique IDs, and lat long coordinates (decimal) for some locations

net_d_manage <- function(data=net2016, verbose=TRUE){
	var <- c("id", paste0("id_l", 1:10))
	ind <- cbind(data[[1]]$id, data[[1]][,-which(colnames(data[[1]]) %in% var)])
	colnames(ind)[1] <- "id"
	ind$id <- as.integer(ind$id)

	child <- data[[2]] %>%
				dplyr::mutate(id=as.integer(id))

	net1 <- data[[1]][,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id=as.integer(id)) %>%
				dplyr::filter(id!=id_link)
	net1$link <- apply(net1[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net2 <- child[,var] %>%
				tidyr::gather(., link, id_link, id_l1:id_l10) %>%
				dplyr::filter(!is.na(id_link)) %>%
				dplyr::select(id, id_link) %>%
				dplyr::mutate(id_link=as.integer(id_link)) %>%
				dplyr::filter(id!=id_link)
	net2$link <- apply(net2[, c("id", "id_link")], 1, function(x){
		paste(sort(x), collapse="-")
	})

	net <- rbind(net1, net2) %>%
				dplyr::mutate(dup=duplicated(link)) %>%
				dplyr::filter(dup==FALSE) %>%
				dplyr::select(id, id_link)

	if(verbose){
		note(paste0("\nNumber of children with links to id not present in the main dataset: ", length(unique(net2$id[!(net2$id_link %in% ind$id)])), "\n"))
	
		if(length(unique(net2$id[!(net2$id_link %in% ind$id)]))>0){
			note(paste0("ID of the children: ", paste(unique(net2$id[!(net2$id_link %in% ind$id)]), collapse=", "), "\n"))
		}
	}
	return(list(links=net, nodes=ind))
}

#' Editing the report on the network
#'
#' @param output_file The output_file pathway. The file will be a html document so the extension must be '.html'.
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @import ggplot2 dygraphs knitr plotly dlnm splines
#' @return A markdown report.

report_net <- function(output_file=file.path(path.workspace, "network.html")){
	note("\nNo report will be generated if you do not have the proper elements installed to use markdown.\n")
	rmarkdown::render(input = system.file("rmd", "network.Rmd", package = "namibia"),
		output_file=output_file, output_format = rmarkdown::html_document(toc=TRUE, toc_depth=3, toc_float=TRUE))
	note("\n....report generated\n")
}

#' Editing the report on the network for Chris' paper
#'
#' @param output_file The output_file pathway. The file will be a html document so the extension must be '.html'.
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @import ggplot2 knitr
#' @return A markdown report.

report_net_chris <- function(output_file=file.path(path.workspace, "network_Chris.html")){
	note("\nNo report will be generated if you do not have the proper elements installed to use markdown.\n")
	rmarkdown::render(input = system.file("rmd", "net_Chris.Rmd", package = "namibia"),
		output_file=output_file, output_format = rmarkdown::html_document(toc=TRUE, toc_depth=3, toc_float=TRUE))
	note("\n....report generated\n")
}

#' Editing the report on the additional network analysis
#'
#' @param output_file The output_file pathway. The file will be a html document so the extension must be '.html'.
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @import ggplot2 knitr
#' @return A markdown report.

report_net_phone <- function(output_file=file.path(path.workspace, "network_phone.html")){
	note("\nNo report will be generated if you do not have the proper elements installed to use markdown.\n")
	rmarkdown::render(input = system.file("rmd", "net_phone.Rmd", package = "namibia"),
		output_file=output_file, output_format = rmarkdown::html_document(toc=TRUE, toc_depth=3, toc_float=TRUE))
	note("\n....report generated\n")
}
