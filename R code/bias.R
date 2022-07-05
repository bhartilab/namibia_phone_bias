#' Editing the report on the bias
#'
#' @param output_file The output_file pathway. The file will be a html document so the extension must be '.html'.
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @import ggplot2 dygraphs knitr plotly dlnm splines
#' @return A markdown report.

report_bias <- function(output_file=file.path(path.workspace, "bias.html")){
	note("\nNo report will be generated if you do not have the proper elements installed to use markdown.\n")
	rmarkdown::render(input = system.file("rmd", "bias.Rmd", package = "namibia"),
		output_file=output_file, output_format = rmarkdown::html_document(toc=TRUE, toc_depth=3, toc_float=TRUE))
	note("\n....report generated\n")
}
