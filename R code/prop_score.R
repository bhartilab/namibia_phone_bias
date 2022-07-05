#' Editing the report on the propensity score
#'
#' @param output_file The output_file pathway. The file will be a html document so the extension must be '.html'.
#' @import ggplot2 gridExtra VIM mice
#' @return A markdown report.

report_prop <- function(output_file=file.path(path.workspace, "prop_score.html")){
	note("\nNo report will be generated if you do not have the proper elements installed to use markdown.\n")
	rmarkdown::render(input = system.file("rmd", "prop_score.Rmd", package = "namibia"),
		output_file=output_file, output_format = rmarkdown::html_document(toc=TRUE, toc_depth=3, toc_float=TRUE))
	note("\n....report generated\n")
}
