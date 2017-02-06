###
#TOOLS FOR QUALTRICS PERCEPTIONS SURVEYS
###


#' CSV cleaner
#'
#'Takes out all the stuff from MechTurk and Qualtrics we don't want
#'@param spread_sheet a R data.frame
#'@keywords cats
#'@return a clean data.frame
#'@export
#'@examples This could be an example
csvcleaner=function(spread_sheet) {
	#takes out all the stuff from MechTurk and Qualtrics we don't want
	spread_sheet=spread_sheet[2:nrow(spread_sheet),]
	badcols= c("IPAddress", "ResponseId", "mTurkCode")
	spread_sheet= spread_sheet[,!(names(spread_sheet) %in% badcols)]
	#previews is when we go in and mess wit it
	spread_sheet= spread_sheet[!(spread_sheet[['Distribution.Channel']]=='preview'), ]
	spread_sheet= spread_sheet[!is.na((spread_sheet[[17]])), ]
	spread_sheet= droplevels(spread_sheet)

	return(spread_sheet)
}






#' Barplotter by column
#'
#'Takes a vector of column indexes, plots bars for each
#'@param  
#' spread_sheet a data.frame
#' file_name to be used for the png file
#' vector_of_column_indexes a vector of column indexes (as in numbers)
#'@keywords barplot
#'@return barplots named by column index, index_barplot.png
#'@export
#'@examples This could be an example
barplot_by_column= function(spread_sheet, file_name, vector_of_column_indexes){
	#takes a vector of column indexes, plots bars for each
	for (ind in vector_of_column_indexes)
		{
		outputfile= paste(file_name, paste(ind, ".png"), sep="_")
		print (ind)
		png(outputfile,  width=1280, height=640, res=100)
		#print (xtabs(~spread_sheet[[ind]]))
		barplot (xtabs(~spread_sheet[[ind]]), main=colnames(spread_sheet[ind]))
		dev.off()
		}	
		
}