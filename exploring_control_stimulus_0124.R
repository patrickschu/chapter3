
"""
Computations with the first run on MechTurk
"""
setwd='~/Downloads/chapter3/rplots'

csvcleaner=function(spread_sheet) {
	#takes out all the stuff from MechTurk and Qualtrics we don't want
	badcols= c("IPAddress", "ResponseId", "mTurkCode")
	spread_sheet= spread_sheet[,!(names(spread_sheet) %in% badcols)]
	return(spread_sheet)
}


barplot_by_column= function(spread_sheet, vector_of_column_indexes){
	#takes a list of column indexes, plots bars for each
	for (ind in vector_of_column_indexes)
		{
		outputfile= paste(ind, "barplot.png", sep="_")
		
		print (ind)
		png(outputfile,  width=960, height=640, res=100)
		barplot (xtabs(~spread_sheet[[ind]]), main=colnames(spread_sheet[ind]))
		dev.off()
		}	
		
}

spread=read.csv("/Users/ps22344/Downloads/chapter3/csv/creating_stimulus_0123_2ndrun_January 24, 2017_17.05 2.csv", header=T)

print (colnames(spread))

spread=csvcleaner(spread)

print (colnames(spread))

barplot_by_column(spread, c(17:24))


