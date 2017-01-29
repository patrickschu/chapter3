
"""
Computations with the first run on MechTurk
"""
setwd('/Users/ps22344/Downloads/chapter3/rplots')

csvcleaner=function(spread_sheet) {
	#takes out all the stuff from MechTurk and Qualtrics we don't want
	spread_sheet=spread_sheet[2:nrow(spread_sheet),]
	badcols= c("IPAddress", "ResponseId", "mTurkCode")
	spread_sheet= spread_sheet[,!(names(spread_sheet) %in% badcols)]
	#from 
	spread_sheet= droplevels(spread_sheet)

	return(spread_sheet)
}


barplot_by_column= function(spread_sheet, vector_of_column_indexes){
	#takes a list of column indexes, plots bars for each
	for (ind in vector_of_column_indexes)
		{
		outputfile= paste(ind, "barplot.png", sep="_")
		print (ind)
		png(outputfile,  width=1280, height=640, res=100)
		#print (xtabs(~spread_sheet[[ind]]))
		barplot (xtabs(~spread_sheet[[ind]]), main=colnames(spread_sheet[ind]))
		dev.off()
		}	
		
}

#read in & clean
spread=read.csv("~/Downloads/chapter3/creating_stimulus_0123_3rddrun - Copy_January 29, 2017_10.51.csv", skip=1, header=T)
spread=csvcleaner(spread)

#plot
barplot_by_column(spread, c(17:24))

#do the significance
#dummycode to 0 and 1
spread['dummygender']=sapply(spread[[17]], FUN= function(x) if (x=='female') return (0) else return(1))
head(spread[['dummygender']])
t.test(spread[,'dummygender'], mu=0.5)
mean(spread[,'dummygender'])

