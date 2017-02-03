
###
#Computations with the first run on MechTurk
###


#setwd('/Users/ps22344/Downloads/chapter3/rplots')
setwd('~/Downloads/chapter3/rplots')


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

nafinder= function(spread_sheet, vector_of_column_indexes){
	print ('Running nafinder')
	for (ind in vector_of_column_indexes)
		{
		cat (ind, colnames(spread_sheet)[ind])
		nas=sapply(spread_sheet[[ind]], anyNA)
		print (sum(nas))
		}
}

#read in & clean
spread=read.csv("/Users/ps22344/Downloads/creating_stimulus_0123_3rddrun - Copy_February3,2017_12.25.csv",  skip=1, header=T, na.strings=c(""))
#spread=read.csv("E:/cygwin/home/ps22344/Downloads/creating_stimulus_0123_3rddrun - Copy_February 2, 2017_11.37.csv", skip=1, header=T)
spread=csvcleaner(spread)

#output and inspect
cat("rows", nrow(spread))
spread[205,20]==" "

#plot
#barplot_by_column(spread, c(17:25))

#Nas
nafinder(spread, c(1:27))


#do the significance
#dummycode to 0 and 1
spread['dummygender']=sapply(spread[[17]], FUN= function(x) if (x=='female') return (0) else return(1))
head(spread[['dummygender']])
t.test(spread[,'dummygender'], mu=0.5)
mean(spread[,'dummygender'])

##explore participant data
levelprinter= function(spread_sheet, vector_of_column_indexes){
	#takes a list of column indexes, prints level for each
	for (ind in vector_of_column_indexes)
		{
		cat ("\n****\n", ind, colnames(spread_sheet)[ind])
		cat (levels(spread_sheet[[ind]]), sep='\n')
		#print (xtabs(~spread_sheet[[ind]]))
		}	
}

#levelprinter(spread, c(26:35))


levelmatcher= function(spread_sheet, vector_of_levels){
	#takes a vector of two levels and combines them in output
	# what are the comments by age?
	level1index=vector_of_levels[1]
	level2index=vector_of_levels[2]
	#cat ("levels", levels(spread_sheet[[level1index]]))
	by(
	data= spread_sheet, 
	INDICES= spread_sheet[[level1index]], 
	FUN= function(x) print(x[colnames(x)[level2index]])
	#spread_sheet[spread_sheet[colnames(spread_sheet)[level1index]]=='male'),]
	)
	
}

#levelmatcher(spread, c(17,20))
#means by participant gender
#femmi=spread[spread[[27]]=='female',]['dummygender']


#SIG WITH CHI SQUARE
row1=c(100,100)
row2=c(121,81)
counts= rbind(row1, row2)
chisq.test(counts)

