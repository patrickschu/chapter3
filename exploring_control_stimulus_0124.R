
"""
Computations with the first run on MechTurk
"""
setwd('/Users/ps22344/Downloads/chapter3/rplots')

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
		png(outputfile,  width=1280, height=640, res=100)
		#print (xtabs(~spread_sheet[[ind]]))
		barplot (xtabs(~spread_sheet[[ind]]), main=colnames(spread_sheet[ind]))
		dev.off()
		}	
		
}

spread=read.csv("/Users/ps22344/Downloads/creating_stimulus_0123_3rddrun - Copy_January 26, 2017_10.16.csv", header=T)

print (colnames(spread))

spread=csvcleaner(spread)

print (colnames(spread[17]))
spread['gender']=as.factor(spread[[17]])
spread[spread[['gender']]=="", ]
# [17] "The.author.is....gender"                              
#[18] "The.author.is.....hetero/homo                     
#[19] "The.author.is.....friendly                            
#[20] "The.author.is.writing.for.a....addressee"                
#[21] "The.author.seems....sensitive                           
#[22] "I.d.guess.the.author.is....ethnicicty                    
#[23] "The.author.seems.....assertive                      
#[24] "The.author.seems.....educated

#barplot_by_column(spread, c(17:24))

print (nrow(spread[spread[['gender']]=="male",]))
meanie = nrow(spread[spread[['gender']]=="male",])/nrow(spread)
print (meanie)
vec=c(rep(1,59), rep(0,42))
vec

t.test(vec, mu=0.5)


