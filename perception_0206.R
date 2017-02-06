
###
#Computations with perception surveys on MechTurk
###

#moving parts
#setwd('/Users/ps22344/Downloads/chapter3/rplots')
setwd('~/Downloads/chapter3/rplots')
filename="/Users/ps22344/Desktop/emoticons_0203_February 6, 2017_12.14.csv"

#Read in, clean
spread=read.csv(filename,  skip=1, header=T, na.strings=c(""))
#spread=read.csv(filename, skip=1, header=T)
cat ("Input file has ", nrow(spread), "rows")

spread=csvcleaner(spread)
cat ("Cleaned file has ", nrow(spread), "rows")
print (summary(spread))


#output and inspect
#barplot_by_column(spread, "emoticons", c(17:34))
perceptionfeatures=c()
participantfeatures=c()

