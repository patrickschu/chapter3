###
#Computations with perception surveys on MechTurk
###
#install.packages(c("devtools", "roxygen2", "pwr", "effsize", "likert"), repos='http://cran.us.r-project.org')
library(devtools)
library(roxygen2)
library(pwr)
library(effsize)
library(grDevices)

setwd('~/Downloads/chapter3/surveytools')
document()

plotnames=c(
"Gender",
"Addressee",
"Friendly",
"Sensitive",
"Assertive",
"Educated",
"Likely to Reply?"
)


files=c(
'/Users/ps22344/Downloads/clippings_0208.csv',
'/Users/ps22344/Downloads/adapted_control_0206.csv',
'/Users/ps22344/Downloads/capitals_0212.csv',
'/Users/ps22344/Downloads/prosody_0211.csv',
'/Users/ps22344/Downloads/punctuation_0208.csv',
'/Users/ps22344/Downloads/second_emoticons_0211.csv'
)

perceptionfeatures=c(            
"author_gender"                                     
, "author_orient" 
, "author_audience" 
,"cat"                                   
, "author_friendly"  
, "author_sensitive"                                  
, "author_ethnicity"                           
, "author_assertive"                                
, "author_attractive"                                
, "author_education"                                                                    
, "would_you_reply"  
)

perceptionfeatures_text=c(
"men_are"                                           
, "women_are"   
, "further_comments"
)

participantfeatures=c(
"participant_gender"                            
, "participant_gender_other"               
, "participant_ethnicity"               
, "participant_ethnicity_other"  
, "participant_age"                                               
, "participant_orientation"             
, "participant_orientation_other"
, "participant_grew_up"                
, "participant_residence") 


participantfeaturesnumeric=c(
"participant_gender"                            
      
, "participant_ethnicity"               
, "participant_age"                                               
, "participant_orientation"             
) 

setwd('~/Downloads/chapter3')
install('surveytools')
library('surveytools')

controlspread= read.csv("~/Downloads/adapted_control_0206.csv",  header=T, na.strings=c(""))
controlspread=surveytools:::csvcleaner(controlspread)

print (summary(controlspread))



setwd('~/Downloads/chapter3/rplots')


#moving parts
###
#
###
filename='/Users/ps22344/Downloads/second_emoticons_0211.csv'
###
#
###


#Read in, clean
spread=read.csv(filename,   header=T, na.strings=c(""))
#spread=read.csv(filename,  header=T)
cat ("Input file has ", nrow(spread), "rows")
spread=surveytools:::csvcleaner(spread)
cat ("Cleaned file has ", nrow(spread), "rows")
print (summary(spread))
print (nrow(controlspread[controlspread[['author_gender']]=="male",]))
print (nrow(controlspread[controlspread[['author_gender']]=="female",]))
print (nrow(controlspread[spread[['author_gender']]=="male",]))
print (nrow(controlspread[spread[['author_gender']]=="female",]))
spread['stimulus']=as.factor("treatment")
fullspread=surveytools:::spreadsheetbuilder(files)
#output and inspect
#surveytools:::barplot_by_column(spread, "capitals", perceptionfeatures)

# for (c in perceptionfeatures) {cat("\n++++\n"); surveytools:::chisquaretester2(controlspread, spread, c, output="text")}

# surveytools:::basicstatsmaker(controlspread, participantfeatures[c(1:(length(participantfeatures)-2))])
# surveytools:::basicstatsmaker(spread, participantfeatures[c(1:(length(participantfeatures)-2))])



# # for (fili in files)
# {
	# cat ("\n\n+++++\n\n", fili, "\n\n")
	# #Read in, clean
	# spread=read.csv(fili,   header=T, na.strings=c(""))
	# #spread=read.csv(filename,  header=T)
	# cat ("Input file has ", nrow(spread), "rows")
	# spread=surveytools:::csvcleaner(spread)
	# cat ("Cleaned file has ", nrow(spread), "rows")
	# surveytools:::basicstatsmaker(spread, perceptionfeatures)
	
# }


#check for differences in participant makeup
#for (fili in files)
# {
	# spread=read.csv(fili,   header=T, na.strings=c(""))
	# spread=read.csv(filename,  header=T)
	# cat ("Input file has ", nrow(spread), "rows")
	# spread=surveytools:::csvcleaner(spread)
	# cat ("Cleaned file has ", nrow(spread), "rows")
	# for (c in participantfeaturesnumeric) {cat("\n++++\n"); c= surveytools:::chisquaretester2(controlspread, spread, c, output="text")}
# }


##
#PLOTTING RELATIVE TO CONTROL
##
#plot the means of control

relativeplotter= function(control_stimulus, data_set, vector_of_columns, filename)
#plot the means for all stimuli to compare
{
	cat("running relativeplotter")
	#these are our 0s
	controlmeans= sapply(vector_of_columns, function(x) mean(as.numeric(control_stimulus[[x]]), na.rm=TRUE))
	#out units
	controlsd= sapply(vector_of_columns, function(x) sd(as.numeric(control_stimulus[[x]]), na.rm=TRUE))
	#set up plot
	#png(paste(filename,".png"), width=331.8, height=215.9, unit="mm", res=750)
	plot(100, 
	xlim=c(1,length(vector_of_columns)), 
	ylim=c(.75,-.75),
	xlab= "Feature",
	ylab= "Distance to control mean (standard deviations)",
	type="n", 
	xaxt="n")
	abline(a=0, b=0)
	abline(v=2.5, lty=3)
	axis(side=1, at=seq(1, length(vector_of_columns), 1), labels=vector_of_columns, cex.axis=0.8)

	#iterate over stimuli
	stimuluscounter=0
	for (lev in levels(data_set[['stimulus']]))
		{
		cat ("\n---\nStimulus is", lev)
		stimuluscounter= stimuluscounter + 1	
		colcounter= 1
		for (column in vector_of_columns)
			{
			cat ("\n--working on", column, "mean:")
			print (mean(as.numeric(na.omit(data_set[data_set[['stimulus']]==lev,][[column]]))))
			cat ("controls", controlmeans[column], controlsd[column])
			colmean= mean(as.numeric(na.omit(data_set[data_set[['stimulus']]==lev,][[column]])))
			distance= (colmean - controlmeans[column]) / controlsd[column]
			cat ("\ndistance:", distance)
			#points(colcounter, distance, col=stimuluscounter)
			text(colcounter, distance, col=stimuluscounter, labels= lev, cex=0.7)
			colcounter= colcounter+1
			}
		}
	
	#add transparent text for orientation
	text(1.5,-0.5, 
	"f", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	
	text(1.5,0.5, 
	"m", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	
	text(7/2+2.5,0.5, 
	"less", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	text(7/2+2.5,-0.5, 
	"more", 
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	#dev.off()	
}


controlspread['stimulus']= as.factor('control')
print (summary(controlspread))
relativeplotter(controlspread, fullspread, perceptionfeatures[!perceptionfeatures %in% c("author_attractive", "author_orient")], "testplot")




relativeplotter_attractive= function(control_stimulus, data_set, vector_of_columns, filename)
#plot the means for all stimuli to compare
{
	cat("running relativeplotter")
	#these are our 0s
	controlmeans= sapply(vector_of_columns, function(x) mean(as.numeric(control_stimulus[[x]]), na.rm=TRUE))
	#out units
	controlsd= sapply(vector_of_columns, function(x) sd(as.numeric(control_stimulus[[x]]), na.rm=TRUE))
	#set up plot
	#png(paste(filename,".png"), width=331.8, height=215.9, unit="mm", res=750)
	plot(100, 
	xlim=c(1,length(vector_of_columns)), 
	ylim=c(.75,-.75),
	xlab= "Feature",
	ylab= "Distance to control mean (standard deviations)",
	type="n", 
	xaxt="n")
	abline(a=0, b=0)
	abline(v=2.5, lty=3)
	axis(side=1, at=seq(1, length(vector_of_columns), 1), labels=vector_of_columns, cex.axis=0.8)

	#iterate over stimuli
	stimuluscounter=0
	for (lev in levels(data_set[['stimulus']]))
		{
		cat ("\n---\nStimulus is", lev)
		stimuluscounter= stimuluscounter + 1	
		colcounter= 1
		for (column in vector_of_columns)
			{
			cat ("\n--working on", column, "mean:")
			print (mean(as.numeric(na.omit(data_set[data_set[['stimulus']]==lev,][[column]]))))
			cat ("controls", controlmeans[column], controlsd[column])
			colmean= mean(as.numeric(na.omit(data_set[data_set[['stimulus']]==lev,][[column]])))
			distance= (colmean - controlmeans[column]) / controlsd[column]
			cat ("\ndistance:", distance)
			#points(colcounter, distance, col=stimuluscounter)
			text(colcounter, distance, col=stimuluscounter, labels= lev, cex=0.7)
			colcounter= colcounter+1
			}
		}
	
	#add transparent text for orientation
	text(1.5,-0.5, 
	"f", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	
	text(1.5,0.5, 
	"m", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	
	text(7/2+2.5,0.5, 
	"less", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	text(7/2+2.5,-0.5, 
	"more", 
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	#dev.off()	
}

relativeplotter(spread, fullspread, c("author_attractive"), "atttractiveplot")



##
#EFFECT SIZES
##
#http://www.ats.ucla.edu/stat/mult_pkg/faq/general/effect_size_power/effect_size_power.htm



