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
"Assertive",
"Educated",
"Friendly",
"Sensitive",
"Likely to Reply?"
)



files=c(
#'/Users/ps22344/Downloads/adapted_control_0206.csv', 
'/Users/ps22344/Downloads/Capitals_0212.csv', 
'/Users/ps22344/Downloads/Clippings_0208.csv', 
'/Users/ps22344/Downloads/Prosody_0211.csv', 
'/Users/ps22344/Downloads/Punctuation_0208.csv', 
'/Users/ps22344/Downloads/second_Emoticons_0211.csv', 
'/Users/ps22344/Downloads/Single\ letter_0218.csv'
)



perceptionfeatures=c(            
"author_gender"                                     
, "author_orient" 
, "author_audience" 
,"cat"  
, "author_assertive"  
, "author_education"                                
, "author_friendly"  
, "author_sensitive"                                  
, "author_ethnicity"                           
, "author_attractive"                                
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


setwd('~/Downloads/chapter3/rplots')


#moving parts
###
#
###
filename='/Users/ps22344/Downloads/Punctuation_0208.csv'
###
#
###


#Read in, clean
spread=read.csv(filename,   header=T, na.strings=c(""))
#spread=read.csv(filename,  header=T)
cat ("Input file has ", nrow(spread), "rows")
spread=surveytools:::csvcleaner(spread)
cat ("Cleaned file has ", nrow(spread), "rows")
print (nrow(controlspread[controlspread[['author_gender']]=="male",]))
print (nrow(controlspread[controlspread[['author_gender']]=="female",]))
print (nrow(controlspread[spread[['author_gender']]=="male",]))
print (nrow(controlspread[spread[['author_gender']]=="female",]))
spread['stimulus']=as.factor("treatment")
fullspread=surveytools:::spreadsheetbuilder(files)
#output and inspect
#surveytools:::barplot_by_column(spread, "capitals", perceptionfeatures)

sink("chis.txt")
for (fili in files){
	cat("\n---\nstarting", fili)
	spread=read.csv(fili ,   header=T, na.strings=c(""))
	#spread=read.csv(filename,  header=T)
	cat ("Input file has ", nrow(spread), "rows")
	spread=surveytools:::csvcleaner(spread)
	for (c in perceptionfeatures) {
	cat("\n++++\n"); surveytools:::chisquaretester2(controlspread, spread, c, output="text")
	
	}
	cat("\n---\nending", fili)
}
sink()

# surveytools:::basicstatsmaker(controlspread, participantfeatures[c(1:(length(participantfeatures)-2))])
surveytools:::basicstatsmaker(spread, perceptionfeatures)



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




controlspread['stimulus']= as.factor('control')
print (summary(controlspread))
surveytools:::relativeplotter(controlspread, fullspread, perceptionfeatures[!perceptionfeatures %in% c("author_attractive", "author_orient", "cat", "author_ethnicity")], "testplot")




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
	
	
	text(2+2.5,0.5, 
	"less", 	
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	text(5/2+2.5,-0.5, 
	"more", 
	cex=4, 	
	col=rgb(col2rgb("blue")['red',], col2rgb("blue")['green',], col2rgb("blue")['blue',], alpha=100, maxColorValue=255))
	
	#dev.off()	
}

#relativeplotter(spread, fullspread, c("author_attractive"), "atttractiveplot")



##
#EFFECT SIZES
##
#http://www.ats.ucla.edu/stat/mult_pkg/faq/general/effect_size_power/effect_size_power.htm

#BARPLOTTING
barspread=surveytools:::spreadsheetbuilder(c('/Users/ps22344/Downloads/Clippings_0208.csv', '/Users/ps22344/Downloads/Prosody_0211.csv', '/Users/ps22344/Downloads/adapted_control_0206.csv'))
###
#
###

#gender by stimulus
#ok so the consulting guy says my functions are overkill
# i disagree, look at the mess below
#this is terrible!
png("barplottest.png", width=331.8, height=215.9, unit="mm", res=500)
par(cex=1.25)
percentages=aggregate(barspread[['author_gender']], list(barspread[['stimulus']]), function(x) table(x)/length(x))

g=t(percentages[['x']])

colnames(g)= c('Clippings', 'Prosody', 'Control')
g= g[,c('Control', 'Clippings', 'Prosody')]

par(mar=c(5.1,  4.1,  4.1, 12.0))
barplot(g, 
main= "Author gender by stimulus",
#cex.main=1.5,
col=c("black", "white"),
ylab= "Proportion",
legend.text= TRUE,
args.legend= list(x=4.5,y=.9)
)
dev.off()


#education by stimulus
barspread=surveytools:::spreadsheetbuilder(c('/Users/ps22344/Downloads/Clippings_0208.csv', '/Users/ps22344/Downloads/Prosody_0211.csv', '/Users/ps22344/Downloads/adapted_control_0206.csv'))


levels(barspread$author_education) = list("Below average"= c("Somewhat uneducated",  "Very uneducated"),  "Average"= "Of average education", "Above average"=c("Very educated", "Somewhat educated"))
percentages=aggregate(barspread[['author_education']], list(barspread[['stimulus']]), function(x) table(x)/length(x))
f=t(percentages[['x']])
colnames(f)= c('Clippings', 'Prosody', 'Control')
f= f[,c('Control', 'Clippings', 'Prosody')]
png("barplottest.png", width=331.8, height=215.9, unit="mm", res=500)
par(cex=1.25)
# #$mar
# #[1]  5.1  4.1  4.1 12.0
par(mar=c(5.1,  4.1,  4.1, 12.0))
barplot(f,
main= "Author education by stimulus",
col= c(1,1,0),
density= c(200,50,100),
ylab= "Proportion",
legend.text= TRUE,
args.legend= list(x=4.5,y=.9)
)

dev.off()