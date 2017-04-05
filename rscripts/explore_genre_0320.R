###
#Computations with perception surveys on MechTurk



for (package in c("devtools", "roxygen2", "pwr", "effsize", "likert")) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos='http://cran.us.r-project.org')
        library(package, character.only=T)
    }
}

setwd('~/Downloads/chapter3/surveytools')
document()





files=c(
#'/Users/ps22344/Downloads/adapted_control_0206.csv', 
'/Users/ps22344/Downloads/Capitals_0212.csv', 
'/Users/ps22344/Downloads/Clippings_0208.csv', 
'/Users/ps22344/Downloads/Prosody_0211.csv', 
'/Users/ps22344/Downloads/Punctuation_0208.csv', 
'/Users/ps22344/Downloads/second_Emoticons_0228.csv', 
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


setwd('~/Downloads/chapter3/rplots')

controlspread= read.csv("~/Downloads/adapted_control_0206.csv",  header=T, na.strings=c(""))
controlspread=surveytools:::csvcleaner(controlspread)
men= controlspread[controlspread[['author_gender']] == 'male'&controlspread[['author_orient']] == 'heterosexual'&controlspread[['author_audience']] == 'woman',]
women= controlspread[controlspread[['author_gender']] == 'female'&controlspread[['author_orient']] == 'heterosexual'&controlspread[['author_audience']] == 'man',]
gaymen= controlspread[controlspread[['author_gender']] == 'male'&controlspread[['author_orient']] == 'homosexual'&controlspread[['author_audience']] == 'man',]
gaywomen= controlspread[controlspread[['author_gender']] == 'female'&controlspread[['author_orient']] == 'homosexual'&controlspread[['author_audience']] == 'woman',]
controlspread=rbind(men,women)
controlspread=rbind(gaymen,gaywomen)
controlspread=rbind(men, women, gaymen, gaywomen)
summary(controlspread$author_orient)
controlspread['stimulus']= as.factor('control')

#moving parts
###
#
###
filename='/Users/ps22344/Downloads/Prosody_0211.csv'
###
#
###


#Read in, clean
spread=read.csv(filename,   header=T, na.strings=c(""))
#spread=read.csv(filename,  header=T)
cat ("Input file has ", nrow(spread), "rows")
spread=surveytools:::csvcleaner(spread)
fullspread=surveytools:::spreadsheetbuilder(files)

menonly= fullspread[fullspread[['author_gender']] == 'male'&fullspread[['author_orient']] == 'heterosexual'&fullspread[['author_audience']] == 'woman',]
womenonly= fullspread[fullspread[['author_gender']] == 'female'&fullspread[['author_orient']] == 'heterosexual'&fullspread[['author_audience']] == 'man',]
gaymenonly= fullspread[fullspread[['author_gender']] == 'male'&fullspread[['author_orient']] == 'homosexual'&fullspread[['author_audience']] == 'man',]
gaywomenonly= fullspread[fullspread[['author_gender']] == 'female'&fullspread[['author_orient']] == 'homosexual'&fullspread[['author_audience']] == 'woman',]

tt=rbind(menonly, womenonly)
tt=rbind(gaymenonly, gaywomenonly)
tt=rbind(menonly, womenonly,gaymenonly, gaywomenonly)

#surveytools:::relativeplotter(controlspread, tt, perceptionfeatures[!perceptionfeatures %in% c("author_attractive", "author_orient", "cat", "author_ethnicity")], "testplotgenre", 50)

#sink("chis_genre_0405.txt")
for (fili in files){
	cat("\n---\nstarting", fili)
	spread=read.csv(fili ,   header=T, na.strings=c(""))
	cat ("Input file has ", nrow(spread), "rows")
	spread=surveytools:::csvcleaner(spread)
	menonly= spread[spread[['author_gender']] == 'male'&spread[['author_orient']] == 'heterosexual'&spread[['author_audience']] == 'woman',]
	womenonly= spread[spread[['author_gender']] == 'female'&spread[['author_orient']] == 'heterosexual'&spread[['author_audience']] == 'man',]
	gaymenonly= spread[spread[['author_gender']] == 'male'&spread[['author_orient']] == 'homosexual'&spread[['author_audience']] == 'man',]
	gaywomenonly= spread[spread[['author_gender']] == 'female'&spread[['author_orient']] == 'homosexual'&spread[['author_audience']] == 'woman',]
	spread=rbind(menonly, womenonly, gaymenonly, gaywomenonly)
	cat("SUMMARY")
	print(summary(spread$author_orient))
	print(summary(controlspread$author_orient))
	for (c in perceptionfeatures) {
	cat("\n++++\n"); surveytools:::chisquaretester2(controlspread, spread, c, output="text")
	}
	cat("\n---\nending", fili)
	sink(paste(fili,"_chisq.text"))
	for (c in perceptionfeatures) {
	cat("\n++++\n"); surveytools:::chisquaretester2(controlspread, spread, c, output="text")
	sink()
	}
	
}
#sink()

