
###
#Computations with perception surveys on MechTurk
###

#moving parts
#setwd('/Users/ps22344/Downloads/chapter3/rplots')



setwd('~/Downloads/chapter3')
install('surveytools')
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

perceptionfeatures=c(            
"author_gender"                                     
, "author_orient"                                   
, "author_friendly"                                   
, "author_audience"                       
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



##
#Chi square comparison
##
#compute author features

chisquaretester= function(control_stimulus, stimulus, column)
{
	#takes the spreadsheet for contronl group (control_stimulus), treatment group to be compared to control_stimulus
	#takes vector_of_columns with names, indices to computed. They need to be equivalent in both spreadsheets
	#make tables control stimulus and stimulus
	control= na.omit(control_stimulus[[column]])
	treatment= na.omit(stimulus[[column]])
	print ("checking for NAs")
	cat (length(control_stimulus[[column]])- length(control), " NAs found in 'control_stimulus\n")
	cat (length(stimulus[[column]])-length(treatment), " NAs found in 'stimulus'")
	
	
	#print (summary(control))
	#print (summary(treatment))
	
}

chisquaretester(spread, spread, 16)

