###
#Computations with perception surveys on MechTurk
###
install.packages(c("devtools", "roxygen2"), repos='http://cran.us.r-project.org')
library(devtools)
library(roxygen2)
setwd('~/Downloads/chapter3/surveytools')
document()


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

setwd('~/Downloads/chapter3')
install('surveytools')
library('surveytools')

controlspread= read.csv("/Users/ps22344/Downloads/control_stimulus_adapted_0206.csv",  header=T, na.strings=c(""))
controlspread=csvcleaner(controlspread)
print (summary(controlspread))



setwd('~/Downloads/chapter3/rplots')


#moving parts
###
#
###
filename="~/Downloads/punctuation_0208.csv"
###
#
###


#Read in, clean
spread=read.csv(filename,   header=T, na.strings=c(""))
#spread=read.csv(filename,  header=T)
cat ("Input file has ", nrow(spread), "rows")
spread=csvcleaner(spread)
cat ("Cleaned file has ", nrow(spread), "rows")
print (summary(spread))


#output and inspect
barplot_by_column(spread, "punctuation", perceptionfeatures)




#' Chi square comparison, version 2 
#'
#' Run chi square test of independence on survey results
#' Updated based on stats consulting
#' @param
#' control_stimulus dataframe with control group
#' stimulus dataframe with treatment group to compare to control_stimulus
#' column the column to be read from both spread sheets
#' output "txt" for verbose text output, "csv" for csv output. Defaults to "csv"
#' @returns
#' prints out text with p values, expected vs observed, or csv representation of entire results
chisquaretester2= function(control_stimulus, stimulus, column, output="csv")
{
	
	control_stimulus['stimulus']="control"
	stimulus['stimulus']="treatment"
	dataset=rbind(control_stimulus, stimulus)

	chisquare= chisq.test(table(dataset[['stimulus']], dataset[[column]]))
	if (output=="text")
	{
		print ("checking for NAs")
		cat (length(control_stimulus[[column]])- length(na.omit(control_stimulus[[column]])), " NAs found in 'control_stimulus\n")
		cat (length(stimulus[[column]])-length(na.omit(stimulus[[column]])), " NAs found in 'stimulus'")
		cat ("\nInvestigating feature", column, "\n")
		print (table(control_stimulus[[column]]))
		print (table(stimulus[[column]]))
	}
	if (output=="csv")
	{
		write.csv(sapply(chisquare, unlist))
	}
	
	cat ("\np value", chisquare$p.value, "\n")
	cat ("expected: ", chisquare$expected, "\nobserved: ", chisquare$observed, "\n")
	
}

for (c in perceptionfeatures) {cat("\n++++\n"); chisquaretester2(controlspread, spread, c, output="text")}

basicstatsmaker(controlspread, participantfeatures[c(1:(length(participantfeatures)-2))])

