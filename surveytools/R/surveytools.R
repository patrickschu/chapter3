###
#TOOLS FOR QUALTRICS PERCEPTIONS SURVEYS
###

#misc and helper functions
cols=c("start_date"                                            
 , "end_date"                                              
 , "response_type"                                         
 , "progress"                                              
 , "duration_in_seconds"                                 
 , "finished"                                              
 , "recorded_date"                                         
 , "recipient_Last_Name"                                   
 , "recipient_first_Name"                                  
, "recipient_email"                                       
, "external_data_reference"                               
, "location_Latitude"                                     
, "location_Longitude"                                    
, "distribution_Channel"                                  
, "author_gender"                                     
, "author_orient"                                   
, "author_friendly"                                   
, "author_audience"                       
, "author_sensitive"                                  
, "author_ethnicity"                           
, "author_assertive"                                
, "author_attractive"                                
, "author_education"                                
, "men_are"                                           
, "women_are"                                         
, "would_you_reply"                           
, "further_comments"       
, "participant_gender"                            
, "participant_gender_other"               
, "participant_ethnicity"               
, "participant_ethnicity_other"  
, "participant_age"                                               
, "participant_orientation"             
, "participant_orientation_other"
, "participant_grew_up"                
, "participant_residence")                  




#' CSV cleaner
#'
#'Takes out all the stuff from MechTurk and Qualtrics we don't want
#'@param spread_sheet a R data.frame
#'@keywords cats
#'@return a clean data.frame
#'@export
#'@examples This could be an example
csvcleaner=function(spread_sheet) {
	##takes out all the stuff from MechTurk and Qualtrics we don't want
	#previews is when we go in and mess wit it
	spread_sheet= spread_sheet[!(spread_sheet[['DistributionChannel']]=='preview'), ]
	#these we don't need
	print (nrow(spread_sheet))
	badcols= c("IPAddress", "ResponseId", "mTurkCode")
	print (nrow(spread_sheet))
	spread_sheet= spread_sheet[,!(names(spread_sheet) %in% badcols)]
	print (nrow(spread_sheet))
	#trash headers introdudced by Qualtrics
	spread_sheet=spread_sheet[4:nrow(spread_sheet),]
	spread_sheet= spread_sheet[!is.na((spread_sheet[[17]])), ]
	spread_sheet= droplevels(spread_sheet)
	colnames(spread_sheet)= cols
	print (nrow(spread_sheet))
	spread_sheet=ordermachine(spread_sheet)
	return(spread_sheet)
}

#' Order machine to get our labels right
#' 
#' Takes a spread_sheet, orders the factors as indicated.
#'@param
#' Our Qualtrics spreadsheet
#'@keywords file under: ugly things
ordermachine= function(spread_sheet){
	print ("Running the ordermachine")
	spread_sheet[['author_gender']]=factor(spread_sheet[['author_gender']], levels=c("female", "male"))
	spread_sheet[['author_orient']]=factor(spread_sheet[['author_orient']], levels=c("heterosexual", "homosexual"))
	spread_sheet[['author_friendly']]=factor(spread_sheet[['author_friendly']], levels=c("Very friendly", "Friendly", "Somewhat friendly", "Somewhat unfriendly", "Very unfriendly"))
	spread_sheet[['author_audience']]=factor(spread_sheet[['author_audience']], levels=c( "woman", "man" ))
	spread_sheet[['author_sensitive']]=factor(spread_sheet[['author_sensitive']], levels=c("Very sensitive", "Sensitive", "Somewhat sensitive", "Insensitive", "Very insensitive" ))
	spread_sheet[['author_ethnicity']]=factor(spread_sheet[['author_ethnicity']], levels=c("Asian", "Black / African American", "Hispanic / Latino", "White" ))
	spread_sheet[['author_assertive']]=factor(spread_sheet[['author_assertive']], levels=c("Very assertive", "Assertive", "Somewhat assertive", "Somewhat timid", "Timid" ))
	spread_sheet[['author_education']]=factor(spread_sheet[['author_education']], levels=c("Very educated", "Somewhat educated","Of average education", "Somewhat uneducated", "Very uneducated"))
	spread_sheet[['author_attractive']]=factor(spread_sheet[['author_attractive']], levels=c("Very Attractive", "Attractive" , "Somewhat attractive", "Unattractive" , "Very Unattractive" )) 
 	spread_sheet[['would_you_reply']]=factor(spread_sheet[['would_you_reply']], levels=c("Yes, very likely", "Likely",  "Somewhat likely",  "Unlikely", "No, very unlikely"))
 	spread_sheet[['participant_age']]= as.numeric(spread_sheet[['participant_age']])
 	print ("Done with ordering")
 	return(spread_sheet)

}



#' Barplotter by column
#'
#'Takes a vector of column indexes, plots bars for each
#'@param  
#' spread_sheet a data.frame
#' file_name to be used for the png file
#' vector_of_column_indexes a vector of column indexes (as in numbers)
#'@keywords barplot
#'@return barplots named by column index, index_barplot.png
#'@export
#'@examples This could be an example
barplot_by_column= function(spread_sheet, file_name, vector_of_column_indexes){
	#takes a vector of column indexes, plots bars for each
	for (ind in vector_of_column_indexes)
		{
		outputfile= paste(file_name, paste(ind, ".png"), sep="_")
		print (ind)
		png(outputfile,  width=1280, height=640, res=100)
		#print (xtabs(~spread_sheet[[ind]]))
		barplot (xtabs(~spread_sheet[[ind]]), main=colnames(spread_sheet[ind]))
		dev.off()
		}	
		
}


#' Chi square comparison, version 1 (old)
#' Run chi square test of independence on survey results
#' OUTDATED
#' @param
#' control_stimulus dataframe with control group
#' stimulus dataframe with treatment group to compare to control_stimulus
#' column the column to be read from both spread sheets
#' output "txt" for verbose text output, "csv" for csv output. Defaults to "csv"
#' @return
#' prints out text with p values, expected vs observed, or csv representation of entire results
chisquaretester= function(control_stimulus, stimulus, column, output="csv")
{
	cat ("\n\n***YOU SHOULD NOT RUN THIS VERSION ANYMORE***\n\n")
	control= na.omit(control_stimulus[[column]])
	treatment= na.omit(stimulus[[column]])
	chisquare= chisq.test(table(control,treatment))
	if (output=="text")
	{
		print ("checking for NAs")
		cat (length(control_stimulus[[column]])- length(control), " NAs found in 'control_stimulus\n")
		cat (length(stimulus[[column]])-length(treatment), " NAs found in 'stimulus'")
		cat ("\nInvestigating feature", column, "\n")
		print (table(control))
		print (table(treatment))
	}
	if (output=="csv")
	{
		write.csv(sapply(chisquare, unlist))
	}
	
	cat ("\np value", chisquare$p.value, "\n")
	cat ("expected: ", chisquare$expected, "\nobserved: ", chisquare$observed, "\n")
	
}


#' Print basic stats
#' 
#' Takes a spread_sheet, prints relevant stats for qualtrics survey
#'@param
#' spread_sheet Our Qualtrics spreadsheet
#' column_indexes A vector of column names or numbers, columns must be factors
#'@keywords why not
basicstatsmaker= function(spread_sheet, column_indexes)
{
	#print out: number of total responses number of men, women
	cat ("Total number of responses in spread_sheet", nrow(spread_sheet), "\n")
	for (ind in column_indexes)
	{
		if (is.factor(spread_sheet[[ind]]))
			{
			cat("\n\n+++", ind, ", ", nrow(spread_sheet) - length(na.omit(spread_sheet[[ind]])), "NAs in here" )
			sapply(levels(spread_sheet[[ind]]), function(x) cat("\n", x, ",", nrow(spread_sheet[spread_sheet[[ind]]==x,]), ",", nrow(spread_sheet[spread_sheet[[ind]]==x,])/nrow(spread_sheet)))
			}
		if (is.numeric(spread_sheet[[ind]]))
			{
			vec= na.omit(spread_sheet[[ind]])
			cat("\n\n+++", ind )
			cat("\nmean", mean(vec), 
			"\nmedian,", median(vec), 
			"\nstandard_dev,", sd(vec), 
			"\nmin-max,", min(vec),"-", max(vec))
			}
	}
	
	
}


# 1 Start Date
# 2 End Date
# 3 Response Type
# 4 IP Address
# 5 Progress
# 6 Duration (in seconds)
# 7 Finished
# 8 Recorded Date
# 9 Response ID
# 10 Recipient Last Name
# 11 Recipient First Name
# 12 Recipient Email
# 13 External Data Reference
# 14 Location Latitude
# 15 Location Longitude
# 16 Distribution Channel
# 17 The author is ...
# 18 The author is ....1
# 19 The author is ....2
# 20 The author is writing for a ...
# 21 The author seems ...
# 22 I'd guess the author is ...
# 23 The author seems ....1
# 24 The author seems ....2
# 25 The author seems ....3
# 26 Men are ...
# 27 Women are ...
# 28 Would you reply to this ad?
# 29 Do you have any further comments? Please share!
# 30 I am ... - Selected Choice
# 31 I am ... - other, please specify - Text
# 32 I consider myself ... - Selected Choice
# 33 I consider myself ... - other, please specify - Text
# 34 My age:
# 35 I consider myself ... - Selected Choice.1
# 36 I consider myself ... - other, please specify - Text.1
# 37 I grew up in this city and this state:
# 38 I now live in this city:


# [1] "Start.Date"                                            
 # [2] "End.Date"                                              
 # [3] "Response.Type"                                         
 # [4] "Progress"                                              
 # [5] "Duration..in.seconds."                                 
 # [6] "Finished"                                              
 # [7] "Recorded.Date"                                         
 # [8] "Recipient.Last.Name"                                   
 # [9] "Recipient.First.Name"                                  
# [10] "Recipient.Email"                                       
# [11] "External.Data.Reference"                               
# [12] "Location.Latitude"                                     
# [13] "Location.Longitude"                                    
# [14] "Distribution.Channel"                                  
# [15] "The.author.is...."                                     
# [16] "The.author.is.....1"                                   
# [17] "The.author.is.....2"                                   
# [18] "The.author.is.writing.for.a...."                       
# [19] "The.author.seems...."                                  
# [20] "I.d.guess.the.author.is...."                           
# [21] "The.author.seems.....1"                                
# [22] "The.author.seems.....2"                                
# [23] "The.author.seems.....3"                                
# [24] "Men.are...."                                           
# [25] "Women.are...."                                         
# [26] "Would.you.reply.to.this.ad."                           
# [27] "Do.you.have.any.further.comments..Please.share."       
# [28] "I.am.......Selected.Choice"                            
# [29] "I.am.......other..please.specify...Text"               
# [30] "I.consider.myself.......Selected.Choice"               
# [31] "I.consider.myself.......other..please.specify...Text"  
# [32] "My.age."                                               
# [33] "I.consider.myself.......Selected.Choice.1"             
# [34] "I.consider.myself.......other..please.specify...Text.1"
# [35] "I.grew.up.in.this.city.and.this.state."                
# [36] "I.now.live.in.this.city." 