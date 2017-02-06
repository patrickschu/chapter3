
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

##
#Chi square comparison
##




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
# 39 mTurkCode