for (package in c("devtools", "roxygen2", "pwr", "effsize", "likert")) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos='http://cran.us.r-project.org')
        library(package, character.only=T)
    }
}

setwd('~/Downloads/chapter3/surveytools')
document()

setwd('~/Downloads/chapter3')
install('surveytools')
library('surveytools')


#BARPLOTTING

###
#
###

#gender by stimulus
#ok so the consulting guy says my functions are overkill
# i disagree, look at the mess below
#this is terrible!
barspread=surveytools:::spreadsheetbuilder(c('/Users/ps22344/Downloads/Clippings_0208.csv', '/Users/ps22344/Downloads/Prosody_0211.csv', '/Users/ps22344/Downloads/adapted_control_0206.csv'))

png("plot_author_gender.png", width=331.8, height=215.9, unit="mm", res=500)
par(cex=1.25)
percentages=aggregate(barspread[['author_gender']], list(barspread[['stimulus']]), function(x) table(x)/length(x))

g=t(percentages[['x']])

colnames(g)= c('Clippings', 'Prosody', 'Control')
g= g[,c('Control', 'Clippings', 'Prosody')]

par(mar=c(5.1,  4.1,  4.1, 12.0))
barplot(g, 
main= "Perceived author gender",
#cex.main=1.5,
col=c("grey", "white"),
ylab= "Proportion",
legend.text= TRUE,
args.legend= list(x=4.5,y=.9)
)
mtext(text=expression(paste("By stimulus, ", italic("p"), " < 0.05 only")), side=3, line= 0.2, cex= 1.2)

dev.off()


#education by stimulus
barspread=surveytools:::spreadsheetbuilder(c('/Users/ps22344/Downloads/Single\ letter_0218.csv','/Users/ps22344/Downloads/Clippings_0208.csv', '/Users/ps22344/Downloads/Prosody_0211.csv', '/Users/ps22344/Downloads/adapted_control_0206.csv'))


levels(barspread$author_education) = list("Below average"= c("Somewhat uneducated",  "Very uneducated"),  "Average"= "Of average education", "Above average"=c("Very educated", "Somewhat educated"))
percentages=aggregate(barspread[['author_education']], list(barspread[['stimulus']]), function(x) table(x)/length(x))
f=t(percentages[['x']])
colnames(f)= c('Single letter', 'Clippings', 'Prosody', 'Control')
f= f[,c('Control', 'Clippings', 'Prosody', 'Single letter')]
png("plot_education.png", width=331.8, height=215.9, unit="mm", res=500)
par(cex=1.25)
# #$mar
# #[1]  5.1  4.1  4.1 12.0
par(mar=c(5.1,  4.1,  4.1, 14.0))
barplot(f,
main= "Perceived author education",
col= c(1,1,0),
density= c(200,50,100),
ylab= "Proportion",
xlab= "Feature",
legend.text= TRUE,
args.legend= list(x=6.25,y=.9)
)
mtext(text=expression(paste("By stimulus, ", italic("p"), " < 0.05 only")), side=3, line= 0.2, cex= 1.2)
dev.off()



#addressee gender by stimulus
barspread=surveytools:::spreadsheetbuilder(c('/Users/ps22344/Downloads/second_Emoticons_0228.csv', '/Users/ps22344/Downloads/adapted_control_0206.csv'))
png("plot_addressee_gender.png", width=331.8, height=215.9, unit="mm", res=500)
par(cex=1.25)
percentages=aggregate(barspread[['author_audience']], list(barspread[['stimulus']]), function(x) table(x)/length(x))

g=t(percentages[['x']])

colnames(g)= c('Emoticons', 'Control')
g= g[,c('Control', 'Emoticons')]

par(mar=c(5.1,  4.1,  4.1, 12.0))
barplot(g, 
main= "Perceived addressee gender",
#cex.main=1.5,
col=c("grey", "white"),
ylab= "Proportion",
legend.text= TRUE,
args.legend= list(x=3,y=.9)
)
mtext(text=expression(paste("By stimulus, ", italic("p"), " < 0.05 only")), side=3, line= 0.2, cex= 1.2)

dev.off()
