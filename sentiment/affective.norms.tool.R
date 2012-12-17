#########################
#            Assignment  5           # 
#########################
#clean up workspace, set new directory
rm(list=ls(all=T))
setwd("/QMSS/Data Mining/HW5/Data")
library("topicmodels")
library(rpart)
library(rpartOrdinal)
library(MASS)
library(wle)
library(HH)
library("car")
library("rgl")

#read in data and assign it to a list called "sotu"
sotu=list()
clinton1994=scan("SOTU/clinton4.txt",what="c"); sotu[[1]]=clinton1994
clinton1995=scan("SOTU/clinton5.txt",what="c"); sotu[[2]]=clinton1995
clinton1996=scan("SOTU/clinton6.txt",what="c"); sotu[[3]]=clinton1996
clinton1997=scan("SOTU/clinton7.txt",what="c"); sotu[[4]]=clinton1997
clinton1998=scan("SOTU/clinton8.txt",what="c"); sotu[[5]]=clinton1998
clinton1999=scan("SOTU/clinton9.txt",what="c"); sotu[[6]]=clinton1999
clinton2000=scan("SOTU/clinton0.txt",what="c"); sotu[[7]]=clinton2000
bush2002=scan("SOTU/bush2.txt",what="c"); sotu[[8]]=bush2002
bush2003=scan("SOTU/bush3.txt",what="c"); sotu[[9]]=bush2003
bush2004=scan("SOTU/bush4.txt",what="c"); sotu[[10]]=bush2004
bush2005=scan("SOTU/bush5.txt",what="c"); sotu[[11]]=bush2005
bush2006=scan("SOTU/bush6.txt",what="c"); sotu[[12]]=bush2006
bush2007=scan("SOTU/bush7.txt",what="c"); sotu[[13]]=bush2007
bush2008=scan("SOTU/bush8.txt",what="c"); sotu[[14]]=bush2008
obama2010=scan("SOTU/obama10.txt",what="c"); sotu[[15]]=obama2010
obama2011=scan("SOTU/obama11.txt",what="c"); sotu[[16]]=obama2011

#create a character vector of every word for each document called "alldocs"
alldocs <- unlist(sotu)
length(alldocs)

#take only the unique words
allwords <- unique(alldocs)
length(allwords)

#COMMENT: while there are 103767 words in these documents, only 7589 are unique.

#read in "stop words"
#stopwords <- scan("stopwords.txt", what="c")

# check out a random sample of fifty stopwords to get a sense of what they are.
#rand_stopwords <- sample(stopwords, 50)
#rand_stopwords

#COMMENT: some of these seem like they may be semantically important...

##create a document term matrix

#first, create a shell data frame with 16 rows (one for each document) and 7589 columns (one for each unique word)
wordmat <- data.frame(matrix(0,nrow=length(sotu),ncol=length(allwords)))
#assign the words to the names of each column
names(wordmat) <- sort(allwords)

#create a table of word counts for each speech
tlist <- lapply(sotu,table)


# for each speech, match the unique words in each speech with the names of the columns of the matrix,
# which are the unique words in all the speeches
# Next, assign the table of word counts for each speech to a row of the matrix
for(i in 1:length(tlist)){
mvec <- match(names(tlist[[i]]),allwords)
wordmat[i,mvec] <- tlist[[i]]
}
head(wordmat[,1:6])

#make a line plot of verbosity (eq number of unique words used) over time.  NOTE: figure out how to color the line by president and maybe give the x-axis labels by year.
N <- nrow(wordmat)
unique_words <- numeric(N)
for(i in 1:N){
	wordmat_unq <- wordmat[i,]
	non_zero_all_words <- wordmat_unq[which(wordmat_unq!=0)]
	unique_words[i] <- ncol(non_zero_all_words)
}

plot(unique_words, type="l", main="Verbosity of SOTU speeches over time" )

#create a vector of the column numbers where the word column matches a stop word
stopvec <- match(names(wordmat),stopwords)
#create our Document/Term Frequency matrix by selecting non-stop word columns. This is done be only selecting the columns that returned "NA" from the match function above, as in those whose column name (the word) did not match a stop word.
TF<- wordmat[,is.na(stopvec)]



################################
#        Affective Norms Function               #
################################
TF <- wordmat

aff.norms <- read.csv("/Users/brianabelson/Dropbox/HI LAB/Research/WALKING DEAD STUDY/CHEX EXPORT/content analysis/sentiment/aff.norms3.csv")
N.docs<- nrow(TF)
results<-data.frame(matrix(0,nrow=N.docs,ncol=6))				
for (i in 1:N.docs){
				#STEP 1A#
												  # Get the row numbers of the aff.norms data frame that 
												  # match words in the column names of the TF Matrix (the words) 													      # In addition, remove all those columns that don't match (the "NA"s).
												  
				norm.matches.vec <- na.omit(match(names(TF[which(TF[i,]!=0)]), as.character(aff.norms[,1])))

				#STEP 1B#
												  # Create a new affective norms data frame of 
												  # just the words that are matched in the document. 
													# Alphabetize this list so it matches up with the TF matrix we will create below.
				aff.norms.of.doc<- aff.norms[norm.matches.vec,]


				#STEP 2A# 
			                   # Get the column numbers of the TF Matrix (the words in the document)  
			                   # that match words in the affective norms data frame.			         
			                   # In addition, remove all those columns that don't match (the "NA"s).

				TF.matches.vec <- na.omit(match(as.character(aff.norms[,1]), names(TF[which(TF[i,]!=0)])))
				
				#STEP 2B#
                          # Create a new TF Matrix of just the word counts
                          # are matched in the affective norm words. 
                          # In addition, transpose the dataframe to enable vector multiplication below.

				word.counts <- t(TF[i,TF.matches.vec])

				#STEP 3# 
											# Multiply the valence, arousoal, dominance scores 
											# by the number of words in the document.
											# Sum these new vectors and divide by the number of 
											# words in the original document to get the valence score for each
											# document.
																	valence.mean      <-  sum(aff.norms.of.doc$valence*word.counts)/
																	sum(word.counts)
																	arousal.mean      <-  sum(aff.norms.of.doc$arousal*word.counts)/
																	sum(word.counts)

																	dominance.mean <-  sum(aff.norms.of.doc$dominance*word.counts)/
																	sum(word.counts)

																	valence.var      <-  abs(var(aff.norms.of.doc$valence))
																	arousal.var      <-  abs(var(aff.norms.of.doc$arousal))
																	dominance.var <-  abs(var(aff.norms.of.doc$dominance))															

				#STEP 4#
											 # Bind these results into a dataframe which consists of the sum of scores 
											 #divided by the total words in the document 												
				results[i,] <- data.frame(valence.mean, arousal.mean, dominance.mean, 
				                                    valence.var, arousal.var, dominance.var)
}
results
colnames(results) <- c("valence.mean", "arousal.mean", "dominance.mean",
																		"valence.var", "arousal.var", "dominance.var")
rownames(results) <- c("clinton1994", "clinton1995", "clinton1996", "clinton1997","clinton1998",
									  "clinton1999","clinton2000","bush2002","bush2003","bush2004","bush2005",
									  "bush2006","bush2007","bush2008","obama2010","obama2011")
						
#VALENCE																								
par(mfrow=c(1,1),bg="white", family="HersheySans")
index<- as.numeric(c(seq(1994,2000,1), seq(2002,2008,1), seq(2010,2011,1)))
x.axis<- c("94", "95", "96",  "97", "98", "99", "00", "02", "03", "04", "05", "06", "07", "08", "10", "11")
prezzes<- as.character(c("clinton", "clinton", "clinton", "clinton","clinton", "clinton", "clinton", "bush", "bush", "bush", "bush", "bush", "bush","bush", "obama", "obama"))
radius <- sqrt((results$valence.var^10)/ pi )
symbols(index[1:7], results$valence.mean[1:7], circles=radius[1:7], inches=.4, xlim=c(1993, 2012), ylim=c(-1.2, 1.3), bg="blue", fg="white", xlab="Year", ylab="Mean Valence", main="Valence Mean and Variance for State of the Union Addresses, 1994-2011")
symbols(index[8:14], results$valence.mean[8:14], circles=radius[8:14], inches=.4, bg="red", fg="white", add=T)
symbols(index[15:16], results$valence.mean[15:16], circles=radius[15:16], inches=.4, bg="cyan", fg="white", add=T)
abline(a=0,b=0, lty=5, col="gray")
text(index, results$valence.mean, prezzes, cex=.75)

#AROUSAL	
index<- as.numeric(c(seq(1994,2000,1), seq(2002,2008,1), seq(2010,2011,1)))
x.axis<- c("94", "95", "96",  "97", "98", "99", "00", "02", "03", "04", "05", "06", "07", "08", "10", "11")
prezzes<- as.character(c("clinton", "clinton", "clinton", "clinton","clinton", "clinton", "clinton", "bush", "bush", "bush", "bush", "bush", "bush","bush", "obama", "obama"))
radius <- sqrt((results$arousal.var^10)/ pi )
symbols(index[1:7], results$arousal.mean[1:7], circles=radius[1:7], inches=.4, xlim=c(1993, 2012), ylim=c(-1.2, 1.3), bg="blue", fg="white", xlab="Year", ylab="Mean Arousal", main="Arousal Mean and Variance for State of the Union Addresses, 1994-2011")
symbols(index[8:14], results$arousal.mean[8:14], circles=radius[8:14], inches=.4, bg="red", fg="white", add=T)
symbols(index[15:16], results$arousal.mean[15:16], circles=radius[15:16], inches=.4, bg="cyan", fg="white", add=T)
abline(a=0,b=0, lty=5, col="gray")
text(index, results$arousal.mean, prezzes, cex=.75)


#DOMINANCE
index<- as.numeric(c(seq(1994,2000,1), seq(2002,2008,1), seq(2010,2011,1)))
x.axis<- c("94", "95", "96",  "97", "98", "99", "00", "02", "03", "04", "05", "06", "07", "08", "10", "11")
prezzes<- as.character(c("clinton", "clinton", "clinton", "clinton","clinton", "clinton", "clinton", "bush", "bush", "bush", "bush", "bush", "bush","bush", "obama", "obama"))
radius <- sqrt((results$dominance.var^10)/ pi )
symbols(index[1:7], results$dominance.mean[1:7], circles=radius[1:7], inches=.4, xlim=c(1993, 2012), ylim=c(-1.2, 1.3), bg="blue", fg="white", xlab="Year", ylab="Mean Dominance", main="Dominance Mean and Variance for State of the Union Addresses, 1994-2011")
symbols(index[8:14], results$dominance.mean[8:14], circles=radius[8:14], inches=.4, bg="red", fg="white", add=T)
symbols(index[15:16], results$dominance.mean[15:16], circles=radius[15:16], inches=.4, bg="cyan", fg="white", add=T)
abline(a=0,b=0, lty=5, col="gray")
text(index, results$dominance.mean, prezzes, cex=.75)

plot3d(results$valence.mean[8:14], results$arousal.mean[8:14], results$dominance.mean[8:14], size=5, type="s", col="red")
plot3d(results$valence.mean[15:16], results$arousal.mean[15:16], results$dominance.mean[15:16], size=8, type="s", col="cyan", add=T, box=F)

head(matches.vec.no.na)
View(norms.vec)



# see how many words were removed through this process
N1 <- ncol(wordmat)
N2 <-  ncol(TF)
Diff <- N1 - N2
Diff
#were all stop words found in the speeches?
N3 <- length(stopwords)
N3 - Diff

#plot unique, non-stop words.
N <- nrow(TF)
unique_non_stop_words <- numeric(N)
for(i in 1:N){
	TF_unq <- TF[i,]
	non_zero <- TF_unq[which(TF_unq!=0)]
	unique_non_stop_words[i] <- ncol(non_zero)
}
plot(unique_non_stop_words, type="l")

#CREATE TFIDF MATRIX

sum(apply(TF, 2, function(x){all(x>0)}))
sum(apply(TF, 2, function(x){sum(x>0)==1}))

count <- apply(TF, 2, function(x)sum(x>0))
IDF <- log(2*16/count)
TFIDF <- TF
for(j in 1:ncol(TF)){
	 TFIDF[,j] <- TF[, j] * IDF[j]
}

#plot the importance of various words over time
par(mfrow=c(3,3))
plot(TFIDF[,"children"], axes=T, type="l")
plot(TFIDF[,"terror"], axes=T, type="l")
plot(TFIDF[,"peace"], axes=T, type="l")
plot(TFIDF[,"war"], axes=T, type="l")
plot(TFIDF[,"hussein"], axes=T, type="l")
plot(TFIDF[,"nuclear"], axes=T, type="l")
plot(TFIDF[,"tax"], axes=T, type="l")
plot(TFIDF[,"drugs"], axes=T, type="l")
plot(TFIDF[,"immigration"], axes=T, type="l")

#list the top ten words for every candidate and every year
#CLINTON
names(sort(TFIDF[1,], decreasing=T)[1:10]) #1994
names(sort(TFIDF[2,], decreasing=T)[1:10]) #1995
names(sort(TFIDF[3,], decreasing=T)[1:10]) #1996
names(sort(TFIDF[4,], decreasing=T)[1:10]) #1997
names(sort(TFIDF[5,], decreasing=T)[1:10]) #1998
names(sort(TFIDF[6,], decreasing=T)[1:10]) #1999
names(sort(TFIDF[7,], decreasing=T)[1:10]) #2000

#BUSH
names(sort(TFIDF[8,], decreasing=T)[1:10]) #2002
names(sort(TFIDF[9,], decreasing=T)[1:10]) #2003
names(sort(TFIDF[10,], decreasing=T)[1:10]) #2004
names(sort(TFIDF[11,], decreasing=T)[1:10]) #2005
names(sort(TFIDF[12,], decreasing=T)[1:10]) #2006
names(sort(TFIDF[13,], decreasing=T)[1:10]) #2007
names(sort(TFIDF[14,], decreasing=T)[1:10]) #2008

#OBAMA
names(sort(TFIDF[15,], decreasing=T)[1:10]) #2010
names(sort(TFIDF[16,], decreasing=T)[1:10]) #2011

#hierarchical clustering by candidate. Clusters are determined by TFIDF scores
labels <- list("Clinton94","Clinton95","Clinton96","Clinton97", "Clinton98","Clinton99","Clinton00", "Bush02","Bush03", "Bush04", "Bush05", "Bush06","Bush07", "Bush08", "Obama10","Obama11")
plot.new()
plot(hclust(dist(TFIDF),method="ward"), labels=labels, main='Clusters of SOTU Speeches by TFIDF',cex.main=.8,cex=.8)



## LATENT SEMANTIC INDEXING
LSI <- svd(TFIDF)

##first concept from Latent Semantic Indexing
#through LSI we can see that the first column of U is indeed all negative.
LSI$u[,1]
#The ten terms that are mostly associated with the first concept are shown below. We can see there are standard politicans’ talking points.
names(TF)[LSI$v[,1] %in% sort(LSI$v[,1])[1:10]]

# The second concept is shown below. Notice all Clinton’s speeches have positive values and all Bush’s speeches have negative values, while Obama’s speeches are very close to zero. The ten most negative terms are associated with anti-terrorism, which make sense considering the negative values of all the Bush’s speeches. On the other hands, the ten most positive terms are associated with domestic/economic policy and Clinton administration.
## second concept
LSI$u[,2]

names(TF)[LSI$v[,2] %in% sort(LSI$v[,2])[1:10]]

names(TF)[LSI$v[,2] %in% sort(LSI$v[,2], decreasing=T)[1:10]]

###############################
#                   PART    4                       #
###############################
## unknown speech
unknown <- scan("SOTU/unknown2.txt", what="c")

## delete stopwords
unknown <- subset(unknown, is.na(match(unknown, stopwords)))

## delete words that are not in the bush/clinton speeches
unknown <- subset(unknown, !is.na(match(unknown, names(TF))))

## only look at unique words
unknown <- unique(unknown)

## words count for all clinton speeches
clinton.count.by.speech <- apply(TF[1:7,], 2, function(x) sum(x>0))
clinton.count.by.word <- apply(TF[1:7, ], 2, sum)

## words count for all bush speeches
bush.count.by.speech <- apply(TF[8:14,], 2, function(x) sum(x>0))
bush.count.by.word <- apply(TF[8:14, ], 2, sum)

# calculate total words by each politician (bush and clinton)
TotalBushWords <- sum(bush.count.by.word)
TotalClintonWords <- sum(clinton.count.by.word)
ccount.speech <- clinton.count.by.speech[match(unknown, names(TF))]
ccount.word <- clinton.count.by.word[match(unknown, names(TF))]
bcount.speech <- bush.count.by.speech[match(unknown, names(TF))]
bcount.word <- bush.count.by.word[match(unknown, names(TF))]
unknown.count <- data.frame(unknown, ccount.speech, ccount.word, bcount.speech, bcount.word)

unknown.count <- within(unknown.count, {
log.prob.bush.by.speech <- log((bcount.speech+1)/(7+2))
log.prob.clinton.by.speech <- log((ccount.speech+1)/(7+2))
log.prob.bush.by.word <- log((bcount.word+1)/(TotalBushWords+2))
log.prob.clinton.by.word <- log((ccount.word+1)/(TotalClintonWords+2))
})

with(unknown.count, {sum(log.prob.clinton.by.speech)})

with(unknown.count, {sum(log.prob.bush.by.speech)})

with(unknown.count, {sum(log.prob.clinton.by.word)})

with(unknown.count, {sum(log.prob.bush.by.word)})


#generate density estimation plots
# GREEN = CLINTON, #RED = BUSH
plot.new()
par(mfrow=c(2,1))
plot(density(unknown.count$log.prob.clinton.by.speech, from=-2.5,to=0), col=2, main="Density Estimation (probability calculated by speeches)", xlab="log prob")
points(density(unknown.count$log.prob.bush.by.speech, from=-2.5, to=0), type="l", col=3)
plot(density(unknown.count$log.prob.clinton.by.word, from=-12, to=0), col=2, main="Density Estimation (probability calculated by words)", xlab="log.prob")
points(density(unknown.count$log.prob.bush.by.word, from=-12, to=0), type="l", col=3)

plot(unknown.count$log.prob.clinton.by.speech, col=3)

##LATENT DIRICHLET ALLOCATION
LDAresults <- LDA(TF, 10, method='Gibbs')
typeof(LDAresults)
ncol(TFIDF)

# fit a logistic model to predict "BUSH"
TFIDF$BUSH<- c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0)
model1 <- glm(BUSH ~ ., data=TFIDF, family=binomial("logit"))
model1.stp  <- stepAIC(model1, direction="both")

