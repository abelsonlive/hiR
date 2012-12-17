aff.norms <- read.csv("/Users/brianabelson/Dropbox/HI LAB/Research/WALKING DEAD STUDY/CHEX EXPORT/content analysis/sentiment/aff.norms3.csv")
N.docs<- nrow(TF)
results<-data.frame(matrix(0,nrow=N.docs,ncol=7))
names(results) <- c("doc_id","valence_mean", "arousal_mean", "dominance_mean", "valence_var", "arousal_var", "dominance_var")
for (i in 1:N.docs){

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
  valence.mean      <-  sum(aff.norms.of.doc$valence*word.counts)/ sum(word.counts)
  arousal.mean      <-  sum(aff.norms.of.doc$arousal*word.counts)/ sum(word.counts)
  dominance.mean <-  sum(aff.norms.of.doc$dominance*word.counts)/sum(word.counts)
  valence.var      <-  abs(var(aff.norms.of.doc$valence))
  arousal.var      <-  abs(var(aff.norms.of.doc$arousal))
  dominance.var <-  abs(var(aff.norms.of.doc$dominance))

  #STEP 4#
   # Bind these results into a dataframe which consists of the sum of scores
   #divided by the total words in the document
  results[i,] <- data.frame(valence.mean, arousal.mean, dominance.mean, valence.var, arousal.var, dominance.var)
}
