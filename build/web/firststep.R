library(tm)

# Collect data
tweets.AC<-read.csv('E:/xampp/htdocs/FYP/web/ad.csv',header=T)
tweets.ACBW<-read.csv('E:/xampp/htdocs/FYP/web/fe.csv',header=T)
tweets.ACSF<-read.csv('E:/xampp/htdocs/FYP/web/others.csv',header=T)
tweets.test<-read.csv('E:/xampp/htdocs/FYP/web/testing.csv',header=T)
write.table("Tweet", file = "E:/xampp/htdocs/FYP/web/abc.csv",row.names=F, col.names=F, sep=",")
write.table("Tweet", file = "E:/xampp/htdocs/FYP/web/def.csv",row.names=F, col.names=F, sep=",")
write.table("Tweet", file = "E:/xampp/htdocs/FYP/web/ghi.csv",row.names=F, col.names=F, sep=",")
#write.table("", file = "E:/xampp/htdocs/FYP/web/datac.csv",row.names=F, col.names=F, sep=",")
tweets.AC["class"]<-rep("AC",nrow(tweets.AC))
tweets.ACBW["class"]<-rep("ACBW",nrow(tweets.ACBW))
tweets.ACSF["class"]<-rep("ACSF",nrow(tweets.ACSF))

# Helper Function
replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  x
}

# Do our punctuation stuff
tweets.AC$Tweet <- replacePunctuation(tweets.AC$Tweet)
tweets.ACBW$Tweet <- replacePunctuation(tweets.ACBW$Tweet)
tweets.ACSF$Tweet <- replacePunctuation(tweets.ACSF$Tweet)
tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)

# Create corpus
tweets.AC.corpus <- Corpus(VectorSource(as.vector(tweets.AC$Tweet)))
tweets.ACBW.corpus <- Corpus(VectorSource(as.vector(tweets.ACBW$Tweet)))
tweets.ACSF.corpus <- Corpus(VectorSource(as.vector(tweets.ACSF$Tweet)))
tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))

# Create term document matrix
tweets.AC.matrix <- t(TermDocumentMatrix(tweets.AC.corpus,control = list(wordLengths=c(3,Inf))));
tweets.ACBW.matrix <- t(TermDocumentMatrix(tweets.ACBW.corpus,control = list(wordLengths=c(3,Inf))));
tweets.ACSF.matrix <- t(TermDocumentMatrix(tweets.ACSF.corpus,control = list(wordLengths=c(3,Inf))));
tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(3,Inf))));

# Probability Matrix
probabilityMatrix <-function(docMatrix)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  # Add one
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  # Calculate the natural log of the probabilities
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Add pretty names to the columns
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

tweets.AC.pMatrix<-probabilityMatrix(tweets.AC.matrix)
tweets.ACBW.pMatrix<-probabilityMatrix(tweets.ACBW.matrix)
tweets.ACSF.pMatrix<-probabilityMatrix(tweets.ACSF.matrix)
#Predict

# Get the test matrix
# Get words in the first document

getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
  # Count how many words were not found in the mandrill matrix
  charactersNotFound<-length(testChars)-length(charactersFound)
  # Add the normalized probabilities for the words founds together
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  # We use ln(1/total smoothed words) for words not found
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  #This is our probability
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}

# Get the matrix
tweets.test.matrix<-as.matrix(tweets.test.matrix)

# A holder for classification 
classified<-NULL

for(documentNumber in 1:nrow(tweets.test.matrix))
{
  # Extract the test words
  tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
  # Get the probabilities
  ACProbability <- getProbability(tweets.test.chars,tweets.AC.pMatrix)
  ACBWProbability <- getProbability(tweets.test.chars,tweets.ACBW.pMatrix)
  ACSFProbability <- getProbability(tweets.test.chars,tweets.ACSF.pMatrix)
  # Add it to the classification list
  classified<-c(classified,if(ACProbability>ACBWProbability && ACProbability>ACSFProbability){"AdmitCard"} else if(ACBWProbability>ACProbability && ACBWProbability>ACSFProbability) {"Fees"} else {"University is responsible for replying Acknowledgement"} )
}
i<-1
for(documentNumber in 1:nrow(tweets.test.matrix))
{ 
 if(classified[i]== "AdmitCard")
 {
   write.table(tweets.test$Tweet[i], file="E:/xampp/htdocs/FYP/web/abc.csv", append=T, row.names=F, col.names=F,  sep=",")
   # Collect data
   tweets.AC<-read.csv('E:/xampp/htdocs/FYP/web/AP.csv',header=T)
   tweets.ACBW<-read.csv('E:/xampp/htdocs/FYP/web/MS.csv',header=T)
   tweets.ACSF<-read.csv('E:/xampp/htdocs/FYP/web/AC.csv',header=T)
   tweets.ACTT<-read.csv('E:/xampp/htdocs/FYP/web/AT.csv',header=T)
   tweets.test<-read.csv('E:/xampp/htdocs/FYP/web/abc.csv',header=T)
   
   tweets.AC["class"]<-rep("AC",nrow(tweets.AC))
   tweets.ACBW["class"]<-rep("ACBW",nrow(tweets.ACBW))
   tweets.ACSF["class"]<-rep("ACSF",nrow(tweets.ACSF))
   tweets.ACTT["class"]<-rep("ACTT",nrow(tweets.ACTT))
   # Helper Function
   replacePunctuation <- function(x)
   {
     x <- tolower(x)
     x <- gsub("[.]+[ ]"," ",x)
     x <- gsub("[:]+[ ]"," ",x)
     x <- gsub("[?]"," ",x)
     x <- gsub("[!]"," ",x)
     x <- gsub("[;]"," ",x)
     x <- gsub("[,]"," ",x)
     x
   }
   
   # Do our punctuation stuff
   tweets.AC$Tweet <- replacePunctuation(tweets.AC$Tweet)
   tweets.ACBW$Tweet <- replacePunctuation(tweets.ACBW$Tweet)
   tweets.ACSF$Tweet <- replacePunctuation(tweets.ACSF$Tweet)
   tweets.ACTT$Tweet <- replacePunctuation(tweets.ACTT$Tweet)
   tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)
   
   # Create corpus
   tweets.AC.corpus <- Corpus(VectorSource(as.vector(tweets.AC$Tweet)))
   tweets.ACBW.corpus <- Corpus(VectorSource(as.vector(tweets.ACBW$Tweet)))
   tweets.ACSF.corpus <- Corpus(VectorSource(as.vector(tweets.ACSF$Tweet)))
   tweets.ACTT.corpus <- Corpus(VectorSource(as.vector(tweets.ACTT$Tweet)))
   tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))
   
   # Create term document matrix
   tweets.AC.matrix <- t(TermDocumentMatrix(tweets.AC.corpus,control = list(wordLengths=c(3,Inf))));
   tweets.ACBW.matrix <- t(TermDocumentMatrix(tweets.ACBW.corpus,control = list(wordLengths=c(3,Inf))));
   tweets.ACSF.matrix <- t(TermDocumentMatrix(tweets.ACSF.corpus,control = list(wordLengths=c(3,Inf))));
   tweets.ACTT.matrix <- t(TermDocumentMatrix(tweets.ACTT.corpus,control = list(wordLengths=c(3,Inf))));
   tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(3,Inf))));
   
   # Probability Matrix
   probabilityMatrix <-function(docMatrix)
   {
     # Sum up the term frequencies
     termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
     # Add one
     termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
     # Calculate the probabilties
     termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
     # Calculate the natural log of the probabilities
     termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
     # Add pretty names to the columns
     colnames(termSums)<-c("term","count","additive","probability","lnProbability")
     termSums
   }
   
   tweets.AC.pMatrix<-probabilityMatrix(tweets.AC.matrix)
   tweets.ACBW.pMatrix<-probabilityMatrix(tweets.ACBW.matrix)
   tweets.ACSF.pMatrix<-probabilityMatrix(tweets.ACSF.matrix)
   tweets.ACTT.pMatrix<-probabilityMatrix(tweets.ACTT.matrix)
   #Predict
   
   # Get the test matrix
   # Get words in the first document
   
   getProbability <- function(testChars,probabilityMatrix)
   {
     charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
     # Count how many words were not found in the mandrill matrix
     charactersNotFound<-length(testChars)-length(charactersFound)
     # Add the normalized probabilities for the words founds together
     charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
     # We use ln(1/total smoothed words) for words not found
     charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
     #This is our probability
     prob<-charactersFoundSum+charactersNotFoundSum 
     prob
   }
   
   # Get the matrix
   tweets.test.matrix<-as.matrix(tweets.test.matrix)
   
   # A holder for classification 
   classified<-NULL
   
   for(documentNumber in 1:nrow(tweets.test.matrix))
   {
     # Extract the test words
     tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
     # Get the probabilities
     ACProbability <- getProbability(tweets.test.chars,tweets.AC.pMatrix)
     ACBWProbability <- getProbability(tweets.test.chars,tweets.ACBW.pMatrix)
     ACSFProbability <- getProbability(tweets.test.chars,tweets.ACSF.pMatrix)
     ACTTProbability <- getProbability(tweets.test.chars,tweets.ACTT.pMatrix)
     # Add it to the classification list
       classified<-c(classified,if(ACProbability>ACBWProbability && ACProbability>ACSFProbability && ACProbability>ACTTProbability){"Please make sure that your photograph satisfies the instructions provided in 'Guideline for the Application-photograph' and should be within the required size and format."} else if(ACTTProbability>ACProbability && ACTTProbability>ACSFProbability && ACTTProbability>ACSFProbability) {"After confirmation of receipt of fee at the bank ADMIT CARD will be made available on the Admission portal five working days. You can get the admit card after logging in your Admission portal."} else if(ACBWProbability>ACProbability && ACBWProbability>ACSFProbability && ACBWProbability>ACTTProbability) {"Upload your marksheet in correct format.If u want to reupload it follow this procedure.If your marks are increased after scruitny then rewrite your correct marks and reupload your marksheet latest by date dy dygy last"} else {"This is one of the admit card problem solved by NED"} ) }
   write.csv(cbind(classified,tweets.test$Tweet), file="E:/xampp/htdocs/FYP/web/datac.csv")
    }
  else if(classified[i]== "Fees")
  {
    write.table(tweets.test$Tweet[i], file="E:/xampp/htdocs/FYP/web/def.csv", append=T, row.names=F, col.names=F,  sep=",")
    # Collect data
    tweets.AC<-read.csv('E:/xampp/htdocs/FYP/web/FP.csv',header=T)
    tweets.ACBW<-read.csv('E:/xampp/htdocs/FYP/web/FR.csv',header=T)
    tweets.ACSF<-read.csv('E:/xampp/htdocs/FYP/web/TID.csv',header=T)
    tweets.test<-read.csv('E:/xampp/htdocs/FYP/web/def.csv',header=T)
    
    tweets.AC["class"]<-rep("AC",nrow(tweets.AC))
    tweets.ACBW["class"]<-rep("ACBW",nrow(tweets.ACBW))
    tweets.ACSF["class"]<-rep("ACSF",nrow(tweets.ACSF))
    
    # Helper Function
    replacePunctuation <- function(x)
    {
      x <- tolower(x)
      x <- gsub("[.]+[ ]"," ",x)
      x <- gsub("[:]+[ ]"," ",x)
      x <- gsub("[?]"," ",x)
      x <- gsub("[!]"," ",x)
      x <- gsub("[;]"," ",x)
      x <- gsub("[,]"," ",x)
      x
    }
    
    # Do our punctuation stuff
    tweets.AC$Tweet <- replacePunctuation(tweets.AC$Tweet)
    tweets.ACBW$Tweet <- replacePunctuation(tweets.ACBW$Tweet)
    tweets.ACSF$Tweet <- replacePunctuation(tweets.ACSF$Tweet)
    tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)
    
    # Create corpus
    tweets.AC.corpus <- Corpus(VectorSource(as.vector(tweets.AC$Tweet)))
    tweets.ACBW.corpus <- Corpus(VectorSource(as.vector(tweets.ACBW$Tweet)))
    tweets.ACSF.corpus <- Corpus(VectorSource(as.vector(tweets.ACSF$Tweet)))
    tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))
    
    # Create term document matrix
    tweets.AC.matrix <- t(TermDocumentMatrix(tweets.AC.corpus,control = list(wordLengths=c(3,Inf))));
    tweets.ACBW.matrix <- t(TermDocumentMatrix(tweets.ACBW.corpus,control = list(wordLengths=c(3,Inf))));
    tweets.ACSF.matrix <- t(TermDocumentMatrix(tweets.ACSF.corpus,control = list(wordLengths=c(3,Inf))));
    tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(3,Inf))));
    
    # Probability Matrix
    probabilityMatrix <-function(docMatrix)
    {
      # Sum up the term frequencies
      termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
      # Add one
      termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
      # Calculate the probabilties
      termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
      # Calculate the natural log of the probabilities
      termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
      # Add pretty names to the columns
      colnames(termSums)<-c("term","count","additive","probability","lnProbability")
      termSums
    }
    
    tweets.AC.pMatrix<-probabilityMatrix(tweets.AC.matrix)
    tweets.ACBW.pMatrix<-probabilityMatrix(tweets.ACBW.matrix)
    tweets.ACSF.pMatrix<-probabilityMatrix(tweets.ACSF.matrix)
    #Predict
    
    # Get the test matrix
    # Get words in the first document
    
    getProbability <- function(testChars,probabilityMatrix)
    {
      charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
      # Count how many words were not found in the mandrill matrix
      charactersNotFound<-length(testChars)-length(charactersFound)
      # Add the normalized probabilities for the words founds together
      charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
      # We use ln(1/total smoothed words) for words not found
      charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
      #This is our probability
      prob<-charactersFoundSum+charactersNotFoundSum 
      prob
    }
    
    # Get the matrix
    tweets.test.matrix<-as.matrix(tweets.test.matrix)
    
    # A holder for classification 
    classified<-NULL
    
    for(documentNumber in 1:nrow(tweets.test.matrix))
    {
      # Extract the test words
      tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
      # Get the probabilities
      ACProbability <- getProbability(tweets.test.chars,tweets.AC.pMatrix)
      ACBWProbability <- getProbability(tweets.test.chars,tweets.ACBW.pMatrix)
      ACSFProbability <- getProbability(tweets.test.chars,tweets.ACSF.pMatrix)
      # Add it to the classification list
      classified<-c(classified,if(ACProbability>ACBWProbability && ACProbability>ACSFProbability){"
Following procedure is to be followed for the payment of the application fee: i) After completing online Application Form  required application fee (including selffinance/sponsor fee  if applicable) shall be indicated on the same website on a Transaction Slip/Voucher  which contains an important number called Transaction ID. ii) Take a Print-out of Transaction Slip/Voucher and pay the indicated fee at any branch of banks specified on Transaction Slip/Voucher iii) It is very important that you take the print out of Transaction Slip/Voucher  as the payment at the bank will be against that Transaction ID mentioned on the Transaction Slip/Voucher."} else if(ACBWProbability>ACProbability && ACBWProbability>ACSFProbability) {"Yes when you opt for Regular Seat total self-finance fee will be refunded but if you get admission under Self Finance seat then the amount will not be refunded."} else {"fee (including selffinance/sponsor fee  if applicable) shall be indicated on the same website on a Transaction Slip/Voucher  which contains an important number called Transaction ID. ii) Take a Print-out of Transaction Slip/Voucher and pay the indicated fee at any branch of banks specified on Transaction Slip/Voucher iii) It is very important that you take the print out of Transaction Slip/Voucher  as the payment at the bank will be against that Transaction ID mentioned on the Transaction Slip/Voucher."} )
    }
    write.csv(cbind(classified,tweets.test$Tweet), file="E:/xampp/htdocs/FYP/web/datac.csv")
    
    
    
     }
else {
  write.table(tweets.test$Tweet[i], file="E:/xampp/htdocs/FYP/web/ghi.csv", append=T, row.names=F, col.names=F,  sep=",")
  write.csv(cbind(classified,tweets.test$Tweet), file="E:/xampp/htdocs/FYP/web/datac.csv")
  }
  i<-i+1
}


