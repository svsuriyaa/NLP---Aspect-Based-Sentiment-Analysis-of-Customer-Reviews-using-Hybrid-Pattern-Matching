#**********************************************************************************************************************************************************
# 
#                                            TEAM RVESTERS - VISUALIZATION - PART 1
#
#**********************************************************************************************************************************************************


#Packages to be loaded
#install.packages("stringi")
#install.packages("stringr")

#Libraries to be loaded
library(stringi) 
library(stringr)

#Reading the dataset - The directory where the datsets are loaded is to be mentioned here
polarities<-read.csv("phrases_polarities")

#Forming a new column called 'Extracted' to split the opinions/adjectives from Feature/noun
for (i in 1:nrow(polarities))
{
  polarities$StrAspects[i] <- toString(polarities$Aspects[i])#Converting to string in order to use 'gsub()' function
  polarities$StrPhrases[i] <- toString(polarities$Phrases[i])
  polarities$Extracted[i] <- gsub(polarities$StrAspects[i],"",polarities$StrPhrases[i],fixed = T)#Replaces the 'Feature' from StrPhrases with " "
}

#Forming a new data frame to map the feature/noun to every word of the opinion. For Eg: Aspect = Phone, Phrase = seem good, Phrase is splitted as "seem","good" and mapped to the Aspect.
#So, two rows are formed with 'Phone' in 'Aspect' column for two rows and 'seem','good' in two separate rows
new_split = data.frame(matrix(nrow=20000,ncol=8))
colnames(new_split) <- c("Aspect","Adjective","AspAdj","ProductAspect","ServiceAspect","Polarity","SentimentTag","Merged")

#Initializing counter for using inside the for loop
counter = 1

#Splitting the adjectives
for (i in 1:nrow(polarities))
{ 
  for (j in 1:length(unlist(str_split(polarities$Extracted[i]," ")))-1)
  {
    split = unlist(str_split(stri_trim(polarities$Extracted[i])," "))
    
    #Assigning values  from 'polarities' to the data frame 'new_split'
    new_split$Adjective[counter] = toString(split[j])
    new_split$Aspect[counter] = polarities$StrAspects[i]
    new_split$ProductAspect[counter] = polarities$ProductAspect[i]
    new_split$ServiceAspect[counter] = polarities$ServiceAspect[i]
    new_split$Polarity[counter] = polarities$Polarity[i]
    counter = counter + 1
  }
}

#Adding the Sentiment Tag to the dataframe 'new_split' by checking the values of polarity
for (i in 1:nrow(new_split))
{
  new_split$SentimentTag[i] <- ifelse(new_split$Polarity[i] > 0,'Positive',ifelse(new_split$Polarity[i] < 0,'Negative','Neutral'))
}

#Forming the noun- opinion/adjective/adverb pair - in order to find the frequency of bi grams most commonly used

for (k in 1:nrow(new_split))
{
  new_split$AspAdj[k] <- paste(new_split$Aspect[k],new_split$Adjective[k])
}

#Removing the unnecessary columns
new_split <- subset(new_split,new_split$Adjective!="")

#Forming the 'Merged' column - Aspect + Adjective + SentimentTag - to find the frequency at that level
new_split$Merged <- paste(new_split$AspAdj,new_split$SentimentTag)

#Finding the frequency of Aspect - Adjective - Sentiment
Freq_Pairs = data.frame(sort(table(new_split$Merged),desc=TRUE))
colnames(Freq_Pairs)= c("Merged","Freq")
new_split_1<-unique(merge(new_split, Freq_Pairs, by = "Merged"))
new_split_1$Polarity<-NULL#Since the Sentiment Tag is derived already, we do not need the column 'Polarity'


#Grouping in order to find the Aspect - Adjective - Sentiment in new_split table so that we can find out the number of times the customer has mentioned that feature and the tone of it(Positive, Neutral, Negative)
new_split_2<- aggregate(new_split_1$Freq,by = list(category = new_split_1$Merged,new_split_1$Aspect,new_split_1$Adjective,new_split_1$AspAdj,new_split_1$SentimentTag),FUN=sum)
colnames(new_split_2) = c("Merged","Aspect","Adjective","AspAdj","SentimentTag","Freq")

#Selecting the Aspect - Adjective pair mentioned more than once
Top_Freq <- data.frame(subset(new_split_2,new_split_2$Freq>1))
