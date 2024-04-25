library(tm)
library(stringr)
library(rlist)
library(tokenizers)
library(lexicon)
library(wordcloud)

VAD<- NRC_VAD

Valence_Pos_75_Data <- subset(VAD[1:2], VAD$Valence >= .75,
                              select=c(Word))
Valence_Pos_75_Data<- unlist(Valence_Pos_75_Data)

Valence_Neg_25_Data <- subset(VAD[1:2], VAD$Valence <= .25,
                              select=c(Word))
Valence_Neg_25_Data<- unlist(Valence_Neg_25_Data)
length(Valence_Neg_25_Data)

#x <- (seq(1,200,1))
df1<-list()

for (i in 2:length(x)){
  
  iteration <- 
  folder<-paste(("data)file.csv"), iteration)

  folder<-gsub(" ", "", folder)
  #folder<- file.names
  filelist<-list.files(path=folder, full.names = FALSE)
  cbind(filelist)
  #list.txt <- dir(pattern = "*.txt") # creates the list of all the asc files in the directory
  if (length(filelist)<3){

  }else{
    
    
    #write.csv(filelist, "UK_filelist")
    filelist<-paste(folder,"/", filelist, sep="")
    typeof(filelist)
    filelist<-lapply(filelist, FUN = readLines)
    corpus.list<-lapply(filelist, FUN=paste, collapse= " ")
    
    corpus.list2<- gsub(pattern = "\\W", replace= " ", corpus.list)
    corpus.list2<- gsub(pattern = "\\d", replace=" ", corpus.list2)
    corpus.list2<-tolower((corpus.list2))
    corpus.list2<-removeWords(corpus.list2, stopwords())
    corpus.list2<- gsub(pattern ="\\b[A-z]\\b{1}", replace= " ", corpus.list2)
    corpus.list2<-stripWhitespace(corpus.list2)

    
    corpus.official<- Corpus(VectorSource(corpus.list2))
    tdm<- TermDocumentMatrix(corpus.official)

    corpus3<- str_split(corpus.list2, pattern = "\\s+")
    tokens<-unlist(corpus3)
    
    Number_Words<- lapply(corpus3, function(x){ sum(!is.na(match(x,tokens)))})
    Number_Words<-unlist(Number_Words)
    
    Number_Pas_Val<- lapply(corpus3, function(x){ sum(!is.na(match(x,Valence_Pos_75_Data)))})
    Number_Pas_Val <-unlist(Number_Pas_Val)
    Number_Neg_Val<- lapply(corpus3, function(x){ sum(!is.na(match(x,Valence_Neg_25_Data)))})
    Number_Neg_Val <-unlist(Number_Neg_Val)

    
    
    PosDivWords<- (Number_Pas_Val/Number_Words)
    PosDivWords<- as.numeric(PosDivWords)
    MeanPosDivWords<- mean(PosDivWords)
    as.data.frame(PosDivWords)
    
    NegDivWords<- (Number_Neg_Val/Number_Words)
    NegDivWords<- as.numeric(NegDivWords)
    MeanNegDivWords<- mean(NegDivWords)
    as.data.frame(NegDivWords)
    

    
    TemporalOutput<- cbind(PosDivWords,NegDivWords )
    as.data.frame(TemporalOutput)
    
    
  }
  df1<-rbind(df1,TemporalOutput)
  #df2<-rbind(df2,output2)
  #df3<-rbind(df3,output3)
  
  print(i)
}
df1
write.csv(df1, "test")
#write.csv(df1,"Test3")
