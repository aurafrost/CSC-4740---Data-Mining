#Jimmy Tran
#0395
#Data Mining CSC 4740
library(tm) #import for tm package
require(tm) #check if tm is present
library(slam) #import slam package
require(slam) #require slam package
library(SnowballC) #import SnowballC package
require(SnowballC) #require SnowballC package
library(lsa) #import lsa package
require(lsa) #require lsa package
#keywords 
#{statistics, analysis, linear, data,
#learning, mining, knowledge, machine,
#information, pattern, predictive, sampling,
#dimensional, vector, intelligence, algorithm}

dm<-scan("DataMining.txt", character(0)) #reads .txt file
dm<-VCorpus(VectorSource(dm)) #dm to corpus
dm<-tm_map(dm, content_transformer(tolower)) #changes all words to lowercase
dm<-tm_map(dm, removePunctuation) #removes punctuation
dm<-tm_map(dm, removeNumbers) #removes numbers
dmdtm<-TermDocumentMatrix(dm) #creates a term doc matrix
#get term counts in given text file
count1<-sum(tm_term_score(dmdtm,"statistics"))
count2<-sum(tm_term_score(dmdtm,"analysis"))
count3<-sum(tm_term_score(dmdtm,"linear"))
count4<-sum(tm_term_score(dmdtm,"data"))
count5<-sum(tm_term_score(dmdtm,"learning"))
count6<-sum(tm_term_score(dmdtm,"mining"))
count7<-sum(tm_term_score(dmdtm,"knowledge"))
count8<-sum(tm_term_score(dmdtm,"machine"))
count9<-sum(tm_term_score(dmdtm,"information"))
count10<-sum(tm_term_score(dmdtm,"pattern"))
count11<-sum(tm_term_score(dmdtm,"predictive"))
count12<-sum(tm_term_score(dmdtm,"sampling"))
count13<-sum(tm_term_score(dmdtm,"dimensional"))
count14<-sum(tm_term_score(dmdtm,"vector"))
count15<-sum(tm_term_score(dmdtm,"intelligence"))
count16<-sum(tm_term_score(dmdtm,"algorithm"))
dmcounts<-c(count1,count2,count3,count4,count5,count6,count7,count8,
            count9,count10,count11,count12,count13,count14,count15,count16)

la<-scan("LinearAlgebra.txt", character(0))
la<-VCorpus(VectorSource(la)) #la to corpus
la<-tm_map(la, content_transformer(tolower)) #changes all words to lowercase
la<-tm_map(la, removePunctuation) #removes punctuation
la<-tm_map(la, removeNumbers) #removes numbers
ladtm<-TermDocumentMatrix(la) #creates a term doc matrix
#get term counts in given text file
count1<-sum(tm_term_score(ladtm,"statistics"))
count2<-sum(tm_term_score(ladtm,"analysis"))
count3<-sum(tm_term_score(ladtm,"linear"))
count4<-sum(tm_term_score(ladtm,"data"))
count5<-sum(tm_term_score(ladtm,"learning"))
count6<-sum(tm_term_score(ladtm,"mining"))
count7<-sum(tm_term_score(ladtm,"knowledge"))
count8<-sum(tm_term_score(ladtm,"machine"))
count9<-sum(tm_term_score(ladtm,"information"))
count10<-sum(tm_term_score(ladtm,"pattern"))
count11<-sum(tm_term_score(ladtm,"predictive"))
count12<-sum(tm_term_score(ladtm,"sampling"))
count13<-sum(tm_term_score(ladtm,"dimensional"))
count14<-sum(tm_term_score(ladtm,"vector"))
count15<-sum(tm_term_score(ladtm,"intelligence"))
count16<-sum(tm_term_score(ladtm,"algorithm"))
lacounts<-c(count1,count2,count3,count4,count5,count6,count7,count8,
            count9,count10,count11,count12,count13,count14,count15,count16)

ml<-scan("MachineLearning.txt", character(0))
ml<-VCorpus(VectorSource(ml)) #ml to corpus
ml<-tm_map(ml, content_transformer(tolower)) #changes all words to lowercase
ml<-tm_map(ml, removePunctuation) #removes punctuation
ml<-tm_map(ml, removeNumbers) #removes numbers
mldtm<-TermDocumentMatrix(ml) #creates a term doc matrix
#get term counts in given text file
count1<-sum(tm_term_score(mldtm,"statistics"))
count2<-sum(tm_term_score(mldtm,"analysis"))
count3<-sum(tm_term_score(mldtm,"linear"))
count4<-sum(tm_term_score(mldtm,"data"))
count5<-sum(tm_term_score(mldtm,"learning"))
count6<-sum(tm_term_score(mldtm,"mining"))
count7<-sum(tm_term_score(mldtm,"knowledge"))
count8<-sum(tm_term_score(mldtm,"machine"))
count9<-sum(tm_term_score(mldtm,"information"))
count10<-sum(tm_term_score(mldtm,"pattern"))
count11<-sum(tm_term_score(mldtm,"predictive"))
count12<-sum(tm_term_score(mldtm,"sampling"))
count13<-sum(tm_term_score(mldtm,"dimensional"))
count14<-sum(tm_term_score(mldtm,"vector"))
count15<-sum(tm_term_score(mldtm,"intelligence"))
count16<-sum(tm_term_score(mldtm,"algorithm"))
mlcounts<-c(count1,count2,count3,count4,count5,count6,count7,count8,
            count9,count10,count11,count12,count13,count14,count15,count16)

st<-scan("Statistics.txt", character(0))
st<-VCorpus(VectorSource(st)) #st to corpus
st<-tm_map(st, content_transformer(tolower)) #changes all words to lowercase
st<-tm_map(st, removePunctuation) #removes punctuation
st<-tm_map(st, removeNumbers) #removes numbers
stdtm<-TermDocumentMatrix(st) #creates a term doc matrix
#get term counts in given text file
count1<-sum(tm_term_score(stdtm,"statistics"))
count2<-sum(tm_term_score(stdtm,"analysis"))
count3<-sum(tm_term_score(stdtm,"linear"))
count4<-sum(tm_term_score(stdtm,"data"))
count5<-sum(tm_term_score(stdtm,"learning"))
count6<-sum(tm_term_score(stdtm,"mining"))
count7<-sum(tm_term_score(stdtm,"knowledge"))
count8<-sum(tm_term_score(stdtm,"machine"))
count9<-sum(tm_term_score(stdtm,"information"))
count10<-sum(tm_term_score(stdtm,"pattern"))
count11<-sum(tm_term_score(stdtm,"predictive"))
count12<-sum(tm_term_score(stdtm,"sampling"))
count13<-sum(tm_term_score(stdtm,"dimensional"))
count14<-sum(tm_term_score(stdtm,"vector"))
count15<-sum(tm_term_score(stdtm,"intelligence"))
count16<-sum(tm_term_score(stdtm,"algorithm"))
stcounts<-c(count1,count2,count3,count4,count5,count6,count7,count8,
            count9,count10,count11,count12,count13,count14,count15,count16)

allcounts<-c(dmcounts,lacounts,dmcounts,stcounts) #put all counts into one vector
allmatrix<-matrix(allcounts,nrow=16,ncol=4,byrow = TRUE) #convert vector to matrix
cosine(allmatrix) #cosine function
#Output
#          [,1]      [,2]       [,3]       [,4]
#[1,] 1.0000000 0.4616621 0.15981760 0.27725515
#[2,] 0.4616621 1.0000000 0.27769201 0.27022121
#[3,] 0.1598176 0.2776920 1.00000000 0.03896548
#[4,] 0.2772551 0.2702212 0.03896548 1.00000000