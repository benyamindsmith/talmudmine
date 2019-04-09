# install.packages("tidyverse")
# install.packages("qdapTools")

setwd("E:/")

library(qdapTools)
library(stringi)
library(stringr)
library(reshape2)
library(ggplot2)
library(readr)
library(tibble)

a2<-read_docx("Talmud Mining/2a.docx")
b2<-read_docx("Talmud Mining/2b.docx")
a3<-read_docx("Talmud Mining/3a.docx")
b3<-read_docx("Talmud Mining/3b.docx")


#'Data seems to be seperated by phrases already. 
#'Lets try to get the word frequencies

a2p<-paste(a2, collapse="")
b2p<-paste(b2, collapse="")
a3p<-paste(a3,collapse="")
b3p<-paste(b3, collapse="")




###############Approach A#########################

#' "the Talmud" 

tlmd<- list("2a"=a2p, "2b"=b2p, "3a"=a3p, "3b"=b3p)

#'Clean and split into individual elements
tlmd<-lapply(tlmd,enc2utf8)  #preserves text
tlmd<-lapply(tlmd, function(x) str_replace_all(x, "[^[:alnum:]]", " "))
tlmd<-lapply(tlmd,function(x) str_split(x," "))


#'Get frequency tables (by amud)

tlmdtab<-lapply(tlmd,table)

#Sort frequencies in asecending order
tlmdtab<-lapply(tlmdtab,function(x) sort(x,decreasing=TRUE))

#'Get rid of empty space values
tlmdtab$`2a`[1]<-NA
tlmdtab$`2b`[1]<-NA
tlmdtab$`3a`[1]<-NA
tlmdtab$`3b`[1]<-NA


##################Approach B #######################
#'For the entire corpus

tlmdM<-paste(c(a2p,b2p,a3p,b3p),collapse="")

#' Clean and split individual elements
#' 
tlmdM<-sapply(tlmdM,function(x)  iconv(x, "UTF-8", "UTF-8"))  #preserves text
tlmdM<-sapply(tlmdM, function(x) str_replace_all(x, "[^[:alnum:]]", " "))
tlmdM<- strsplit(tlmdM," ")

tlmdMtab<-table(tlmdM)
tlmdMtab<-sort(tlmdMtab, decreasing = TRUE)

#'Too much trouble with encoding via data frames. Best to use tibble
tlmdMtab<-as.tibble(tlmdMtab)

 #'Clean
              
 tlmdMtab<-tlmdMtab[2:741,]
#'Reduce size, get top 50

st<-tlmdMtab[1:50,]

ggplot(st,
       aes(x=n,
           fill=tlmdM))+
  geom_bar()
