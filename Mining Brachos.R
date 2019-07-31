#'Mining the Mishna
#'
#'A project by Benyamin (Benjamin) Smith (email: <benyamindsmith at gmail.com>)
#'
#'Because the Mishna is so vast, lets start with one Mesechta first. 
#'The Mesechta that we are going to look at will be Meseches Brachos. 
#'
#'First we need to gather the data. This can be done with some functions I made using the 
#' rvest package. Lets now define them.
library(rvest)
#'For extracting text: 
pipeit<-function(url,code){
  read_html(url)%>%html_nodes(code)%>%html_text()
}
#'For extracting links: 
pipelink<-function(url,code){
  read_html(url)%>%html_nodes(code)%>%html_attr("href")
}
#'For extracting images (I don't think we are going to need this one, but here it is):
pipeimg<-function(url,code){
  read_html(url)%>%html_nodes(code)%>%html_attr("src")
}
#'Lets now get the set of links for the Mishnayos in Brachos as a list by perek
#####
links<-list(PerekAleph=c("https://www.sefaria.org/Mishnah_Berakhot.1.1?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.1.2?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.1.3?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.1.4?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.1.5?lang=he&with=all&lang2=he"),
         PerekBeis=c("https://www.sefaria.org/Mishnah_Berakhot.2.1?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.2?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.3?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.4?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.5?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.6?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.7?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.2.8?lang=he&with=all&lang2=he"),
         PerekGimmel=c("https://www.sefaria.org/Mishnah_Berakhot.3.1?lang=he&with=all&lang2=he",
                       "https://www.sefaria.org/Mishnah_Berakhot.3.2?lang=he&with=all&lang2=he",
                       "https://www.sefaria.org/Mishnah_Berakhot.3.3?lang=he&with=all&lang2=he",
                       "https://www.sefaria.org/Mishnah_Berakhot.3.4?lang=he&with=all&lang2=he",
                       "https://www.sefaria.org/Mishnah_Berakhot.3.5?lang=he&with=all&lang2=he",
                       "https://www.sefaria.org/Mishnah_Berakhot.3.6?lang=he&with=all&lang2=he"),
         PerekDaled=c("https://www.sefaria.org/Mishnah_Berakhot.4.1?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.2?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.3?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.4?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.5?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.6?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.4.7?lang=he&with=all&lang2=he"),
         PerekHey=c("https://www.sefaria.org/Mishnah_Berakhot.5.1?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.5.2?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.5.3?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.5.4?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.5.5?lang=he&with=all&lang2=he"),
         PerekVov=c("https://www.sefaria.org/Mishnah_Berakhot.6.1?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.2?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.3?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.4?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.5?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.6?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.7?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.6.8?lang=he&with=all&lang2=he"),
         PerekZayin=c("https://www.sefaria.org/Mishnah_Berakhot.7.1?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.7.2?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.7.3?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.7.4?lang=he&with=all&lang2=he",
                      "https://www.sefaria.org/Mishnah_Berakhot.7.5?lang=he&with=all&lang2=he"),
         PerekChes=c("https://www.sefaria.org/Mishnah_Berakhot.8.1?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.2?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.3?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.4?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.5?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.6?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.7?lang=he&with=all&lang2=he",
                     "https://www.sefaria.org/Mishnah_Berakhot.8.8?lang=he&with=all&lang2=he"),
         PerekTes=c("https://www.sefaria.org/Mishnah_Berakhot.9.1?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.9.2?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.9.3?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.9.4?lang=he&with=all&lang2=he",
                    "https://www.sefaria.org/Mishnah_Berakhot.9.5?lang=he&with=all&lang2=he"))
#####
#'Now lets get all the text. Remember to add Sys.sleep() to imitate human use
#' PSA- Please use Sefaria API- this was created before I knew about it 
brachos<-lapply(links,function(x) sapply(x, function(y) {Sys.sleep(sample(10, 1) * 0.1)
                                                        pipeit(y,".highlight")}))

#'Lets clean it to get the Hebrew text exclusively
brachos<-lapply(brachos,function(x) sapply(x, function(y) gsub("[A-z]","",y)))
brachos<-lapply(brachos,function(x) sapply(x, function(y) gsub("[0-9]","",y)))
brachos<-lapply(brachos, function(x) sapply(x, function(y) gsub("\\n.*","",y)))
brachos<-lapply(brachos, function(x) sapply(x, function(y) trimws(y)))
brachos<-lapply(brachos, function(x) sapply(x, function(y) substring(y,3))) #Added this here to remove headings
#####
#'Lets create a list of the Taanaim in the Maseches Brachos (Note: This is complete)
tannaim<-c("רַבִּי אֱלִיעֶזֶר",
           "חֲכָמִים",
           "רַבָּן גַּמְלִיאֵל",
           "רַבִּי יְהוֹשֻׁעַ",
           "בֵּית שַׁמַּאי",
           "בֵית הִלֵּל",
           "רַבִּי טַרְפוֹן",
           "רַבִּי אֶלְעָזָר בֶּן עֲזַרְיָה",
           "רַבִּי מֵאִיר",
           "רַבִּי יְהוּדָה",
           "רַבִּי יוֹסֵי",
           "רַבָּן שִׁמְעוֹן בֶּן גַּמְלִיאֵל" ,
           "רַבִּי נְחוּנְיָא בֶּן הַקָּנָה",
           "רַבִּי עֲקִיבָא",
           "רַבִּי חֲנִינָא בֶן דּוֹסָא",
           "רַבִּי יִשְׁמָעֵאל")
#####
#'To make this readable in R lets convert taanaim to utf8

tanint<-sapply(tannaim,utf8ToInt)

#'reconvert

tanutf<-lapply(tanint,intToUtf8)
tanutf<-unlist(unname(tanutf))
tanutf<-utf8::utf8_normalize(tannaim)

#'Now lets get some matching by using the "grepl" function
#'
#'Lets convert the Mishna text from UTF-8 into integers and paste it as one string

brachoscd<-lapply(brachos, function(x) sapply(x, function(y) utf8ToInt(y)))
brachoscd<-lapply(brachoscd, function(x) sapply(x, function(y) as.character(y)))
brachoscd<-lapply(brachoscd, function(x) sapply(x, function(y) capture.output(cat(y))))

#'Similarly lets do this to the Taanaim

tancd<-lapply(tanint, function(x) sapply(x, function(y) as.character(y)))
tancd<-unname(tancd)
tancd<-lapply(tancd,  function(x) capture.output(cat(x)))
names(tancd)<-tannaim

#'Now lets get the truth tables
#' Note: string_which and grepl are not producing the desired result; 
#' Thus fuzzzy string matching is required (i.e. "agrepl")

tt<-lapply(tancd,function(z) lapply(brachoscd, function(x) (sapply(x, function(y) agrepl(z, y)))))

#' Now lets flatten the list appropriately

tt<-lapply(tt, function(x) lapply(x, unlist))
tt<-lapply(tt, unlist)
#' Lets build a dataframe now: 

dtt<-data.frame(tt)
colnames<-tannaim

#' This dataframe serves as a truth table for the taanaim and their interactions in Maseches Brachos. 
#' 
#' Lets visualize the Tannaim's interactions with one another by using a hierarchical edge graph.
#'First, we are going to need to create a hierarchical data frame. 
#'
#'Lets first get dtt in long form

dttl<-reshape::melt(dtt)

dttl$mishna<-rep(unname(unlist(links)),16)

dttl<-dttl[which(dttl$value==TRUE),]
dttl$mishna<-sapply(dttl$mishna,function(x) gsub("[A-z]","",x))
dttl$mishna<-sapply(dttl$mishna,function(x) gsub("://../.","",x))
dttl$mishna<-sapply(dttl$mishna,function(x) gsub("?=&=&2=","",x))
dttl$mishna<-sapply(dttl$mishna,function(x) gsub("\\?","",x))

#'Lets order the dataframe with sql
library(sqldf)
library(plyr)
dttl<-sqldf("SELECT * FROM dttl
             ORDER BY mishna;")
dttl$variable<-as.character(dttl$variable)
tanrp<-unique(dttl$variable)

#'Make the Tannaim look normal
dttl$variable<-replace(dttl$variable,tanrp %in% dttl$variable, tannaim)

library(data.tree)
library(networkD3)
dttl<-data.frame(dttl$mishna,dttl$variable, stringsAsFactors = FALSE)

#' Lets first define the pathstring

dttl$pathString<- paste("משניות ברכות",
                        dttl$dttl.mishna, 
                        dttl$dttl.variable,
                        sep="/")

####THE Hierachical data frame
hdt<-as.Node(dttl)

#' The network data frame
hdtl<- ToListExplicit(hdt, unname = TRUE)

#' The visualization
radialNetwork(hdtl, 
              linkColour = "burlywood",
              nodeColour="midnightblue",
              fontSize = 14)
