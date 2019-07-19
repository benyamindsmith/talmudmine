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
brachos<-lapply(links,function(x) sapply(x, function(y) {Sys.sleep(sample(10, 1) * 0.1)
                                                        pipeit(y,".highlight")}))

#'Lets clean it to get the Hebrew text exclusively
brachos<-lapply(brachos,function(x) sapply(x, function(y) gsub("[A-z]","",y)))
brachos<-lapply(brachos,function(x) sapply(x, function(y) gsub("[0-9]","",y)))
brachos<-lapply(brachos, function(x) sapply(x, function(y) gsub("\\n.*","",y)))
brachos<-lapply(brachos, function(x) sapply(x, function(y) trimws(y)))

#'Lets create a list of the Taanaim in the Maseches Brachos
tannaim<-c("רַבִּי אֱלִיעֶזֶר",
           "חֲכָמִים",
           "רַבָּן גַּמְלִיאֵל",
           "רַבִּי אֱלִיעֶזֶר",
           "רַבִּי יְהוֹשֻׁעַ",
           "בֵּית שַׁמַּאי",
           "בֵית הִלֵּל",
           "רַבִּי טַרְפוֹן",
           "",
           "",
           "",)
