##url <- paste0("https://en.wikipedia.org/wiki/","Electronic_health_record")

library(rvest)

#1.1 CMS Promoting Interoperability Programs Registration System
term1<- read_html("https://ehrincentives.cms.gov/hitech/loginCredentials.action")
word1<-html_nodes(term1,".clearfix")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.2 CMS Promoting Interoperability (PI)
term2<- read_html("https://www.cms.gov/Regulations-and-Guidance/Legislation/EHRIncentivePrograms/index.html?redirect=/EHRincentiveprograms")
word2<-html_nodes(term2,"#mainContent")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.3 CMS Quality Payment Program
term3<- read_html("https://qpp.cms.gov/mips/promoting-interoperability")
word3<-html_nodes(term3,".body-container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.4 Healthcare Innovation Group - BREAKING: CMS Finalizes “Promoting Interoperability” Rule
term4<- read_html("https://www.hcinnovationgroup.com/policy-value-based-care/article/13030586/breaking-cms-finalizes-promoting-interoperability-rule")
word4<-html_nodes(term4,".block")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.5 Saignite - WHAT IS PROMOTING INTEROPERABILITY?
term5<- read_html("https://www.saignite.com/industry-expertise/medicaid-promoting-interoperability/what-is-meaningful-use/")
word5<-html_nodes(term5,".content.side")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.6 HIMSS - CMS Finalizes Changes to Interoperability Initiatives and EHR Incentive Program for Hospitals
term6<- read_html("https://www.himss.org/news/cms-finalizes-changes-interoperability-initiatives-and-ehr-incentive-program-hospitals")
word6<-html_nodes(term6,".main-container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#1.7 HiTech Answers - New Resources Available on the CMS Promoting Interoperability Programs Website
term7<- read_html("https://www.hitechanswers.net/new-resources-available-on-the-cms-promoting-interoperability-programs-website/")
word7<-html_nodes(term7,"#main-content")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.1 American Hospital Association - CMS issues factsheets on 2019 Medicare Promoting Interoperability Program
term2.1<- read_html("https://www.aha.org/news/headline/2019-03-27-cms-issues-factsheets-2019-medicare-promoting-interoperability-program")
word2.1<-html_nodes(term2.1,".container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.2 PrognoCIS - 4 main objectives of Promoting Interoperability (PI) Program
term2.2<- read_html("https://prognocis.com/objectives-of-promoting-interoperability-program/")
word2.2<-html_nodes(term2.2,".container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.3 American Academy of Family Physicians - Promoting Interoperability Performance Category
term2.3<- read_html("https://www.aafp.org/practice-management/payment/medicare-payment/mips/aci.html")
word2.3<-html_nodes(term2.3,".block-drop-shadow.nine.move-right")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.4 Dark Daily - CMS Finalizes Rule Rebranding ‘Meaningful Use’ Program to ‘Promoting Interoperability’
term2.4<- read_html("https://www.darkdaily.com/cms-finalizes-rule-rebranding-meaningful-use-program-to-promoting-interoperability/")
word2.4<-html_nodes(term2.4,".container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.5 Texas Medical Association - Promoting Interoperability
term2.5<- read_html("https://www.texmed.org/PromotingInteroperability/")
word2.5<-html_nodes(term2.5,"#aspnetForm")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.6 Azalea Health - Promoting Interoperability (PI)
term2.6<- read_html("https://www.azaleahealth.com/faqs/promoting-interoperability-program/")
word2.6<-html_nodes(term2.6,".wrap")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.7 Practice Fusion - What is the Promoting Interoperability (formerly Advancing Care Information) performance category in MIPS?
term2.7<- read_html("https://knowledgebase.practicefusion.com/knowledgebase/articles/1835764-what-is-the-promoting-interoperability-formerly-a")
word2.7<-html_nodes(term2.7,".uvContainer.uvBody")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.8 Indian Health Service - Promoting Interoperability (PI)
term2.8<- read_html("https://www.ihs.gov/promotinginteroperability/")
word2.8<-html_nodes(term2.8,"#site_content")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.9 State of Indiana - Indiana Medicaid Promoting Interoperability Program
term2.9<- read_html("https://www.in.gov/medicaid/providers/632.htm")
word2.9<-html_nodes(term2.9,".content-body")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#2.10 State of New Jersey, Department of Health - Health Information Technology (Health IT) Program
term2.10<- read_html("https://www.nj.gov/health/njhit/ehr/incentive-program/")
word2.10<-html_nodes(term2.10,".row")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.1 eCQI Resource Center - CMS Extends eCQM and Promoting Interoperability Program Submission and Attestation Deadline to March 14, 2019, at 11:59 p.m. PT for the CY 2018 Hospital IQR and Promoting Interoperability Programs for Hospitals
term3.1<- read_html("https://ecqi.healthit.gov/ecqi/ecqi-news/cms-extends-ecqm-and-promoting-interoperability-program-submission-and-attestation-deadline-march-14-2019-1159-p.m.-pt-cy-2018-hospital-iqr-and-promoting-interoperability-programs")
word3.1<-html_nodes(term3.1,".col-md-12.inner")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.3 Commonwealth of Kentucky, Cabinet for Health and Family Services - Medicaid EHR Incentive Program (Promoting Interoperability)
term3.3<- read_html("https://chfs.ky.gov/agencies/dms/ehr/Pages/default.aspx")
word3.3<-html_nodes(term3.3,"#bodyContent")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.4 Minnesota Department of Human Services - Minnesota Promoting Interoperability Program (MPIP)
term3.4<- read_html("https://mn.gov/dhs/partners-and-providers/news-initiatives-reports-workgroups/minnesota-health-care-programs/minnesota-promoting-interoperability-program/")
word3.4<-html_nodes(term3.4,".content")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.5 Arkansas Medicaid - Promoting Interoperability (PI) Program
term3.5<- read_html("https://medicaid.mmis.arkansas.gov/provider/ehr/ehr.aspx")
word3.5<-html_nodes(term3.5,".content")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.6 American College of Surgeons - Promoting Interoperability Program
term3.6<- read_html("https://www.facs.org/advocacy/regulatory/pi")
word3.6<-html_nodes(term3.6,"#content_element_0_acsCol9MainColumn")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.7 IPRO - Promoting Interoperability (PI)
term3.7<- read_html("https://ipro.org/for-providers/medicare-qpp/promo-interoperability-pi")
word3.7<-html_nodes(term3.7,".clearfix")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.8 Wyoming Department of Health - Medicaid Promoting Interoperability Program - Formally Meaningful Use EHR Incentive Program
term3.8<- read_html("https://health.wyo.gov/healthcarefin/medicaid/meaningfuluse/")
word3.8<-html_nodes(term3.8,"#page-container")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.9 Center for Disease Control and Prevention - Public Health and Promoting Interoperability Programs (formerly, known as Electronic Health Records Meaningful Use)
term3.9<- read_html("https://www.cdc.gov/ehrmeaningfuluse/index.html")
word3.9<-html_nodes(term3.9,".row")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.10 Iowa Department of Human Services - Health Information Technology (HIT) and Promoting Interoperability (PI) Program
term3.10<- read_html("https://dhs.iowa.gov/ime/providers/tools-trainings-and-services/medicaid-initiatives/EHRincentives")
word3.10<-html_nodes(term3.10,".interior-page")%>% 
  html_text()%>% 
  gsub("\n", "", .) %>% 
  trimws()

#3.4
#term3.4<- read_html()
#word3.4<-html_nodes(term3.4,".row")%>% 
#  html_text()%>% 
#  gsub("\n", "", .) %>% 
#  trimws()

word<-paste(word1, word2, word3, word4, word5, word6, word7,
            word2.1, word2.2, word2.3, word2.4, word2.5, word2.6, word2.7, word2.8, word2.9, word2.10,
            word3.1, word3.3, word3.4, word3.5, word3.6, word3.7, word3.8, word3.9, word3.10
            )




library(wordcloud)
library(tm)
library(SnowballC)
library(wordcloud2)
docs <- Corpus(VectorSource(word))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))
myStopWords <- c("may", "now", "also", "many", "use", "used", "typically","given",
                 "like", "will", "can", "often", "see", "one", "pdf", "issn", "journal","isbn","abér",
                 "copy","tweet","retweets","retweeted","reply","retweet","replies","hours","liked","link","ago",
                 "follow",
                 tolower(month.name))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, removeWords, myStopWords)
docs <- tm_map(docs, content_transformer(gsub), pattern = "–", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "•", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "”", replacement = "")
docs <- tm_map(docs, content_transformer(gsub), pattern = "“", replacement = "")
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=round(sqrt(v),0))

set.seed(5)
cloud<-wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                 max.words=1000, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Set3"))
###
#install.packages("stringr")
library(stringr)
d$word=str_replace_all(d$word,"[^[:graph:]]", " ") 

#install.packages("syuzhet")
library(syuzhet)

sentRes <- get_sentiment(paste0(d$word))
head(sentRes)

dWSe <- data.frame(d,sentRes=sentRes, color=rep("black",length(sentRes)), 
                   stringsAsFactors = FALSE)
dWSe[dWSe$sentRes<0,4] <- "red"
dWSe[dWSe$sentRes>0,4] <- "green"


NegativeWords <- dWSe[dWSe$sentRes<0,]
PositiveWords <- dWSe[dWSe$sentRes>0,]
NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]
NeutralWords <- dWSe[dWSe$sentRes==0,]



set.seed(5)
wordcloud(words = NoneNeutralWords$word, freq = NoneNeutralWords$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=NoneNeutralWords$color,
          ordered.colors=TRUE
)


weighted.mean(NoneNeutralWords$sentRes,NoneNeutralWords$freq)


dWSe <- data.frame(d,sentRes=sentRes, color=rep("#00000000",length(sentRes)), 
                   stringsAsFactors = FALSE)

for (i in 1:length(dWSe$word)) {
  curRate=dWSe$sentRes[i]
  if(curRate<0){
    dWSe$color[i]=rgb(red = 179/255,green = 0,blue = 12/255,alpha = -curRate)
  }
  if(curRate>0){
    dWSe$color[i]=rgb(red = 13/255,green = 89/255,blue = 1/255,alpha = curRate)
  }
}

NoneNeutralWords <- dWSe[dWSe$sentRes!=0,]

set.seed(5)
wordcloud(words = NoneNeutralWords$word, freq = sqrt(NoneNeutralWords$freq), scale=c(3,.2),
          min.freq = 1, max.words=130, random.order=FALSE, rot.per=0.15, 
          colors=NoneNeutralWords$color,
          ordered.colors=TRUE
)

set.seed(5)
wordcloud2(NoneNeutralWords, backgroundColor = "#E9E7E2",color=NoneNeutralWords$color,shape="circle")



