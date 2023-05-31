# import libraries
library(XML)
library(dplyr)
library(stringr)
library(sqldf)
#external pointer to the root of the xml document
xmlDoc <- xmlParse("pubmed-tfm-xml.xml")
root = xmlRoot(xmlDoc)

n <- xmlSize(root)


#Data frames 

articles.df <- data.frame(
  pmid = integer(),
  language = list(),
  articleTitle = character(),
  authorListComplete = character(),
  stringsAsFactors = F
)

journals = data.frame(
  ISSN = character(),
  JournalTitle = character(),
  ISOAbbreviation = character(),
  stringsAsFactors = F
)

articleJournalJoin = data.frame(
  journalID = character(),
  articleID = character(),
  stringsAsFactors = F
)

journalIssues = data.frame(
  issueId = integer(),
  pmid = integer(),
  journalISSN = character(),
  issueDate = character(),
  volume = character(),
  stringsAsFactors = F
)

authors <- data.frame(
  pmid = integer(),
  LastName = character(),
  ForeName = character(),
  Initials = character()
)



#Read data from Journal tag 
parseJournal <- function(journal, journalID) {
  i = 1
  while(!is.null(journal[[i]])){
    # print(paste(i,xmlName(journal[[i]]),xmlValue(journal[[i]])))
    # journals[journalID,'journalID'] <<- journalID
    if(grepl(xmlName(journal[[i]]),'ISSN')){
      journals[journalID,'ISSN'] <<- xmlValue(journal[[i]])
    }
    else if(grepl(xmlName(journal[[i]]),'Title')){
      journals[journalID,'JournalTitle'] <<- xmlValue(journal[[i]])
    }
    else if(grepl(xmlName(journal[[i]]),'ISOAbbreviation')) {
      journals[journalID, 'ISOAbbreviation'] <<- xmlValue(journal[[i]])
    }
    else if(grepl(xmlName(journal[[i]]),'JournalIssue')) {
      #Parse Journal issue
      parseJournalIssue(journal[[i]],journal[['ISSN']],pmid,issueID)
      issueID <<- issueID + 1
    }
    else
    {
      print("found a new tag inside journal named")
      print(xmlName(journal[[i]]))
    }
    i = i + 1
  }
}

#Helper function for parsing issues of journal tag
#Potential issue found in Issue: Issues are not just numeric
parseJournalIssue <- function(journalIssue,ISSN,pmid,issueID) {
  nodeDepth <- 1
  Date <- NULL
  volume <- NULL
  issue <- NULL
  while(!is.null(journalIssue[[nodeDepth]])) {
    
    if(grepl(xmlName(journalIssue[[nodeDepth]]),'PubDate')) {
      
      Date <- processPubDate(journalIssue[[nodeDepth]])
    }
    else if(grepl(xmlName(journalIssue[[nodeDepth]]),'Issue')) {
      issue = xmlValue(journalIssue[[nodeDepth]])
      if(is.na(as.numeric(issue))){
        issueList <- str_split(issue,'-')
        issue <- issueList[[1]][1]
      }
    }
    else if(grepl(xmlName(journalIssue[[nodeDepth]]),'Volume')) {
      volume = xmlValue(journalIssue[[nodeDepth]])
    }
    nodeDepth = nodeDepth + 1
  }
  
  
  
  Date <- ReplaceValueIfNull(Date)
  volume <- ReplaceValueIfNull(volume)
  issue <- ReplaceValueIfNull(issue)
  
  # print(paste('issueID:',issueID,'ISSN:',xmlValue(ISSN),'Date:',Date,'Volume:',volume,'Issue:',issue))
  
  journalIssues[issueID, 'issueId'] <<- issueID
  journalIssues[issueID,'journalISSN'] <<- xmlValue(ISSN)
  journalIssues[issueID,'pmid'] <<- pmid
  journalIssues[issueID,'issueDate'] <<- Date
  journalIssues[issueID, 'volume'] <<- volume
  journalIssues[issueID,'issue'] <<- issue
}

#Helper function for parsing dates of journal issue
processPubDate <- function(journalPubDate) {
  
  pubDateDepth <- 1
  Day <- NULL
  Month <- NULL
  Year <- NULL
  
  if(grepl('MedlineDate',xmlName(journalPubDate[[pubDateDepth]]))) {
    Day <- "01"
    dateValue <- str_split(xmlValue(journalPubDate[[pubDateDepth]])," ")
    
    Month<-substring(dateValue[[1]][2],1,3)
    Year <- dateValue[[1]][1]
  }
  else{
    while(!is.null(journalPubDate[[pubDateDepth]])) {
      if(grepl(xmlName(journalPubDate[[pubDateDepth]]),'Day')) {
        Day <- (journalPubDate[[pubDateDepth]])
      }
      else if(grepl(xmlName(journalPubDate[[pubDateDepth]]), 'Month')) {
        Month <- (journalPubDate[[pubDateDepth]])
      }
      else if(grepl(xmlName(journalPubDate[[pubDateDepth]]),'Year')){
        Year <- xmlValue(journalPubDate[[pubDateDepth]])
      }
      pubDateDepth <- pubDateDepth + 1
    }
    
    if(is.null(Day)) {
      
      Day <- '01'
    }
    else{
      Day <- xmlValue(Day)
    }
    
    if(is.null(Month)) {
      Month <- 'Jan'
    }
    else {
      Month <- xmlValue(Month)
    }
  }
  
  
  Date <- createDate(Year,Month,Day)
}

#Helper function to create the final date string
createDate <- function(year, month, day) {
  
  monthInNum = switch(
    month,
    "Jan" = "01",
    "Feb" = "02",
    "Mar" = "03",
    "Apr" = "04",
    "May" = "05",
    "Jun" = "06",
    "Jul" = "07",
    "Aug" = "08",
    "Sep" ="09",
    "Oct" = "10",
    "Nov" = "11",
    "Dec" ="12"
  )
  date2 = paste0(year,'/',monthInNum,'/',day)
  return(date2)
}


ReplaceValueIfNull <- function(x) {
  if(is.null(x) || is.na(x)) {
    return("Not Provided")
  }
  return(x)
}


parseAuthorListWIthPmid <- function(authorList, pmid) {
  i <- 1
  while(!is.null(authorList[[i]])) {
    currAuthor <- authorList[[i]]
    authors[authorId, 'pmid'] <<- pmid
    authors[authorId,'authorNum'] <<- authorId
    authors[authorId, 'LastName'] <<- xmlValue(currAuthor[['LastName']])
    authors[authorId, 'ForeName'] <<- xmlValue(currAuthor[['ForeName']])
    authors[authorId, 'Initials'] <<- xmlValue(currAuthor[['Initials']])
    authorId <<- authorId + 1
    i <- i + 1
  }
}


bt <- Sys.time()
articleID <- 1
journalID <- 1
issueID <- 1
authorId <- 1

for(i in 1:n){
  #Get current node from the root
  curNode <- root[[i]]
  
  
  #curNode <- root[[798]] - for testing purposes
  
  #Get current node's PMID
  pmid <- xmlAttrs(curNode)
  # print(pmid)
  
  article <- curNode[[1]]
  
  nodeDepth <- 1
  language <- list()
  langLen <- 1
  while(!is.null(article[[nodeDepth]]))
  {
    nodeName <- xmlName(article[[nodeDepth]]) 
    nodeActual <- article[[nodeDepth]]
    
    if(grepl('Journal', nodeName)) {
      # print('inside Journal')
      #Parse Journal Call
      journalISSN <- xmlValue(nodeActual[['ISSN']])
      parseJournal(nodeActual,journalID)
      # journals <<- unique(journals)
    }
    
    else if(grepl('Language', nodeName)) {
      
      language[langLen] <- xmlValue(nodeActual)
      langLen <- langLen + 1
      
    } 
    
    else if(grepl('ArticleTitle',nodeName)) {
      
      articleTitle <- xmlValue(nodeActual)
      
    } 
    
    else if(grepl('AuthorList',nodeName)) {
      
      authorListFlag <- xmlGetAttr(nodeActual,name = "CompleteYN", default = "NA")
      # parseAuthor
      parseAuthorListWIthPmid(nodeActual,pmid)
      # print(tail(authors,3))
    }
    
    else {
      
      print(paste('new element found:',nodeName))
    }
    
    
    nodeDepth = nodeDepth + 1
    
  }
  
  # articles.df[articleID,"articleID"] <- articleID
  articles.df[articleID,'pmid'] <- pmid
  articles.df[articleID,'articleTitle'] <- articleTitle
  articles.df[articleID,'language'] <- language[langLen - 1]
  langLen = langLen - 1
  articles.df[articleID,'authorListComplete'] <- authorListFlag
  
  articleJournalJoin[articleID,'articleID'] <- pmid
  articleJournalJoin[articleID,'journalID'] <- journalISSN
  articleID <- articleID + 1
  journalID <- journalID + 1
  
}
hopefullyUniqueAuthorList <- as.factor(authors$LastName)
authorIds <- as.numeric(hopefullyUniqueAuthorList)
authors <- cbind(authors,authorIds)
joinPmidAuthors <- cbind(pmid=as.integer(authors$pmid), author_id=as.integer(authors$authorIds))
authors <- authors %>% distinct(authorIds, .keep_all = TRUE) %>% arrange(authorIds)
journals <- journals %>% distinct(ISSN, .keep_all = TRUE) %>% arrange(ISSN)
et <- Sys.time()

t.loop <- et - bt

cat("Time elapsed: ", round((t.loop),3), " sec")
# print(articles.df['articleID'][[29900:30000]])


colSums(is.na(journals))
colSums(is.na(articleJournalJoin))
colSums(is.na(journalIssues))
colSums(is.na(authors))
# Cleaning data for all the dataframes and converting to appropriate data types.

journals$ISSN <- journals$ISSN %>% replace_na('Not Provided')
articleJournalJoin$journalID<-articleJournalJoin$journalID%>% replace_na('Not Provided')
journalIssues$journalISSN<-journalIssues$journalISSN%>% replace_na('Not Provided')
authors$LastName<-authors$LastName%>%replace_na('Not present')
authors$ForeName<-authors$ForeName%>%replace_na('Not present')
authors$Initials<-authors$Initials%>%replace_na('Not present')
authors<-na.omit(authors)
authors<-subset(authors, select = -c(1,5))
articles.df$pmid <- as.integer(articles.df$pmid)
articleJournalJoin$articleID<-as.integer(articleJournalJoin$articleID)
journalIssues$pmid<-as.integer(journalIssues$pmid)
journalIssues$volume<-as.integer(journalIssues$volume)
journalIssues$issue<-as.integer(journalIssues$issue)
journalIssues<-na.omit(journalIssues)
joinPmidAuthors<-data.frame(joinPmidAuthors)

#write.csv(articles.df,"articles2.csv",row.names = FALSE)
#write.csv(journals,"journals2.csv",row.names = TRUE)
#write.csv(articleJournalJoin,"articleJournalJoin2",row.names=FALSE)
#write.csv(journalIssues,"issues2.csv",row.names= TRUE)
#write.csv(authors,'authors.csv',row.names = TRUE)

# Connecting to database
library(RSQLite)

#fpath = "/CS5200.PracticumII.SaripalliS"
db_file = "PubmedArticleSet.db"
# if database file already exists, we connect to it, otherwise
# we create a new database
dbcon <- dbConnect(RSQLite::SQLite(),db_file)

#Dropping Tables if exits

dbExecute(dbcon,"DROP table IF EXISTS Authors") 
dbExecute(dbcon,"DROP table IF EXISTS Articles") 
dbExecute(dbcon,"DROP table IF EXISTS Journals") 
dbExecute(dbcon,"DROP table IF EXISTS ArticlesJournalJoin")
dbExecute(dbcon,"DROP table IF EXISTS Journal_Issues")
dbExecute(dbcon,"DROP table IF EXISTS AuthorArticleJoin") 


# Creating Articles Table

dbSendStatement(dbcon,"CREATE TABLE Articles(
  Article_id INTEGER PRIMARY KEY,
  Article_Title TEXT,
  Author_list TEXT,
  Language TEXT,
 CHECK (Author_list IN ('Y','N','y','n')))")

dbSendStatement(dbcon,"CREATE TABLE Journals(
  ISSN TEXT PRIMARY KEY,
  Journal_Title TEXT,
  ISOAbbreviation TEXT)")

dbSendStatement(dbcon,"CREATE TABLE Journal_Issues(
  issue_id  PRIMARY KEY,
  PMID INTEGER,
  journal_issn TEXT,
  issue_date date,volume INTEGER,issue INTEGER,
                FOREIGN KEY(PMID) REFERENCES Articles(Article_id),
                FOREIGN KEY(journal_issn) REFERENCES Journals(ISSN))")

dbSendStatement(dbcon,"CREATE TABLE ArticlesJournalJoin(journalID TEXT,articleID INTEGER,
                FOREIGN KEY(journalID) REFERENCES Journals(ISSN),
                FOREIGN KEY(articleID) REFERENCES Articles(Article_id),
                PRIMARY KEY (journalID,articleID))")

dbSendStatement(dbcon,"CREATE TABLE Authors(
  Last_name TEXT,
  fore_name TEXT,
  Initials TEXT,
  Author_ID INTEGER Primary key)")

dbSendStatement(dbcon,"CREATE TABLE AuthorArticleJoin(PMID INTEGER,Author_ID INTEGER,
                FOREIGN KEY(Author_ID) REFERENCES Authors(Author_ID),
                FOREIGN KEY(PMID) REFERENCES Articles(Article_id))")


# Writing into Articles table

dbWriteTable(dbcon, "Articles",articles.df, overwrite = T, row.names = F)
#Articles_table <- dbReadTable(dbcon, "Articles")
dbGetQuery(dbcon,"select * from Articles limit 5;")

dbWriteTable(dbcon, "Journals",journals, overwrite = T, row.names = F)
#Journals_table <- dbReadTable(dbcon, "Journals")
dbGetQuery(dbcon,"select * from Journals limit 5;")

dbWriteTable(dbcon, "Journal_Issues",journalIssues, overwrite = T, row.names = F)
#Journal_Issues_table <- dbReadTable(dbcon, "Journal_Issues")
dbGetQuery(dbcon,"select * from Journal_Issues limit 5;")

dbWriteTable(dbcon, "ArticlesJournalJoin",articleJournalJoin, overwrite = T, row.names = F)
#ArticlesJournalJoin_table <- dbReadTable(dbcon, "ArticlesJournalJoin")
dbGetQuery(dbcon,"select * from ArticlesJournalJoin limit 5;")

dbWriteTable(dbcon, "Authors",authors, overwrite = T, row.names = F)
#Authors_table <- dbReadTable(dbcon, "Authors")
dbGetQuery(dbcon,"select * from Authors limit 5;")

dbWriteTable(dbcon, "AuthorArticleJoin",joinPmidAuthors, overwrite = T, row.names = F)
AuthorArticleJoin_table <- dbReadTable(dbcon, "AuthorArticleJoin")
#dbGetQuery(dbcon,"select * from AuthorArticleJoin limit 5;")
dim(AuthorArticleJoin_table)











