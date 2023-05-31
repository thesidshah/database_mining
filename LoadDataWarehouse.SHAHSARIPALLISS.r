#Installing packages
if("RSQLite" %in% rownames(installed.packages())== FALSE) {
  install.packages("RSQLite")
}
if("RMySQL" %in% rownames(installed.packages())==FALSE) {
  install.packages("RMySQL")
}

#Importing packages

#Step 1 : import data from db file

#For SQLite
library(RSQLite)

#Connecting to db file created in part 1
db_file = "PubmedArticleSet (2).db"
dbcon <- dbConnect(RSQLite::SQLite(),db_file)

#Importing all tables

Articles_table <- dbReadTable(dbcon, "Articles")
head(Articles_table)

Journals_table <- dbReadTable(dbcon, "Journals")
head(Journals_table)

Journal_Issues_table <- dbReadTable(dbcon, "Journal_Issues")
head(Journal_Issues_table)

ArticlesJournalJoin_table <- dbReadTable(dbcon, "ArticlesJournalJoin")
head(ArticlesJournalJoin_table)

Authors_table <- dbReadTable(dbcon, "Authors")
head(Authors_table)

AuthorArticleJoin_table <- dbReadTable(dbcon, "AuthorArticleJoin")
head(AuthorArticleJoin_table)

library(RMySQL)
mydb = dbConnect(RMySQL::MySQL(),
                 dbname='practicum2',
                 host='localhost',
                 port=3306,
                 user='admin',
                 password='admin')
dbExecute(mydb,"DROP DATABASE practicum2")
dbExecute(mydb,"CREATE SCHEMA practicum2")
dbExecute(mydb,"USE practicum2")
dbExecute(mydb,"DROP table IF EXISTS Authors") 
dbExecute(mydb,"DROP table IF EXISTS articles") 
dbExecute(mydb,"DROP table IF EXISTS journals") 
dbExecute(mydb,"DROP table IF EXISTS ArticlesJournalJoin")
dbExecute(mydb,"DROP table IF EXISTS JournalIssues") 

# Creating Articles Table

dbSendStatement(mydb,"CREATE TABLE articles(
  pmid INTEGER PRIMARY KEY,
  articleTitle TEXT,
  authorListComplete TEXT,
  language TEXT,
 CHECK (authorListComplete IN ('Y','N','y','n')))")

dbSendStatement(mydb,"CREATE TABLE journals(
  ISSN VARCHAR(20) PRIMARY KEY,
  JournalTitle TEXT,
  ISOAbbreviation TEXT)")


# dbSendQuery(mydb, "SET GLOBAL local_infile = true;")
# Writing into Articles table
dbGetQuery(mydb,"DESC articles")
dbWriteTable(mydb, "articles",Articles_table, overwrite = F, row.names = F, append = T)
#Articles_table <- dbReadTable(dbcon, "Articles")
dbGetQuery(mydb,"select * from articles limit 5;")

dbWriteTable(mydb, "journals",Journals_table, overwrite = F, row.names = F, append = T)
#Journals_table <- dbReadTable(dbcon, "Journals")
dbGetQuery(mydb,"select * from journals limit 5;")

dbGetQuery(mydb, "DESC journals")

dbSendStatement(mydb,"CREATE TABLE journal_issues(
  issueId INTEGER PRIMARY KEY,
  pmid INTEGER  ,
  journalISSN VARCHAR(20),
  issueDate date,volume INTEGER,issue INTEGER,
                FOREIGN KEY(PMID) REFERENCES articles(pmid),
                FOREIGN KEY(journalISSN) REFERENCES journals(ISSN))")

dbSendStatement(mydb,"CREATE TABLE IF NOT EXISTS articlesjournaljoin(journalID VARCHAR(20),articleID INTEGER,
                CONSTRAINT Journal
                FOREIGN KEY(journalID) REFERENCES journals(ISSN),
                Constraint Articles
                FOREIGN KEY(articleID) REFERENCES articles(pmid))")

dbSendStatement(mydb,"CREATE TABLE IF NOT EXISTS authors(
  LastName TEXT,
  ForeName TEXT,
  Initials TEXT,
  authorIds INTEGER Primary key)")

dbSendStatement(mydb,"CREATE TABLE authorarticlejoin(PMID INTEGER,author_id INTEGER,
                FOREIGN KEY(author_id) REFERENCES Authors(authorIds),
                FOREIGN KEY(PMID) REFERENCES articles(pmid))")
dbGetQuery(mydb,"select * from journal_issues")

dbWriteTable(mydb, "journal_issues",Journal_Issues_table, overwrite = F, row.names = F, append = T)
#Journal_Issues_table <- dbReadTable(mydb, "Journal_Issues")
dbGetQuery(mydb,"select * from Journal_Issues limit 5;")

dbWriteTable(mydb, "articlesjournaljoin",ArticlesJournalJoin_table, overwrite = F, row.names = F, append = T)
#ArticlesJournalJoin_table <- dbReadTable(mydb, "ArticlesJournalJoin")
dbGetQuery(mydb,"select * from articlesjournaljoin limit 5;")

dbWriteTable(mydb, "authors",Authors_table, overwrite = F, row.names = F, append = T)
#Authors_table <- dbReadTable(mydb, "Authors")
dbGetQuery(mydb,"select * from authors limit 5;")

dbGetQuery(mydb,"desc authors")

dbWriteTable(mydb, "authorarticlejoin",AuthorArticleJoin_table, overwrite = F, row.names = F, append = T)

dbGetQuery(mydb,"select * from authorarticlejoin limit 5;")

coAuthorCount <- dbGetQuery(mydb,"select distinct a.author_id as authorIds,count(a.author_id) as coAuthorCount from authorarticlejoin a inner 
           join authorarticlejoin b on a.author_id <> b.author_id and a.pmid=b.pmid group by a.pmid;")
count_articles<-dbGetQuery(mydb,"select author_id,count(pmid) from authorarticlejoin group by author_id,pmid")
count_articles

FactTableForAuthor <- left_join(Authors_table,coAuthorCount)

number_of_articles_per_author<-dbGetQuery(mydb,"select author_id as authorIds,count(*) as numArticles from AuthorArticleJoin 
                                          group by author_id having numArticles")
number_of_articles_per_author<-na.omit(number_of_articles_per_author)

FactTableForAuthor <- left_join(FactTableForAuthor, number_of_articles_per_author)
library(dplyr)
library(tidyr)
FactTableForAuthor$coAuthorCount <- replace_na(FactTableForAuthor$coAuthorCount, 0)

dbSendStatement(mydb,"CREATE TABLE author_fact(LastName varchar(50),	
ForeName varchar(50),
Initials varchar(50),
authorIds INTEGER,
coAuthorCount INTEGER,
numArticles INTEGER)")
dbGetQuery(mydb,"desc author_fact")

dbWriteTable(mydb, "author_fact",FactTableForAuthor, overwrite = F, row.names = F, append = T)  
dbGetQuery(mydb,"desc journals")
journal_w_issue = dbGetQuery(mydb,"select * from journals j left join journal_issues i on j.ISSN = i.journalISSN")

dbGetQuery(mydb,"select * from journal_issues limit 1")
dbGetQuery(mydb,"select * from articlesjournaljoin limit 1")

journal_yqm<-dbGetQuery(mydb,"select journalID,count(articleID) as no_of_articles,year(ji.issueDate) as year,quarter(ji.issueDate) as quarter,
month(ji.issueDate) as month 
from articlesjournaljoin ajj inner join  journal_issues 
                        ji on ajj.journalid=ji.journalISSN group by journalID,year(ji.issueDate),quarter(ji.issueDate),month(ji.issueDate)")

# journal_fact<-,jornal,journal_yqm by ISSN
journalFactTable <- left_join(Journals_table,journal_yqm, by = c('ISSN' = 'journalID'))

#The data has 0s wherever the issue date is not provided in the 
#dataset or when for a particular month / quarter / year the data is missing.


dbSendStatement(mydb,"CREATE TABLE journal_fact(ISSN varchar(50),
JournalTitle varchar(100),
ISOAbbreviation varchar(100),
no_of_articles INTEGER,
year INTEGER,
quarter INTEGER,
month INTEGER)")
dbGetQuery(mydb,"desc journal_fact")

dbWriteTable(mydb, "journal_fact",journalFactTable, overwrite = F, row.names = F, append = T)
