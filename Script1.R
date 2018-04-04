usercon <- read.table('user_contacts.dat', header=TRUE)
usertagbook<- read.table('user_taggedbookmarks.dat', header=TRUE)
relate<- read.table('bookmark_tags.dat', header=TRUE)
bookdata <- read.csv('bookmarksexp.txt', row.names = NULL,sep="\t", header=TRUE)
page <- read.csv(url("http://www.ifla.org/"))

bookdata$url[1]
plot(usercon)
barplot(usercon$userID)
rm(list = ls())
usercon[1:2]
str(usercon)
x <- 1263

head(usercon[1:2],n=length(usercon[usercon$userID==x,2])-1,20,20)
usercon[1:2]
usercon[usercon$userID ==1263,]
?frequency
?length
length(usercon[usercon$userID==1263,2])
length(usercon[usercon$userID<1263,2])
length(usercon[usercon$userID==8,2])
usercon
rm(list=ls())
?head
?body
usercon
rm(friends1263)



friends1263 <- usercon[1:2]$contactID[(length(usercon[usercon$userID<1263,2])+1):(length(usercon[usercon$userID<1263,2])+length(usercon[usercon$userID==1263,2]))]#Stores the friends of any user
length(friends1263)
for(i in 1:length(friends1263)) print(friends1263[i])[]#Loop for each of the friends of user

relate$tagID==tag[1]
relate$bookmarkID==1490
pair
rm(pair)
book <- usertagbook[usertagbook$userID==1263,2]#Store the bookmarks
tag <- usertagbook[usertagbook$userID==1263,3]#store the tag
length(relate[relate$bookmarkID==book[5],2])
relate[relate$bookmarkID==1263,2]
relate[relate$bookmarkID==book[1],1]

if(relate$tagID[1]==tag[1])


cur <- 0
length(book)
for(i in 1:length(book))
{
  len <- length(relate[relate$bookmarkID==book[i],2])
  val <- relate[relate$bookmarkID==book[i],1]
  #len2 <- length(relate[relate$bookmarkID==tag[i],2])
  relate[relate$bookmarkID==book[i],2]
  nexter <- cur+len
  
  for(j in 1:len)
  { 
    if(isTRUE(relate$bookmarkID==val[1]) && isTRUE(relate$tagID==tag[cur:nexter]))
    {
      print("Times")
    }
    print(relate$tagWeight[cur])
  }
  # print("Times Sucess")
  cur <- cur+len
}
i <- 0
length(relate[relate$bookmarkID,2])
rm(d)
relate$tagID==tag[1]
relate$bookmarkID==book[1]
d <- NULL
for(i in 1:length(book))
{
  for(j in 1:length(relate[relate$bookmarkID,2]))
  {
    if(isTRUE(relate$tagID[j]==tag[i])&&isTRUE(relate$bookmarkID[j]==book[i]))
    {
      print("---------------")
      print(relate$tagWeight[j])
      print(tag[i])
      x1 <- cbind(relate$tagWeight[j],tag[i])
      d <- rbind(d,x1)
    }
  }
}
relate[relate$bookmarkID[1:length(relate)]==book[1:length(book)],]
book[1:100]
relate$bookmarkID[1:100]
len
d
r[] <- unique(d[1:(length(d)/2)])
r <- order(r)
r
rm(q1)
 q1 <- NULL 
r
d


for(i in 1:length(r))
{
  q <- 0
  for(j in 1:length(d))
  {
    
   if(r[i]==d[j])
    {
     print("Match")
      
      q <- q+d[j]
      
   }
  
  }
  q1 <- rbind(q1,q)
  
}
 
 
bookcount <- NULL 

ubook <- unique(book)

for(i in 1:length(ubook))
{
  c1 <- 0
  c2 <- 0
  for(j in 1:length(book))
  {
    if(ubook[i]==book[j])
    {
      c1 <- c1+1
      c2 <- 1
    }
    
  }
  print("Loop")
  bookcount <- rbind(bookcount,c1)
} 

ubook <- cbind(ubook,bookcount)
 
length(r)
length(q1)
tot <- cbind(r,q1)
 
 
 
 max(tot)
 
  kek <- q1/sum(q1)
  sum(kek)
 
q
r[4]
d[1]
sum(d)
kek

total <- cbind(tot,kek)


for(i in 1:length(book)){}
book
urel <- unique(relate[1:((length(relate))/4)])
relate
urel
length(urel[urel$bookmarkID,1])
ubook
bmtotal <- NULL
weight <- 0
counter <- 1
urel$bookmarkID[3]

for(i in 1:length(relate[,1]))
{
  for(j in 1:length(total[,1]))
  {
    print("---------")
    print(relate$bookmarkID[i])
    print(urel$bookmarkID[counter])
    print("---------")
    if((relate$bookmarkID[i]==urel$bookmarkID[counter])&&(relate$tagID[i]==total[j,1]))
    {
        weight <- (total[j,3]*relate$tagWeight[i])+weight
        print("Added")
    }
   
    
  }
  bmtotal <- rbind(bmtotal,weight)
  weight <- 0
  if(relate$bookmarkID[i]>=urel$bookmarkID[counter]){
 counter <- counter+1
  }
}
bmtotal

total[,1]
length(relate[,1])





final1 <- cbind(relate$bookmarkID[1:487122],bmtotal)
final2<-final1[!(final1[,2]==0),]

final1
bookdata[1:4,1]



require(RCurl)

require(XML)
webpage <- getURL("http://www.haaretz.com/")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


#Formating the page

pagetree<-tm_map(pagetree,content_transformer(tolower))

library(tm)
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
pagetree <- tm_m
ap(pagetree, toSpace, "-")
pagetree <- tm_map(pagetree, toSpace, "<")
pagetree <- tm_map(pagetree,toSpace,">")
pagetree <- tm_map(pagetree,toSpace,"/")
dar=pagetree
dar
write(pagetree,file="c:/Docs/page1.txt",1,TRUE,sep="") 
pagetree
#LDA

library(lda)

library(topicmodels)
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
ldaOut <-lda(pagetree,k, method="Gibbs", control=list(nstart=nstart, seed = seed,best=best, burnin = burnin, iter = iter, thin=thin))