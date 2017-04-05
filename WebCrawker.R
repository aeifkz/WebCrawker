library("RCurl")
library("XML")
library("RMySQL")

# url root for ptt
root = 'https://www.ptt.cc'

#DB password , don't upload to github
pass = ''


# max crawl article count
max_crawl = 3


# extract data from html file , and package to list
WebCrawker.getBoardData <- function() {
  
  url = "https://www.ptt.cc/bbs/index.html"
  
  #download.file(url,destfile = "test.html")
  
  
  #download html from url
  #data = getURL(url,ssl.verifyHost=F,ssl.verifypeer=F)
  
  #parse html content
  #doc = htmlParse(data)
  
  print("get html from test.html file")
  doc = htmlParse(file = "test.html")
  
  #get board-name and board-title
  na.name = xpathApply(doc,"//div[@class='b-ent']//div[@class='board-name']",xmlValue)
  na.des = xpathApply(doc,"//div[@class='b-ent']//div[@class='board-title']",xmlValue)
  
  # Refer to https://www.r-bloggers.com/web-scraping-google-urls/
  # get url from data
  href = xpathApply(doc,"//div[@class='b-ent']/a[@href]",xmlAttrs)
  na.href = sapply(href, function(x) x[["href"]])
  
  board_list = list(name=na.name,des=na.des,href=na.href)
  
  return(board_list)
  
}

# store board data to MySQL database
WebCrawker.storeBoardData <- function(board_list) {
  
  
  
  dbconn = dbConnect(MySQL(), user='root', password=pass, host='localhost', dbname="ptt")
  
  #change encoding to utf8
  dbSendQuery(dbconn , "SET NAMES utf8")
  
  for( i in  1:length(list$name) ) {
    
    query = paste( "INSERT INTO board (name,des,url) VALUES('" , 
                   board_list$name[i] , "','" , 
                   board_list$des[i] , "','"  , 
                   board_list$href[i] , "')"  , sep='' )
    
    dbSendQuery(dbconn , query)
    
  }
  
  dbDisconnect(dbconn)
}

# extract data from html file , and package to list
WebCrawker.getArticleData <- function(board_url) {
  
  url =  paste(root,board_url,sep="")
  
  na.title = list()
  na.href = list()
  na.prev = list()
  
  #get the article which has score more than this value
  score = 90

  for(i in 1:max_crawl) {
    
    Sys.sleep(0.5)
    
    data = getURL(url,ssl.verifyHost=F,ssl.verifypeer=F)
    doc = htmlParse(data)
    
    # get article where nrec is 爆 or more than 90 , use XPath Axes's descendant keyword
    xpath = paste("//div[@id='main-container']//div[@class='r-list-container action-bar-margin bbs-screen']//div[@class='r-ent' and descendant::span='爆']//div[@class='title']//a" ,
                  " | //div[@id='main-container']//div[@class='r-list-container action-bar-margin bbs-screen']//div[@class='r-ent' and descendant::span > " ,
                  score , "]//div[@class='title']//a" )
    
    
    temp = xpathApply(doc, xpath,xmlValue)
    na.title = c(na.title,temp)
    
    href = xpathApply(doc,xpath,xmlAttrs)
    temp = sapply(href, function(x) x[["href"]])
    na.href = c(na.href,temp)
    
    # Refer to http://stackoverflow.com/questions/1064968/how-to-use-xpath-contains-here
    prev = xpathApply(doc,"//div[@id='main-container']//div[@class='btn-group btn-group-paging']//a[contains(text(),'上頁')]",xmlAttrs)
    na.prev = sapply(prev, function(x) x[["href"]])
    na.prev
    
    url =  paste(root,na.prev,sep="")
    
  }
  
  article_list = list(title=na.title,href=na.href)
  
  return(article_list)
}


# store article data to MySQL database
WebCrawker.storeArticleData <- function(article_list,board_url) {
  
  
  dbconn = dbConnect(MySQL(), user='root', password=pass, host='localhost', dbname="ptt")
  
  #change encoding to utf8
  dbSendQuery(dbconn , "SET NAMES utf8")
  
  query = paste( "select board_id from board where url='" , board_url , "'"  , sep='') 
  
  rs  = dbSendQuery(dbconn , query)
  
  board_id = fetch(rs, n=1)

  # aware to close result set  
  dbClearResult(rs)
  
  
  for( i in 1:length(article_list$title) ) {
    
    query = paste( "INSERT INTO article (title,href,board_id) VALUES('" , 
                    article_list$title[i] , "','" , 
                    article_list$href[i] ,  "','" , 
                    board_id  , "')"  , sep='' )
    
    dbSendQuery(dbconn , query)
    
  }
  
  dbDisconnect(dbconn)
}


# extract data from html file , and package to list
WebCrawker.getContentData <- function(article_url) {
  
  # store to map data
  nrec_list = 1:3
  names(nrec_list) = c('推 ','噓 ','→ ')  
  
  url =  paste(root,article_url,sep="")
  
  data = getURL(url,ssl.verifyHost=F,ssl.verifypeer=F)
  doc = htmlParse(data)
  
  # get the author of content
  xpath = paste("//div[@class='push']//span[@class='f3 hl push-userid']")
  na.author = xpathApply(doc, xpath,xmlValue)
  # refer to  http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
  # get trim str result
  na.author = sub("^\\s+", "", na.author)
  na.author = sub("\\s+$", "", na.author)
  na.author
  
  # get the rec of content
  xpath = paste("//div[@class='push']//span[@class='hl push-tag'] | //div[@class='push']//span[@class='f1 hl push-tag']")
  na.nrec = xpathApply(doc, xpath,xmlValue)
  na.nrec = unlist(na.nrec)
  na.nrec = nrec_list[na.nrec]
  
  # get content
  xpath = paste("//div[@class='push']//span[@class='f3 push-content']")
  na.content = xpathApply(doc, xpath,xmlValue)
  na.content = substr(na.content , start=3, stop=nchar(na.content))
  na.content
  
  content_list = list(author=na.author,nrec=na.nrec,content=na.content)
  
  return(content_list)
  
}


# store content data to MySQL database
WebCrawker.storeContentData <- function(content_list,article_url) {
  
  dbconn = dbConnect(MySQL(), user='root', password=pass, host='localhost', dbname="ptt")
  
  #change encoding to utf8
  dbSendQuery(dbconn , "SET NAMES utf8")
  
  query = paste( "select article_id from article where href='" , article_url  , "'"  , sep='') 
  
  rs  = dbSendQuery(dbconn , query)
  
  article_id = fetch(rs, n=1)
  
  # aware to close result set  
  dbClearResult(rs)
  
  
  for( i in 1:length(content_list$author) ) {
    
    content = dbEscapeStrings(dbconn , content_list$content[i])
    
    query = paste( "INSERT INTO content (article_id , rec_type , author_id , content) VALUES('" , 
                   article_id , "','" , 
                   content_list$nrec[i]   ,  "','" ,
                   content_list$author[i]   ,  "','" , 
                   content  , "')"  , sep='' )
    
    
    dbSendQuery(dbconn , query)
    
  }
  
  dbDisconnect(dbconn)
}

