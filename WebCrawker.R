root = 'https://www.ptt.cc'
pass = 'secret'

# extract data from html file , and package to list
WebCrawker.getBoardData <- function() {
  
  library("RCurl")
  library("XML")
  
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
  
  library(RMySQL)
  
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
