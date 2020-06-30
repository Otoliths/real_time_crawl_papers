library(magrittr)
library(rvest)
library(Rcrawler)
library(tibble)
library(stringr)
library(lubridate)
library(rlist)

list <- readRDS("list.rds")
get.springer <- function(journal= NULL){
  i <- j <- k <- match(journal,list$journal)
  title <- doi <- type <- date <- journal <- NULL
  url <- list$url[i]
  doi <- read_html(url) %>% html_nodes(".c-card__title a") %>% html_attr("href")
  doi <- gsub("https://link.springer.com/article/", "",doi[-1])
  title <- read_html(url) %>% html_nodes(".c-card__title a")
  title <- html_text(title)[-1]
  li <- read_html(url) %>% html_nodes(".c-meta__item") %>% html_text()
  type <- li[grep("Content type",li)]
  type <- gsub("Content type: ","",type)
  Published <- li[grep("Published",li)]
  Published <- lubridate::dmy(gsub("Published: ","",Published))
  data <- tibble::tibble(title = title,type = type,
                         date = Published, doi = doi)
  data$journal <- rep(list$journal[j], dim(data)[1])
  data$type <- stringr::str_to_title(data$type)
  data$title <- stringr::str_to_title(data$title)
  cat(sprintf("\n %s is OK!!! ",list$journal[k]),sep = "\n")
  return(data)
  rm(url)
}

get.sciencedirect <- function(journal= NULL){
  i <- j <- k <- match(journal,list$journal)
  title <- doi <- type <- date <- journal <- NULL
  url <- list$url[i]
  url <- "https://www.sciencedirect.com/journal/science-of-the-total-environment"
  doi <- read_html(url) %>% html_nodes(".u-font-serif a") %>% html_attr("href")
  doi <- paste0("https://www.sciencedirect.com",doi)
  doi <- sapply(doi,function(i){
    s <- read_html(i) %>% html_nodes(".doi") %>% html_attr("href") 
    s <- gsub("https://doi.org/","",s)
  })
  title <- read_html(url) %>% html_nodes(".u-font-serif a") %>% html_text()
  type <- read_html(url) %>% html_nodes(".js-article-subtype") %>% html_text()
  Published <- read_html(url) %>% html_nodes(".js-article-item-aip-date") %>% html_text()
  Published <- gsub("In Press, Journal Pre-proof, Available online ","",Published)
  Published <- lubridate::dmy(gsub("Published: ","",Published))
  data <- tibble::tibble(title = title,type = type,
                         date = Published, doi = doi)
  data$journal <- rep(list$journal[j], dim(data)[1])
  data$type <- stringr::str_to_title(data$type)
  data$title <- stringr::str_to_title(data$title)
  cat(sprintf("\n %s is OK!!! ",list$journal[k]),sep = "\n")
  return(data)
  rm(url)
}


get.wiley <- function(journal=NULL){
  i <- j <- k <- match(journal,list$journal)
  title <- type <- date <- doi <- NULL
  url <- list$url[i]
  dat1 <- Rcrawler::ContentScraper(Url= url,
                                   CssPatterns = c(".visitable",".meta__type",".meta__epubDate"), 
                                   PatternsName = c("title","type","date"),
                                   ManyPerPattern = TRUE)
  dat2 <- Rcrawler::ContentScraper(Url= url,
                                   CssPatterns = c(".visitable"),
                                   PatternsName = c("doi"),
                                   ManyPerPattern = TRUE,astext = F)
  data <- c(dat1,dat2)
  data <- tibble::as_tibble(data)
  data$doi <- sub("<a href=\"/doi/", "", data$doi)
  data$doi <- gsub("\".*","",data$doi)
  data$journal <- rep(list$journal[j], dim(data)[1])
  data$date <- sub("First published: ","",data$date)
  data$date <- gsub("^\\s+|\\s+$", "", data$date)#去除字符串前后空格
  data$date <- sub("January","/01/",data$date)
  data$date <- sub("February","/02/",data$date)
  data$date <- sub("March","/03/",data$date)
  data$date <- sub("April","/04/",data$date)
  data$date <- sub("May","/05/",data$date)
  data$date <- sub("June","/06/",data$date)
  data$date <- sub("July","/07/",data$date)
  data$date <- sub("August","/08/",data$date)
  data$date <- sub("September","/09/",data$date)
  data$date <- sub("October","/10/",data$date)
  data$date <- sub("November","/11/",data$date)
  data$date <- sub("December","/12/",data$date)
  data$date <- gsub("\\s", "", data$date)
  data$date <- lubridate::dmy(data$date)#Parsing dates and times
  data$type <- stringr::str_to_title(data$type)
  data$title <- stringr::str_to_title(data$title)
  cat(sprintf("\n %s is OK!!! ",list$journal[k]),sep = "\n")
  return(data)
  rm(url)
}

get.information <- function(journal= NULL){
  cat(sprintf("last Update: %s",Sys.Date()),sep = "\n")
  title <- type <- date <- doi <- NULL
  v <- match(journal,list$journal)
  type <- list$type[v]
  data <- data.frame()
  if(type=="wiley"){
    data <- get.wiley(journal)
  }
  else if (type=="springer"){
    data <- get.springer(journal)
  }
  else if (type=="sciencedirect"){
    data <- get.sciencedirect(journal)
  }
  # df_list <- list(df1,df2)
  # data <- Reduce(function(x,y) merge(x,y,all=T),df_list)
  return(data)
}

paper <- lapply(list$journal,get.information)
paper <- rlist::list.stack(paper)
keywords <- c("migration","anguilla","eel","potamodromous","diadromous","anadromous","catadromous","amphidromous","oceanodromous")
row <- lapply(keywords,grep,stringr::str_to_lower(paper$title))
row <- unique(c(row[[1]],row[[2]],row[[3]],row[[4]],row[[5]],row[[6]],row[[7]],row[[8]],row[[9]]))
new_paper <- paper[row,]
new_paper <- rbind(readRDS("new_paper.rds"),new_paper)
#new_paper <- rbind(readRDS(paste0("new_paper/",Sys.Date()-1,”.rds")),new_paper)
new_paper <- unique(new_paper)
new_paper <- new_paper[order(new_paper$date,decreasing = T),]
if (!file.exists("new_paper”)){
    dir.create("new_paper")
  }
path <- paste0("new_paper/",Sys.Date(),".rds")
saveRDS(new_paper,path)
