##Function: getshows()
##Download npr stories from 
##  Morning Edition, All things Considered, or Weekend edition Sunday/Saturday into a directory/folder
##This will: 1) change your working directory to the path in "directory", 
##   2) create a folder for the show where all future downloads can be stored, (does this only if this folder doesn't exist yet)
##   3) change the working directory again to the created show folder 
##   3) download the show files into the created show folder
##   4) rename the files to sort properly and remove bad charaters from the file names, 
##     5) delete folders in the show directory older than 2 wks 
##         (eg, only removes files in the created show folder, not files from elsewhere), and
##   6) resets your working directory back to what it was before you called this function
##
##USAGE: show should be one of the following: "atc", "me" "wesat", "wesun" 
##                  These codes are for 
##            All things Considered, Morning Edition, Weekend Edition Saturday, Weekend Edition Sunday, respectively
##       date is either "today", or a date in the format "YYYY-mm-dd" (possible range is today-5 days)
##       directory is the path to the directory (folder) where you want the downloaded files to be stored 
##                 ex: "/home/myName/Documents/podcasts/"
         
getshows <- function(show="atc",date="today", directory) {
    wdir<-getwd()
    basedir<-directory
    library(stringr)
    if (show=="wesun"){
        show_long<-"weekend-edition-sunday"
        show_short<-"WEsun"}
    if (show=="wesat"){
        show_long<-"weekend-edition-saturday"
        show_short<-"WEsat"}
    if (show=="atc"){
        show_long<-"all-things-considered"
        show_short<-"ATC"       }
    if (show=="me"){
        show_long<-"morning-edition"
        show_short<-"ME"}
    setwd(basedir)
    w<-paste(basedir, 
             show_short, sep="")
    if(!file.exists(show_short)){dir.create(show_short)}
    setwd(w)
  
    ##remove old downloads
    oldfiles<-list.files()
    oldfileinfo<-file.info(oldfiles)
    oldfiles<-oldfiles[Sys.time()-oldfileinfo$mtime>14]
    if(length(oldfiles>0)){
        for (i in 1:length(oldfiles)){
            unlink(oldfiles, recursive=TRUE)
        }
    }
    
    ##proceed with getting new files
    URL_today<-paste("http://www.npr.org/programs/", show_long, "/#",sep="")
    URL_archive<-paste("http://www.npr.org/programs/", 
                       show_long, "/archive",sep="")
    
    ##set up date variables and create show folder
    Date<-ifelse(date =="today", as.character(Sys.Date()), date) ##Use sys.date to get today if date is "today"
    yrmoda<-str_split(Date, "-", n=3) #split the date
    datestr<-paste(yrmoda[[1]][1], yrmoda[[1]][2],yrmoda[[1]][3],sep="_")
    datestring<-paste0(yrmoda[[1]][1], yrmoda[[1]][2],yrmoda[[1]][3])
    if (as.Date(Date)<(Sys.Date()-5)){
        ##some error checking and feedback for the user
        print('wrong input; need "today" or "YYYY-MM-DD" less than 5 days old' )
        print(paste0(Date, ' is not within 5 days of today' ))
    } else{
        folder<-paste(show_short,datestr, sep="_")
        dir.create(folder)
        d<-paste(w, folder, sep="/")
        setwd(d)
        URL<-URL_today   
        
        if (date !="today"){        
            ###Get specific URL for older shows by scrapping the URL archive webpage
            archive   <- GET(URL_archive, add_headers(`Connection` = "keep-alive", `User-Agent` = "httr"))
            parse<-XML::htmlParse(archive, asText = T)
            links<-XML::getHTMLLinks(parse)
            links<-unique(links[grep(Date, links)])
            URL<-links[grep("https", links)]
        }
        
        ##check if we have a URL
        if (length(URL)<1){
            print("show not found, check date or URL")
        }else {print("found that show URL")}
        
        ##Get content from URL
        web_page   <- GET(URL, add_headers(`Connection` = "keep-alive", `User-Agent` = "httr"))
        #nodes<-html_nodes(content(web_page), "h4")[grep('audio', html_nodes(content(web_page), "h4"))]
        parse<-XML::htmlParse(web_page, asText = T)
        links<-XML::getHTMLLinks(parse)
        links<-links[grep('ondemand.npr.org', links)]
        ##remove all the music transition mp3s and remove duplicates
        story_lines<-unique(links[grep('siteplayer', links)])
        Num<-length(story_lines)
        if (Num>1){
            print("found URLs for stories")
        }else {print("can't find the story links")}
        title<-str_split(story_lines, paste0(show, '_') )
        title<-sapply(title, "[[", 2)
        title<-str_split(title, '.mp3') 
        title<-sapply(title, "[[", 1)
        
        #title<-str_split(nodes, '>') 
        #title<-sapply(title, "[[", 2)
        #title<-str_split(title, '<') 
        #title<-sapply(title, "[[", 1)
        
        ##zero pad track number; don't need, put this in download line
        #track<-sprintf("%02d",seq(1:Num))
        
        ##omit bad symbols from title
        bad<-c("-")
        title<- gsub(bad,"_", title)
        bad<-c(":")
        title<- gsub(bad,"_", title)
        bad<-c("&")
        title<- gsub(bad,"_", title)
        bad<-c("?")
        title<- gsub(bad,"", title)
        bad<-c("!")
        title<- gsub(bad,"", title)
        bad<-c("%")
        title<- gsub(bad,"", title)
        bad<-c("'")
        title<- gsub(bad,"", title)
        bad<-c(",")
        title<- gsub(bad,"", title)
        bad<-c(" ")
        title<- gsub(bad,"_", title)
        
        ##download file
        for(i in 1:Num){
            download.file(story_lines[i], paste(show_short, "_", datestring,
                                                "_", sprintf("%02d", i),"_",title[i], ".mp3", sep=""), quiet = TRUE, mode = "wb",
                          cacheOK = TRUE, extra = getOption("download.file.extra"))
        }
        setwd(wdir)
        output<-paste("there were", Num, "stories", sep=" ") 
        print(output) 
    }
}
