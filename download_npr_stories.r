##Download npr stories from 
##  Morning Edition, All things Considered, or Weekend edition Sunday/Saturday into a directory/folder
##This will: 1) change your working directory to the path in "directory", 
##   2) create a folder for the show where all future downloads can be stored, (does this only if this folder doesn't exist yet)
##   3) change the working directory again to the created show folder 
##   3) download the show files into the created show folder
##   4) rename the files to sort properly and remove bad charaters from the file names, 
     5) delete folders in the show directory older than 2 wks 
         (eg, only removes files in the created show folder, not files from elsewhere), and
##   6) resets your working directory back to what it was before you called this function
##
##USAGE: show is one of the following: "atc", "me" "wesat", "wesun" for 
##            All things Considered, Morning Edition, Weekend Edition Saturday, Weekend Edition Sunday, respectively
##       date is either "today", or a date in the format "YYYY-mm-dd" (today-10)
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
    if (date =="today"){
        Date<-as.character(Sys.Date())##Use for day is today
        yrmoda<-str_split(Date, "-", n=3) #split the date
        datestr<-paste(yrmoda[[1]][1], yrmoda[[1]][2],yrmoda[[1]][3],sep="_")
        if (as.Date(Date)<(Sys.Date()-10)){
            print('wrong input; need "today" or "YYYY-MM-DD" less than 10 days old' )
            print(paste0(Date, ' is not within 10 days of today' ))
        } else{
            folder<-paste(show_short,datestr, sep="_")
            dir.create(folder)
            d<-paste(w, folder, sep="/")
            setwd(d)
            web_page <- readLines(URL_today)
            story_lines <- web_page[grep('li class="audio-tool audio-tool-download"><a href=', web_page)]
            Num<-length(story_lines)
            if (Num>1){
                print("found today's show")
            }else {print("can't find today's show")}
            file_name<-str_split(story_lines, '"') 
            list<-"empty"
            for (i in 1:Num) {
                list<-rbind(list,file_name[[i]][4])
            }
            list<-list[-1,]
            list<-unique(list)
            Num<-length(list)
            ##pull out title of story; starts w datestring and show_short
            title1<-str_split(list, '/' )
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][9])}
            datestring1<-str_split(title,"_")
            datestring<-as.character()
            for (i in 1:Num) {
                datestring<-rbind(datestring,datestring1[[i]][1])}
            
            title1<-str_split(title, ".mp3") 
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][1])}
            title1<-str_split(title, paste0(datestring,"_",show))
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][2])}
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
            
            ##download file
            for(i in 1:Num){
                download.file(list[i], paste(show_short, "_", datestring[1],
                         "_", sprintf("%02d", i),title[i], ".mp3", sep=""), quiet = TRUE, mode = "wb",
                         cacheOK = TRUE, extra = getOption("download.file.extra"))
            }
            setwd(wdir)
            output<-paste("there were", Num, "stories", sep=" ") 
            print(output) 
        }
    }
    
    else {
        Date<-date ##YYYY-MM-DD
        print('date must be "YYYY-MM-DD" (range=today-10), or "today"')
        yrmoda<-str_split(Date, "-", n=3) #split the date
        datestr<-paste(yrmoda[[1]][1], yrmoda[[1]][2],yrmoda[[1]][3],sep="_")
        if (as.Date(Date)<(Sys.Date()-10)){
            print('wrong input; need "today" or "YYYY-MM-DD" less than 10 days old' )
            print(paste0(Date, ' is not within 10 days of today' ))
        }
        else{ 
            setwd(w)
            folder<-paste(show_short,datestr, sep="_")
            dir.create(folder)
            d<-paste(w, folder, sep="/")
            setwd(d)
            archive <- readLines(URL_archive)
            shows<-archive[grep('?showDate=', archive)]
            shows<-str_split(shows, '"')
            show_num<-length(shows)
            show_list<-"empty"
            for (i in 1:show_num) {
                show_list<-rbind(show_list,shows[[i]][4])}
            show_list<-show_list[-1,]
            URL<-show_list[grep(Date, show_list)]
            if (length(URL)<1){
                print("show not found, check date")
            }else {print("found that show")}
            web_page <- readLines(URL)
            story_lines <- web_page[grep('li class="audio-tool audio-tool-download"><a href=', web_page)]
            #story_lines <- web_page[grep("http://pd.npr", web_page)]
            Num<-length(story_lines)
            file_name<-str_split(story_lines, '"') 
            list<-"empty"
            for (i in 1:Num) {
                list<-rbind(list,file_name[[i]][4])}
            list<-list[-1,]
            list<-unique(list)
            Num<-length(list)
            ##pull out title of story; starts w datestring and show_short
            title1<-str_split(list, '/' )
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][9])}
            datestring1<-str_split(title,"_")
            datestring<-as.character()
            for (i in 1:Num) {
                datestring<-rbind(datestring,datestring1[[i]][1])}
            
            title1<-str_split(title, ".mp3") 
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][1])}
            title1<-str_split(title, paste0(datestring,"_",show))
            title<-as.character()
            for (i in 1:Num) {
                title<-rbind(title,title1[[i]][2])}
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
            
            ##download file
            for(i in 1:Num){
                download.file(list[i], paste(show_short, "_", datestring[1],
                                             "_", sprintf("%02d", i),title[i], ".mp3", sep=""), quiet = TRUE, mode = "wb",
                              cacheOK = TRUE, extra = getOption("download.file.extra"))
            }
            setwd(wdir)
            output<-paste("there were", Num, "stories", sep=" ") 
            print(output)
        }
    }
}
