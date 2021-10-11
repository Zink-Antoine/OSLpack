#####################Fonction ReadFile##################################################

#' ReadFile
#' load one or several BIN/BINX files
#'
#' @param list name of files list (default "liste fichier.txt")
#' @param n number of files (default n=1)
#'
#' @return FILE object of class "list" containing S4 Risoe.BINFileData objects
#' @return NFILE File name (*.bin/*.binx)
#'
#' @import Luminescence
#' @import R2WinBUGS
#' @import rv
#' @importFrom tcltk tk_choose.dir
#'
#' @export
#'
#' @examples
#' \dontrun{
#' File<-ReadFile(n=1)
#' file<-File$FILE
#' nomFile<-File$NFILE
#' }



ReadFile<-function(list="liste fichier.txt"
,n=1) {

#choose directory
Dir<-tk_choose.dir(getwd(), "Choose a suitable folder")
setwd(Dir)

#print the file list
Liste<-readLines(list)
print(Liste)

#select n file(s)
FileBin<-scan("",what=list(""),n)
FileBin

#load BIN/BINX file
FileData<-list()
for (i in 1:n){
FileData<-c(FileData,read_BIN2R(FileBin[[1]][i]))
		}
A<-list(FILE=FileData,NFILE=FileBin)
return(A)

}




