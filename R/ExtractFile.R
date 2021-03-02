#' ExtractFile
#'
#' selects the useful data from a Risoe.BINfileData-class object
#'
#' @param files a list of Risoe.BINfileData-class objects
#' @param n_file number of the item to be selected in the 'files' list
#' @param sequence selected sequence
#'
#' @return The Risoe.BINfileData-class object is rewritten to take into account only the data included in the sequence
#' @export
#'
#' @examples
ExtractFile<-function(files,n_file=1,sequence){
#extract data from a Risoe.BINfileData-class object
if (class(files[[n_file]])!="Risoe.BINfileData") {stop("invalid 'files' class")}

if (missing(sequence)) {
  selection<-files[[n_file]]@METADATA$SEL=="TRUE"
  files[[n_file]]@DATA<-files[[n_file]]@DATA[selection]
  files[[n_file]]@METADATA<-files[[n_file]]@METADATA[selection,]
  files[[n_file]]@.RESERVED<-files[[n_file]]@.RESERVED[selection]
 }
else{
  files[[n_file]]@DATA<-files[[n_file]]@DATA[sequence]
  files[[n_file]]@METADATA<-files[[n_file]]@METADATA[sequence,]
  files[[n_file]]@.RESERVED<-files[[n_file]]@.RESERVED[sequence]
}
files
}
