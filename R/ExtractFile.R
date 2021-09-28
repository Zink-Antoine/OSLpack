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
ExtractFile<-
  function(files,n_file=1,sequence){
    if (class(files)!="Risoe.BINfileData"){
      if (class(files[[n_file]])!="Risoe.BINfileData") {stop("invalid 'files' class")}
      files<-files[[n_file]]
    }

    if (missing(sequence)) {
      selection<-files@METADATA$SEL=="TRUE"
      files@DATA[[n_file]]<-files@DATA[selection]
      files@METADATA<-files@METADATA[selection,]
      files@.RESERVED<-files@.RESERVED[selection]
    }
    else{
      files@DATA<-files@DATA[sequence]
      files@METADATA<-files@METADATA[sequence,]
      files@.RESERVED<-files@.RESERVED[sequence]
    }
    files
  }
