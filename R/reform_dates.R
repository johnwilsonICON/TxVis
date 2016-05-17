#' Reformat dates from long to wide.
#' 
#' This is, broadly speaking a helper function, that turns the \code{txvis} object from a long
#' table to a wide table with rows for each patient
#' and columns for regular intervals of dates from a defined \code{start} and \code{end} along
#' a regular \code{interval}.  In cases where treatments overlap within a time bin there are 
#' three coded \code{conflict} resolution methods: 
#' 
#' \code{majority} in which coding priority is given to the treatment that occupies the most time within the interval
#' \code{first} in which coding priority is given to the treatment that occurs first chronologically within the interval
#' \code{last} in which coding priority is given to the treatment that occurs last chronologically within the interval
#'
#' @import lubridate
#' @param x A \code{txvis} object
#' @param start The starting period for the date matrix. Default is the earliest date in the \code{txvis} object.
#' @param end The ending period for the date matrix. Default is the earliest date in the \code{txvis} object.
#' @param interval The length of the interval in text format.  See Details.
#' @param conflict Conflict resolution.
#'
#' 
#' @examples
#'
#'  hlth_data <- create_txvis(patient   = treat$patient, 
#'                            treatment = treat$treatment,
#'                            start     = treat$start,
#'                            end       = treat$end,
#'                            date_format = "%B %d, %Y")
#'                            
#'  wide <- reform_dates(hlth_data)
#' 
#' @export

reform_dates <- function(x, nsequ=NULL, start = NULL, end = NULL, interval = "month", conflict = "majority"){
  
  # Basic check:
  if (!"txvis" %in% class(x)) {
    stop("You must pass a txvis object.")
  }
  
  # The default will be to use the date range:
  if (is.null(start)) {
    start <- min(x[[1]]$start)
  }
  
  if (is.null(end)) {
    end <- max(x[[1]]$end)
  }
  
  # Generate the sequence of dates:
  out_dates <- seq(start, end, by = interval)
  out_intervals <- lubridate::interval(out_dates[1:length(out_dates) - 1],
                                       out_dates[2:length(out_dates)])
  
  treat_out <- lapply(unique(x[[1]]$pt_id), function(pat){
  
    x[[1]]$intervals <- interval(x[[1]]$start, x[[1]]$end)
    short_patient <- x[[1]]$intervals[x[[1]]$pt_id %in% pat]
    short_tx <- as.character(x[[1]]$tx[x[[1]]$pt_id %in% pat])
    
    treatments <- lapply(short_patient, function(y) { lubridate::int_overlaps(y, out_intervals) })
    treatments <- do.call(rbind,
                          lapply(1:length(treatments), 
                                 function(y) { ifelse(treatments[[y]], short_tx[y], NA)}))
    
    colnames(treatments) <- out_dates[-length(out_dates)]
    
    if (any(colSums(!is.na(treatments)) > 1)) {
      
      warning("Treatment overlaps.")
      
      overlaps <- which(colSums(!is.na(treatments)) > 1)
      
      # There are overlapping treatments within a time period we assign treatments based on the passed rules:
      # Type "majority": we need to test which of the records is longer:
      if (conflict == "majority") {
        
        for (k in 1:length(overlaps)) {
          
          durations <- lubridate::as.duration(lubridate::intersect(out_intervals[overlaps[k]], 
                                                                   short_patient))
          
          # This works with one overlap or multiple.
          drop <- (!(1:nrow(treatments)) == which.max(durations))
          treatments[drop,overlaps[k]] <- NA
        }
      }
      if (conflict == "first") {
        # Whichever treatment is already in progress, or is "earliest" in the time period.
        for (k in 1:length(overlaps)) {
          
          starts <- lubridate::int_start(lubridate::intersect(out_intervals[overlaps[k]], 
                                                              short_patient))
          # This works with one overlap or multiple.
          drop <- (!(1:nrow(treatments)) == which.min(starts))
          treatments[drop,overlaps[k]] <- NA
        }
      }
      if (conflict == "last") {
        # Whichever treatment initiates, or is "latest" in the time period.
        for (k in 1:length(overlaps)) {
          
          starts <- lubridate::int_start(lubridate::intersect(out_intervals[overlaps[k]], 
                                                              short_patient))
          # This works with one overlap or multiple.
          drop <- (!(1:nrow(treatments)) == which.max(starts))
          treatments[drop,overlaps[k]] <- NA
        }
      }

    }
    
    apply(treatments, 2, function(x) {ifelse(all(is.na(x)), NA, x[!is.na(x)])})
    
  })
  
  filled <- do.call(rbind.data.frame, c(treat_out, list(stringsAsFactors = FALSE)))
  rownames(filled) <- unique(x[[1]]$pt_id)
  colnames(filled) <- out_dates[-length(out_dates)]
  
  nseq <- ifelse(!is.null(nsequ), nsequ, ncol(filled)) #nseq will override start and end date specifications, to only return that number of intervals.
  
  filled$pt_id<-rownames(filled)
  colorder <- c("pt_id",colnames(filled[,1:(ncol(filled)-1)]))
  filled<-filled[,colorder]
  filled<-filled[,1:(nseq+1)]
  colnames(filled)<- c("pt_id",paste("seq",rep(1:nseq),sep="_"))
  return(filled)
}

######
#date-related cuts

#then apply time cuts, currently hard-coding 30 day brackets
#CODING DECISION: align all starting dates, so that time is from index date, rather than a set calendar date.
#Could be user input
#treats<-merge(treats,aggregate(start_date~pt_id,data=treats,function(x) min(x)),by="pt_id", all.x=T)
#colnames(treats)[c(3,6)]<-c("start_date","index_date")
#treats["date_brk"]<- cut(as.numeric(treats$start_date - treats$index_date)+.001,breaks=seq(from=0,to=30*10,by=30))
#treats["dur"]<-as.numeric(treats$end_date - treats$start_date)
#treats["days_from_index"]<-as.numeric(treats$start_date - treats$index_date)
