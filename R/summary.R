#' @export

#' @import plyr

summary.txvis <- function(x) {
  # This function will summarize useful information in a txvis class object such as:
  #
  # 1. Number of: patients, treatments, & classes
  # 2. Mean, max and min: number of treatments/patient, start/end tx dates
  
  treats.summary <- matrix( nrow = 5, ncol = length( levels(x[[1]]$tx )))
  colnames( treats.summary  ) <- levels( x[[1]]$tx )
  rownames( treats.summary ) <- c( "Unique treatments","Mean treatments per patient","Mean duration of treatment","Min duration of treatment","Max duration of treatment")
  
  y <- ddply( x[[1]], .( pt_id,tx ), nrow)

  treats.summary[1,] <- by( x[[1]]$pt_id, x[[1]]$tx, length )
  treats.summary[2,] <- round( by( y$V1, y$tx, mean ),2 )
  treats.summary[3,] <- round( by(( x[[1]]$end_date - x[[1]]$start_date ), x[[1]]$tx, mean ),2 )
  treats.summary[4,] <- by(( x[[1]]$end_date - x[[1]]$start_date ), x[[1]]$tx, min )
  treats.summary[5,] <- by(( x[[1]]$end_date - x[[1]]$start_date ), x[[1]]$tx, max )

  cat('Summary of txvis class object.\n\n')
  print( treats.summary )  
  
  cat('        \n')  
    if (nrow(x[[2]]) == 0) {
      cat("No coded event data.\n\n")
    } else {

      z <- ddply( x[[2]], .( ev_pt_id,event ), nrow)
            
      events.summary <- matrix( nrow = 4, ncol = length( levels(x[[2]]$event )))
      colnames( events.summary  ) <- levels( x[[2]]$event )
      rownames( events.summary ) <- c( "Mean events per patient","Mean duration of event","Min duration of event","Max duration of event")
      
      events.summary[1,] <- round( by( z$V1, z$event, mean ),2 )
      events.summary[2,] <- round( by(( x[[2]]$ev_end_date - x[[2]]$ev_date ), x[[2]]$event, mean ),2 )
      events.summary[3,] <- by(( x[[2]]$ev_end_date - x[[2]]$ev_date ), x[[2]]$event, min )
      events.summary[4,] <- by(( x[[2]]$ev_end_date - x[[2]]$ev_date ), x[[2]]$event, max )
      
      print( events.summary )  
    }
  }

