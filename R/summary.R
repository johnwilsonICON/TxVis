#' @export

summary.txvis <- function(object, ...) {
  # This function will summarize useful information in a txvis class object such as:
  #
  # 1. Number of: patients, treatments, & classes
  # 2. Mean, max and min: number of treatments/patient, start/end tx dates
  
  treats.summary <- matrix(nrow = 5, 
                           ncol = length(levels(object[[1]]$tx)))

  colnames(treats.summary) <- levels(object[[1]]$tx)
  
  rownames(treats.summary) <- c("Unique treatments",
                                 "Mean treatments per patient",
                                 "Mean duration of treatment",
                                 "Min duration of treatment",
                                 "Max duration of treatment")
  
  y <- plyr::ddply(object[[1]], 
                   plyr::.(pt_id, tx), 
                   nrow)

  treats.summary[1, ] <- by(object[[1]]$pt_id, 
                           object[[1]]$tx,
                           length)
  
  treats.summary[2, ] <- round(by(y$V1, y$tx, mean),2)
  treats.summary[3, ] <- round(by((object[[1]]$end_date - object[[1]]$start_date), 
                                  object[[1]]$tx, mean),2)
  treats.summary[4, ] <- by((object[[1]]$end_date - object[[1]]$start_date), object[[1]]$tx, min)
  treats.summary[5, ] <- by((object[[1]]$end_date - object[[1]]$start_date), object[[1]]$tx, max)

  cat('Summary of txvis class object.\n\n')
  print(treats.summary)  
  
  cat('        \n')  
    if (nrow(object[[2]]) == 0) {
      cat("No coded event data.\n\n")
    } else {

      z <- plyr::ddply(object[[2]], plyr::.(ev_pt_id, event), nrow)
            
      events.summary <- matrix(nrow = 4, ncol = length(levels(object[[2]]$event)))
      colnames(events.summary ) <- levels(object[[2]]$event)
      rownames(events.summary) <- c("Mean events per patient","Mean duration of event","Min duration of event","Max duration of event")
      
      events.summary[1, ] <- round(by(z$V1, z$event, mean),2)
      events.summary[2, ] <- round(by((object[[2]]$ev_end_date - object[[2]]$ev_date), object[[2]]$event, mean),2)
      
      events.summary[3, ] <- by((object[[2]]$ev_end_date - object[[2]]$ev_date), 
                                object[[2]]$event, 
                                min)
      
      events.summary[4, ] <- by((object[[2]]$ev_end_date - object[[2]]$ev_date), 
                                object[[2]]$event, 
                                max)
      
      print(events.summary)  
    }
  }