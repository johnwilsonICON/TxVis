#' Generate an icicle plot for treatment data.
#' 
#' @description Using a \code{txvis} object, plot the sequencing of treatments using an icicle plot.
#' 
#' @param txvis An object of class \code{txvis}.
#' @param nsequ The number of sequences to be shown (default is 4)
#' @param seq.v.dat Should the data be plotted by sequence order or by date?
#' @param start If supplied the data will be truncated to all sequences after a start date.
#' @param end If supplied the data will be truncated to all sequences before the end date.
#' @param interval Length of time for intervening intervals for data plotted by date.
#' @param conflict If two treatments fall within the same date interval, which should be displayed?
#' @param tx_color A \code{colorRampPalette} to color the sequnces.  Note that the first element of the color vector always codes to "None".
#' 
#' @return A \code{ggplot2} object.
#' 
#' @examples
#'  hlth_data <- create_txvis(patient        = treat$pat_id, 
#'                            treatment      = treat$treatment,
#'                            start          = treat$start,
#'                            end            = treat$end,
#'                            date_format    = "%B %d, %Y",
#'                            ev_patient     = events$pat_id,
#'                            events         = events$event,
#'                            event_date     = events$start,
#'                            event_end_date = events$end)
#'                            
#'  # Basic plotting:                          
#'  tx_icicle(hlth_data)
#'  
#'  #  Add additional ggplot2 styline:
#'  tx_indiv(hlth_data) + ggplot2::theme_bw()
#'  
#'  # Use a customized color palette:
#'  colors <- c("#FFFFFF", RColorBrewer::brewer.pal(length(levels(hlth_data[[1]]$tx)), 'Accent'))
#'  tx_icicle(hlth_data, tx_colour = colors)
#' @export

tx_icicle <- function(txvis, 
                      nsequ = NULL,
                      seq.v.dat = "seq",
                      start = NULL, 
                      end = NULL, 
                      interval = "month", 
                      conflict = "majority", 
                      tx_colour=NULL) {
  
  if (!"txvis" %in% class(txvis)) {
    stop('You must pass a txvis object.')
  }

  if (seq.v.dat == "seq") {
    txvis.ref <- data.frame(t(apply(reform_seq(txvis,nsequ), 1, function(x) {x[is.na(x)] <- "None";(x)})),
                            stringsAsFactors = F)
  } else {
    txvis.ref <- data.frame(t(apply(reform_dates(txvis,nsequ,start, end, interval, conflict), 1, function(x) {x[is.na(x)] <- "None";(x)})),
                            stringsAsFactors = F)
  }
  
  seq.cols <- paste0(rep("seq_", (ncol(txvis.ref) - 1)), 
                     c(1:(ncol(txvis.ref) - 1)))
  
  seq.fun  <- paste0(seq.cols, collapse = " + ")

  input_agged_seq <- aggregate(data = txvis.ref, 
                               as.formula(paste0("pt_id ~ ", seq.fun)) ,
                               FUN = length)
  
  input_agged_seq <- input_agged_seq[do.call(order,input_agged_seq[,seq.cols]),]
  input_agged_seq <- input_agged_seq[rep(row.names(input_agged_seq), input_agged_seq$pt_id),]  #creates a row for each data point
  input_agged_seq["x"] <- 1:nrow(input_agged_seq)
  
  plot_input <- reshape2::melt(input_agged_seq,id = c("pt_id", "x"))
  
  plot_input$variable <- as.character(plot_input$variable)
  
  plot_input[nchar(plot_input$variable) == 5,"variable"] <- paste0(substr(plot_input[nchar(plot_input$variable) == 5,"variable"],1,4),"0",substr(plot_input[nchar(plot_input$variable) == 5,"variable"],5,5))
  plot_input$variable <- as.numeric(substr(plot_input$variable,5,6))
  
  if (is.null(tx_colour)) { 
    colors <- colorRampPalette(c("dark blue", "white"))(length(unique(plot_input$value)))
  } else { 
    colors <- tx_colour
  }

  ggplot2::ggplot(plot_input, 
                  ggplot2::aes(x = 100 * x / max(x), y = variable, fill = value)) + 
    ggplot2::geom_tile() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"), 
                   panel.border     = ggplot2::element_rect(colour = "black", 
                                                            fill = NA, size = 1)) + 
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_reverse(expand = c(0,0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(fill = "Treatment",
                  x = "Percent",
                  y = "Sequence number")
  
}
