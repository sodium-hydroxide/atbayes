#' Title
#'
#' @param data 
#' @param column 
#' @param order_by 
#' @param time_step 
#' @param logarithmic 
#'
#' @return
#' @export
#'
#' @examples
time_derivative <- function(
        data,
        column = "",
        order_by = NULL,
        time_step = 1.0,
        logarithmic = FALSE) {

    if (column == "") {stop("Must select a column to take derivative of.")}

    if (!("btools" %in% installed.packages())) {
        devtools::install_github("sodium-hydroxide/btools")
    }

    data <- btools::shift(
        data = data,
        column = "value",
        order_by = order_by
    )

    .data. <- data
    .data.$value <- data[[column]]

    .data.$derivative <-
        (.data.$value_1_back - .data.$value) / time_step
    .data.$derivative[nrow(.data.)] <- 0
    
    if (logarithmic) {
        .data.$mean <- 
            (.data.$value_1_back - .data.$value) * 0.5
        
        .data.$zero_over_zero <- 
            .data.$mean == 0 &
            .data.$derivative == 0
        
        .data.$log_derivative <- .data.$derivative / .data.$mean
        
        .data.$log_derivative[.data.$zero_over_zero] <- 0
        
        data[[paste(column, "log_dot")]] <- .data.$log_derivative
    }
    else {
        data[[paste(column, "dot")]]
    }

    return(data)
}