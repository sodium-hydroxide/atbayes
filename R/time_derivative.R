time_derivative <- function(
        data,
        column = "",
        order_by = "",
        time_step = 1.0,
        logarithmic = FALSE) {

    if (column == "") {stop("Must select a column to take derivative of.")}

    if (!("btools" %in% installed.packages())) {
        devtools::install_github("sodium-hydroxide/btools")
    }

    .data. <- btools::shift(
        data = data,
        column = column,
        order_by = order_by
    )
    
    data[[paste(column, "_dot", sep = "")]] <-
        (.data.[[paste(column, "_1_back", sep = "")]] - .data.[[column]]) /
        time_step
    
    
}