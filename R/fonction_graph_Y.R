#' @export
graph_Y <- function(..., x, type = c("via", "temp", "diff", "gar"))
{
    graph <- switch(match.arg(type),
                    "via" = graph_Y_via(...),
                    "temp" = graph_Y_temp(...),
                    "diff" = graph_Y_diff(...),
                    "gar" = graph_Y_gar(...))
    graph +
        labs(x = TeX(paste0("$T_{", x, "}$")),
             y = "Y") +
        scale_y_continuous(labels = label_number(decimal.mark = ",")) +
        scale_x_continuous(labels = label_number(decimal.mark = ",")) +
        expand_limits(y = 0) +
        theme_bw()
}
