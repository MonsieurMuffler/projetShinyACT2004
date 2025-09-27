#' @export
graph_FY <- function(..., x, type = c("via", "temp", "diff", "gar"))
{
    graph <- switch(match.arg(type),
                    "via" = graph_FY_via(...),
                    "temp" = graph_FY_temp(...),
                    "diff" = graph_FY_diff(...),
                    "gar" = graph_FY_gar(...))
    graph +
        labs(x = "x",
             y = TeX("$F_Y(x)$")) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1), labels = label_number(decimal.mark = ",")) +
        scale_x_continuous(labels = label_number(decimal.mark = ",")) +
        theme_bw()
}
