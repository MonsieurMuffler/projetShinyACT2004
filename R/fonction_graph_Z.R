#' @export
graph_Z <- function(..., x, type = c("ent", "temp", "cap_diff", "mixte", "diff"))
{
    graph <- switch(match.arg(type),
                    "ent" = graph_Z_ent(...),
                    "temp" = graph_Z_temp(...),
                    "cap_diff" = graph_Z_cap_diff(...),
                    "mixte" = graph_Z_mixte(...),
                    "diff" = graph_Z_diff(...))
    graph +
        labs(x = TeX(paste0("$T_{", x, "}$")),
             y = "Z") +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
        theme_bw()
}