#' @export
graph_FZ <- function(..., x, type = c("ent", "temp", "cap_diff", "mixte", "diff"))
{
    graph <- switch(match.arg(type),
                    "ent" = graph_FZ_ent(...),
                    "temp" = graph_FZ_temp(...),
                    "cap_diff" = graph_FZ_cap_diff(...),
                    "mixte" = graph_FZ_mixte(...),
                    "diff" = graph_FZ_diff(...))
    graph +
        labs(x = "x",
             y = TeX("$F_Z(x)$")) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(-0.1, 1.1)) +
        theme_bw()
}