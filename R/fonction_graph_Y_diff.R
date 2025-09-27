#' @export
graph_Y_diff <- function(Y, t, n)
{
    ggplot() +
        geom_segment(aes(x = 0, xend = n, y = 0, yend = 0), linewidth = 1) +
        geom_line(aes(x = t[t >= n], y = Y[t >= n] - Y[t == n]), linewidth = 1) +
        labs(title = "Valeur actualisée (rente différée)")
}
