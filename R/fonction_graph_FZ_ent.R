#' @export
graph_FZ_ent <- function(Z, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Z, y = 1 - tqx), linewidth = 1) +
        geom_segment(aes(x = -0.1, xend = Z[length(Z)], y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = 1, xend = 1.1, y = 1, yend = 1), linewidth = 1) +
        labs(title = "Fonction de répartition pour une assurance vie entière")
}