#' @export
graph_FY_via <- function(Y, t, tqx, n)
{
    ggplot() +
        geom_line(aes(x = Y, y = tqx), linewidth = 1) +
        geom_segment(aes(x = -0.5, xend = 0, y = 0, yend = 0), linewidth = 1) +
        geom_segment(aes(x = max(Y), xend = max(Y) * 1.05, y = 1, yend = 1), linewidth = 1) +
        labs(title = "Fonction de répartition (rente viagère)")
}
