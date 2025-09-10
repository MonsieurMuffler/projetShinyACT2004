#' @export
graph_Z_ent <- function(Z, t, n)
{
    ggplot(mapping = aes(x = t, y = Z)) +
        geom_line(linewidth = 1) +
        labs(title = "Vie entière")
}