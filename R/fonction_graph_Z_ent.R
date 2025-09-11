#' @export
graph_Z_ent <- function(Z, t, n)
{
    ggplot() +
        geom_line(aes(x = t, y = Z), linewidth = 1) +
        labs(title = "Assurance vie entière")
}
