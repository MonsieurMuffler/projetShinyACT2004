#' @export
graph_Y_via <- function(Y, t, n)
{
    ggplot() +
        geom_line(aes(x = t, y = Y), linewidth = 1) +
        labs(title = "Valeur actualisée (rente viagère)")
}
