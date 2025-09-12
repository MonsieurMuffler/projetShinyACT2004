## Paquetages
library(shiny)
library(projetShinyACT2004)
library(ggplot2)
library(latex2exp)

## Serveur
function(input, output, session) {

    ## Âge initial
    the_x <- reactive(input$the_x)

    ## Durée
    the_n <- reactive(input$the_n)

    ## Âge ultime
    the_omega <- reactive(input$the_omega)

    ## Force d'intérêt
    the_delta <- reactive(input$the_delta)

    ## Force de mortalité
    the_mu <- reactive(input$the_mu)

    ## Type de produit
    the_type <- reactive(switch(input$the_type,
                                   "Assurance temporaire" = "temp",
                                   "Capital différé" = "cap_diff",
                                   "Assurance mixte" = "mixte",
                                   "Assurance différée" = "diff"))


    ## Valeurs intermédiaires
    the_t <- reactive(seq(0, ifelse(input$the_loi == "De Moivre",
                                    the_omega(),
                                    1000) - the_x(), 0.2))
    the_Z <- reactive(Z(the_delta(), the_t()))
    the_tqx <- reactive(switch(input$the_loi,
                               "De Moivre" = tqx("demoivre",
                                                 t = the_t(),
                                                 omega = the_omega(),
                                                 x = the_x()),
                               "Exponentielle" = tqx("expo",
                                                     t = the_t(),
                                                     mu = the_mu())))
    the_max <- reactive(switch(input$the_loi,
                               "De Moivre" = TRUE,
                               "Exponentielle" = the_t() <= 100))

    ## Assurance vie entière (valeur actualisée)
    output$Z_ent <- renderPlot({
        graph_Z(Z = the_Z()[the_max()],
                t = the_t()[the_max()],
                n = the_n(),
                x = the_x(),
                type = "ent")
    })

    ## Autre produit (valeur actualisée)
    output$Z_comp <- renderPlot({
        graph_Z(Z = the_Z()[the_max()],
                t = the_t()[the_max()],
                n = the_n(),
                x = the_x(),
                type = the_type())
    })

    ## Assurance vie entière (fonction de répartition)
    output$FZ_ent <- renderPlot({
        graph_FZ(Z = the_Z(),
                 t = the_t(),
                 tqx = the_tqx(),
                 n = the_n(),
                 type = "ent")
    })

    ## Autre produit (fonction de répartition)
    output$FZ_comp <- renderPlot({
        graph_FZ(Z = the_Z(),
                 t = the_t(),
                 tqx = the_tqx(),
                 n = the_n(),
                 type = the_type())
    })
}
