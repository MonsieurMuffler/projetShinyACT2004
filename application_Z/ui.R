#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    ## Titre
    titlePanel("Bip bip"),

    ## Paramètres
    sidebarLayout(
        sidebarPanel(
            # Loi de mortalité
            selectInput(inputId = "the_loi",
                        label = "Loi de mortalité : ",
                        choices = c("De Moivre",
                                    "Exponentielle")),
            # Âge intital
            sliderInput(inputId = "the_x",
                        label = "Âge initial : ",
                        value = 0,
                        min = 0,
                        max = 100,
                        step = 1),
            # Durée des produits temporaires
            sliderInput(inputId = "the_n",
                        label = "Durée des produits temporaires : ",
                        value = 30,
                        min = 0,
                        max = 100,
                        step = 1),
            # Âge ultime
            sliderInput(inputId = "the_omega",
                        label = "Âge ultime : ",
                        value = 100,
                        min = 0,
                        max = 100,
                        step = 1),
            # Force d'intérêt
            numericInput(inputId = "the_delta",
                         label = "Force d'intérêt : ",
                         value = 0.05,
                         min = 0,
                         max = 1,
                         step = 0.01),
            # Force de mortalité
            numericInput(inputId = "the_mu",
                         label = "Force de mortalité : ",
                         value = 0.03,
                         min = 0,
                         max = 1,
                         step = 0.01),
            # Produit à comparer
            selectInput(inputId = "the_type",
                        label = "Type de produit : ",
                        choices = c("Assurance temporaire",
                                    "Capital différé",
                                    "Assurance mixte",
                                    "Assurance différée"))
        ),

        ## Graphiques
        mainPanel(
            column(6,
                   plotOutput("Z_ent"),
                   plotOutput("Z_comp")),
            column(6,
                   plotOutput("FZ_ent"),
                   plotOutput("FZ_comp"))
        )
    )
)
