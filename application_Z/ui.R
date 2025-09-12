## Paquetages
library(shiny)

## Page
fluidPage(

    ## Titre
    titlePanel(paste0("Variable aléatoire Z")),

    ## Paramètres
    sidebarLayout(
        sidebarPanel(
            # Produit à comparer
            selectInput(inputId = "the_type",
                        label = "Produit à comparer avec l'assurance vie entière : ",
                        choices = c("Assurance temporaire",
                                    "Capital différé",
                                    "Assurance mixte",
                                    "Assurance différée")),
            # Force d'intérêt
            numericInput(inputId = "the_delta",
                         label = "Force d'intérêt : ",
                         value = 0.05,
                         min = 0,
                         max = 0.2,
                         step = 0.01),
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
                        label = "Durée des périodes temporaires/différées : ",
                        value = 30,
                        min = 0,
                        max = 100,
                        step = 1),
            # Âge ultime
            sliderInput(inputId = "the_omega",
                        label = "Âge ultime (loi de De Moivre) : ",
                        value = 100,
                        min = 0,
                        max = 100,
                        step = 1),
            # Force de mortalité
            numericInput(inputId = "the_mu",
                         label = "Force de mortalité (loi exponentielle) : ",
                         value = 0.03,
                         min = 0,
                         max = 0.2,
                         step = 0.01)
        ),

        ## Graphiques
        mainPanel(
            column(6,
                   plotOutput("Z_ent"),
                   plotOutput("FZ_ent")),
            column(6,
                   plotOutput("Z_comp"),
                   plotOutput("FZ_comp"))
        )
    )
)
