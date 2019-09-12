

library(shiny)
library(stringr)
library(dplyr)
library(tidyr)

ui <- fluidPage(

    # Application title
        titlePanel("Dihybrid Cross Simulation"),
        h4("J. Kim"),
    # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        textInput("genotype_mother", "Genotype of mother", "TtDd"),
                        textInput("genotype_father", "Genotype of father", "TtDd"),
                        sliderInput("num_trials", "Number of trials", 1, 100, 20)
                ),

                mainPanel(
                        h3("Overview"),
                        p("This application simulates the dihybrid cross covered during the lab of Principles of Genetic Epidemiology 1."),
                        p("With given genotypes of mother and father, this will perform a random experiment and estimate the probability of each genotype of children."),
                        p("Try different genotypes and numbers of trials!"),
                        h3("Results of an experiment"),
                        tableOutput("results"),
                        tableOutput("results2")
                )
        )
)


server <- function(input, output) {
        
        str_sort_ <- function(str) {
                str_split(str, "") %>%
                        unlist %>%
                        str_sort(decreasing = TRUE) %>%
                        paste0(collapse = "")
        }
        
   


        pkd_mother <- reactive({
                input$genotype_mother %>%
                        str_sub(1, 2) %>%
                        str_split("") %>%
                        unlist %>%
                        sample(size = input$num_trials, replace = TRUE)
        })
        
        pkd_father <- reactive({
                input$genotype_father %>%
                        str_sub(1, 2) %>%
                        str_split("") %>%
                        unlist %>%
                        sample(size = input$num_trials, replace = TRUE)
        })
        
        dimples_mother <-reactive({
                input$genotype_mother %>%
                        str_sub(3, 4) %>%
                        str_split("") %>%
                        unlist %>%
                        sample(size = input$num_trials, replace = TRUE)
        })
        
        dimples_father <-reactive({
                input$genotype_father %>%
                        str_sub(3, 4) %>%
                        str_split("") %>%
                        unlist %>%
                        sample(size = input$num_trials, replace = TRUE)
        })
        
        pkd_children <- reactive({
                paste(pkd_mother(), pkd_father(), sep = "") %>%
                        sapply(str_sort_, USE.NAMES = FALSE)
        })
        dimples_children <- reactive({
                paste(dimples_mother(), dimples_father(), sep = "") %>%
                        sapply(str_sort_, USE.NAMES = FALSE)
        })
        
        genotype_children <- reactive({
                paste(pkd_children(), dimples_children(), sep = "")
        })
        
      
        table1 <- reactive({
                table(genotype = genotype_children()) %>%
                        as_tibble %>%
                        arrange(desc(genotype))
        })
        
        
        
        ## Theoretical
        
        pkd_mother_theory <- reactive({
                input$genotype_mother %>%
                        str_sub(1, 2) %>%
                        str_split("") %>%
                        unlist
        })
        
        pkd_father_theory <- reactive({
                input$genotype_father %>%
                        str_sub(1, 2) %>%
                        str_split("") %>%
                        unlist
        })
        
        dimples_mother_theory <-reactive({
                input$genotype_mother %>%
                        str_sub(3, 4) %>%
                        str_split("") %>%
                        unlist
        })
        
        dimples_father_theory <-reactive({
                input$genotype_father %>%
                        str_sub(3, 4) %>%
                        str_split("") %>%
                        unlist
        })
        
        pkd_children_theory <- reactive({
                expand.grid(pkd_mother_theory(), pkd_father_theory(),
                            stringsAsFactors = FALSE) %>%
                        t %>%
                        as.data.frame(stringsAsFactors = FALSE) %>%
                        as.list %>%
                        lapply(paste, collapse = "") %>%
                        lapply(str_sort_)
        })
        
        dimples_children_theory <- reactive({
                expand.grid(dimples_mother_theory(), dimples_father_theory(),
                            stringsAsFactors = FALSE) %>%
                        t %>%
                        as.data.frame(stringsAsFactors = FALSE) %>%
                        as.list %>%
                        lapply(paste, collapse = "") %>%
                        lapply(str_sort_)
        })
        
        
        table2 <- reactive({
                expand.grid(pkd_children_theory(), dimples_children_theory()) %>%
                        mutate(genotype = paste0(Var1, Var2)) %>%
                        pull(genotype) %>%
                        table(genotype = .) %>%
                        as_tibble %>%
                        arrange(desc(genotype))
                        
        })
        
        
        output$results <- renderTable({
                full_join(table2(), table1(), by = "genotype") %>%
                        rename(Theoretical = n.x, Experimental = n.y) %>%
                        rename(Genotype = genotype) %>%
                        mutate(Experimental = replace_na(Experimental, 0)) %>%
                        mutate(Experimental = Experimental / input$num_trials) %>%
                        mutate(Theoretical = Theoretical / 16)
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
