########################################
# Shiny App for NAF grading            #
# Jakob Hernandez and Vladimir Brodsky #
########################################

# Load Packages
library(shiny)
library(shinythemes)
library(data.table)
library(tidylog)
library(tidyverse)
library(shinyjs)
library(pracma)
library(quadprog)
library(htmltools)

################################################################################
# User interface

ui <- fluidPage(
  
  theme = shinytheme("slate"), useShinyjs(),
                navbarPage(
                  "NAFalyzer v1.0.0",
                  tabPanel("Load data", icon = icon("chart-line"),
                    sidebarPanel(
                      tags$h4("Load Data"),
                      
                      fileInput("test_data", label = "Upload NAF data in CSV format:",
                                accept = ".csv"),
                      selectInput("calculation_method", label = "Calculation method", 
                                  choices = list("Frontiers Algorithm" = "lisa_method", "Monte-Carlo simulation" = "jakob_method"),
                                  selected = "jakob_method"),
                      radioButtons("show_metabolites", label = "Show metabolites?",
                                   choices = list("No" = FALSE, "Yes" = TRUE)),
                      conditionalPanel(
                        condition = "output.plot && input.show_metabolites == 'TRUE'",
                        sliderInput("n_metabolite_showen", "Which metabolite should be overlayed?", min = 1, max = 3, value = 1, step = 1)
                      )
                      
                    ), # sidebarPanel
                    mainPanel(
                      textOutput("n_naf_text"), # Number of NAFs loaded
                      
                      br(),
                      
                      plotOutput(outputId = "plot"), # Plot NAF distribution
                      
                      fluidRow(
                        column(2,
                               conditionalPanel(
                                 condition = "output.plot && !output.on_final_naf",
                                 actionButton("next_naf", label = "Next"))
                        ),
                        column(2, 
                               conditionalPanel(
                                 condition = "output.plot && !output.on_final_naf && output.not_on_first_naf",
                                 actionButton("back_naf_1", label = "Back")
                               ) # conditionalPanel
                               ) # column
                        
                      ), # fluidRow
                      
                      fluidRow(
                        column(2,
                               conditionalPanel(
                                 condition = "output.plot && output.on_final_naf",
                                 actionButton("save_grade", label = "Submit")
                               ) # conditionalPanel
                               ),# column
                        column(2, 
                               conditionalPanel(
                                 condition = "output.plot && output.on_final_naf && 
                                 output.n_naf_text != '1 NAF is loaded'",
                                 actionButton("back_naf_2", label = "Back")
                               ) # conditionalPanel
                        ),
                        column(10, hidden(p(id = "data_submitted",
                                           "The data has been submitted. Resutls can now be viewed."))) # column
                      ), # fluidRow
                      
                    ) # mainPanel
                  ),# tabPanel
                  navbarMenu("Results", icon = icon("chart-simple"),
                             
                    tabPanel("Data Frame",
                      p(id = "need_data", "Please load and submitt your data"),
                    
                      conditionalPanel(
                        condition = "input.calculation_method == 'jakob_method'",
                        hidden(tableOutput(outputId = "rel_dis"))),
                        
                      conditionalPanel(
                        condition = "input.calculation_method == 'lisa_method'",
                        hidden(tableOutput(outputId = "Lisa"))),  
                      
                      conditionalPanel(
                        condition = "output.rel_dis",
                        downloadButton("downloadData", "Download")
                      ),
                      ###download panel for frontiers output
                      conditionalPanel(
                        condition = "output.Lisa",
                        downloadButton("downloadDataL", "Download")
                      ),
                                 
       
                      hr(),
                      h6(p('For more information see "Info > How it works"', style = "color:gray"))
                    ),# tabPanel
                    
                    tabPanel("Violin plots",
                      p(id = "need_data_2", "Please load and submitt your data"),

                      conditionalPanel(
                        condition = "input.calculation_method == 'jakob_method' &&
                        output.violin",
                        sliderInput("n_violin_plot", label = "Show violin plot of NAF:",
                                    min = 1, max = 1, value = 1, step = 1, ticks = TRUE)
                      ), # conditionalPanel
                      conditionalPanel(
                        condition = "input.calculation_method == 'jakob_method'",
                        hidden(plotOutput(outputId = "violin"))
                      ), # conditionalPanel
                      
                      hr(),
                      h6(p('For more information see "Info > Evaluating your data"', style = "color:gray"))
                    ) #tabPanel
                  
                  ), #navbarMenu
                  navbarMenu("Info", icon = icon("circle-info"),
                             tabPanel("How it works",
                               htmltools::includeMarkdown("description_md/how_it_works.md")
                             ), # tabPanel
                             tabPanel("Evaluating your data",
                               htmltools::includeMarkdown("description_md/evaluate_data.md"))
                  ) # navbarMenu
                )# navbarPage
                )# fluidPage

################################################################################
# Server

server <- function(input, output, session) {
  
  ##############################################################################
  # Load Data and Grade NAFs
  
  # Input Data
  datasetInput <- reactive({
    
    df <- input$test_data
    ext <- tools::file_ext(df$datapath)
    
    req(df)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read_delim(df$datapath, delim = ";") %>%
      mutate(Fraction = as.factor(Fraction),
             NAF = as.factor(NAF))
  
    
  }) # datasetInput
  
  n_naf <- reactive({
    
    if(ncol(datasetInput()) == 4){
      1
    } else {
      as.numeric(nlevels(as.factor(datasetInput()$NAF)))
    }
    
  }) # n_naf
  
  on_naf_n <- reactiveVal({
    1
  })  
    
  observeEvent(input$test_data, {
    on_naf_n(1)
    
    shinyjs::hide(id = "data_submitted")
    
  })
  
  observeEvent(input$test_data, {
    updateSliderInput(inputId = "n_violin_plot", max = n_naf())
  })
  
  observeEvent(input$test_data, {
    updateSliderInput(inputId = "n_metabolite_showen", max = sum(grepl("^M", names(datasetInput()))))
  })
  
  # Update which what the NAF number currenty loaded
  observeEvent(input$next_naf, {
    
    if(on_naf_n() < n_naf()){
      
      on_naf_n(on_naf_n() + 1)
      
    } # if
    
  })# observeEvent
  
  observeEvent(input$back_naf_1, {
    
    if(on_naf_n() > 1){
      
      on_naf_n(on_naf_n() - 1)
      
    } # if
    
  })# observeEvent
  
  observeEvent(input$back_naf_2, {
    
    if(on_naf_n() > 1){
      
      on_naf_n(on_naf_n() - 1)
      
    } # if
  
  })
    
    
  output$n_naf_text <- renderText({
      if(n_naf() == 1){
        paste(isolate(n_naf()), "NAF is loaded")
      } else {
        paste(isolate(n_naf()), "NAFs are loaded. On", on_naf_n(),"of", isolate(n_naf()))
      }
  })
  
  # Status/Output Text Box
  output$plot <- renderPlot({
    req(datasetInput())
    
    test_data_loaded <- datasetInput()
    
    if (n_naf() == 1) {
      
      test_data_loaded <- test_data_loaded %>%
      mutate(NAF = 1)
      
    }
    
    if (input$show_metabolites == FALSE){
      test_data_loaded <- test_data_loaded %>%
        select(grep("^[^M]", names(test_data_loaded)))
    }
    
    if (input$show_metabolites == TRUE){
      test_data_loaded <- test_data_loaded %>%
        select(grep(str_c("(^[^M]|M",input$n_metabolite_showen,")"), names(test_data_loaded)))
    }
    
    test_data_loaded %>%
      pivot_longer(names(test_data_loaded)[grepl("^[CM]", names(test_data_loaded))], names_to = "Comp", values_to = "prop") %>%
      filter(NAF == on_naf_n()) %>%
      ggplot() +
        geom_line(mapping = aes(x = Fraction, y = prop, color = Comp, group = Comp)) +
        theme_classic()
  }, width = 500)
  

  # Render output when on final NAF
  output$on_final_naf <- renderText({
    if(on_naf_n() == n_naf()){
      "On final NAF"
    }
  })
  
  outputOptions(output, "on_final_naf", suspendWhenHidden = FALSE)
  
  # Render if not on first NAF
  output$not_on_first_naf <- renderText({
    if(on_naf_n() > 1){
      "Not on first NAF"
    }
  })
  
  outputOptions(output, "not_on_first_naf", suspendWhenHidden = FALSE)
  
  # Submit resutls
  observeEvent(input$save_grade, {
    
    test_data_loaded <- datasetInput()
    
    if (n_naf() == 1) {
      
      test_data_loaded <- test_data_loaded %>%
        mutate(NAF = 1)
      
    }
    
    
    shinyjs::show(id = "data_submitted")
    
    shinyjs::hide(id = "need_data")
    
    shinyjs::hide(id = "need_data_2")
    
    shinyjs::show(id = "rel_dis")
    
    shinyjs::show(id = "violin")
    
    shinyjs::show(id = "Lisa")
    
  }) # observeEvent
  
  ##############################################################################
  ## calculate distributions ###################################################
  ##############################################################################
  # Preallocate memory
  
  number_compartments <- reactive({
    
    sum(grepl("^C", names(datasetInput())))
    
  }) # number_compartments
  
  number_metabolites <- reactive({
    
    sum(grepl("^M", names(datasetInput())))
    
  }) # number_compartments
  
  
  rel_distrubtion <- reactiveValues(df = data.frame("NAF" = 5, "Met" = 5),
                                    data_violin = data.frame("NAF" = c(), "Met" = c()))
  
  observeEvent(input$test_data, {
    
    for (i in c(1:number_compartments())) {
      df_2 = data.frame(mean = 2, lc = 2, uc = 2)
      df_3 = data.frame(val = c())
      
      names(df_2)[names(df_2) == "mean"] <- str_c("C", i, "_mean")
      names(df_2)[names(df_2) == "lc"] <- str_c("C", i, "_lc")
      names(df_2)[names(df_2) == "uc"] <- str_c("C", i, "_uc")
      
      names(df_3)[names(df_3) == "val"] <- str_c("V", i)
      
      rel_distrubtion$df = bind_cols(rel_distrubtion$df, df_2)
      rel_distrubtion$data_violin = bind_cols(rel_distrubtion$data_violin, df_3)
    }  
    
  })
                                    
  
  rel_distrubtion_Lisa <- reactiveValues(df = data.frame("NAF" = 5,
                                                         "Chl_Suc" = 2, "Cyt_Suc" = 2, "Vac_Suc" = 2,
                                                         "Chl_Glc" = 2, "Cyt_Glc" = 2, "Vac_Glc" = 2,
                                                         "Chl_Frc" = 2, "Cyt_Frc" = 2, "Vac_Frc" = 2))
  
################################################################################
  # Start analyzing NAFs -- Jakob's method
  
  calc_method <- reactive({
    ifelse(input$calculation_method == "jakob_method", 1, 2)
  })
  
  observeEvent(input$save_grade, {
    
    test_data_loaded <- datasetInput()
    
    number_of_fractions <- as.factor(test_data_loaded$Fraction) %>%
      levels() %>%
      length()
    
    if (n_naf() == 1) {
      
      test_data_loaded <- test_data_loaded %>%
        mutate(NAF = 1)
      
    } #if
    
   if (calc_method() == 1) {
    
    for (m in c(1:number_metabolites())) {
      
      for (n in c(1:n_naf())) {
        
        A <- as.matrix(test_data_loaded[((n-1)*number_of_fractions+1):(n*number_of_fractions),grep("^C", names(test_data_loaded))])
        
        B <- as.matrix(test_data_loaded[((n-1)*number_of_fractions+1):(n*number_of_fractions),grep("^M", names(test_data_loaded))[m]])
        
        x = lsqlincon(A,B,lb = rep(0, number_compartments()), ub = rep(1, number_compartments()))
        
        res_old <- proportions(x)
        
        for (j in c(1:1000)) {
          
          a_r <- as.vector(A) + rnorm(number_of_fractions * number_compartments(), mean = 0, sd = 0.05)
          
          a_r[a_r < 0] = 0
          
          A_r <- matrix(a_r, number_of_fractions, number_compartments(), byrow = FALSE)
          
          b_r <- as.vector(B) + rnorm(number_of_fractions, mean = 0, sd = 0.05)
          
          b_r[b_r < 0] = 0
          
          B_r <- matrix(b_r, number_of_fractions, 1)
          
          for (i in c(1:number_compartments())) {
            A_r[,i] = proportions(A_r[,i])
          }
          
          B_r[,1] = proportions(B_r[,1])
          
          x_r = lsqlincon(A_r,B_r,lb = rep(0, number_compartments()), ub = rep(1, number_compartments()))
          
          res_new <- proportions(x_r)
          
          res_old <- rbind(res_old, res_new)
          
        } # for
        
        res_old <- as.tibble(res_old)
        
        res_old_l <- res_old %>% pivot_longer(cols = names(res_old), names_to = "comp")
        
        res_fin <- res_old_l %>%
          group_by(comp) %>%
          summarise(mean = mean(value),
                    l_con = round(quantile(value, prob = c(.025)), digits = 3),
                    u_con = round(quantile(value, prob = c(.925)), digits = 3))
        
        res_fin <- res_fin %>% pivot_wider(names_from = "comp",values_from = c(mean, l_con, u_con))
        
        col_order <- c()
        
        for (i in c(1:number_compartments())) {
          col_order = append(col_order,names(res_fin)[grep(str_c("V",i),names(res_fin))])
        }
        
        res_fin <- res_fin %>% 
          select(col_order)
        
        rel_distrubtion$df[((n-1)*number_metabolites()+m),] = c(n,m,as.numeric(res_fin))
        
        res_old_c_m <- res_old %>%
          mutate(NAF = n, Met = m) %>%
          select(NAF, Met, names(res_old))
        
        rel_distrubtion$data_violin <- bind_rows(rel_distrubtion$data_violin,res_old_c_m)
        
      } # for
      
    }# for
     
   }
    
  })
    
  
  
################################################################################
## Vladi #######################################################################
  
  observeEvent(input$save_grade, { ###input my data 
    
    test_data_loaded <- datasetInput()
    
    if (n_naf() == 1) {
      
      test_data_loaded <- test_data_loaded %>%
        mutate(NAF = 1)
      
    } #if
    
    if (calc_method() == 2) {
      
      ####
      test_a <- as_tibble(test_data_loaded)%>%
        select(NAF, everything())%>%
        pivot_wider(names_from = "Fraction",
                    values_from = -c(NAF,Fraction))%>%
        select(-NAF)
      
      test_a_C <- as_tibble(test_data_loaded)%>%
        select(c(NAF, Fraction, starts_with("C")))%>%
        pivot_wider(names_from = "Fraction",
                    values_from = -c(NAF,Fraction),
                    names_glue = "{.value}{Fraction}") %>% 
        select(-NAF)
      
      test_a_M <- as_tibble(test_data_loaded)%>%
        select(c(NAF, Fraction, starts_with("M")))%>%
        pivot_wider(names_from = "Fraction",
                    values_from = -c(NAF,Fraction),
                    names_glue = "{.value}{Fraction}") %>% 
        select(-NAF)
      
      test_a
      test_a_C
      test_a_M
      ####
      {
        # initialize empty data frame to store results
        result_C <- list()
        
        # loop over columns
        for (i in 1:ncol(test_a_C)) {
          
          # extract column prefix
          col_name <- names(test_a_C)[i]
          prefix <- str_extract(col_name, "^C\\d+")
          
          if (!is.null(prefix)) {
            
            # get column names for prefix
            col_names <- grep(paste0("^", prefix, "\\w+$"), names(test_a_C), value = TRUE)
            
            # loop over suffixes
            for (j in 1:(length(col_names)-1)) {
              for (k in (j+1):length(col_names)) {
                
                # compute column difference (if not the same column)
                if (col_names[j] != col_names[k]) {
                  col_name_diff <- paste0(col_names[j], "-", col_names[k])
                  result_C[[col_name_diff]] <- test_a_C[[col_names[j]]] - test_a_C[[col_names[k]]]
                }
              }
            }
          }
        }
        
        # print result
        C_diff <- as_tibble(result_C)
        #####
        
        
        # initialize empty data frame to store results
        result_M <- list()
        
        # loop over columns
        for (i in 1:ncol(test_a_M)) {
          
          # extract column prefix
          col_name <- names(test_a_M)[i]
          prefix <- str_extract(col_name, "^M\\d+")
          
          if (!is.null(prefix)) {
            
            # get column names for prefix
            col_names <- grep(paste0("^", prefix, "\\w+$"), names(test_a_M), value = TRUE)
            
            # loop over suffixes
            for (j in 1:(length(col_names)-1)) {
              
              for (k in (j+1):length(col_names)) {
                
                # compute column difference (if not the same column)
                if (col_names[j] != col_names[k]) {
                  col_name_diff <- paste0(col_names[j], "-", col_names[k])
                  result_M[[col_name_diff]] <- test_a_M[[col_names[j]]] - test_a_M[[col_names[k]]]
                }
              }
            }
          }
        }
        
        # print result
        M_diff <- as_tibble(result_M)
        
        MC <- cbind(M_diff, C_diff)
        
        MCT <- as_tibble(MC)
        
        MC
        
      }
      ####
      # initialize empty list to store results
      result_MC <- list()
      
      # loop over Metabolite difference columns
      for (i in colnames(M_diff)) {
        
        # get prefixes for all Metabolite differences from Step 1 
        met_dif <- grep("^M\\d+F\\d+-M\\d+F\\d+$", names(M_diff[i]), value = T)
        
        # get fourth character of Metabolite (first fraction) 
        f1 <- substr(i, 4, 4)
        
        # get ninth character of Metabolite (second fraction) 
        f2 <- substr(i, 9, 9)
        
        # find matching Compartment differences for the same fraction combination 
        comp_dif_match <- grep(paste0("^C\\dF", f1, "-C\\dF",  f2), names(MC), value = T)
        
        
        # loop over each matching compartment difference for current metabolite  
        for (k in 1:(length(comp_dif_match))) {
          
          
          # create column names of current Metabolite difference and the substracted Compartment difference 
          MC_diff <- paste0(met_dif, "-", comp_dif_match[k])
          
          # get the absolute difference based on the matching names from MC dataset
          result_MC[[MC_diff]] <- abs(MC[[met_dif]] - MC[[comp_dif_match[k]]])
          
          # create final tibble with all the differences (step 2.1)
          list_of_metabolites_minus_compartments <- as_tibble(result_MC)
          
        }
      }
      
      # create empty list to store minimal values 
      minima_of_MC <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(M_diff)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # get first fraction
        f4 <- substr(i, 4, 4)
        
        #get second fraction
        f9 <- substr(i, 9, 9)
        
        
        # find matching Compartment differences for the same fraction combination for the current metabolite 
        comp_dif_match_forminima <- grep(paste0("^M" , f2 , "F" , f4, "-M\\dF",  f9 , "-C\\dF" , f4 , "-C\\dF" , f9) , 
                                         names(list_of_metabolites_minus_compartments), value = T)
        
        # create column names for new list: min-Metabolite
        minima <- paste0(colnames(M_diff[i]) , "-min")
        
        # select all matching Compartments that belong to currently picked metabolite 
        subset_of_compartments_minima <- list_of_metabolites_minus_compartments %>% 
          select(matches(comp_dif_match_forminima))
        
        # select, row-wise, the minimum out of each compartment
        minima_of_MC[[minima]] <- apply(subset_of_compartments_minima, 1, FUN = min)
        
        minima_of_MC_tibble <- as_tibble(minima_of_MC)
        
      }
      
      # create empty list to store minimal values substracted from compartments
      MC_minus_min <- list()
      
      # loop over column name of each minimum for eachgradient difference for each Metabolite
      for (i in colnames(minima_of_MC_tibble)) {
        
        # get prefix for current Metabolite minimum 
        minima_prefix <- grep("^M\\d+F\\d+-M\\d+F\\d+-min", names(minima_of_MC_tibble[i]), value = T)
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # get first fraction
        f4 <- substr(i, 4, 4)
        
        #get second fraction
        f9 <- substr(i, 9, 9)
        
        
        # find matching Compartment differences for the same fraction combination for the current metabolite 
        comp_dif_match_for_MC_minusminima <- grep(paste0("^M" , f2 , "F" , f4, "-M\\dF",  f9 , "-C\\dF" , f4 , "-C\\dF" , f9) , 
                                                  names(list_of_metabolites_minus_compartments), value = T)
        
        # loop over each fitting compartment 
        for (k in 1:(length(comp_dif_match_for_MC_minusminima))) {
          
          # get names for each compartment - minimum
          MC_minus_minima <- paste0(comp_dif_match_for_MC_minusminima[k] , "-" , minima_prefix)
          
          # substract each compartment from the minimum
          MC_minus_min[[MC_minus_minima]] <- list_of_metabolites_minus_compartments[[comp_dif_match_for_MC_minusminima[k]]] - minima_of_MC_tibble[[minima_prefix]]
          
          # create the tibble 
          MC_minus_min_tibble <- as_tibble(MC_minus_min)
          
        }
        
      }
      ####
      # create list that stores all compartments that get noted at 0.05
      bound_0.05 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(M_diff)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_bounds <- grep(paste0("^M" , f2 ) , 
                                          names(MC_minus_min_tibble), value = T)
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_bounds) {
          
          # get identifier of compartment
          f12 <- substr(j, 12, 12)
          
          # get all densities for the current compartment
          comp_dif_match_for_bounds2 <- grep(paste0("^M\\d+F\\d+-M\\d+F\\d+-C" , f12 ) , 
                                             comp_dif_match_for_bounds, value = T)
          
          # filter data table according to current compartment
          subset_of_compartments_bound <- MC_minus_min_tibble %>% 
            select(matches(comp_dif_match_for_bounds2))
          
          # calculate whether values are above or below significance threshold
          hits <- subset_of_compartments_bound < 0.05
          
          # create column labels for list 
          bound_label <- paste0("M" , f2 ,"C" , f12)
          
          # summarize all compartment hits, rowwise 
          bound_0.05[[bound_label]] <- apply(hits, 1, FUN = sum)
          
          bound_0.05_table <- as_tibble(bound_0.05)
          
        }
        
      }
      
      # create list that stores all compartments that get noted at 0.075
      bound_0.075 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(M_diff)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_bounds <- grep(paste0("^M" , f2 ) , 
                                          names(MC_minus_min_tibble), value = T)
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_bounds) {
          
          # get identifier of compartment
          f12 <- substr(j, 12, 12)
          
          # get all densities for the current compartment
          comp_dif_match_for_bounds2 <- grep(paste0("^M\\d+F\\d+-M\\d+F\\d+-C" , f12 ) , 
                                             comp_dif_match_for_bounds, value = T)
          
          # filter data table according to current compartment
          subset_of_compartments_bound <- MC_minus_min_tibble %>% 
            select(matches(comp_dif_match_for_bounds2))
          
          # calculate whether values are above or below significance threshold
          hits <- subset_of_compartments_bound < 0.075
          
          # create column labels for list 
          bound_label <- paste0("M" , f2 ,"C" , f12)
          
          # summarize all compartment hits, rowwise 
          bound_0.075[[bound_label]] <- apply(hits, 1, FUN = sum)
          
          bound_0.075_table <- as_tibble(bound_0.075)
          
        }
        
      }
      
      
      # create list that stores all compartments that get noted at 0.1
      bound_0.1 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(M_diff)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_bounds <- grep(paste0("^M" , f2 ) , 
                                          names(MC_minus_min_tibble), value = T)
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_bounds) {
          
          # get identifier of compartment
          f12 <- substr(j, 12, 12)
          
          # get all densities for the current compartment
          comp_dif_match_for_bounds2 <- grep(paste0("^M\\d+F\\d+-M\\d+F\\d+-C" , f12 ) , 
                                             comp_dif_match_for_bounds, value = T)
          
          # filter data table according to current compartment
          subset_of_compartments_bound <- MC_minus_min_tibble %>% 
            select(matches(comp_dif_match_for_bounds2))
          
          # calculate whether values are above or below significance threshold
          hits <- subset_of_compartments_bound < 0.1
          
          # create column labels for list 
          bound_label <- paste0("M" , f2 ,"C" , f12)
          
          # summarize all compartment hits, rowwise 
          bound_0.1[[bound_label]] <- apply(hits, 1, FUN = sum)
          
          bound_0.1_table <- as_tibble(bound_0.1)
          
        }
        
      }
      
      bound_0.05_table
      bound_0.075_table
      bound_0.1_table
      
      # create list that stores all compartments that get noted at 0.05
      summary_0.05 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(bound_0.05_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_summary <- grep(paste0("^M" , f2 ) , 
                                           names(bound_0.05_table), value = T)
        
        
        # filter data table according to current compartment
        subset_of_compartments_bound <- bound_0.05_table %>% 
          select(matches(comp_dif_match_for_summary))
        
        #create column labels for list 
        summary_label <- paste0("M" , f2 )
        
        # summarize all compartment hits, rowwise 
        summary_0.05[[summary_label]] <- apply(subset_of_compartments_bound, 1, FUN = sum)
        
        summary_0.05_table <- as_tibble(summary_0.05)
        
      }
      
      # create list that stores all compartments that get noted at 0.05
      summary_0.075 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(bound_0.075_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_summary <- grep(paste0("^M" , f2 ) , 
                                           names(bound_0.075_table), value = T)
        
        
        # filter data table according to current compartment
        subset_of_compartments_bound <- bound_0.075_table %>% 
          select(matches(comp_dif_match_for_summary))
        
        #create column labels for list 
        summary_label <- paste0("M" , f2 )
        
        # summarize all compartment hits, rowwise 
        summary_0.075[[summary_label]] <- apply(subset_of_compartments_bound, 1, FUN = sum)
        
        summary_0.075_table <- as_tibble(summary_0.075)
        
      }
      
      # create list that stores all compartments that get noted at 0.1
      summary_0.1 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(bound_0.1_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_summary <- grep(paste0("^M" , f2 ) , 
                                           names(bound_0.1_table), value = T)
        
        
        # filter data table according to current compartment
        subset_of_compartments_bound <- bound_0.1_table %>% 
          select(matches(comp_dif_match_for_summary))
        
        #create column labels for list 
        summary_label <- paste0("M" , f2 )
        
        # summarize all compartment hits, rowwise 
        summary_0.1[[summary_label]] <- apply(subset_of_compartments_bound, 1, FUN = sum)
        
        summary_0.1_table <- as_tibble(summary_0.1)
        
      }
      
      str(summary_0.05_table)
      str(summary_0.075_table)
      str(summary_0.1_table)
      ####
      # create list that stores all compartments that get noted at 0.05
      relative_0.05 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(summary_0.05_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_relative_C <- grep(paste0("^M" , f2 ) ,  names(bound_0.05_table), value = T)
        
        comp_dif_match_for_relative_M <- grep(paste0("^M" , f2 ) ,  names(summary_0.05_table), value = T)
        
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_relative_C) {
          
          f4 <- substr(j, 4, 4)
          
          comp_dif_match_for_relative_C_current <- grep(paste0("^M" , f2 , "C" , f4) ,  names(bound_0.05_table), value = T)
          
          relative_label <- paste0("C" , f4 , comp_dif_match_for_relative_M , "_0.05") 
          
          relative_0.05[[relative_label]] <- bound_0.05_table[[comp_dif_match_for_relative_C_current]] / summary_0.05_table[[comp_dif_match_for_relative_M]]
          
          relative_0.05_table <- as_tibble(relative_0.05)
          
          
        }
        
      }
      
      # create list that stores all compartments that get noted at 0.075
      relative_0.075 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(summary_0.075_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_relative_C <- grep(paste0("^M" , f2 ) ,  names(bound_0.075_table), value = T)
        
        comp_dif_match_for_relative_M <- grep(paste0("^M" , f2 ) ,  names(summary_0.075_table), value = T)
        
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_relative_C) {
          
          f4 <- substr(j, 4, 4)
          
          comp_dif_match_for_relative_C_current <- grep(paste0("^M" , f2 , "C" , f4) ,  names(bound_0.075_table), value = T)
          
          relative_label <- paste0("C" , f4 , comp_dif_match_for_relative_M , "_0.075") 
          
          relative_0.075[[relative_label]] <- bound_0.075_table[[comp_dif_match_for_relative_C_current]] / summary_0.075_table[[comp_dif_match_for_relative_M]]
          
          relative_0.075_table <- as_tibble(relative_0.075)
          
          
        }
        
      }
      
      # create list that stores all compartments that get noted at 0.05
      relative_0.1 <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(summary_0.1_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        # find all matching Compartment differencesfor the current metabolite 
        comp_dif_match_for_relative_C <- grep(paste0("^M" , f2 ) ,  names(bound_0.1_table), value = T)
        
        comp_dif_match_for_relative_M <- grep(paste0("^M" , f2 ) ,  names(summary_0.1_table), value = T)
        
        
        # loop through each compartment of every density 
        for (j in comp_dif_match_for_relative_C) {
          
          f4 <- substr(j, 4, 4)
          
          comp_dif_match_for_relative_C_current <- grep(paste0("^M" , f2 , "C" , f4) ,  names(bound_0.1_table), value = T)
          
          relative_label <- paste0("C" , f4 , comp_dif_match_for_relative_M , "_0.1") 
          
          relative_0.1[[relative_label]] <- bound_0.1_table[[comp_dif_match_for_relative_C_current]] / summary_0.1_table[[comp_dif_match_for_relative_M]]
          
          relative_0.1_table <- as_tibble(relative_0.1)
          
          relatives_for_all_errors <- cbind(relative_0.05_table , relative_0.075_table , relative_0.1_table)
          
          
        }
        
      }
      
      relative_0.05_table
      relative_0.075_table
      relative_0.1_table
      relatives_for_all_errors
      
      
      relative_dis_mean <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(bound_0.05_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        f4 <- substr(i, 4, 4) 
        
        # find all matching Compartment differencesfor the current metabolite 
        match_for_relative_C <- grep(paste0("^C" , f4 , "M" , f2 ) ,  names(relatives_for_all_errors), value = T)
        
        subset_of_compartments_error <- relatives_for_all_errors %>% 
          select(matches(match_for_relative_C))
        
        #create column labels for list 
        mean_label <- paste0("C" , f4 , "_M" , f2  )
        
        # summarize all compartment hits, rowwise 
        relative_dis_mean[[mean_label]] <- apply(subset_of_compartments_error, 1, FUN = mean)
        
        relative_dis_mean_table <- as_tibble(relative_dis_mean)
        
      }
      
      relative_dis_mean_table
      
      
      relative_dis_sd <- list()
      
      # loop over column name of each gradient difference for each Metabolite
      for (i in colnames(bound_0.05_table)) {
        
        # get identifier of Metabolite 
        f2 <- substr(i, 2, 2) 
        
        f4 <- substr(i, 4, 4) 
        
        # find all matching Compartment differencesfor the current metabolite 
        match_for_relative_C <- grep(paste0("^C" , f4 , "M" , f2 ) ,  names(relatives_for_all_errors), value = T)
        
        subset_of_compartments_error <- relatives_for_all_errors %>% 
          select(matches(match_for_relative_C))
        
        #create column labels for list 
        sd_label <- paste0("C" , f4 , "M" , f2  )
        
        # summarize all compartment hits, rowwise 
        relative_dis_sd[[sd_label]] <- apply(subset_of_compartments_error, 1, FUN = sd)
        
        relative_dis_sd_table <- as_tibble(relative_dis_sd)
        
        
        
      }
      
      relative_dis_mean_table
      
      relative_dis_sd_table
      ####
      number_of_compartments <- length(grep("M1$", names(relative_dis_mean_table)))
      
      zero_inds <- relative_dis_mean_table == 0
      
      one_inds <- relative_dis_mean_table == 1
      
      relative_dis_mean_table[zero_inds] <- 0.075
      
      relative_dis_mean_table[one_inds] <- 1 - (0.075*(number_of_compartments-1))
      
      visually_pleasing_relative_dis_mean_table <- relative_dis_mean_table %>% 
        mutate(NAF = row_number()) %>% 
        pivot_longer(!NAF ,
                     names_to = c("Compartment" , "Metabolite") ,
                     names_pattern = "^(C\\d)_(M\\d)" ,
                     values_to = "Distribution") %>% 
        pivot_wider(names_from = "Metabolite",
                    values_from = "Distribution")
      
      
      visually_pleasing_relative_dis_mean_table
    ####
      rel_distrubtion_Lisa$df = isolate(visually_pleasing_relative_dis_mean_table)
    } 
    
   })
  
#reactive
output$rel_dis <- renderTable({
  rel_distrubtion$df
}) 




output$violin <- renderPlot({
  rel_distrubtion$data_violin %>%
    pivot_longer(names(rel_distrubtion$data_violin)[grep("^V",names(rel_distrubtion$data_violin))], 
                 names_to = "Comp", values_to = "values") %>%
    filter(NAF == input$n_violin_plot) %>%
    ggplot(aes(x = Comp, y = values, fill = Comp)) +
    geom_violin(draw_quantiles = c(0.5)) +
    facet_grid(NAF ~ Met)+
    theme_classic()
  }, height = 240) 

###render frontiers table VB
output$Lisa <- renderTable({
  rel_distrubtion_Lisa$df
})
 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Relativ_distribution", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rel_distrubtion$df, file, row.names = FALSE)
    }
  )
###create download handler for frontiers method VB
  output$downloadDataL <- downloadHandler(
    filename = function() {
      paste("Relativ_distribution_frontiers", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rel_distrubtion_Lisa$df, file, row.names = FALSE)
    }
  )
 
} # server

################################################################################
# Create shiny app

shinyApp(ui = ui, server = server)




