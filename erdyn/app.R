#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinybrowser)
# library(shinyjs)
# library(shinyBS)
library(magick)
library(imager)
library(readxl)
# library(curl) # make the jsonlite suggested dependency explicit
library(qgraph)
library(patchwork)
library(plyr)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(
    title = "Personalised Happiness"
    , titleWidth = 235
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home"))#,
      # menuItem("Structural Graphical VAR", tabName = "svar-plot", icon = icon("arrow-alt-circle-right")),
      # menuItem("Personalised Happiness", tabName = "pers-happ", icon = icon("arrow-alt-circle-right")),
      # menuItem("Multilevel VAR", tabName = "mlvar-plot", icon = icon("arrow-alt-circle-right")),
      # # menuItem("Centrality Plots", tabName = "Centrality", icon = icon("arrow-alt-circle-right")),
      # menuItem("Zero-Order Correlations", tabName = "cors", icon = icon("arrow-alt-circle-right")),
      # menuItem("Sample Size Tables", tabName = "sampsize", icon = icon("arrow-alt-circle-right")),
      # menuItem("LASSO Graphical VAR", tabName = "lasso-plot", icon = icon("arrow-alt-circle-right")),
      # menuItem("Robustness Tests", tabName = "rob-plot", icon = icon("arrow-alt-circle-right")),
      # menuItem("Time Series Plots", tabName = "ts-plot", icon = icon("arrow-alt-circle-right"))
    )
  ),
  dashboardBody(
    tabItems(
      ### Home page -----------------------------------------------
      tabItem(
        "home",
        h2("Home Page and Study Information"),
        fluidRow(
          box(
            shinybrowser::detect(),
            h3("Background: Emotion as an Idiographic Dynamic System"),
            width = 12,
            htmlOutput("home_box1Text"),
            htmlOutput("home_box1")
          ),
          box(
            h3("Background: Emotion as a Goal-Directed Idiographic Dynamic System"),
            width = 12,
            htmlOutput("home_box2Text"),
            htmlOutput("home_box2")
          ),
          box(
            h3("Indicators Across Studies"),
            width = 12,
            textOutput("home_box4Text"),
            htmlOutput("home_box4")
          )
        )
      ),
      ### Personalised Happiness page -----------------------------------------------
      # tabItem(
      #   "pers-happ",
      #   h2("Personalised Happiness"),
      #   fluidRow(
      #     column(width = 12,
      #            box(
      #              h3("Main Findings"),
      #              width = 12,
      #              htmlOutput("pershapp_box1Text"),
      #              htmlOutput("pershapp_box1")
      #            ),
      #            box(
      #              h3("Proportion of Participants with Best Fitting Models"),
      #              width = 12,
      #              # htmlOutput("pershapp_box2Text"),
      #              htmlOutput("pershapp_tab")
      #            ),
      #            box(
      #              h3("Difference from Aggregate Network Structure"),
      #              width = 12,
      #              htmlOutput("pershapp_distText"),
      #              htmlOutput("pershapp_dist")
      #            )
      #     )
      #   )
      # ),
      ## sVAR Networks page -----------------------------------------------
      tabItem(
        "svar-plot",
        h2("Descriptives"),
        fluidRow(
          column(width = 9,
                 box(
                   h3("Between-Person and Person-Specific Descriptive Statistics"),
                   width = 12,
                   textOutput("desc_box1Text")
                 ),
                 box(
                   h3("Between-Person Descriptives"),
                   width = 12,
                   textOutput("desc_box2Text"),
                   plotOutput("desc_box2Plot")
                 ),
                 box(
                   h3("Person-Specific Correlations"),
                   width = 12,
                   htmlOutput("desc_box3Text"),
                   plotOutput("desc_box3Plot")
                 ),
                 # box(
                 #   h3("Model Tables"),
                 #   width = 12,
                 #   htmlOutput("sVARtab")
                 # )
                 # box(
                 #   h3("Population Networks"),
                 #   width = 12,
                 #   textOutput("sVARPlot_popText"),
                 #   plotOutput("sVARPlot_pop")
                 # )
          ),
          column(width = 3,
                 box(
                   title = p("Inputs", style = 'font-size:20px;color:white;'),
                   width = 12,
                   solidHeader = T,
                   background = "navy",
                   selectizeInput("study2",
                                  "Study #1:",
                                  choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
                   selectizeInput("SID2",
                                  "Participant ID:",
                                  choices = ""),
                   selectizeInput("hrmnz2"
                                  , "Harmonized Variables?"
                                  , choices = c("Harmonized", "Unharmonized")),
                   selectizeInput("dir2"
                                  , "Theoretical Model Constraint?"
                                  , choices = c("Top-Down", "Bottom-Up", "Bi-Directional", "Non-Directional"))#,
                   # selectizeInput("study2",
                   #                "Study #2:",
                   #                choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
                   # selectizeInput("SID2",
                   #                "Participant ID #2:",
                   #                choices = ""),
                   # selectizeInput("hrmnz2"
                   #                , "Harmonized Variables?"
                   #                , choices = c("Harmonized", "Unharmonized"))
                 )
          )
        )
      ),
      ### mlVAR Networks page -----------------------------------------------
      # tabItem(
      #   "mlvar-plot",
      #   h2("Multilevel VAR (Population)"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Capturing Population Happiness with Multilevel Vector Autoregressive Models"),
      #              width = 12,
      #              textOutput("mlVAR_box1Text")
      #            ),
      #            box(
      #              h3("Population Network Plots"),
      #              width = 12,
      #              textOutput("mlVARText"),
      #              plotOutput("mlVARPlot")
      #            ),
      #            box(
      #              h3("Centrality Plots"),
      #              width = 12,
      #              htmlOutput("mlvar_centralityText"),
      #              plotOutput("mlvar_centrality")
      #            ),
      #            box(
      #              h3("Model Tables"),
      #              width = 12,
      #              htmlOutput("mlVARtab")
      #            )
      #            # box(
      #            #   h3("Population Networks"),
      #            #   width = 12,
      #            #   textOutput("sVARPlot_popText"),
      #            #   plotOutput("sVARPlot_pop")
      #            # )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("study4",
      #                             "Study:",
      #                             choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #              selectizeInput("hrmnz4"
      #                             , "Harmonized Variables?"
      #                             , choices = c("Harmonized", "Unharmonized"))
      #            )
      #     )
      #   )
      # ),
      ### Zero Order Inputs --------------------------------------------
      # tabItem(
      #   "cors",
      #   h2("Zero-Order Correlation Tables"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Zero-Order Correlations"),
      #              width = 12,
      #              textOutput("cors_tabText"),
      #              htmlOutput("cors_tab")
      #            )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("study6",
      #                             "Study:",
      #                             choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #            )
      #     )
      #   )
      # ),
      ### Sample Size Inputs --------------------------------------------
      # tabItem(
      #   "sampsize",
      #   h2("Sample Size Tables"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Sample Sizes"),
      #              width = 12,
      #              textOutput("sampsize_tabText"),
      #              htmlOutput("sampsize_tab")
      #            )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("study5",
      #                             "Study:",
      #                             choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #            )
      #     )
      #   )
      # ),
      ### Network Inputs --------------------------------------------
      # tabItem(
      #   "lasso-plot",
      #   h2("Network Plots"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Idiographic Network Plots"),
      #              width = 12,
      #              textOutput("gVARText"),
      #              plotOutput("gVARPlot")
      #            ),
      #            box(
      #              h3("Centrality Plots"),
      #              width = 12,
      #              htmlOutput("centralityText"),
      #              plotOutput("centrality")
      #            ),
      #            box(
      #              h3("Population Networks"),
      #              width = 12,
      #              textOutput("gVARText_pop"),
      #              plotOutput("gVARPlot_pop")
      #            )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("study1",
      #                             "Study #1:",
      #                             choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #              selectizeInput("SID1",
      #                             "Participant ID:",
      #                             choices = ""),
      #              selectizeInput("hrmnz1"
      #                             , "Harmonized Variables?"
      #                             , choices = c("Harmonized", "Unharmonized"))#,
      #              # selectizeInput("study2",
      #              #                "Study #2:",
      #              #                choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #              # selectizeInput("SID2",
      #              #                "Participant ID #2:",
      #              #                choices = ""),
      #              # selectizeInput("hrmnz2"
      #              #                , "Harmonized Variables?"
      #              #                , choices = c("Harmonized", "Unharmonized"))
      #            )
      #     )
      #   )
      # ),
      ### Time Series Plots --------------------------------------------
      # tabItem(
      #   "ts-plot",
      #   h2("Time Series Plots"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Observed Time Series"),
      #              width = 12,
      #              textOutput("tsPlotText"),
      #              htmlOutput("tsPlot")
      #            )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("study3",
      #                             "Study:",
      #                             choices = c("GSOEP", "BHPS", "SHP", "HILDA", "LISS")),
      #              selectizeInput("SID3",
      #                             "Participant ID:",
      #                             choices = ""),
      #              selectizeInput("hrmnz3"
      #                             , "Harmonized Variables?"
      #                             , choices = c("Harmonized", "Unharmonized"))
      #            )
      #     )
      #   )
      # ),
      ### Robustness Tests --------------------------------------------
      # tabItem(
      #   "rob-plot",
      #   h2("Robustness Tests"),
      #   fluidRow(
      #     column(width = 9,
      #            box(
      #              h3("Summary"),
      #              width = 12,
      #              htmlOutput("rob_summaryText"),
      #              # htmlOutput("tsPlot")
      #            ),
      #            box(
      #              h3("Details of Selected Robustness Test"),
      #              width = 12,
      #              htmlOutput("rob_descText"),
      #              # htmlOutput("tsPlot")
      #            ),
      #            box(
      #              h3("Proportion of Participants in Each Category"),
      #              width = 12,
      #              htmlOutput("rob_tab")
      #            ),
      #            box(
      #              h3("Density Plot"),
      #              width = 12, 
      #              htmlOutput("rob_fig")
      #            )
      #     ),
      #     column(width = 3,
      #            box(
      #              title = p("Inputs", style = 'font-size:20px;color:white;'),
      #              width = 12,
      #              solidHeader = T,
      #              background = "navy",
      #              selectizeInput("robtest",
      #                             "Robustness Test:",
      #                             choices = c("sVAR Cutoffs", "Regularized graphicalVAR", "mlVAR Random Effects")),
      #              conditionalPanel(
      #                condition = "input.robtest == 'sVAR Cutoffs'",
      #                selectInput(
      #                  inputId = "robdiffcut",
      #                  label = "Choose Minimum Fit Difference",
      #                  selected = "100",
      #                  choices = c("0", "100", "1000", "1000")
      #                )
      #              ),
      #              conditionalPanel(
      #                condition = "input.robtest == 'mlVAR Random Effects'",
      #                selectInput(
      #                  inputId = "robsigcut",
      #                  label = "Choose Significance Threshold",
      #                  selected = "0",
      #                  choices = c("0%", "65%", "80%", "95%")
      #                )
      #              )
      #            )
      #     )
      #   )
      # )
    )
  )
)


# Define server logic required to draw a histogram

load_url <- function (url, ..., sha1 = NULL) {
    # based very closely on code for devtools::source_url
    stopifnot(is.character(url), length(url) == 1)
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    request <- httr::GET(url)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
    if (is.null(sha1)) {
        message("SHA-1 hash of file is ", file_sha1)
    }
    else {
        if (nchar(sha1) < 6) {
            stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
        }
        file_sha1 <- substr(file_sha1, 1, nchar(sha1))
        if (!identical(file_sha1, sha1)) {
            stop("SHA-1 hash of downloaded file (", file_sha1, 
                 ")\n  does not match expected value (", sha1, 
                 ")", call. = FALSE)
        }
    }
    load(temp_file, envir = .GlobalEnv)
}
library(RColorBrewer)
edge_colors <- RColorBrewer::brewer.pal(8, "Purples")[c(3,4,6)]

loadRData <- function(SID, type, study, h){
    #loads an RData file, and returns it
    path <- sprintf("%s/04_results/02_graphicalVAR/small/%s/%s/%s.RData?raw=true", wd, h, study, SID)
    load_url(path)
    get(ls()[ls() == type])
}

# load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/idiographic_plots.RData")
#load("~/Box Sync/network/PAIRS/PAIRS_graphicalVAR/centralityPlots.RData")
# load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/centralityPlots.RData")
# load_url("https://github.com/emoriebeck/PAIRS_graphicalVAR/raw/master/app_data.RData")
wd <- "https://github.com/emoriebeck/erdyn/blob/main"

# load_url("https://github.com/emoriebeck/personalised-happiness/blob/main/04_results/sids.RData?raw=true")
# load_url("https://github.com/emoriebeck/personalised-happiness/blob/main/04_results/svar_subs.RData?raw=true")
# load_url("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/01_mlVAR/pop_results_small.RData")

server <- function(input, output, session) {
    # print(sids)
    print(svar_subs)
    print(input)
    
    ### Pop-Up Box ----------------------------------------------
    showModal(
      modalDialog(
        title = "Welcome to the Interactive Results for this project: Emotion regulation as an idiographic dynamic system",
        easyClose = TRUE,
        footer = modalButton("Get Started"),
        tags$p(
          "In this project, we examined how emotion can be understood as (1) an idiographic dynamic system and (2) a ",
          "goal-directed idiographic dynamic system characterized by equilibria. To do so, we use data from two ",
          "intensive longitudinal studies that assessed both emotion and emotion regulation goals (i.e. whether ",
          "people wanted to increase, maintain, or decrease their emotions. We modeled these data as stochastic ",
          "differential equation models in which changes in emotion were predicted from current emotion levels (RQ1) ",
          "and main effect of emotion regulation goals as well as interaction between the two (RQ2). We then validated ",
          "individual differences in these models as predictors of baseline assessments of well-being. "
        ),
        tags$p(
          "As you'll see, we found that people differ in their emotion systems, the degree to which these systems ",
          "are goal-directed, and their emotion regulation efficacy. We also see that there are some associations ",
          "between components of these systems and baseline well-being."
        ),
        tags$p(
          "We suggest checking out the full set of materials on the OSF and GitHub:"
        ),
        tags$ul(
          tags$li(
            "Preprint: ", HTML('<a href="" target="_blank">PsyArxiv</a>')
          ),
          tags$li(
            "OSF Materials: ", HTML('<a href="" target="_blank"></a>')
          ),
          tags$li(
            "GitHub Materials: ", HTML('<a href="https://github.com/emoriebeck/erdyn/tree/main" target="_blank">https://github.com/emoriebeck/personalised-happiness/tree/main</a>')
          )#,
          # tags$li(
          #   "Static Web Page: ", HTML('<a href="https://emoriebeck.github.com/personalised-happiness" target="_blank">https://emoriebeck.github.com/personalised-happiness</a>')
          # )
        ),
        tags$p("Thanks for visiting!"),
        tags$p(HTML("&mdash; Tabea Springstein, Rohit Batra, and Emorie D. Beck"))
      )
    )
    
    ### Reactive Outputs ----------------------------------------
    # observe({
    #     subs1 <- (sids %>% filter(hrmnz == input$hrmnz1 & study == input$study1))$SID
    #     set.seed(4)
    #     subs1 <- sample(subs1, 100)
    #     updateSelectizeInput(session, 'SID1', choices = c("", subs1))
    # })
    # 
    # observe({
    #   dir2 = mapvalues(input$dir2, c("Top-Down", "Bottom-Up", "Bi-Directional", "Non-Directional"), c("topdown", "bottomup", "bidirectional", "nondirectional"), warn_missing = F)
    #   svar_subs2 <- (svar_subs %>% filter(hrmnzn == input$hrmnz2 & Study == input$study2 & dir == dir2))$SID
    #   set.seed(4)
    #   subs2 <- sample(svar_subs2, 100)
    #   updateSelectizeInput(session, 'SID2', choices = c("", subs2))
    # })
    # 
    # svar_subs_all <- svar_subs %>% select(-dir) %>% distinct()
    # 
    # observe({
    #   subs3 <- (svar_subs_all %>% filter(hrmnzn == input$hrmnz1 & Study == input$study1))$SID
    #   set.seed(4)
    #   subs3 <- sample(subs3, 100)
    #   updateSelectizeInput(session, 'SID3', choices = c("", subs3))
    # })
    
    # observe({
    #     subs3 <- (sids %>% filter(hrmnz == input$hrmnz3 & study == input$study3))$SID
    #     set.seed(4)
    #     subs3 <- sample(subs3, 100)
    #     updateSelectizeInput(session, 'SID3', choices = c("", subs3))
    # })
    # 
    # observe({
    #     subs4 <- (sids %>% filter(hrmnz == input$hrmnz4 & study == input$study4))$SID
    #     set.seed(4)
    #     subs4 <- sample(subs4, 100)
    #     updateSelectizeInput(session, 'SID4', choices = c("", subs4))
    # })
    
    ### Home  Outputs --------------------------------------------
    output$home_box1 <- renderText({
      # pulling plot object
      file <- "https://github.com/emoriebeck/erdyn/raw/main/05-results/plots/fig-1-cao.png"
      print(file)
      dims <- image_info(image_read(file))
      screen_wdth <- shinybrowser::get_width()
      img_wdth <- screen_wdth*.5
      img_ht <- (img_wdth*dims$height)/dims$width
      return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
      # "
    })
    
    output$home_box1Text <- renderText({
      return("<strong>Figure 1</strong>. <em>Emotion State Change predicted by Emotion State Level. 
             Attractor Location is defined as Emotion State Level where Emotion State Change is 
             expected to be 0. ")
    })
    
    output$home_box2 <- renderText({
      # pulling plot object
      file <- "https://github.com/emoriebeck/erdyn/raw/main/05-results/plots/fig-2-int.png"
      print(file)
      dims <- image_info(image_read(file))
      screen_wdth <- shinybrowser::get_width()
      img_wdth <- screen_wdth*.3
      img_ht <- (img_wdth*dims$height)/dims$width
      return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
      # "
    })
    
    output$home_box2Text <- renderText({
      return("<strong>Figure 2. </strong><em>Example Plots of Emotion State Change predicted by 
             Emotion State Level for goal to increase or goal to decrease. Panels A and C show 
             significant interactions, though only Panel C indicates successful regulation in 
             line with goals.")
    })
    
    output$home_box4 <- renderText({
      file <- "https://github.com/emoriebeck/erdyn/raw/main/05-results/tables/tab-1-hypotheses.html"
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$home_box4Text <- renderText({
      return("A comprehensive overview of our hypotheses for this research question is presented in Table 1.")
    })
    
    output$bg_box1Text <- renderText({
      return("In this study, we tested ... More details on the modelling 
             procedure can be found in the Method and online materials. ")
    })
    
    output$sVARText <- renderText({
      return("Network figures of lagged associations between life and domain satisfaction. 
             Arrows indicate lagged associations among indicators of domain and life 
             satisfaction, such that any arrow indicates that satisfaction at time t 
             (arrow origin) is associated with satisfaction at time t+1 (arrow head). 
             The darker colour node (lfs) indicates life satisfaction, while white 
             nodes indicate domain satisfaction. “lfs” = life satisfaction; “hos” 
             = satisfaction with home; “amt” = satisfaction with amount of leisure 
             time; “wrk” = satisfaction with work; “hlt” = satisfaction with health; 
             “inc” = satisfaction with income.")
    })
    
    ### Network Plot Outputs ----------------------------------------
    output$sVARPlot <- renderPlot({
        validate(need(input$SID2, 'Please select a Participant ID'))
            plot1 <- plot_fun(input$SID2, input$study2, input$hrmnz2, input$dir2)
            plot(plot1)
    })
    
    output$sVARPlotText <- renderText({
      return("After you choose two participants, you'll see two network figures pop
             up. The darker color node indicates life satisfaction, while nodes 
             indicate domain satisfaction. Top-down patterns (top left) are 
             indexed by low in-strength and high out-strength; bi-directional 
             patterns (top right) are indexed by high in- and out-strength; 
             non-directional patterns (bottom left) are indexed by low in- 
             and out-strength; and bottom-up patterns (bottom right) are indexed 
             by high in-strength and low out-strength. “lfs” = life satisfaction; 
             “hos” = satisfaction with home; “amt” = satisfaction with amount of 
             leisure time; “wrk” = satisfaction with work; “hlt” = satisfaction 
             with health; “inc” = satisfaction with income.<br><br>
             Do you see more top-down or bottom-up patterns in the networks? ")
    })
    
    output$sVARtab <- renderText({
      validate(need(input$SID2, 'Please select a Participant ID'))
      dir2 <- mapvalues(input$dir2, c("Top-Down", "Bottom-Up", "Bi-Directional", "Non-Directional"), c("topdown", "bottomup", "bidirectional", "nondirectional"), warn_missing = F)
      file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/03_tables/model-tabs/%s/%s/%s-%s.html", input$hrmnz2, input$study2, input$SID2, dir2)
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$sVARPlot_pop <- renderPlot({
      if(!("" %in% input)){
          plot1 <- (net_nested %>% filter(Study == input$study2))$plot[[1]]$p
          ttl1 <- (net_nested %>% filter(Study == input$study2))$plot[[1]]$ttl
          
          par(mfrow = c(1,1))
          plot(plot1)
          title(ttl1)
      }
    })
    
    
    output$sVARPlot_popText <- renderText({
      return("Aggregate-level models in the indicated sample(s) using multilevel vector 
             autoregressive models. Arrows indicate fixed effects regression 
             coefficients from the multilevel models. Thicker, darker lines 
             indicate coefficients with higher magnitudes. Dashed lines indicate 
             negative coefficients and solid lines indicate positive coefficients. 
             Arrows indicate lagged associations across waves. lifesat = Life 
             Satisfaction; house = Satisfaction with Housing; work = Satisfaction 
             with Job/Work; amtleis = Satisfaction with Amount of Leisure Time; 
             income = Satisfaction with Income; health = Satisfaction with Health.<br><br>
             Does the population network look similar to the individual one(s)?")
    })
    
    output$svar_centrality <- renderPlot({
        validate(need(input$SID2, 'Please select Participant ID'))
        plot1 <- cent_plot_fun(input$SID2, input$study2, input$hrmnz2, input$dir2)
        plot1
    })
    
    output$svar_centralityText <- renderText({
      return("Centrality captures the extent to which a focal node is central or
             important (broadly construed) in a network. For lagged models, like
             those used in the present study, two useful indicators of strength 
             centrality can help us understand whether some nodes tend to have 
             stronger or weaker connections with other nodes in the network. 
             Strength centrality is calculated as the sum the absolute values 
             of connections originating or terminating in each node. In lagged 
             networks, this is further broken down into the strength of associations of 
             nodes that originate (out-strength) or terminate (in-strength) 
             on each focal node.<br><br>
             
             To understand individual difference in the degree to which individuals 
             showed top-down (life satisfaction predicting domain satisfaction over 
             time), bottom-up (domain satisfaction predicting life satisfaction over 
             time), or bi-directional (domain and life satisfaction predicting each 
             other over time), we conducted a number of steps. First, we separately 
             z-transformed both in- and out-strength for each person separately. A 
             high z-score for the life satisfaction node for in-strength relative 
             to domain satisfaction nodes indicates that it was more strongly 
             predicted by the domains than it or the other domains predicted 
             each domain. A low z-score for the life satisfaction node for 
             in-strength, in contrast, would indicate that the life satisfaction 
             node was less strongly predicted by the domains than it or the other 
             domains predicted each domain. Next, extracted the z-transformed in- 
             and out-strength metrics for the life satisfaction node in each model 
             for each person and combined them within each sample.")
    })
    

# Personalised Happiness --------------------------------------------------


    
    output$pershapp_box1 <- renderText({
      # pulling plot object
      file <- "https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/04_figures/fig-4-density.png"
      print(file)
      dims <- image_info(image_read(file))
      screen_wdth <- shinybrowser::get_width()
      img_wdth <- screen_wdth*.5
      img_ht <- (img_wdth*dims$height)/dims$width
      return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
      # "
    })
    
    output$pershapp_box1Text <- renderText({
      return("<strong>Figure 4. </strong><em>Scatterplot of participants (N = 
      40,074) with different patterns of personalised happiness:</em><br>
      Scatterplot of top-down (y-axis, left; red), bottom-up (x-axis, bottom; 
      blue), bi-directional (top right; purple) and non-directional (origin; 
      bottom left; grey) patterns in personalised models across each of the 
      five studies. The x-axis represents within-person in-strength (domain 
      satisfaction  life satisfaction), and the y-axis represents within-person 
      out-strength (life satisfaction  domain satisfaction). The solid diagonal 
      indicates perfect bi-directional patterns (i.e. the degree of top-down 
      patterns perfectly mirrors the degree of bottom-up patterns). The dashed 
      diagonal lines indicate participants with bi-directional associations that 
      tended to be more top-down (top line) or bottom-up (bottom-line). Colours 
      indicate the degree to which participants showed top-down (red), bottom-up 
      (blue), bidirectional (purple gradient), and nondirectional (grey) associations.")
    })
    
    output$pershapp_tab <- renderText({
      file <- "https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/03_tables/tab-1-prop-pattern.html"
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$pershapp_dist <- renderText({
      # pulling plot object
      file <- "https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/04_figures/fig-6-congruence_cor.png"
      print(file)
      dims <- image_info(image_read(file))
      screen_wdth <- shinybrowser::get_width()
      img_wdth <- screen_wdth*.5
      img_ht <- (img_wdth*dims$height)/dims$width
      return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
      # "
    })
    
    output$pershapp_distText <- renderText({
      return("<strong>Figure 6.</strong><em> Participants (N = 40,074) show little 
      congruence with population models.</em><br>Density distributions of Euclidean 
      distance, a multivariate distance measure of the shortest path between each 
      structure, between each participant’s personalised models with the aggregate-
      level model in that sample. Lower values suggest an individual’s model is 
      highly similar to the aggregate-level model (0 would be an exact match). 
      Higher values suggest that the personalised model were highly dissimilar 
      to the population model.")
    })
    
    ### mlVAR Plot Outputs ----------------------------------------
    output$mlVAR_box1Text <- renderText({
      return("For population models, we estimate parameters using a series of 
             univariate multilevel vector autoregressive models (mlVAR) using 
             the mlVAR package in R73 with correlated temporal random effects.
             Data are within-person standardized before being entered into the
             model both to facilitate interpretation as well as to disentangle 
             between-person variance from the estimation of within-person processes. 
             In mlVAR models, each item included acts a predictor in each model 
             and each sequential model uses a different item as the outcome 
             variable. In other words, each variable is first separately 
             regressed on lag 1 indicators of all variables. Then, like the 
             graphical VAR models, contemporaneous associations are estimated 
             using the inverted concentration matrix of the residuals. Between-
             person differences in levels of domain and life satisfaction are 
             additionally captured as (grand-mean centred) person means at Level 
             2. The population-level effects represent aggregated estimates across 
             the whole population and the random effects represent individual 
             participants’ deviations from them. Because network analyses are 
             greatly aided by network sparsity, edges are thresholded using the 
             significance of the predictors in each of the mlVAR models. In the 
             present study, we focus on the within-person temporal (i.e. lag 1)
             effects.")
    })
    
    output$mlVARPlot <- renderPlot({
      plist <- (net_nested %>% 
        filter(Study == input$study4 & hrmnzn == input$hrmnz4))$plot[[1]]
      plot(plist$p)
      title(plist$ttl)
    })
    
    output$mlVARPlotText <- renderText({
      return("Aggregate-level models in the indicated sample(s) using multilevel vector 
             autoregressive models. Arrows indicate fixed effects regression 
             coefficients from the multilevel models. Thicker, darker lines 
             indicate coefficients with higher magnitudes. Dashed lines indicate 
             negative coefficients and solid lines indicate positive coefficients. 
             Arrows indicate lagged associations across waves. lifesat = Life 
             Satisfaction; house = Satisfaction with Housing; work = Satisfaction 
             with Job/Work; amtleis = Satisfaction with Amount of Leisure Time; 
             income = Satisfaction with Income; health = Satisfaction with Health.<br><br>
             Does the population network look similar to the individual one(s)?")
    })
    
    output$mlVARtab <- renderText({
      tab <- (net_nested %>% 
                  filter(Study == input$study4 & hrmnzn == input$hrmnz4))$lagged[[1]]
      rawHTML <- tab %>% 
        select(-lag) %>%
        knitr::kable(.
              , "html"
              , digits = 2
              , caption = sprintf("<strong>Table SX</strong><br><em>Fixed effect estimates of lagged effects for %s from multilevel vector autoregressive models</em>", input$study4)
              , escape = F
              ) %>%
        kableExtra::kable_classic(full_width = F, html_font = "Times")
      return(rawHTML)
    })
    
    output$mlvar_centrality <- renderPlot({
      # generate bins based on input$bins from ui.R
      cent <- (net_nested %>% filter(Study == input$study4 & hrmnzn == input$hrmnz4))$centrality[[1]]
      cent <- cent %>%
        rownames_to_column("var") %>%
        select(var, InStrength, OutStrength) %>%
        pivot_longer(
          names_to = "measure"
          , values_to = "value"
          , cols = c(InStrength, OutStrength)
        )
      vars <- unique(cent$var); vars <- c("lifesat", vars[vars!="lifesat"])
      p <- cent  %>%
        # group_by(measure) %>%
        # mutate(z = as.numeric(scale(value))) %>%
        # ungroup() %>%
        # mutate(z = ifelse(is.nan(z), 0, z)) %>%
        arrange(measure) %>%
        group_by(measure) %>%
        dplyr::mutate(m_value = mean(value)) %>%
        ungroup() %>%
        mutate(measure = factor(measure, levels = c("InStrength", "OutStrength")
                                , labels = c("In Strength (Bottom-Up)", "Out Strength (Top-Down)")),
               var = factor(var, levels = vars),
               key = ifelse(var == "lifesat", "target", "nt")) %>%
        ggplot(aes(x = var, y = value)) +
        geom_line(aes(group = measure), color = "black", linewidth = .3) + 
        geom_point(aes(shape = key, size = key)) + 
        labs(x = NULL, y = "z-score", shape = NULL,
             title = sprintf("%s: Centrality", input$study4)
             ) +
        # scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) + 
        geom_hline(aes(yintercept = m_value)) + 
        coord_flip() + 
        facet_grid(~measure) + 
        theme_classic()+ 
        theme(axis.text = element_text(face = "bold"),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              strip.background = element_rect(fill = "black"),
              strip.text = element_text(face = "bold", color = "white", size = 12),
              plot.title = element_text(face = "bold", size = 14, hjust = .5),
              legend.position = "none")
      return(p)
    })
    
    output$mlvar_centralityText <- renderText({
      return("Centrality captures the extent to which a focal node is central or
             important (broadly construed) in a network. For lagged models, like
             those used in the present study, two useful indicators of strength 
             centrality can help us understand whether some nodes tend to have 
             stronger or weaker connections with other nodes in the network. 
             Strength centrality is calculated as the sum the absolute values 
             of connections originating or terminating in each node. In lagged 
             networks, this is further broken down into the strength of associations of 
             nodes that originate (out-strength) or terminate (in-strength) 
             on each focal node.")
    })
    
    ### Sample Size Outputs ----------------------------------------
    output$sampsize_tab <- renderText({
      # generate bins based on input$bins from ui.R
      file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/03_tables/sample-sizes/%s.html"
                      , input$study5)
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$sampsize_tabText <- renderText({
      return(sprintf("The table below indicates the number of participants with 
                     valid data at each wave for %s. For some of the variables 
                     not used in the harmonized analyses we focused on, it's 
                     apparent that the domain indicator was added or dropped
                     during data collection.", input$study5))
    })
    
    ### Zero-Order Correlation Outputs ----------------------------------------
    output$cors_tab <- renderText({
      # generate bins based on input$bins from ui.R
      file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/03_tables/zero-order-cors/%s.html"
                      , input$study6)
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$cors_tabText <- renderText({
      return(sprintf("The table below shows the zero-order correlations, means, 
                     and standard deviations for focal and demographic variables 
                     in %s. Values reflect each participant's personal baseline."
                     , input$study5))
    })
    
    ### LASSO graphical VAR Plot Outputs ----------------------------------------
    output$gVARPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # print(as.character(input$wave))
      validate(
        # need(input$SID1, input$SID2, 'Please select 2 Participant IDs'),
        need(input$SID1, 'Please select a Participant ID'))
      
      plot1     <- lasso_plot_fun(     input$SID1, input$study1, input$hrmnz1)
      plot(plot1)
      # centplot1 <- cent_plot_fun(input$SID1, input$study1, input$hrmnz1)
      # # centplot1 + ~plot(plot1)
      # old_par <- par(mar = c(0, 0, 0, 0), mgp = c(1, 0.25, 0),
      #                bg = NA, cex.axis = 0.75, las = 1, tcl = -0.25)
      # wrap_elements(panel = ~plot(plot1), clip = FALSE) +
      #   centplot1 +
      #   ggtitle('Plot 2') +
      #   theme(plot.margin = margin(5.5, 5.5, 5.5, 35))
      # par(old_par)
      # 
      # old_par <- par(mar = c(0, 0, 0, 0), mgp = c(1, 0.25, 0), 
      #                bg = NA, cex.axis = 0.75, las = 1, tcl = -0.25)
      # 
      # plot2 <- plot_fun(input$SID2, input$study2, input$hrmnz2)
      # draw the histogram with the specified number of bins
      # if(!("" %in% input)){
      #     par(mfrow = c(1,1))
      #     plot(plot1)
      #     # plot(plot2)
      # }
    })
    
    output$gVARPlotText <- renderText({
      return("After you choose two participants, you'll see two network figures pop
             up. The darker color node indicates life satisfaction, while nodes 
             indicate domain satisfaction. Top-down patterns (top left) are 
             indexed by low in-strength and high out-strength; bi-directional 
             patterns (top right) are indexed by high in- and out-strength; 
             non-directional patterns (bottom left) are indexed by low in- 
             and out-strength; and bottom-up patterns (bottom right) are indexed 
             by high in-strength and low out-strength. “lfs” = life satisfaction; 
             “hos” = satisfaction with home; “amt” = satisfaction with amount of 
             leisure time; “wrk” = satisfaction with work; “hlt” = satisfaction 
             with health; “inc” = satisfaction with income.<br><br>
             Do you see more top-down or bottom-up patterns in the networks? ")
    })
    output$gVARPlot_pop <- renderPlot({
      # generate bins based on input$bins from ui.R
      # print(as.character(input$wave))
      
      # draw the histogram with the specified number of bins
      if(!("" %in% input)){
        # if(input$study1 != input$study2){
        plot1 <- (net_nested %>% filter(Study == input$study1))$plot[[1]]$p
        ttl1 <- (net_nested %>% filter(Study == input$study1))$plot[[1]]$ttl
        # plot2 <- (net_nested %>% filter(Study == input$study2))$plot[[1]]$p
        # ttl2 <- (net_nested %>% filter(Study == input$study2))$plot[[1]]$ttl
        
        par(mfrow = c(1,1))
        plot(plot1)
        title(ttl1)
        # plot(plot2)
        # title(ttl2)
        # } else {
        #   plot1 <- (net_nested %>% filter(Study == input$study1))$plot[[1]]$p
        #   ttl1 <- (net_nested %>% filter(Study == input$study1))$plot[[1]]$ttl
        #   par(mfrow = c(1,1))
        #   plot(plot1)
        #   title(ttl1)
        # }
      }
    })
    
    
    output$gVARPlot_popText <- renderText({
      return("Aggregate-level models in the indicated sample(s) using multilevel vector 
             autoregressive models. Arrows indicate fixed effects regression 
             coefficients from the multilevel models. Thicker, darker lines 
             indicate coefficients with higher magnitudes. Dashed lines indicate 
             negative coefficients and solid lines indicate positive coefficients. 
             Arrows indicate lagged associations across waves. lifesat = Life 
             Satisfaction; house = Satisfaction with Housing; work = Satisfaction 
             with Job/Work; amtleis = Satisfaction with Amount of Leisure Time; 
             income = Satisfaction with Income; health = Satisfaction with Health.<br><br>
             Does the population network look similar to the individual one(s)?")
    })
    
    ### LASSO Centrality Plot Outputs ----------------------------------------
    output$centrality <- renderPlot({
      # generate bins based on input$bins from ui.R
      validate(
        # need(input$SID3, input$SID4, 'Please select 2 Participant IDs'),
        need(input$SID1, 'Please select Participant ID'))
      # 
      # plot1 <- cent_plot_fun(input$SID3, input$study3, input$hrmnz3)
      plot1 <- lasso_cent_plot_fun(input$SID1, input$study1, input$hrmnz1)
      # plot2 <- cent_plot_fun(input$SID4, input$study4, input$hrmnz4)
      # plot1 + plot2
      plot1
    })
    
    output$centralityText <- renderText({
      return("Centrality captures the extent to which a focal node is central or
             important (broadly construed) in a network. For lagged models, like
             those used in the present study, two useful indicators of strength 
             centrality can help us understand whether some nodes tend to have 
             stronger or weaker connections with other nodes in the network. 
             Strength centrality is calculated as the sum the absolute values 
             of connections originating or terminating in each node. In lagged 
             networks, this is further broken down into the strength of associations of 
             nodes that originate (out-strength) or terminate (in-strength) 
             on each focal node.<br><br>
             
             To understand individual difference in the degree to which individuals 
             showed top-down (life satisfaction predicting domain satisfaction over 
             time), bottom-up (domain satisfaction predicting life satisfaction over 
             time), or bi-directional (domain and life satisfaction predicting each 
             other over time), we conducted a number of steps. First, we separately 
             z-transformed both in- and out-strength for each person separately. A 
             high z-score for the life satisfaction node for in-strength relative 
             to domain satisfaction nodes indicates that it was more strongly 
             predicted by the domains than it or the other domains predicted 
             each domain. A low z-score for the life satisfaction node for 
             in-strength, in contrast, would indicate that the life satisfaction 
             node was less strongly predicted by the domains than it or the other 
             domains predicted each domain. Next, extracted the z-transformed in- 
             and out-strength metrics for the life satisfaction node in each model 
             for each person and combined them within each sample.")
    })
    

    # Robustness Tests --------------------------------------------------------
    output$rob_summaryText <- renderText({
      return("We conducted three sets of robustness tests to estimate the sensitivity 
             of our modeling procedure to different analytic choices. First, we estimated 
             all models using regularized idiodgraphic graphicalVAR models. Rather than stepwise
             estimation, as is used in sVAR, regularization allows for simultaneous estimation 
             models using LASSO. Second, we tested whether the best fitting models in sVAR were 
             robust to different choices of minimal fit differences necessary to conclude a 
             model fit better. Specifically, we tested what proportion of participants had each 
             model as their best fitting models given fit differences of 0, 100 (reported in main 
             analyses), 1,000, or 10,000. Finally, we extracted the random effects from the mlVAR 
             models along with their conditional variances to test whether (1) patterns of random 
             effects mirrored idiographic effects and (2) how different significance thresholds on 
             the conditional variances impacted these results.<br><br>Then, we repeated all core analyses 
             reported on the Personalised Happiness tab. ")
    })
    
    output$rob_descText <- renderText({
      if(input$robtest == "sVAR Cutoffs"){
        res <- "Specifically, we tested graphical VAR models for each participant in which all 
        paths could be freely estimated (bi-directional), bottom-up paths were forced to be zero 
        (top-down), top-down effects were forced to be zero (bottom-up), and both bottom-up and 
        top-down effects were forced to zero (non-directional). We then extracted the Bayesian 
        information criterion (BIC) For each model, we then used a stepwise procedure proceeds 
        as follows: Starting with an empty model, the path that best improves model fit according 
        to a fit index, in our case BIC, is added to the model. This procedure is repeated until 
        there are no remaining pathways that improve model fit. We chose the best fitting model by 
        comparing the BIC of the four estimated models and choosing the one that minimized fit 
        by a cutoff. In the small number of ties, the model with the fewest parameters was retained, 
        for parsimony. <br><br>To ensure that strategy was robust to arbitrary choices about minimum 
        differences, we tested four different cutoff thresholds: 0, 100, 1,000, and 10,000. As can be 
        seen in the table and figure below, this resulted in almost no change in which model best fit 
        each participant's data with the exception of a modest proportion of participants who became 
        'unclassified' when requiring fit differences of at least 100."
      } else if (input$robtest == "Regularized graphicalVAR"){
        res <- "Initially, we tested all our research questions using LASSO graphicalVAR before switching 
        to sVAR following comments from a helpful reviewer. We estimated a Gaussian graphical model (GGM) 
        variation of the vector autoregressive model (VAR), which estimates a partial correlation network 
        in which correlations represent the partial correlation between variables after conditioning on 
        all other variables (a graphical VAR model).The graphical VAR procedure sequentially estimates 
        lagged and contemporaneous models, but these iteratively done on each participant separately. 
        Given the limited sample size for each person, we regularize the models using a variant of the 
        least absolute shrinkage and selection operator (LASSO), graphical LASSO (glasso). glasso 
        ensures the sparsity of the idiographic networks using a tuning parameter (λ). The optimal 
        value of this tuning parameter depends on many factors but can be iteratively chosen using 
        a model selection technique, such as minimizing the Bayesian information criterion or the 
        extended BIC (eBIC). The harshness of the estimation of eBIC is itself controlled by a 
        hyperparameter (γ) that can be set to different values. To be certain that idiographic networks 
        in different studies were comparable, we scaled the eBIC hyperparameter to 0 and allowed the 
        glasso parameters to .25 for discovery. The results of the models are a non-symmetric matrix 
        of sparse partial directed correlations, which can be visualized as networks and on which 
        network indices can be applied. The idiographic models were estimated using the R package
        graphicalVAR. <br><br>As can be seen in the figure and table below, this method tended to result 
        in relatively fewer participants being classified as bi-directional relative to the sVAR 
        procedure, which is likely due to the shrinkage imposed on the model parameters to induce sparsity. 
        However, as can be seen in the figure, this method replicated the overall pattern and main takeaway -- 
        there are individual differences in the degree of top-down and bottom-up patterns across people, 
        which indicates that no one theory of happiness is correct."
      } else {
        "We also performed a series of robustness tests using the random effects from the mlVAR models. 
        Unlike the idiographic models reported above, the mlVAR models rely on multilevel models, which 
        pool information across participants with the goal of reducing error by shrinking random effect 
        estimates that differ greatly from population-level estimates toward zero. Using the conditional 
        variances of the random effects, it is also possible to estimate person-level interval estimates 
        to assess the significance of the person-level estimates. When re-estimating (1) the proportion 
        of participants for whom each model fit best and (2) in- and out-strength centrality of the life 
        satisfaction node using increasingly strict cutoffs, we found discrepancies with the two 
        idiographic methods reported above. First, overall, there was a reduction in the magnitude of 
        estimates, as expected due to shrinkage in multilevel models due to partial pooling. Second, 
        while increasing the required magnitude of differences in fit in the idiographic model comparisons 
        resulted in no change in the proportions of participants in each category, increasing the stringency 
        of the significance threshold resulted in an increasing number of participants in the “non-directional” 
        category and decrease of the number of participants in other categories. For example, while in the 
        unthresholded results, 1.65-11.49% of participants were categorized as non-directional, 94.79-96.47% 
        were categorized as non-directional with p < .05. However, as can be see in Table and Figures below, 
        while the precise estimates in each category varies, the overall pattern of individual differences 
        persists across all methods and cutoffs. Together, all these robustness tests suggest that while 
        the pattern remained the same across significance thresholds, the precise estimates were quite sensitive 
        to the choice of significance thresholds."
      }
    })
    
    output$rob_tab <- renderText({
      robtest <- input$robtest # "sVAR Cutoffs", "Regularized graphicalVAR", "mlVAR Random Effects"
      robtest2 <- mapvalues(robtest, c("sVAR Cutoffs", "Regularized graphicalVAR", "mlVAR Random Effects"), 
                            c("tab-S3-gvar-cutoffs.html", "tab-S1-group-perc-levels.html", "tab-S2-mlVAR-raneff.html"),
                            warn_missing = F)
      file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/03_tables/%s", robtest2)
      print(file)
      rawHTML <- paste(readLines(file), collapse="\n")
      return(rawHTML)
    })
    
    output$rob_fig <- renderText({
      # pulling plot object
      if(input$robtest == "sVAR Cutoffs"){
        file <- "fig-4-density.png"
      } else if (input$robtest == "Regularized graphicalVAR"){
        file <- "fig-s6-gVAR-density.png"
      } else {
        sig <- mapvalues(input$robsigcut, c("0%", "65%", "80%", "95%"), paste0("sig", c("0", "65", "80", "95")), warn_missing = F)
        fignum <- mapvalues(input$robsigcut, c("0%", "65%", "80%", "95%"), c(5,4,3,2), warn_missing = F)
        file <- sprintf("fig-s%s-mlm-density-rob-%s.png", fignum, sig)
      }
      file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/04_figures/%s", file)
      print(file)
      dims <- image_info(image_read(file))
      screen_wdth <- shinybrowser::get_width()
      img_wdth <- screen_wdth*.5
      img_ht <- (img_wdth*dims$height)/dims$width
      return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
      # "
    })
    
    ### Time Series Plot Outputs ----------------------------------------
    output$tsPlot <- renderText({
      validate(
        need(input$SID3, 'Please select a Participant ID'))
        # pulling plot object
        file <- sprintf("https://github.com/emoriebeck/personalised-happiness/raw/main/04_results/04_figures/timeseries/%s-%s-%s.png", input$study3, input$hrmnz3, input$SID3)
        print(file)
        dims <- image_info(image_read(file))
        screen_wdth <- shinybrowser::get_width()
        img_wdth <- screen_wdth*.5
        img_ht <- (img_wdth*dims$height)/dims$width
        return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
        # "
      
    })
    
    output$tsPlotText <- renderText({
      return("After you choose a participant, you'll see the raw time series of their
              domain and life satisfaction. The x-axis indicates wave, the y-axis 
              indicates level (POMP-scored; 0-10).")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

