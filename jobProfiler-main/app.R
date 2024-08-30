### JobProfiler
### Emma Mõttus, emma.mottus@ut.ee; René Mõttus, rene.mottus@ed.ac.uk
### Institute of Psychology, University of Tartu

library(shiny)
library(shinysurveys)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggiraph)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(shinyFeedback)
library(stringr)
library(markdown)
library(rlang)

### READING IN FILES / DATAFRAME -> MATRIX

# Language options: softcoded
langOptions <- read_tsv("appData/langOptions.tsv")

# Files in multiple languages: need translation!
questions <- read_tsv("www/questions.tsv")
countries <- read_tsv("www/countries.tsv")
appText <- read_tsv("www/appText.tsv")
jobOptions <- read_tsv("www/occupationList.tsv")

# Files that don't need translation
occupationsMeans <- read_tsv("appData/jobMeans.tsv")
weights <- read_tsv("appData/weights2D.tsv")
meanNorms <- read_tsv("appData/meanNorms.tsv")
sdNorms <- read_tsv("appData/sdNorms.tsv")
weightsBig5 <- read_tsv("appData/weightsB5.tsv")
itemCors <- read_tsv("appData/itemCors.tsv")

occResponses <- as.matrix(occupationsMeans)
weights <- as.matrix(weights)
weightsBig5 <- as.matrix(weightsBig5)
itemCors <- as.matrix(itemCors)
###

### UI: Pages (tabs), tabs (navtabs) on results page, elements within pages
### NOTE! Text (labels/choice vectors) declared as NULL in UI, redefined in server once the user has selected a language.

# Pages in order of appearance
language <- tabPanel("languageTab",
                     card(class="pages",
                       radioGroupButtons("lang", choiceNames = langOptions$language, choiceValues = langOptions$suffix, individual = TRUE, selected = character(0)),
                       actionButton("langOK", "OK", disabled = TRUE)
                     ))

disclaimer <- tabPanel("disclaimerTab",
                       card(class="pages",
                            uiOutput("disc1"),
                            actionButton("about", label = NULL),
                            actionButton("understood", label = NULL),
                            imageOutput("logo", height = "fit-content"),
                            ))

completionQ <- tabPanel("completionQTab",
                        card(class="pages",
                          radioGroupButtons("completion", label = NULL, choices = c("")),
                          radioGroupButtons("sharedYN", label = NULL, choices = c("")),
                          conditionalPanel("input.sharedYN == 'Yes'",
                                           selectizeInput("sharedBy", label = NULL, choices = c("")),
                                           tags$div(id="padd")),
                          actionButton("completionOK", "OK", disabled = TRUE)
                      ))

surveyIntro <- tabPanel("surveyTabIntro",
                        card(class="pages",
                          radioGroupButtons("gender", label = NULL, choices = c("")),
                          numberInput("age", label = NULL, min = 18, max = 70, step = 1),
                          selectizeInput("country", label = NULL, choices = c("")),
                          textOutput("ageValid"),
                          radioGroupButtons("jobYN", label = NULL, choices = c("")),
                          conditionalPanel("input.jobYN == 'Yes'",
                                           selectizeInput("job", label = NULL, choices = c("")),
                                           radioButtons("jobLength", label = NULL, choices = c("")),
                                           radioButtons("jobHappy", label = NULL, choices = c(""))
                          ),
                          actionButton("next1", label = NULL, disabled = TRUE)
                        ))

surveyMat1 <- tabPanel("matrixTab1",
                       card(class="pages",
                            uiOutput("mat1"),
                            uiOutput("matrixQ1"),
                            ))
surveyMat2 <- tabPanel("matrixTab2",
                       card(class="pages",
                            uiOutput("matrixQ2")))
surveyMat3 <- tabPanel("matrixTab3",
                       card(class="pages",
                            uiOutput("matrixQ3")))
surveyMat4 <- tabPanel("matrixTab4",
                       card(class="pages",
                            uiOutput("matrixQ4")))
surveyMat5 <- tabPanel("matrixTab5",
                       card(class="pages",
                            uiOutput("matrixQ5")))

results <- tabPanel("resultsTab",
                    tags$div(class="pages",
                      navset_card_underline(
                        nav_panel(title = uiOutput("navT1"),
                                  uiOutput("info")
                        ),
                        
                        nav_panel(title = uiOutput("navT2"),
                                  uiOutput("navB2"),
                                  layout_columns(
                                    card(
                                      uiOutput("tenTop"),
                                      tags$ol(
                                        uiOutput("topTen")
                                      )
                                    ),
                                    card(
                                      uiOutput("tenBottom"),
                                      tags$ol(
                                        uiOutput("bottomTen")
                                      )
                                    )
                                  )),
                        nav_panel(title = uiOutput("navT3"),
                                  uiOutput("navB31"),
                                  girafeOutput("girafePlot", width = "100%"),
                                  actionButton("whyHere", label = NULL)
                                  ),
                        nav_panel(title = uiOutput("navT4"),
                                  uiOutput("navB4"),                                  
                                  card(
                                  tableOutput("big5table")),
                                  actionButton("whatB5", label = NULL)
                        ),
                        nav_panel(title = uiOutput("navT5"),
                                  card(
                                    uiOutput("share")
                                  )),
                        nav_panel(title = uiOutput("navT6"),
                                  card(
                                    uiOutput("retake"),
                                    uiOutput("FAQ")
                                  )),
                        nav_spacer()
                      ),
                      downloadButton("save", label = NULL)
))

# Application UI
ui <- page_fluid(
  tags$head(includeCSS("www/style.css")),
  theme = bs_theme(bootswatch = "minty"),
  shinyFeedback::useShinyFeedback(),
  tabsetPanel(id = "appTabs", type = "hidden", selected = "languageTab",
              language,
              disclaimer,
              completionQ,
              surveyIntro,
              surveyMat1,
              surveyMat2,
              surveyMat3,
              surveyMat4,
              surveyMat5,
              results
  ))

### SERVER: UI responsiveness, saving responses...

server <- function(input, output, session) {
  
  # The aim of these is to keep the page from disconnecting by allowing reconnect
  # and "waking up" the session every 5 sec.
  # The latter might well be superfluous, but I'll leave it in just in case --
  # can try removing if server demand grows too big.
  session$allowReconnect(TRUE)
  keepAlive <- reactive({
    reactiveTimer(5000)
    if (keepAlive==0) {1} else {0}
  })
  
  # Text scale: completely false, ... , completely true.
  ratingScale <- reactive(getText(lapply(c(1:6), function(n) paste0("sc", n, collapse=""))))
  
  # Calling function, not necessary but makes code later on tidier.
  lang <- reactive(input$lang)
  
  # Text-fetching function: takes a string (ID) as its argument and returns the corresponding
  # text to display in the UI in the chosen language -- allows for tidier code.
  getText <- function(id) {
    filter(appText, ID %in% id)[[lang()]]
  }
  
  # This session's ID
  ownSession <- paste0(sample(letters, 20),collapse="")
  # Get sharer's session (or repeat flag) from URL
  sharedSession <- reactive({
    link <- parseQueryString(session$clientData$url_search)
    ifelse(is.null(link[['reference']]), NA, link[['reference']])})
  
  ## OBSERVERS: things that happen upon user input
  
  # Language page
  observeEvent(input$lang, updateActionButton(inputId = "langOK", disabled = is.null(input$lang)))
  observeEvent(input$langOK, {
    # Defining text withing UI elements
    # Disclaimer tab
    updateActionButton("about", label = getText("about"), session = session)
    updateActionButton("understood", label = getText("cont"), session = session)
    # Completion tab
    updateRadioGroupButtons("completion", label = getText("comp1"), choiceValues = c(TRUE, FALSE), choiceNames = getText(c("yes", "no")), selected = character(0), session = session)
    updateRadioGroupButtons("sharedYN", label = getText("comp2"), choiceValues = c("Yes", "No"), choiceNames = getText(c("yes", "no")), selected = character(0), session = session)
    updateSelectizeInput("sharedBy", label = getText("comp3"), choices = c("", getText(lapply(c(1:7), function(n) paste0("comp2", n, collapse="")))), options = list(placeholder = getText("sel")), session = session)
    # Intro tab
    updateRadioGroupButtons("gender", getText("int1"), choiceValues = c("Female", "Male"), choiceNames = getText(c("fem", "male")), selected = character(0), session = session)
    updateNumericInput("age", label = getText("int2"), session = session)
    updateSelectizeInput("country", label = getText("int3"), choices = c("", countries[[lang()]]), options = list(placeholder = getText("type")), session = session)
    updateRadioGroupButtons("jobYN", label = getText("int4"), choiceValues = c("Yes", "No"), choiceNames = getText(c("yes", "no")), selected = character(0), session = session)
    updateSelectizeInput("job", label = getText("int5"), choices = c("",jobOptions[[lang()]]), options = list(placeholder = getText("type")), server = TRUE, session = session)
    updateRadioButtons("jobLength", label = getText("int8"), choiceValues = c(1:5), choiceNames = getText(lapply(c(1:5), function(n) paste0("int8", n, collapse=""))), selected = character(0), session = session)
    updateRadioButtons("jobHappy", label = getText("int6"), choiceValues = c(1:6), choiceNames = ratingScale(), selected = character(0), session = session)
    updateActionButton("next1", label = getText("next"), session = session)
    updateActionButton("whyHere", label = getText("here"), session = session)
    updateActionButton("whatB5", label = getText("whatB5"), session = session)
    updateActionButton("save", label = getText("save"), session = session)
    
    updateTabsetPanel(inputId = "appTabs", selected = "disclaimerTab")
    })
  
  # Disclaimer page
  observeEvent(input$about, {
    showModal(modalDialog(
      includeMarkdown(paste0("www/", lang(), "/about.md", collapse="")),
      footer = modalButton(label = getText("dismiss"))
    ))
  })
  observeEvent(input$understood, updateTabsetPanel(inputId = "appTabs", selected = "completionQTab"))
  
  # Completion page
  done0 <- reactive(is.null(input$completion) || is.null(input$sharedYN) || (input$sharedYN=="Yes" && input$sharedBy==""))
  observeEvent(done0(), updateActionButton(inputId = "completionOK", disabled = done0()))
  observeEvent(input$completionOK, updateTabsetPanel(inputId = "appTabs", selected = "surveyTabIntro"))
  
  # Survey intro page
  isInvalid <- reactive(input$age <18 || input$age >70)
  ageCheck <- reactive({
    isValid <- input$age >=18 && input$age <= 70
    shinyFeedback::feedbackWarning("age", !isValid, getText("int7"))
  })
  output$ageValid <- renderText(ageCheck())
  done1 <- reactive(is.null(input$gender) || is.na(input$age) || isInvalid() || input$country=="" || is.null(input$jobYN) || (input$jobYN=="Yes" && (input$job=="" || is.null(input$jobHappy) || is.null(input$jobLength))))
  observeEvent(done1(), updateActionButton(inputId = "next1", disabled = done1()))
  observeEvent(input$next1, updateTabsetPanel(inputId = "appTabs", selected = "matrixTab1"))
  
  # Matrix question pages
  observeEvent(input$matID1, updateActionButton(inputId = "next2", disabled = is.null(input$matID1)))
  observeEvent(input$next2, updateTabsetPanel(inputId = "appTabs", selected = "matrixTab2"))
  observeEvent(input$matID2, updateActionButton(inputId = "next3", disabled = is.null(input$matID2)))
  observeEvent(input$next3, updateTabsetPanel(inputId = "appTabs", selected = "matrixTab3"))
  observeEvent(input$matID3, updateActionButton(inputId = "next4", disabled = is.null(input$matID3)))
  observeEvent(input$next4, updateTabsetPanel(inputId = "appTabs", selected = "matrixTab4"))
  observeEvent(input$matID4, updateActionButton(inputId = "next5", disabled = is.null(input$matID4)))
  observeEvent(input$next5, updateTabsetPanel(inputId = "appTabs", selected = "matrixTab5"))
  observeEvent(input$matID5, updateActionButton(inputId = "submit", disabled = is.null(input$matID5)))
  
  # Upon submission, instead of using an observer, the processes undertaken are
  # packaged in a reactive expression, for the reason that it allows some necessary
  # data structures to be used outside of this specific event (in the feedback page).
  # Not sure how good of a solution this is, but it works.
  f <- eventReactive(input$submit, {
    # Assign one of 4 groups: young/old, female/male 
    normgroup <- ifelse(input$gender == "Female",ifelse(input$age < 35, 1, 2), ifelse(input$age < 35, 3, 4))
    
    # Format response for saving to file
    if (input$sharedYN=="Yes") {
      sharedYN <- TRUE
      sharedBy <- filter(appText, .data[[lang()]]==input$sharedBy)$en
    }
    else {
      sharedYN <- FALSE
      sharedBy <- NA
    }
    
    jobYN <- input$jobYN=="Yes"
    if (!jobYN) {
      job <- NA
      jobLength <- NA
      jobHappy <- NA
    }
    else {
      job <- filter(jobOptions, .data[[lang()]]==input$job)$code
      jobLength <- input$jobLength
      jobHappy <- input$jobHappy
    }
    country <- filter(countries, .data[[lang()]]==input$country)$code
    mat1 <- input$matID1$response %>% map_int(\(x) match(x, ratingScale())) %>% matrix() %>% t()
    mat2 <- input$matID2$response %>% map_int(\(x) match(x, ratingScale())) %>% matrix() %>% t()
    mat3 <- input$matID3$response %>% map_int(\(x) match(x, ratingScale())) %>% matrix() %>% t()
    mat4 <- input$matID4$response %>% map_int(\(x) match(x, ratingScale())) %>% matrix() %>% t()
    mat5 <- input$matID5$response %>% map_int(\(x) match(x, ratingScale())) %>% matrix() %>% t()
    matrixResponses <- cbind(mat1,mat2,mat3,mat4,mat5)

    # Norm personality question responses to age-gender group
    normedItems <- as.matrix((matrixResponses - meanNorms[normgroup,-1]) / sdNorms[normgroup,-1])
    response2D <- normedItems %*% itemCors %*% weights
    B5scores <- normedItems %*% itemCors %*% weightsBig5
    
    timestamp <- Sys.time()
    response <- cbind(data.frame(ownSession, sharedSession(), timestamp, input$lang, input$completion, sharedYN, sharedBy, input$gender,input$age, country, jobYN,job,jobLength,jobHappy), matrixResponses)
    write_tsv(response, paste0("participantResponses/", ownSession, "_", timestamp, ".tsv", collapse = ""), append = FALSE, col_names = FALSE)

    ## calculate [some correlation] between response items and mean items for each occupation
    icc <- function(x,y) cor(c(x,y),c(y,x))
    iccs <- apply(as.matrix(occResponses), 1, icc, as.matrix(normedItems))
    occs <- read_tsv(paste0("www/", lang(), "/jobNames.tsv", collapse=""))
    names(iccs) <- occs$job
    
    # Data to be returned by the expression + a flag which invokes the popup 
    list(response2D = response2D, B5scores = B5scores, iccs = iccs, occs = occs, flag = TRUE)
  })
  
  observeEvent(f()$flag, {
    showModal(modalDialog(
      getText("mod1"),
      footer = actionButton(inputId = "goToResults", label = getText("mod2"))
    ))
  })
  
  observeEvent(input$goToResults, {
    removeModal()
    updateTabsetPanel(inputId = "appTabs", selected = "resultsTab")
  })
  
  observeEvent(input$whyHere,
               showModal(modalDialog(
                 includeMarkdown(paste0("www/", lang(), "/map.md", collapse="")),
                 footer = modalButton(getText("dismiss"))
               )))
  observeEvent(input$whatB5,
               showModal(modalDialog(
                 includeMarkdown(paste0("www/", lang(), "/B5.md", collapse="")),
                 footer = modalButton(getText("dismiss"))
               )))
  
  ## UI OUTPUT ELEMENTS
  # Includes some text that would typically be directly in UI, but for multilanguage
  # purposes is defined in server.
  
  # Disclaimer page
  output$logo <- renderImage(list(src=normalizePath(file.path("./www/UTEGCLogo.jpg")), width="300px", id="logo"), deleteFile = FALSE)
  output$disc1 <- renderUI(
    includeMarkdown(paste0("www/", lang(), "/1stpage.md", collapse=""))
  )
  
  # Matrix questions: because {shinysurveys} doesn't come with an updateRadioMatrixInput function,
  # the whole UI element needs to be declared in server to allow for language variablility.
  output$mat1 <- renderUI(tags$div(
    p(getText("mat1")),
    p(getText("mat2")),
    p(getText("mat3")),
    tags$b(getText("mat4")),
  ))
  output$matrixQ1 <- renderUI(tags$div(
    radioMatrixInput(inputId = "matID1",
                     responseItems = filter(questions, page == 1)[[lang()]],
                     choices = ratingScale(),
                     selected = character(0),
                     .required = FALSE),
    actionButton("next2", label = getText("next"), disabled = TRUE)))
  output$matrixQ2 <- renderUI(tags$div(
    radioMatrixInput(inputId = "matID2",
                     responseItems = filter(questions, page == 2)[[lang()]],
                     choices = ratingScale(),
                     selected = character(0),
                     .required = FALSE),
    actionButton("next3", label = getText("next"), disabled = TRUE)))
  output$matrixQ3 <- renderUI(tags$div(
    radioMatrixInput(inputId = "matID3",
                     responseItems = filter(questions, page == 3)[[lang()]],
                     choices = ratingScale(),
                     selected = character(0),
                     .required = FALSE),
    actionButton("next4", label = getText("next"), disabled = TRUE)))
  output$matrixQ4 <- renderUI(tags$div(
    radioMatrixInput(inputId = "matID4",
                     responseItems = filter(questions, page == 4)[[lang()]],
                     choices = ratingScale(),
                     selected = character(0),
                     .required = FALSE),
    actionButton("next5", label = getText("next"), disabled = TRUE)))
  output$matrixQ5 <- renderUI(tags$div(
    radioMatrixInput(inputId = "matID5",
                     responseItems = filter(questions, page == 5)[[lang()]],
                     choices = ratingScale(),
                     selected = character(0),
                     .required = FALSE),
    actionButton("submit", label = getText("subm"), disabled = TRUE)))
  
  # RESULTS PAGE
  # Headers + misc. text
  output$navT1 <- renderUI(getText("fb1"))
  output$navT2 <- renderUI(getText("fb21"))
  output$navT3 <- renderUI(getText("fb31"))
  output$navT4 <- renderUI(getText("fb41"))
  output$navT5 <- renderUI(getText("fb51"))
  output$navT6 <- renderUI(getText("fb6"))
  output$navB2 <- renderUI(getText("fb22"))
  output$navB31 <- renderUI(tags$div(
    tags$p(getText("fb32")),
    tags$p(getText("fb33"))
  ))
  output$tenTop <- renderUI(tags$h5(getText("fb23")))
  output$tenBottom <- renderUI(tags$h5(getText("fb24")))
  output$navB4 <- renderUI(tags$div(
    tags$p(getText("fb42")), 
    tags$p(getText("fb43"))
  ))

  # Results page tab bodies
  output$info <- renderUI(includeMarkdown(paste0("www/", lang(), "/info.md")))
  
  output$girafePlot <- renderGirafe({
    occs <- f()$occs
    plotData <- data.frame(G = occs$group, J = occs$job, occResponses %*% itemCors %*% weights) %>%
      `names<-`(c("Job Group", "Job", "X", "Y"))
    response2D <- f()$response2D
    
    plot <-  ggplot(plotData) +
      geom_point_interactive(aes(x=X,y=Y, color = `Job Group`, size = 2, tooltip = Job)) +
      theme_minimal()+
      theme(
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank()
      )  +
      guides(shape="none", size="none")+
      annotate(geom="point", x=response2D[1,1], y=response2D[1,2], color="gold", cex=10, shape = 16)+
      annotate(geom="text", x=response2D[1,1], y=response2D[1,2], label=getText("fb35"), color="black", cex=3)
    
    girafe(ggobj = plot, height_svg = 6, width_svg = 8)
  })
  
  output$topTen <- renderUI({
    top10 <- data.frame(names(sort(f()$iccs, decreasing = T)[1:10]))
    apply(top10, 1, function(x) tags$li(x[1]))
  })
  output$bottomTen <- renderUI({
    bottom10 <- data.frame(names(sort(f()$iccs, decreasing = F)[1:10]))
    apply(bottom10, 1, function(x) tags$li(x[1]))
  })
  
  output$big5table <- renderTable({
    big5 <- read_tsv(paste0("www/", lang(), "/big5.tsv", collapse=""), col_names = FALSE)
    data.frame(
      Trait = big5[[1]], 
      Score = ifelse(c(f()$B5scores)<(-0.67), getText("fb44"), ifelse(c(f()$B5scores)<=0.67, getText("fb45"), getText("fb46")))
    )},
    align = 'l')
  
  output$share <- renderUI({
    tagList(
      tags$br(),
      tags$p(getText("fb52")),
      tags$p(paste0("https://apps.psych.ut.ee/JobProfiler?reference=", ownSession, collapse="")),
      tags$br(),
      tags$small(getText("fb53"))
    )
  })
  
  output$FAQ <- renderUI({
    includeMarkdown(paste0("www/", lang(), "/FAQ.md", collapse=""))
  })
  
  output$retake <- renderUI({
    tagList(
      tags$br(),
      tags$a(href=paste0("https://apps.psych.ut.ee/JobProfiler?reference=", ownSession, "REPEAT", collapse=""), getText("retake"))
    )
  })
  
}

shinyApp(ui = ui, server = server)