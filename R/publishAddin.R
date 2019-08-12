#'
#' Publish reports on Atril
#'
#' @description `publishAddin()` opens an [RStudio
#' gadget](https://shiny.rstudio.com/articles/gadgets.html) and
#' [addin](http://rstudio.github.io/rstudioaddins/) that allows you to
#' render and publish your selected Rmd file directly to Atril.
#'
#' @import shiny miniUI rstudioapi yaml
#' @export
publishAddin <- function() {
  
  # Validate requirements
  validateDependencies <- vapply(
    c("rstudioapi", "shiny", "miniUI"),
    requireNamespace, logical(1), quietly = TRUE
  )
  if (any(!validateDependencies)) {
    stop(
      "Install these packages in order to use the reprex addin:\n",
      collapse(names(validateDependencies[!validateDependencies])), call. = FALSE
    )
  }
  
  ui <- miniPage(
    miniContentPanel(
      stableColumnLayout(
        tags$label('Title:')
      ),
      stableColumnLayout(
        uiOutput("title")
      ),
      stableColumnLayout(
        tags$label('Description:')
      ),
      stableColumnLayout(
        uiOutput("description")
      ),
      stableColumnLayout(
        textInput(
          "token", "Your Token:", 
          width="100%", value=getApiKey(), 
          placeholder="Your token is located on your settings page: https://www.atril.me/#/settings"
        )
      ),
      stableColumnLayout(
        selectInput("community", "Community:", c("Olapic - Content In Motion" = "DsmYGvDt", "Olapic" = "mrKxPuR7")),
        selectInput("format", "Output format:", c("HTML" = "html_document", "PDF" = "pdf_document"))
      ),
      stableColumnLayout(
        tags$label('Post:')
      ),
      stableColumnLayout(
        uiOutput("finalPostLink")
      )
    ),
    miniButtonBlock(
      actionButton("cancel", "Cancel", color="warning", class = "btn-default"),
      actionButton("publish", "Upload & Publish", color="warning", class = "btn-primary")
    )
  )
  
  server <- function(input, output, session) {
    
    context <- rstudioapi::getActiveDocumentContext()
    print(context$contents)
    original <- context$contents
    output$finalPostLink <- renderUI({
      tagList("Direct Link: ...")
    })
    
    if(!endsWith(tolower(context$path), '.rmd')){
      rstudioapi::showDialog("Error", "We are expecting a .Rmd notebook. Please open it on the editor.")
      stopApp(message("Closed by error"))
      return()
    }
    
    metadata <- parseMetadata(context$path)
    if(is.na(metadata)){
      showNotification(paste("There was an error loading the markdown file:", context$path), duration = 5, type="error")
    }
    
    title <- metadata$title
    description <- metadata$subtitle
    output$title <- renderUI({
      tagList(tags$p(title))
    })
    output$description <- renderUI({
      tagList(tags$p(description), tags$br())
    })
    
    observeEvent(input$publish, {
      # Progress bar with 4 steps:
      # 1. Validation
      # 2. Rendering
      # 3. Uploading and Publishing
      # 4. Complete
      progress <- Progress$new(session, min=1, max=4, style='old')
      on.exit(progress$close())
      
      # Step 1: Validate the data
      progress$set(value = 1)
      setApiKey(input$token)
      message = validate(input, context)
      if(!is.na(message)){
        showNotification(message, duration = 5, type="error")
        return()
      }
      
      # Step 2: Render the markdown
      progress$set(value = 2)
      progress$set(message = 'Rendering the notebook...')
      renderOutput = renderMarkdown(context$path, input$format)
      if(is.na(renderOutput)){
        showNotification("There was an error rendering the markdown. Please check the logs for the error.", duration = 5, type="error")
      }
      
      # Step 3: Upload and Publish
      progress$set(value = 3)
      progress$set(message = 'Uploading and publishing...')
      finalPostUrl = uploadAndPublish(renderOutput, input$token)
      
      # Step 4: Finish the process
      progress$set(value = 4)
      showNotification("Notebook published successfully", duration = 5, type="message")
      output$postUrl <- renderText({finalPostUrl})
      
      url <- a(href=finalPostUrl, finalPostUrl)
      output$finalPostLink <- renderUI({
        tagList("Direct Link:", url)
      })
    })
    
    observeEvent(input$cancel, {
      stopApp(message("App stopped"))
    })
    
    observeEvent(input$stop, {
      stopApp(message("App stopped"))
    })
    
    onStop(function(){
      cat("Session stopped\n")
      stopApp(message("Closed by user"))
    })
  }
  
  viewer <- dialogViewer("Upload & Publish on Atril", width = 600, height = 450)
  runGadget(ui, server, viewer = viewer)
}

validate <- function(input, context) {
  if(is.na(context$path) || context$path == ''){
    return("Notebook not recognized")
  }  
  if(!endsWith(tolower(context$path), '.rmd')){
    return("We are expecting a .Rmd notebook")
  }
  if(!input$format %in% c('html_document', 'pdf_document')){
    return("Output format not recognized")
  }
  if(is.na(input$token) || input$token == ''){
    return("Token is required for publishing. Please copy&paste it from your settings page.")
  }
  if(is.na(input$community) || input$community == ''){
    return("Community is required for publishing")
  }
  NA
}

renderMarkdown <- function(path, format) {
  tryCatch({
    rmarkdown::render(path, format)
  }, error = function(e) {
    warning(e)
  })
}

uploadAndPublish <- function(output, token) {
  Sys.sleep(4)
  'https://www.atril.me/posts/helpdesk/kayAafeN'
}

stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}

getApiKey <- function() {
  Sys.getenv('ATRIL_TOKEN')
}

setApiKey <- function(value) {
  Sys.setenv('ATRIL_TOKEN' = value)
}

parseMetadata <- function(file) {
  yml <- NA
  tryCatch({
    yml <- yaml::read_yaml(file)
  }, error = function(e) {
    warning(e)
  })
  yml
}