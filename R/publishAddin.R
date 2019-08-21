#'
#' Publish reports on Atril
#'
#' @description `publishAddin()` opens an [RStudio
#' gadget](https://shiny.rstudio.com/articles/gadgets.html) and
#' [addin](http://rstudio.github.io/rstudioaddins/) that allows you to
#' render and publish your selected Rmd file directly to Atril.
#'
#' @import shiny miniUI rstudioapi yaml shinyjs readr stringr
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
    useShinyjs(),
    miniContentPanel(
      stableColumnLayout(
        textInput(
          "token", "Your Token:", 
          width="100%", value=getApiKey(), 
          placeholder="Copy & Paste your private token from your settings page: https://www.atril.me/#/settings"
        )
      ),
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
        selectInput("community", "Community:", c()),
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
    
    output$title <- renderUI({
      tagList(tags$p('...'))
    })
    output$description <- renderUI({
      tagList(tags$p('...'))
    })
    
    # Parse the Api Key
    apiKey <- getApiKey()
    if(!validateApiKey(apiKey)){
      apiKey <- rstudioapi::showPrompt('Select your API Key', 'Api Key not found. Copy & Paste your token from the settings page: https://www.atril.me/#/settings here:')
      apiKey <- ifelse(is.null(apiKey), '', apiKey)
      if(!validateApiKey(apiKey)){
        rstudioapi::showDialog("Error", "This adding needs a valid Api Key to work")
        stopApp(message("Api key not valid"))
        return()
      }
    }
    setApiKey(apiKey)
    
    # Add all your communities on the select
    updateTextInput(session, "token", value = apiKey)
    
    context <- rstudioapi::getActiveDocumentContext()
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
    if(is.null(title)){
      stopApp('Title is required on the Notebook header')
      return()
    }
    description <- metadata$subtitle
    if(is.null(description)){
      description <- title
    }
    output$title <- renderUI({
      tagList(tags$p(title))
    })
    output$description <- renderUI({
      tagList(tags$p(description), tags$br())
    })
    
    observeEvent(input$token, {
      setApiKey(input$token)
      updateSelectInput(session = session, inputId = "community", choices = getCommunities(session))
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
      title <- metadata$title
      description <- metadata$subtitle
      if(is.null(description)){
        description <- title
      }
      finalPostUrl = uploadAndPublish(renderOutput, input$format, apiKey, input$community, title, description)
      
      # Step 4: Finish the process
      progress$set(value = 4)
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
  
  viewer <- dialogViewer("Upload & Publish on Atril", width = 650, height = 550)
  runGadget(ui, server, viewer = viewer)
}