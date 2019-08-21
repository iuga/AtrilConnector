#
# Utils
#
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
    config.file <- file.path(path.expand('~'), '.atril')
    if(file.exists(config.file)){
        content <- yaml::read_yaml(config.file)
        return(content$api_key)    
    }
    return('')
}

setApiKey <- function(value) {
    config.file <- file.path(path.expand('~'), '.atril')
    content <- list('api_key'=as.character(value))
    yaml::write_yaml(content, config.file)
}

parseMetadata <- function(file) {
    yml <- NA
    yml <- tryCatch({
        file.contents <- read_file(file)
        file.contents <- str_replace_all(file.contents, '\n', '__NEWLINE__')
        yaml.contents <- as.character(regmatches(file.contents, gregexpr("(?<=---)(.*?)(?=---)", file.contents, perl = T))[[1]][1])
        yaml.contents <- str_replace_all(yaml.contents, '__NEWLINE__', '\n')
        yaml::yaml.load(yaml.contents)
    }, error = function(e) {
        warning(e)
    })
    yml
}

validateApiKey <- function(apiKey){
    return(nchar(apiKey) == 32)
}

getCommunities <- function(session){
    progress <- Progress$new(session, min=1, max=3, style='old')
    on.exit(progress$close())
    progress$set(message = 'Listing your communities...')
    progress$set(value = 1)
    
    token <- getApiKey()
    
    response <-httr::GET(paste0('https://www.atril.me/v1/external/communities?api_key=', token))
    
    if(response$status == 200){
        progress$set(value = 2)
        jsonContent <-  httr::content(response, simplifyVector = T) 
        comms <- jsonContent$communities
        communities <- setNames(comms$uid, as.character(comms$name))
        progress$set(value = 3)
        return(communities)
    } else {
        showNotification("There was an error listing your communities", duration = 5, type="error")
        stopApp('Unable to list your communities')
        progress$set(value = 3)
        return(c())
    }
}

uploadAndPublish <- function(output, format, apiKey, communityUid, title, description) {
    response <- httr::POST(
        paste0("https://www.atril.me/v1/external/communities/", communityUid, "/posts/publish?api_key=", apiKey),
        body=list(
            title=title,
            description=description,
            report_file= httr::upload_file(output)
        ),
        encode="multipart"
    )
    postUrl = ''
    if(response$status == 200){
        jsonContent <-  httr::content(response)
        postUrl <- paste0('https://www.atril.me/#/posts/', jsonContent$report$author$username, '/', jsonContent$report$uid)
        showNotification("Notebook published successfully", duration = 5, type="message")
    } else {
        showNotification("There was an error posting the report", duration = 5, type="error")
    }
    postUrl
}