## Write and Read App (wrApp)

library(shiny)
library(bulletr)
library(xml2)
library(shinyjs)
library(lubridate)
library(utils)
library(tidyverse)
# When the app is Standalone not used from within the package
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# appdir<-getwd()

appdir<-getwd()#system.file("shiny-examples", "wR-app", package = "bulletr")

# This variable is called while using the reset button to reset the page
# uses package::shinjs 
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
#tmp <-NULL



ui1<- shinyUI(pageWithSidebar (
  headerPanel( "wrApp- Write & Read dat/x3p fies"),
  sidebarPanel(
    #numericInput("assets", label = "Enter Number of variants in Experiment", value="3")
    
    radioButtons("mode", "Select Write mode", c("Single file" = "singfile",
                                                "Batch" = "batch")),
    
    conditionalPanel( 
      condition = "input.mode == 'singfile'",
      
      selectInput("ftype", "Select File type", c("DAT files" = "datftype", "X3P files" ="x3pftype")),
      conditionalPanel(
        condition = "input.ftype == 'x3pftype'",
        selectInput("templatef", "Choose Meta-Data Source",
                    c("Keep Info from File" = "notemplate","Use Info from Template" = "usetemplate"))
      ),
      conditionalPanel(
        condition = "input.templatef == 'usetemplate' || input.ftype == 'datftype'",
        #tags$head(tags$script(src = "message-handler.js")),
        actionButton("getTemplate", "Choose Template")
      ),
      # conditionalPanel(
      # condition = "input.ftype == 'datftype'",
      # #selectInput("templatef", "DAT format",
      # #           c("DAT Info from Template" = "usetemplate_dat"))
      # actionButton("getTemplate", "Choose Template")
      # ),
      checkboxInput("profiley", "Do you need Profile Adjustment? Check for Yes"),
      #selectInput("changeinfo", "Do you wish to Change any information?", c("No, Keep the defaults", "Yes, I want to make changes")),
      # conditionalPanel(
      #   condition = "input.templatef == 'notemplate'",
      selectInput("information", "View/Edit Information",
                  c("Header Info (Automatic)" = "headInfo","General Information" = "genInfo", "Feature Information" = "featInfo", "Matrix Information" = "matInfo")),
      #),
      actionButton("fileget", "Browse"),
      textOutput("text1"),
      helpText("Please wait for the Loading complete sign to apear before prooceeding"),
      helpText("Note: Houston data sets have Profile = FALSE",
               "while Phoenix data sets have Profile = TRUE"),
      actionButton("write", "Write File"),
      actionButton("save_template", "Save Fields as Template")
    ),# bracket for conditional section
    conditionalPanel( 
      condition = "input.mode == 'batch'",
      # conditionalPanel(
      #   condition = "input.parse == 'parsetext'",
      # )
      selectInput("parse", "Choose Meta-Data Source",
                  c("Parse Meta-data from file name" = "parsetext","Use Template for Batch" = "batchtemplate")),
      checkboxInput("batchrecursive", "Do you want to convert Dat files for all subfolders?"),
      checkboxInput("profileybatch", "Do you need Profile Adjustment? Check for Yes"),
      actionButton("folderbatchget", "Browse"),
      actionButton("choosebatchtemplate", "Choose Template"),
      actionButton("batchwrite", "Batch Write")
    ),
    # This includes shinyjs in the UI
    useShinyjs(),
    # This adds the js code to the page
    extendShinyjs(text = jsResetCode),
    actionButton("reset_button", "Reset Page")
  ),
  mainPanel(
    
    # htmlOutput("text3"),
    # uiOutput("finfo.default"),
    # uiOutput("ginfo.default"),
    # uiOutput("minfo.default")
    # Setting up the text boxes in the main panel for the form layout
    conditionalPanel(
      condition = "input.information == 'headInfo'",
      #textOutput("text3"),
      uiOutput("text3batch"),
      textOutput("text5"),
      htmlOutput("text3")
    ),
    conditionalPanel(
      condition = "input.information == 'featInfo'",
      uiOutput("finfo.default")
    ),
    conditionalPanel(
      condition = "input.information == 'genInfo'",
      uiOutput("ginfo.default")
    ),
    conditionalPanel(
      condition = "input.information == 'matInfo'",
      
      uiOutput("minfo.default")
    )
  )
  
))

# # Setting defaults for template comes in handy
# input.info<- as_list(a121)
# qwe<- as_xml_document(list(structure(list(input.info ))))
# qwe<- as_xml_document(list("ISO5436_2"= list(structure(list(input.info)))))
# xml_attrs(qwe)<- xml_attrs(a121)
# 
# # Unlist and build list again
# mylist <- list(a = 1, b = list(A = 1, B = 2), c = list(C = 1, D = 3))
# tmp <- as.relistable(mylist)
# tmp <- unlist(tmp)
# tmp[grep("(^|.)C$",names(tmp))] <- 5 # 5 is the length of the unlisted list
# tmp <- relist(tmp)
# 
# # value from the text inputs
# paste0(unlist(input.info$Record3)[[i]])
tmp1<- NULL
tmp2<- NULL
tmp3<- NULL

s1<- shinyServer( function(input, output, session) {
  # template_path <<- "empty"
  field.not.generated <- function(x){
    if((length(x) == 0) || is.null(x[[1]])) 
      return(TRUE) 
    else 
      purrr::map(x, field.not.generated)
  }
  
  observeEvent(input$getTemplate, { #handlerExpr = quote(template_path), handler.quoted = TRUE,{
    template_path<<- tryCatch({choose.files(default= "", "Select Template")},error = function(e) {})
    
    if(tryCatch(field.not.generated(template_path), error = function(e) return(FALSE))){
      a121<<- xml2::read_xml("./defaultTemplateXML.xml")
      #winDialog(type = "ok", message = "Custom template not chosen. Using Default template")#, default= "")
      #showNotification("This is a notification.", duration = NULL, type = "warning")
      #session$sendCustomMessage(type = 'testmessage',
      #                          message = 'Custom template not chosen. Using Default template')
      
      showModal(modalDialog(title = "Important message",
                            "Custom template not chosen. Using Meta-Data from Default template",
                            easyClose = TRUE
      ))
      showNotification("Default Template Loaded", duration = 5, type = "message")
      custom.template<<- FALSE
      #input.info<<- as_list(a121)
      #backup.template<- as_list(a121)  
    }else{
      a121<<- xml2::read_xml(template_path)
      custom.template<<- TRUE
      showNotification("Custom Template Loaded", duration = 5, type = "message")
    }
    input.info<<- as_list(a121)
    input.info<<- input.info[[1]]
    
  })
  
  observe({
    if (input$getTemplate == 0){
      a121<<- xml2::read_xml("./defaultTemplateXML.xml")
      input.info<<- as_list(a121)
      # New things after change in the as_list definition
      input.info<<- input.info[[1]]
    }
  })
  
  # observeEvent(input$x3pftype, {
  #   if(input$templatef == 'notemplate'){
  #     a126<<- xml2::read_xml("./defaultTemplateXML.xml")
  #     input.info<<- as_list(a126)
  #     # New things after change in the as_list definition
  #     input.info<<- input.info[[1]]
  #   }
  # })
  
  file1name<- reactiveValues()
  cdat<- reactiveValues()
  cx3p<- reactiveValues()
  fpth<- reactiveValues()
  #ft<- reactiveValues()
  data2<- NULL#reactiveValues() 
  #persistent_data<- reactiveValues() 
  
  loadData <- function(name, file1, profiley) {
    if(substrRight(name,3)[1] == "dat"){
      data1 <- read_dat(path = file1, profiley = profiley)
      
    }else if(substrRight(name,3)[1] == "x3p"){
      data1 <- read_x3p(path = file1, profiley = profiley)
      
    }
    return(data1)
  }
  
  # Finding the extension of the file read in
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  # autoupdate_from_header<- function(data2,file1name){    
  #   output$text2 <- renderText({
  #     paste0("Current Sytem Time = ",as.character(Sys.time()))
  #   })
  #   output$text4 <- renderText({
  #     as.character(substrRight(file1name,3))
  #   })
  #   
  #   # Show the Header Info, Profile etc which is Automatically detected on reading a file
  #  }
  
  
  observeEvent(input$fileget, {
    
    # File Path
    if((!tryCatch(field.not.generated(data2), error = function(e) return(FALSE)))){ #actually evaluates on error to !FALSE
      showModal(modalDialog(title = "Important message",
                            "A file has been already read in, Cannot read another file,
                            please reset app to read in a new file.",
                            easyClose = TRUE))
    }else{
      pt<- tryCatch({choose.files(default= "", "Select Files")},error = function(e) {})  
    }
    
    
    if(tryCatch(field.not.generated(pt), error = function(e) return(FALSE))){
      # if((input$fileget >=2)){
      #   if((!tryCatch(field.not.generated(data2), error = function(e) return(FALSE)))){ #actually evaluates on error to !FALSE
      #     showModal(modalDialog(title = "Important message",
      #                           "A file has been already read in, Cannot read another file,
      #                           please reset app.",
      #                           easyClose = TRUE
      #     ))   
      #   } else{
      #     showModal(modalDialog(title = "Important message",
      #                           "You did Not Choose a File, Please Try Again",
      #                           easyClose = TRUE
      #     ))
      #   }
      #   
      # }else{
      showModal(modalDialog(title = "Important message",
                            "You did Not Choose a File, Please Try Again",
                            easyClose = TRUE
      ))
      # }
    }else {
      
      
      if (substrRight(pt,3)[1] == "dat"){
        if (!is.null(pt)) {
          cdat<- pt
        }
        
        output$text5 <- renderText({
          as.character(cdat)
          paste0(length(cdat), " dat files being processed")
        })
        
        fpth<<- cdat
        file1name<<- basename(cdat)
        data2<<- loadData( fpth,  fpth, input$profiley)
        
        output$text1 <- renderText({
          paste0(1, " file Loaded")
        })
      }
      
      
      if (substrRight(pt,3)[1] == "x3p"){
        if (!is.null(pt)) {
          cx3p<- pt
        }
        output$text5 <- renderText({
          as.character(cx3p)
          paste0(length(cx3p), " x3p files being processed")
        })
        
        fpth<<- cx3p
        file1name<<- basename(cx3p)
        data2<<- loadData( "x3p",  fpth, input$profiley)
        
        output$text1 <- renderText({
          paste0(1, " file Loaded")
        })
        
      }
      
      
      
      
      
      if(input$ftype == 'x3pftype' && input$templatef == 'notemplate'){
        # bhad<<- data2
        input.info$Record1 <<- data2$feature.info
        input.info$Record2 <<- data2$general.info
        input.info$Record3 <<- data2$matrix.info
        
        info_len1<<- length(unlist(data2$feature.info))
        info_len2<<- length(unlist(data2$general.info))
        info_len3<<- length(unlist(data2$matrix.info))
        
      } else if(input$ftype == 'datftype' || input$templatef == 'usetemplate'){
        info_len1<<- length(unlist(input.info$Record1))
        info_len2<<- length(unlist(input.info$Record2))
        info_len3<<- length(unlist(input.info$Record3))
        
        # Setting Values of matrix dimensions by automatically using header.info
        input.info$Record1$Axes$CX$Increment <<- data2$header.info$obs_inc
        input.info$Record1$Axes$CY$Increment <<- data2$header.info$profile_inc
        
        input.info$Record3$MatrixDimension$SizeX <<- data2$header.info$num_obs_per_profile
        input.info$Record3$MatrixDimension$SizeY <<- data2$header.info$num_profiles
      }
      
      if(input$ftype == 'x3pftype'){
        if(input$templatef == 'notemplate'){
          print.tinf= "You have used Original Meta-Data"
        } else if(custom.template){
          print.tinf= "A Named Custom Template for Meta-Data has been Used"}
        else {
          print.tinf= "Default Template for Meta-Data has been used"
        }
      }
      else if(input$ftype == 'datftype'){
        if(custom.template){
          print.tinf= "A Named Custom Template for Meta-Data has been Used"}
        else {
          print.tinf= "Default Template for Meta-Data has been used"
        }
      }
      
      output$text3 <- renderText({
        str1 <- paste0("Number of profiles or Y dimension = ",as.character(data2$header.info$num_profiles))
        str2 <- paste0("Number of Observations per profile or X dimension = ",as.character(data2$header.info$num_obs_per_profile))
        str3 <- paste0("Profile Increments = ", as.character(data2$header.info$profile_inc))
        str4 <- paste0("Observation Increments = ",as.character(data2$header.info$obs_inc))
        str5 <- paste0("Profiley = ", as.character(input$profiley))
        str6 <- paste0("Creator as Read in from file = ", as.character(data2$general.info$Creator))
        str7 <- paste0( as.character(print.tinf))
        # str7 <- paste0( info_len1, "Automatically generated fields  of Feature type info for Updation/verification")
        # str8 <- paste0( info_len2, "Automatically generated fields of General Info for Updation/verification")
        # str9 <- paste0(info_len3, "Automatically generated fields of Matrix Info for Updation/verification")
        # HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, str9, sep = '<br/>'))
        HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
      })
      
      
      
      output$finfo.default <- renderUI({
        tmp1 <<- as.relistable(input.info$Record1)
        tmp1 <<- unlist(tmp1)
        lapply(1:(info_len1), function(i) {
          list(tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
               textInput(paste0(names(tmp1)[i]), label = paste0(names(tmp1)[i]), value = paste0(tmp1[i]))
          )
        }) #end of lapply
      }) # end of renderUI
      
      output$ginfo.default <- renderUI({
        tmp2 <<- as.relistable(input.info$Record2)
        tmp2 <<- unlist(tmp2)
        lapply(1:(info_len2), function(i) {
          list(tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
               textInput(paste0(names(tmp2)[i]), label = paste0(names(tmp2)[i]), value = paste0(tmp2[[i]]))
          )
        }) #end of lapply
      }) # end of renderUI
      
      output$minfo.default <- renderUI({
        tmp3 <<- as.relistable(input.info$Record3)
        tmp3 <<- unlist(tmp3)
        lapply(1:(info_len3), function(i) {
          list(tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
               textInput(paste0(names(tmp3)[i]), label = paste0(names(tmp3)[i]), value = paste0(tmp3[[i]]))
          )
        }) #end of lapply
      }) # end of renderUI
      
      hide("templatef")
      #hide("datftype")
      hide("profiley")
      hide("ftype")
      hide("getTemplate")
    }
  }) # Sngle Browse or Event 'fileget' closed
  
  observeEvent(input$write, {
    form <- reactive({
      tmp1_a <- as.relistable(input.info$Record1)
      tempor1<- sapply(names(tmp1), function(x) input[[x]])
      tmp1_a <- relist(tempor1, skeleton = tmp1_a)
      
      tmp3_a <- as.relistable(input.info$Record3)
      tempor3<- sapply(names(tmp3), function(x) input[[x]])
      tmp3_a <- relist(tempor3, skeleton = tmp3_a)
      
      tmp2_a <- as.relistable(input.info$Record2)
      tempor2<- sapply(names(tmp2), function(x) input[[x]])
      tmp2_a <- relist(tempor2, skeleton = tmp2_a)
      
      # Fields chosen for check are bound to be empty when fields not created
      temp.err1<-tryCatch(field.not.generated(tmp1_a$Revision),error = function(cond) return(FALSE))
      temp.err2<-tryCatch(field.not.generated(tmp2_a$Creator),error = function(cond) return(FALSE))
      temp.err3<-tryCatch(field.not.generated(tmp3_a$MatrixDimension$SizeZ),error = function(cond) return(FALSE))
      
      # If fields are empty use the template or the original info from the file
      # as chosen to write the file
      if(temp.err1 == TRUE){
        tmp1_a<- input.info$Record1
      }
      
      if(temp.err2 == TRUE){
        tmp2_a<- input.info$Record2
      }
      
      if(temp.err3 == TRUE){
        tmp3_a<- input.info$Record3
      }
      
      
      data2$general.info<<- tmp2_a
      data2$feature.info<<- tmp1_a
      data2$matrix.info<<- tmp3_a
      data2
    })
    
    #if(input$mode == 'singfile'){
    fileName <- sprintf("%s_%s-%s-%s_%s.x3p", date(Sys.time()), paste0(hour(Sys.time())), paste0(minute(Sys.time())), paste0(ceiling(second(Sys.time()))), as.character(strsplit(file1name, '.', fixed = TRUE)[[1]][1]))
    setwd(dirname(fpth))
    write_x3p(form(), fileName, profiley = input$profiley)
    #write_x3p(data2, fileName)
    # write_x3p(x = data2$surface.matrix, file = fileName,
    #           general.info = data2$general.info,
    #           feature.info = data2$feature.info,
    #           matrix.info = data2$matrix.info, profiley = input$profiley)
    setwd(appdir)
    
    # }
    # 
    
    # if(input$mode == 'batch'){
    #   form()$surface.matrix<- 
    #   
    #   count<- isolate(ft$a)
    #   #breact<-reactive({
    #   lapply(X = 1:count, FUN = function(i){batch.write(i)})
    #   #})
    # }
    
    output$text5 <- renderText({
      #paste0(names(persistent_data()$matrix.info))#, "  ", names(matrix_form()))
      #paste0(unlist(data2$general.info))
      #paste0(unlist(data2$feature.info))
      #paste0(fpth)
      # paste0(dim(data2$surface.matrix))
      # paste0(names(data2$general.info))#, "  ", names(matrix_form()))
      #as.character(input$as.name(paste0(names(unlist(tmp1))[i])))
      #paste0(
      #if(input$featInfo == 1) paste0("TRUE") else(paste0("FALSE"))
      #paste0(sapply(form()$feature.info, is.na))#, "   ", sum(sapply(form()$general.info, is.null)))
      "All files converted"
    })
    
    
  })
  
  
  
  ################################################################################
  ##################################Batch#########################################
  ################################################################################
  # if(input$mode == 'batch'){
  #   
  # }
  observeEvent(input$choosebatchtemplate,{
    
    template_path<<- tryCatch({choose.files(default= "", "Select Template")},error = function(e) {})
    
    if(tryCatch(field.not.generated(template_path), error = function(e) return(FALSE))){
      a121<<- xml2::read_xml("./defaultTemplateXML.xml")
      #winDialog(type = "ok", message = "Custom template not chosen. Using Default template")#, default= "")
      #showNotification("This is a notification.", duration = NULL, type = "warning")
      #session$sendCustomMessage(type = 'testmessage',
      #                          message = 'Custom template not chosen. Using Default template')
      
      showModal(modalDialog(title = "Important message",
                            "Custom template not chosen. Using Meta-Data from Default template",
                            easyClose = TRUE
      ))
      showNotification("Default Template Loaded", duration = 5, type = "message")
      custom.template<<- FALSE
      #input.info<<- as_list(a121)
      #backup.template<- as_list(a121)  
    }else{
      a121<<- xml2::read_xml(template_path)
      custom.template<<- TRUE
      showNotification("Custom Template Loaded", duration = 5, type = "message")
    }
    
    input.info<<- as_list(a121)
    # New addition because of as_list change
    input.info<<- input.info[[1]]
    
    output$fbatchinfo.template <- renderUI({
      tmp1 <<- as.relistable(input.info$Record1)
      tmp1 <<- unlist(tmp1)
      lapply(1:(info_len1), function(i) {
        list(tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
             textInput(paste0(names(tmp1)[i]), label = paste0(names(tmp1)[i]), value = paste0(tmp1[i]))
        )
      }) #end of lapply
    }) # end of renderUI
    
    output$gbatchinfo.template <- renderUI({
      tmp2 <<- as.relistable(input.info$Record2)
      tmp2 <<- unlist(tmp2)
      lapply(1:(info_len2), function(i) {
        list(tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
             textInput(paste0(names(tmp2)[i]), label = paste0(names(tmp2)[i]), value = paste0(tmp2[[i]]))
        )
      }) #end of lapply
    }) # end of renderUI
  })
  
  write_batch<- function(i){
    fpth<<- cdatbatch[i]
    file1namebatch<<- basename(cdatbatch[i])
    parse.data<- strsplit(file1namebatch[1], "-")
    parse.data<- trimws(parse.data[[1]])
    data2<<- loadData( fpth,  fpth, input$profileybatch)
    
    info_len1<<- length(unlist(input.info$Record1))
    info_len2<<- length(unlist(input.info$Record2))
    info_len3<<- length(unlist(input.info$Record3))
    
    # Setting Values of matrix dimensions by automatically using header.info
    input.info$Record1$Axes$CX$Increment <<- data2$header.info$obs_inc
    input.info$Record1$Axes$CY$Increment <<- data2$header.info$profile_inc
    
    input.info$Record3$MatrixDimension$SizeX <<- data2$header.info$num_obs_per_profile
    input.info$Record3$MatrixDimension$SizeY <<- data2$header.info$num_profiles
    
    input.info$Record2$Date<<- paste0(date(file.info(cdatbatch[i])$mtime),"T", strftime(file.info(cdatbatch[i])$mtime, format = "%H:%M:%S"))
    
    if(input$parse == 'parsetext'){
      if(length(parse.data)== 10) {
        p.name <- parse.data[10]
        p.model <- parse.data[5]
      }else if(length(parse.data)== 8){
        p.name <- parse.data[8]
        p.model <- parse.data[3]
      }
      
      input.info$Record2$Creator<<- paste0("CSAFE, ", gsub(x = p.name, pattern = "\\.dat", ""))
      input.info$Record2$Instrument$Model<<- paste0(p.model)
      input.info$Record2$Instrument$Manufacturer<<- "Sensofar"
      input.info$Record2$ProbingSystem$Type<<- "NonContacting"
      input.info$Record2$ProbingSystem$Identification<<- "20X"
      input.info$Record2$Comment<<- "NA: 0.60"
      if(p.model== "Sneox2") input.info$Record2$Instrument$Serial<<- "350402015" 
      if(p.model== "Sneox1") input.info$Record2$Instrument$Serial<<- "350262016" 
      data2$general.info<<- input.info$Record2
      data2$feature.info<<- input.info$Record1
      data2$matrix.info<<- input.info$Record3
    }
    
    if(input$parse == 'batchtemplate'){
      form <- reactive({
        tmp1_a <- as.relistable(input.info$Record1)
        tempor1<- sapply(names(tmp1), function(x) input[[x]])
        tmp1_a <- relist(tempor1, skeleton = tmp1_a)
        
        # tmp3_a <- as.relistable(input.info$Record3)
        # tempor3<- sapply(names(tmp3), function(x) input[[x]])
        # tmp3_a <- relist(tempor3, skeleton = tmp3_a)
        
        tmp2_a <- as.relistable(input.info$Record2)
        tempor2<- sapply(names(tmp2), function(x) input[[x]])
        tmp2_a <- relist(tempor2, skeleton = tmp2_a)
        
        # Fields chosen for check are bound to be empty when fields not created
        temp.err1<-tryCatch(field.not.generated(tmp1_a$Revision),error = function(cond) return(FALSE))
        temp.err2<-tryCatch(field.not.generated(tmp2_a$Creator),error = function(cond) return(FALSE))
        #temp.err3<-tryCatch(field.not.generated(tmp3_a$MatrixDimension$SizeZ),error = function(cond) return(FALSE))
        
        # If fields are empty use the template or the original info from the file
        # as chosen to write the file
        if(temp.err1 == TRUE){
          tmp1_a<- input.info$Record1
        }
        
        if(temp.err2 == TRUE){
          tmp2_a<- input.info$Record2
        }
        
        # if(temp.err3 == TRUE){
        tmp3_a<- input.info$Record3
        # }
        
        
        data2$general.info<<- tmp2_a
        data2$feature.info<<- tmp1_a
        data2$matrix.info<<- tmp3_a
        data2
      }) # Form closed  
      
      data2<<- isolate(form())
      # 
      # input.info$Record2<<- isolate(form()$general.info)
      # input.info$Record1<<- isolate(form()$feature.info)
      
      
      
    }# if input$parse = batchtemplate closed
    
    output$text3batch <- renderUI({
      str1 <- (paste0("Number of profiles or Y dimension = ",as.character(data2$header.info$num_profiles)))
      str2 <- (paste0("Number of Observations per profile or X dimension = ",as.character(data2$header.info$num_obs_per_profile)))
      str3 <- (paste0("Profile Increments = ", as.character(data2$header.info$profile_inc)))
      str4 <- (paste0("Observation Increments = ",as.character(data2$header.info$obs_inc)))
      str5 <- (paste0("Profiley = ", as.character(input$profileybatch)))
      str6 <- (paste0("Creator as Parsed/ mentioned in template  = ", as.character(data2$general.info$Creator)))
      str7 <- (paste0("Instrument Model", as.character(data2$general.info$Instrument$Model)))
      str8 <- (paste0("Date of dat file creation", as.character(data2$general.info$Date)))
      str9 <- (paste0("Writing file number ", i, " of ",ft))
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, str9, sep = '<br/>'))
    })
    fileName <- sprintf("%s.x3p", as.character(strsplit(file1namebatch, '.', fixed = TRUE)[[1]][1]))
    setwd(dirname(fpth))
    write_x3p(data2, fileName, profiley = input$profileybatch)
    setwd(appdir)
  }
  
  
  observeEvent(input$folderbatchget, {
    
    
    
    if(input$batchrecursive){
      pt<-list.dirs(choose.dir(default= "", "Select RecursiveFolder"), recursive=TRUE)
    }else{
      pt<- choose.dir(default= "", "Select Folder")
    }
    
    if (input$parse == 'parsetext'){
      a121<<- xml2::read_xml("./defaultTemplateXML.xml")
      input.info<<- as_list(a121)
      # New addition after as_list changed its functionality
      # it recognises a parent node now which it originally did not
      input.info<<- input.info[[1]]
    }
    
    #if (input$ftype == 'datftype'){
    if (!is.null(pt)) {
      cdatbatch<<- Sys.glob(paste0(pt,"\\*.dat"))
    }
    
    output$text5 <- renderText({
      as.character(cdatbatch)
      paste0(length(cdatbatch), " dat files being processed")
    })
    # observe({ft$a<- isolate(length(cdat$a))})
    ft<<- length(cdatbatch)
    
    output$text1 <- renderText({
      paste0(ft, " file Loaded")
    })
    
    
  })
  
  observeEvent(input$batchwrite, {
    
    count<- ft
    #breact<-reactive({
    lapply(X = 1:count, FUN = function(i){
      write_batch(i)
    })
    #})
    output$text5 <- renderText({
      "All files converted"
    })
  })
  
  # })
  
  
  
  
  
  
  
  #   
  # fileName <- sprintf("%s_%s-%s-%s_%s.x3p", date(Sys.time()), paste0(hour(Sys.time())), paste0(minute(Sys.time())), paste0(ceiling(second(Sys.time()))), as.character(strsplit(file1name, '.', fixed = TRUE)[[1]][1]))
  # setwd(dirname(fpth))
  # write_x3p(form(), fileName, profiley = input$profiley)
  # setwd(appdir)
  # 
  # 
  # output$text5 <- renderText({
  #        "All files converted"
  # })
  
  
  # })
  
}) #end of shinyServer


shinyApp(ui1, s1)




