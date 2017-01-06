library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    headerPanel("Bullet Matching Algorithm"),
    
    sidebarLayout(
        sidebarPanel(width = 3,
            useShinyjs(),
            tags$head(tags$style("#info{font-size: 18px;}")),
            
            hidden(checkboxInput("stage0", "Stage 0")),
            hidden(checkboxInput("stage1", "Stage 1")),
            hidden(checkboxInput("stage2", "Stage 2")),
            hidden(checkboxInput("stage3", "Stage 3")),
            hidden(checkboxInput("stage4", "Stage 4")),
            hidden(checkboxInput("stage5", "Stage 5")),
            hidden(checkboxInput("stage6", "Stage 6")),
            
            hidden(checkboxInput("stage00", "Stage 0")),
            hidden(checkboxInput("stage11", "Stage 1")),
            hidden(checkboxInput("stage22", "Stage 2")),
            hidden(checkboxInput("stage33", "Stage 3")),
            hidden(checkboxInput("stage44", "Stage 4")),
            hidden(checkboxInput("stage55", "Stage 5")),
            hidden(checkboxInput("stage66", "Stage 6")),

            conditionalPanel(condition = "!input.stage0 || input.stage5",
                 h4("Stage 0 Options"),
                 
                 hr(),
                 
                 selectizeInput("choose1", "Choose First Land", choices = c(dir("images"), "Upload Image")),
                 
                 conditionalPanel(condition = "input.choose1 == 'Upload Image'",
                    fileInput("file1", "First Bullet Land")                 
                 ),
                 
                 selectizeInput("choose2", "Choose Second Land", choices = c(dir("images"), "Upload Image"), selected = dir("images")[2]),
                 
                 conditionalPanel(condition = "input.choose2 == 'Upload Image'",
                    fileInput("file2", "Second Bullet Land")              
                 ),

                 hr(),
                 
                 h4("Step-By-Step Mode"),
                 helpText("Press the following button to begin the step-by-step version of the algorithm, where each parameter can be tweaked according to your liking."),
                 actionButton("confirm0", "Confirm Lands", icon = icon("check")),
                 
                 hr(),
                 
                 h4("Easy Mode"),
                 helpText("Press the following button to automatically use all the default parameters, and get a predicted probability of a match quickly."),
                 actionButton("confirm00", "Confirm Lands", icon = icon("check"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage0 && !input.stage1 || input.stage5",
                h4("Stage 1 Options"),
                
                hr(),
                
                sliderInput("xcoord1", "X Coordinate (First Land)", min = 1, max = 251, value = 136, step = 1),
                sliderInput("xcoord2", "X Coordinate (Second Land)", min = 252, max = 502, value = 386, step = 1),
                
                hr(),
                
                actionButton("confirm", "Confirm Coordinates", icon = icon("check")),
                
                hr(),
                
                actionButton("back", "Back to Stage 0", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage1 && !input.stage2 || input.stage5",
                h4("Stage 2 Options"),
                
                hr(),
                
                sliderInput("bounds1", "Coordinate Bounds 1", min = 0, max = 2400, value = c(0, 2400)),
                sliderInput("bounds2", "Coordinate Bounds 2", min = 0, max = 2400, value = c(0, 2400)),
                
                hr(),
                
                actionButton("confirm2", "Confirm Bounds", icon = icon("check")),
                
                hr(),
                
                actionButton("back2", "Back to Stage 1", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage2 && !input.stage3 || input.stage5",
                h4("Stage 3 Options"),
                
                hr(),
                
                sliderInput("span", "Loess Span", min = 0.01, max = 0.99, value = 0.03, step = 0.01),
                
                hr(),
                
                actionButton("confirm3", "Confirm Span", icon = icon("check")),
                
                hr(),
                
                actionButton("back3", "Back to Stage 2", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage3 && !input.stage4 || input.stage5",
                h4("Stage 4 Options"),
                
                hr(),
                
                numericInput("alignment", "Alignment", min = -1000, max = 1000, step = 1.5625, value = 0),
                
                hr(),
                
                actionButton("confirm4", "Confirm Alignment", icon = icon("check")),
                
                hr(),
                
                actionButton("back4", "Back to Stage 3", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage4",
                 h4("Stage 5 Options"),
                 
                 hr(),
                 
                 sliderInput("smoothfactor", "Smoothing Factor", min = 1, max = 100, value = 35, step = 1),
                 
                 hr(),
                 
                 actionButton("confirm5", "Confirm Smoothing", icon = icon("check")),
                 
                 hr(),
                 
                 actionButton("back5", "Back to Stage 4", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5",
                             h4("Stage 6 Options"),
                             
                             hr(),
                             
                             actionButton("confirm6", "Confirm Features", icon = icon("check")),
                             
                             hr(),
                             
                             actionButton("back6", "Back to Stage 5", icon = icon("backward"))
            ),
            
            hidden(
                h4("Lighting Options"),
                sliderInput("subsample", "Subsample Factor", min = 1, max = 20, value = 2),
                sliderInput("ambient_lighting", "Ambient Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("diffuse_lighting", "Diffuse Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("specular_lighting", "Specular Lighting", min = 0, max = 2, step = 0.05, value = 0.05),
                sliderInput("roughness_lighting", "Roughness Lighting", min = 0, max = 1, step = 0.1, value = 0.5),
                sliderInput("fresnel_lighting", "Fresnel Lighting", min = 0, max = 5, step = 0.1, value = 0.2)
            )
        ),
        
        mainPanel(width = 9,
              conditionalPanel(condition = "input.stage5",
                   h2("Predicted Probability"),
                   hr(),
                   div(id = "info", HTML("We use these features to train a Random Forest to help differentiate between a match and a non-match. Using the forest, we predict on the features you just extracted. Your predicted probability of a match is given below.")),
                   
                   h3(textOutput("rfpred")),
                   
                   hr(),
                   
                   actionButton("restart", "Restart Algorithm", icon = icon("refresh")),
                   
                   hr()
              ),
            conditionalPanel(condition = "!input.stage0 || input.stage5",
                 h2("Stage 0: Preliminary Information"),
                 hr(),
                 div(id = "info", HTML("This app will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun barrel. We compare at the bullet land level.<br><br><b>To begin, choose or upload two .x3p files representing the two bullet lands you wish to compare.</b><br><br>We have provided three example files from the Hamby study which you may use - The two lands from Barrel 1 are a match, but Barrel 2 does not match either.<br><br>This work is part of the <a href='http://forensic.stat.iastate.edu'>Center for Statistics and Applications in Forensic Evidence</a> (CSAFE) at <a href='http://www.iastate.edu'>Iowa State University</a>. These procedures are fully open-source and transparent. The code producing the results is given on each page. For more details on the underlying code, please see the <a href='https://github.com/heike/x3prplus'>GitHub repository</a> for the companion R package <b>x3prplus</b>, as well as our publication <a href='http://arxiv.org/abs/1601.05788'>Automatic Matching of Bullet Lands</a>.<br><hr><a href='http://www.erichare.net' target='_blank'><b>Eric Hare</b></a>, Department of Statistics, Iowa State University, Ames IA 50011-1210, United States.<br><b>Heike Hofmann</b>, Department of Statistics, Iowa State University, Ames IA 50011-1210, United States.<br><b>Alicia Carriquiry</b>, Department of Statistics, Iowa State University, Ames IA 50011-1210, United States."))
            ),
            conditionalPanel(condition = "input.stage0 && !input.stage1 || input.stage5",
                 h2("Stage 1: Finding a Stable Region"),
                 hr(),
                 div(id = "info", HTML("Below you will find surface topologies of the two bullet lands you have uploaded. You can rotate, pan, zoom, and perform a number of other functions to examine the surfaces.<br><br>Our goal is to find a <b>stable region</b>. We want an area of the bullet where there is minimal noise or tank rash, but plenty of pronounced striation markings.<br><br>We step through cross-sections of each land at a fixed step size, and uses the CCF (cross-correlation function) to determine stability (a high CCF means that subsequent cross-sections are similar to each other). We begin this procedure near the area where striation markings are typically most pronounced.<br><br><b>We have automatically identified what is believed to be a stable region.</b> You may choose the location to take a cross-section if the algorithm's choice is not satisfactory."))           
            ),
            conditionalPanel(condition = "input.stage1 && !input.stage2 || input.stage5",
                 h2("Stage 2: Removing Grooves"),
                 hr(),
                 div(id = "info", HTML("The cross-sections you have taken are shown below. Our next goal will be to remove the grooves, which contain no relevant information for matching, and greatly exceed the size of a typical striation mark.<br><br>We use a double-pass smoothing method to determine the location of the grooves. <b>We have again attempted to locate the grooves for you</b>, but you may define them yourself. As you adjust the sliders, the plot will automatically update.")),
                 hr(),
                 
                 plotOutput("crosssection")
            ),
            conditionalPanel(condition = "input.stage2 && !input.stage3 || input.stage5",
                 h2("Stage 3: Removing Global Structure"),
                 hr(),
                 div(id = "info", HTML("We have removed the grooves, but the global structure of the cross-section dominates the overall appearance, making striae more difficult to locate.<br><br>We are going to fit a loess regression to model this structure. The loess regression includes a span parameter which adjusts the amount of smoothing used. Different values will yield different output. We default to a span of 0.03, but this may be adjusted as desired.")),
                 hr(),
                 
                 plotOutput("loess1"),
                 plotOutput("loess2")
            ),
            conditionalPanel(condition = "input.stage3 && !input.stage4 || input.stage5",
                 h2("Stage 4: Aligning Signatures"),
                 hr(),
                 div(id = "info", HTML("The residuals from the loess fit we have extracted in the previous stage are called the bullet <b>signatures</b>. They will form the basis for the rest of the analysis.<br><br>Because the signatures are defined by the residuals, the peaks and valleys visible in this plot represent the striation markings we are looking for. In order to make matching easier, our next step is to align the two signatures. We suggest an optimal alignment, but it can be adjusted if necessary.")),
                 
                 plotOutput("alignment")
            ),
            conditionalPanel(condition = "input.stage4",
                             h2("Stage 5: Peaks and Valleys"),
                             hr(),
                             div(id = "info", HTML("With aligned signatures, we now turn our attention to determining what constitutes a peak or a valley. Since there is a lot of noise, this step involves one more smoothing pass.<br><br>We can specify a smoothing window, called the <b>smoothing factor</b>, as the number of neighbors to include in the window. For instance, a value of 16 would mean that the nearest 16 points, spanning 16 * 1.5625 = 25 micrometers, would be included.")),
                             
                             plotOutput("peaks1"),
                             plotOutput("peaks2")
            ),
            conditionalPanel(condition = "input.stage5",
                 h2("Stage 6: Extract Features"),
                 hr(),
                 div(id = "info", HTML("We now have smoothed, aligned bullet signatures with associated peaks and valleys. This gives us a number of features we can extract.<br><br>At this point, there is really nothing left to configure about the algorithm. The features extracted are displayed below. The definitions of each can be found in Hare 2016. Press Confirm Features when you are ready to get your predicted probability of a match.")),
                 
                 dataTableOutput("features")
            ),
            
            hr(),
            
            h2("Bullet Land Surfaces"),
            plotlyOutput("trendPlot", height = "700px"),

            hr()
        )
    )
))
