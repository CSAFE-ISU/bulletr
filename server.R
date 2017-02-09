library(shiny)
library(dplyr)
library(tidyr)
library(bulletr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(randomForest)
library(RMySQL)

dbname <- "bullets"
user <- "buser"
password <- readLines("buser_pass.txt")
host <- "10.25.122.176"

options(shiny.maxRequestSize = 30*1024^2) 

shinyServer(function(input, output, session) {
    
    bullet1 <- reactive({
        withProgress(message = "Loading bullet data...", expr = {
                
            if (!is.null(input$file1) && input$choose1 == "Upload Image") {
                return(read_x3p(input$file1$datapath))    
            }
            
            con <- dbConnect(MySQL(), user = user, password = password,
                             dbname = dbname, host = host)
            
            bullet_metadata <- dbGetQuery(con, paste0("SELECT * FROM metadata WHERE land_id = ", input$choose1))
            bullet_data <- dbGetQuery(con, paste0("SELECT x,y,value FROM data WHERE land_id = ", input$choose1)) %>%
                arrange(y, x)
            
            attr(bullet_data, "info") <- list(num_profiles = bullet_metadata$num_profiles,
                                              num_obs_per_profile = bullet_metadata$num_obs_per_profile,
                                              profile_inc = bullet_metadata$profile_inc,
                                              obs_inc = bullet_metadata$obs_inc)
    
            dbDisconnect(con)
            
            return(unfortify_x3p(bullet_data))
        })
    })
    
    bullet2 <- reactive({
        withProgress(message = "Loading bullet data...", expr = {
            if (!is.null(input$file2) && input$choose2 == "Upload Image") {
                return(read_x3p(input$file2$datapath))    
            }
            
            con <- dbConnect(MySQL(), user = user, password = password,
                             dbname = dbname, host = host)
            
            bullet_metadata <- dbGetQuery(con, paste0("SELECT * FROM metadata WHERE land_id = ", input$choose2))
            bullet_data <- dbGetQuery(con, paste0("SELECT x,y,value FROM data WHERE land_id = ", input$choose2)) %>%
                arrange(y, x)
            
            attr(bullet_data, "info") <- list(num_profiles = bullet_metadata$num_profiles,
                                              num_obs_per_profile = bullet_metadata$num_obs_per_profile,
                                              profile_inc = bullet_metadata$profile_inc,
                                              obs_inc = bullet_metadata$obs_inc)
            
            dbDisconnect(con)
            
            return(unfortify_x3p(bullet_data))
        })
    })
    
    observeEvent(input$confirm0, {
        if (!is.null(bullet1()) && !is.null(bullet2())) updateCheckboxInput(session, "stage0", value = TRUE)
    })
    
    observeEvent(input$confirm00, {
        if (!is.null(bullet1()) && !is.null(bullet2())) {
            updateCheckboxInput(session, "stage0", value = TRUE)
            updateCheckboxInput(session, "stage00", value = TRUE)
        }
    })
    
    observeEvent(input$stage00, {
        if (input$confirm00 && input$stage00) {
            updateCheckboxInput(session, "stage1", value = TRUE)
            updateCheckboxInput(session, "stage11", value = TRUE)
        }
    }, priority = -1)
    
    observeEvent(input$stage11, {
        if (input$confirm00 && input$stage11) {
            updateCheckboxInput(session, "stage2", value = TRUE)
            updateCheckboxInput(session, "stage22", value = TRUE)
        }
    }, priority = -1)
    
    observeEvent(input$stage22, {
        if (input$confirm00 && input$stage22) {
            updateCheckboxInput(session, "stage3", value = TRUE)
            updateCheckboxInput(session, "stage33", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage33, {
        if (input$confirm00 && input$stage33) {
            updateCheckboxInput(session, "stage4", value = TRUE)
            updateCheckboxInput(session, "stage44", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage44, {
        if (input$confirm00 && input$stage44) {
            updateCheckboxInput(session, "stage5", value = TRUE)
            updateCheckboxInput(session, "stage55", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage55, {
        if (input$confirm00 && input$stage55) {
            updateCheckboxInput(session, "stage6", value = TRUE)
            updateCheckboxInput(session, "stage66", value = TRUE)            
        }
    }, priority = -1)

    theSurface <- reactive({
        if (is.null(bullet1()) || is.null(bullet2())) return(NULL)

        b1 <- bullet1()
        b2 <- bullet2()

        surf.b1 <- b1[[2]]
        surf.b2 <- b2[[2]]

        minrows <- min(nrow(surf.b1), nrow(surf.b2))

        surf.mat <- cbind(surf.b1[1:minrows,], surf.b2[1:minrows,])

        x_idx <- seq(1, nrow(surf.mat), by = 2)
        y_idx <- seq(1, ncol(surf.mat), by = 2)

        return(surf.mat[x_idx, y_idx])
    })

    observe({
        updateSliderInput(session, "xcoord1", max = ncol(theSurface()) / 2, value = ncol(theSurface()) / 4)
        updateSliderInput(session, "xcoord2", max = ncol(theSurface()), min = 1 + ncol(theSurface()) / 2, value = ncol(theSurface()) * 3 / 4)
    })

    output$trendPlot <- renderPlotly({
        if (is.null(theSurface())) return(NULL)

        p <- plot_ly(z = theSurface(), type = "surface", showscale = FALSE, lighting = list(ambient = input$ambient_lighting,
                                                                                             diffuse = input$diffuse_lighting,
                                                                                             specular = input$specular_lighting,
                                                                                             roughness = input$roughness_lighting,
                                                                                             fresnel = input$fresnel_lighting))
        p
    })
    
    observeEvent(input$stage0, {
        if (!is.null(theSurface()) && input$stage0) {
            withProgress(message = "Calculating CCF...", expr = {
                crosscut1 <- bulletCheckCrossCut("",
                                                 bullet = bullet1(),
                                                 xlimits = seq(25, 500, by = 25))
                
                crosscut2 <- bulletCheckCrossCut("",
                                                 bullet = bullet2(),
                                                 xlimits = seq(25, 500, by = 25))
                
                updateSliderInput(session, "xcoord1", value = crosscut1)
                updateSliderInput(session, "xcoord2", value = crosscut2 + ncol(theSurface()) / 2)
            })
        }
    })

    observeEvent(input$confirm, {
        updateCheckboxInput(session, "stage1", value = TRUE)
    })
    
    observeEvent(input$back, {
        updateCheckboxInput(session, "stage0", value = FALSE)
    })
    
    fortified1 <- reactive({
        if (is.null(bullet1()) || !input$stage1) return(NULL)
        
        bul <- bullet1()
        bul[[3]] <- "b1"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    fortified2 <- reactive({
        if (is.null(bullet2()) || !input$stage1) return(NULL)
        
        bul <- bullet2()
        bul[[3]] <- "b2"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    crosscut1 <- reactive({
        if (is.null(bullet1()) || !input$stage1) return(NULL)
        
        return(get_crosscut(bullet = bullet1(), x = input$xcoord1))
    })
    
    crosscut2 <- reactive({
        if (is.null(bullet2()) || !input$stage1) return(NULL)
        
        return(get_crosscut(bullet = bullet2(), x = input$xcoord2 - ncol(theSurface()) / 2))
    })
    
    observe({
        if (!is.null(fortified1()) && !is.null(fortified2())) {
            updateSliderInput(session, "bounds1", max = floor(max(fortified1()$y)), value = c(0, floor(max(fortified1()$y))))
            updateSliderInput(session, "bounds2", max = floor(max(fortified2()$y)), value = c(0, floor(max(fortified2()$y))))
        }
    })
    
    observeEvent(input$stage1, {
        if (!is.null(crosscut1()) && !is.null(crosscut2())) {
            
            withProgress(message = "Locating grooves...", expr = {
                groove1 <- get_grooves(crosscut1())
                groove2 <- get_grooves(crosscut2())
                
                updateSliderInput(session, "bounds1", value = groove1$groove)
                updateSliderInput(session, "bounds2", value = groove2$groove)
            })
        }
    })
    
    output$crosssection <- renderPlot({
        if (is.null(fortified1()) || is.null(fortified2())) return(NULL)
        
        fortified <- fortified1()
        fortified2 <- fortified2()
        
        myx <- unique(fortified$x)
        xval <- myx[which.min(abs(myx - input$xcoord1))]
        myx2 <- unique(fortified2$x)
        xval2 <- myx2[which.min(abs(myx2 - (input$xcoord2 - ncol(theSurface()) / 2)))]

        plotdat <- fortified %>%
            filter(x == xval) %>%
            select(-x) %>%
            full_join(
                fortified2 %>%
                    filter(x == xval2) %>%
                    select(-x)
            , by = c("y" = "y")) %>%
            rename(bullet1 = value.x, bullet2 = value.y) %>%
            gather(key = bullet, value = value, bullet1:bullet2)
        
        plotdat$include <- FALSE
        plotdat$include[plotdat$bullet == "bullet1"] <- (plotdat$y[plotdat$bullet == "bullet1"] >= input$bounds1[1] & plotdat$y[plotdat$bullet == "bullet1"] <= input$bounds1[2])
        plotdat$include[plotdat$bullet == "bullet2"] <- (plotdat$y[plotdat$bullet == "bullet2"] >= input$bounds2[1] & plotdat$y[plotdat$bullet == "bullet2"] <= input$bounds2[2])

        vline.data <- data.frame(zleft = c(input$bounds1[1], input$bounds2[1]),
                                 zright = c(input$bounds1[2], input$bounds2[2]),
                                 bullet = c("bullet1", "bullet2"))
        
        ggplot(data = plotdat, aes(x = y, y = value, alpha = include)) +
            facet_wrap(~bullet, nrow = 2) +
            geom_vline(aes(xintercept = zleft), colour = "blue", data = vline.data) +
            geom_vline(aes(xintercept = zright), colour = "blue", data = vline.data) +
            geom_line(size = 1) +
            xlim(c(0, max(plotdat$y))) +
            theme_bw()
    })
    
    observeEvent(input$confirm2, {
        updateCheckboxInput(session, "stage2", value = TRUE)
    })
    
    observeEvent(input$back2, {
        updateCheckboxInput(session, "stage1", value = FALSE)
    })
    
    loess1 <- reactive({
        if (is.null(crosscut1()) || !input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut1(), groove = list(groove = input$bounds1), span = input$span))
    })
    
    loess2 <- reactive({
        if (is.null(crosscut2()) || !input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut2(), groove = list(groove = input$bounds2), span = input$span))
    })
    
    processed1 <- reactive({
        if (is.null(fortified1()) || !input$stage2) return(NULL)
        
        myx <- unique(fortified1()$x)
        xval <- myx[which.min(abs(myx - input$xcoord1))]
        
        processBullets(bullet = bullet1(), name = "b1", x = xval, grooves = input$bounds1)
    })
    
    processed2 <- reactive({
        if (is.null(fortified2()) || !input$stage2) return(NULL)
        
        myx <- unique(fortified2()$x)
        xval <- myx[which.min(abs(myx - (input$xcoord2  - ncol(theSurface()) / 2)))]
        
        processBullets(bullet = bullet2(), name = "b2", x = xval, grooves = input$bounds2)
    })
    
    smoothed <- reactive({
        if (is.null(processed1()) || is.null(processed2())) return(NULL)
        
        bullets_processed <- list(b1 = processed1(), b2 = processed2())
        
        result <- bullets_processed %>% bind_rows %>% bulletSmooth(span = input$span)
        result$bullet <- c(rep("b1", nrow(processed1())), rep("b2", nrow(processed2())))
        
        return(result)
    })
    
    output$loess1 <- renderPlot({
        if (is.null(loess1()) || is.null(smoothed())) return(NULL)
        
        withProgress(message = "Loading plots...", {
            p1 <- qplot(y, l30, data = filter(smoothed(), bullet == "b1"), geom = "line") +
                theme_bw()
            grid.arrange(loess1()$fitted, p1, ncol = 2)
        })
    })
    
    output$loess2 <- renderPlot({
        if (is.null(loess2()) || is.null(smoothed())) return(NULL)
        
        withProgress(message = "Loading plots...", {
            p2 <- qplot(y, l30, data = filter(smoothed(), bullet == "b2"), geom = "line") +
                theme_bw()
            grid.arrange(loess2()$fitted, p2, ncol = 2)
        })
    })
    
    observeEvent(input$confirm3, {
        updateCheckboxInput(session, "stage3", value = TRUE)
    })
    
    observeEvent(input$back3, {
        updateCheckboxInput(session, "stage2", value = FALSE)
    })
    
    myalign <- reactive({
        if (is.null(smoothed())) return(NULL)

        bulletAlign(data = smoothed())
    })
    
    observeEvent(input$stage3, {
        if (!is.null(myalign())) {
            withProgress(message = "Determining alignment...", expr = {
                updateSliderInput(session, "alignment", value = myalign()$lag)
            })
        }
    })
    
    chosenalign <- reactive({
        if (is.null(myalign())) return(NULL)
        
        alignval <- round(input$alignment / 1.5625, digits = 0) * 1.5625
        
        chosen <- myalign()
        chosen$lag <- alignval
        chosen$bullets$y[chosen$bullets$bullet == "b2"] <- chosen$bullets$y[chosen$bullets$bullet == "b2"] - min(chosen$bullets$y[chosen$bullets$bullet == "b2"]) + chosen$lag
        
        return(chosen)
    })
    
    output$alignment <- renderPlot({
        if (is.null(chosenalign())) return(NULL)
            
        mydat <- chosenalign()$bullets
        
        qplot(y, l30, data = mydat, geom = "line", colour = bullet, alpha = I(0.8)) +
            theme(legend.position = "bottom") +
            theme_bw()
    })
    
    observeEvent(input$confirm4, {
        updateCheckboxInput(session, "stage4", value = TRUE)
    })
    
    observeEvent(input$back4, {
        updateCheckboxInput(session, "stage3", value = FALSE)
    })
    
    peaks1 <- reactive({
        if (is.null(chosenalign()) || !input$stage4) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        return(get_peaks(subset(lofX, bullet == "b1"), smoothfactor = input$smoothfactor))
    })
    
    peaks2 <- reactive({
        if (is.null(chosenalign()) || !input$stage4) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        return(get_peaks(subset(lofX, bullet == "b2"), smoothfactor = input$smoothfactor))
    })
    
    output$peaks1 <- renderPlot({
        if (is.null(peaks1())) return(NULL)
        
        return(peaks1()$plot)
    })
    
    output$peaks2 <- renderPlot({
        if (is.null(peaks2())) return(NULL)
        
        return(peaks2()$plot)
    })

    CMS <- reactive({
        if (is.null(peaks1()) || is.null(peaks2())) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        peaks1 <- peaks1()
        peaks2 <- peaks2()
        
        peaks1$lines$bullet <- "b1"
        peaks2$lines$bullet <- "b2"
        
        lines <- striation_identify(peaks1$lines, peaks2$lines)
        maxCMS <- maxCMS(lines$match == TRUE)
        list(maxCMS = maxCMS, ccf = bAlign$ccf, lag = bAlign$lag, 
             lines = lines, bullets = lofX)
    })

    features <- reactive({
        if (is.null(CMS())) return(NULL)

        res <- CMS()

        lofX <- res$bullets
        aligned <- chosenalign()
        b12 <- unique(lofX$bullet)

        subLOFx1 <- subset(aligned$bullets, bullet==b12[1])
        subLOFx2 <- subset(aligned$bullets, bullet==b12[2])

        ys <- intersect(subLOFx1$y, subLOFx2$y)
        idx1 <- which(subLOFx1$y %in% ys)
        idx2 <- which(subLOFx2$y %in% ys)
        distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=TRUE)
        distr.sd <- sd(subLOFx1$val, na.rm=TRUE) + sd(subLOFx2$val, na.rm=TRUE)
        km <- which(res$lines$match)
        knm <- which(!res$lines$match)
        if (length(km) == 0) km <- c(length(knm)+1,0)
        if (length(knm) == 0) knm <- c(length(km)+1,0)
        # browser()
        # feature extraction

       signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))

       data.frame(ccf=res$ccf, lag=res$lag,
                  D=distr.dist,
                  sd.D = distr.sd,
                  b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
                  #num.matches = sum(res$lines$match),
                  signature.length = signature.length,
                  matches.per.y = sum(res$lines$match) / signature.length,
                  #num.mismatches = sum(!res$lines$match),
                  mismatches.per.y = sum(!res$lines$match) / signature.length,
                  #cms = res$maxCMS,
                  cms.per.y = res$maxCMS / signature.length,
                  #cms2 = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match),
                  cms2.per.y = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / signature.length,
                  #non_cms = bulletr::maxCMS(!res$lines$match),
                  non_cms.per.y = bulletr::maxCMS(!res$lines$match) / signature.length,
                  #left_cms = max(knm[1] - km[1], 0),
                  left_cms.per.y = max(knm[1] - km[1], 0) / signature.length,
                  #right_cms = max(km[length(km)] - knm[length(knm)],0),
                  right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / signature.length,
                  #left_noncms = max(km[1] - knm[1], 0),
                  left_noncms.per.y = max(km[1] - knm[1], 0) / signature.length,
                  #right_noncms = max(knm[length(knm)]-km[length(km)],0),
                  right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / signature.length,
                  #sumpeaks = sum(abs(res$lines$heights[res$lines$match])),
                  sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / signature.length
        )
    })

    output$features <- renderDataTable({
        if (is.null(features())) return(NULL)

        result <- as.data.frame(t(features()))
        result <- cbind(feature = rownames(result), result)
        names(result)[2] <- "value"
        
        clean_result <- result %>%
            filter(feature %in% c("ccf", "D", "signature.length", "matches.per.y",
                                  "mismatches.per.y", "cms.per.y", "non_cms.per.y",
                                  "sumpeaks.per.y")) %>%
            mutate(feature = c("CCF", "D", "Signature Length in Millimeters", "Matches Per Millimeter",
                               "Mismatches Per Millimeter", "CMS Per Millimeter",
                               "Non-CMS Per Millimeter", "Peak Sum Per Millimeter"),
                   value = c(as.numeric(as.character(value[1:2])), as.numeric(as.character(value[3])) / 1000 * 1.5625, as.numeric(as.character(value[4:8])) / 1.5625 * 1000))
        
        clean_result$value <- sprintf("%.4f", clean_result$value)
        
        return(clean_result)
    })
    
    observeEvent(input$confirm5, {
        updateCheckboxInput(session, "stage5", value = TRUE)
    })
    
    observeEvent(input$back5, {
        updateCheckboxInput(session, "stage4", value = FALSE)
    })
    
    observeEvent(input$confirm6, {
        updateCheckboxInput(session, "stage6", value = TRUE)
    })
    
    observeEvent(input$back6, {
        updateCheckboxInput(session, "stage5", value = FALSE)
    })

    output$rfpred <- renderText({
        if (is.null(features())) return(NULL)

        features <- features()
        features$b1 <- gsub(".x3p", "", basename(as.character(features$b1)))
        features$b2 <- gsub(".x3p", "", basename(as.character(features$b2)))
        features$span <- span

        includes <- setdiff(names(features), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))

        load("data/rf.RData")

        matchprob <- sprintf("%.4f", predict(rtrees, newdata = features[,includes], type = "prob")[,2])
        if (matchprob == "0.0000") matchprob <- "< .0001" else if (matchprob == "1.0000") matchprob <- "> .9999"

        return(paste0("The probability of a match is ", matchprob))
    })
    
    observeEvent(input$restart, {
        updateCheckboxInput(session, "stage0", value = FALSE)
        updateCheckboxInput(session, "stage1", value = FALSE)
        updateCheckboxInput(session, "stage2", value = FALSE)
        updateCheckboxInput(session, "stage3", value = FALSE)
        updateCheckboxInput(session, "stage4", value = FALSE)
        updateCheckboxInput(session, "stage5", value = FALSE)
        updateCheckboxInput(session, "stage6", value = FALSE)
        
        updateCheckboxInput(session, "stage00", value = FALSE)
        updateCheckboxInput(session, "stage11", value = FALSE)
        updateCheckboxInput(session, "stage22", value = FALSE)
        updateCheckboxInput(session, "stage33", value = FALSE)
        updateCheckboxInput(session, "stage44", value = FALSE)
        updateCheckboxInput(session, "stage55", value = FALSE)
    })

})
