###########################################
###### server.R web app for PF map  #######
###### A simiple proof of concept   #######
###### application. Accept a CSV    #######
###### file, ask user what are the  #######
###### X, Y coordinate and element  #######
###### and the make a continuous map#######
###### and post it on the GglemapAPI#######
###########################################

library(shiny)
library(sp)
library(gstat)


shinyServer(function(input, output){
  
  data <- reactive({
   dat <- input$dat
   if (is.null(dat))
    return(NULL)
  read.csv(dat$datapath) # removed unneccesary arguments, header=input$header, sep=input$sep, quote=input$quote
  })

  grid <- reactive({
    grd <- input$grd
    if (is.null(grd))
      return(NULL)
    read.csv(grd$datapath)
  })
  
  output$selectUIdatX <- renderUI({ 
    if (is.null(data())) return(NULL)
    selectInput("X", "Select Easting or X", as.list(names(data())) )
 
  })
  
  output$selectUIdatY <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("Y", "Select Northing or Y", as.list(names(data())) )
   
  })
    
  output$selectUIdatZ <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("Z", "Select target variable", as.list(names(data())) )
    
  })
  
  output$ESDAPlots <- renderPlot({
    library(ggplot2)
    library(grid)
    library(gridExtra)
    
    if (is.null(data())) return(NULL)
    if (is.null(grid())) return(NULL)
    
    data <- data()
    grid <- grid()
    
    df <- data.frame(Xvar = data[[input$X]], Yvar = data[[input$Y]], Zvar = data[[input$Z]])
    
    plot <- ggplot(df, aes(Zvar)) 
    plot1 <- ggplot(df, aes(Zvar, Zvar))
    plot2 <- ggplot(df, aes(Xvar, Yvar, Zvar))
    
    theHisto <- plot + geom_bar(fill="white", colour="darkgreen")+ xlab(names(data[input$Z])) +
                                ggtitle(paste("Histogram for", (names(data[input$Z])), sep = " "))
    
    theScatter <- plot2 + geom_point(aes(alpha = Zvar))+ theme_bw()+
                          xlab("Easting") +ylab("Northing")+  labs(colour = names(data[input$Z])) +
                          ggtitle(paste("Scatterplot for", (names(data[input$Z])), "ppm",sep = " "))
    
    theBoxplot <- plot1 + geom_boxplot(notch = T, outlier.colour = "green", outlier.size = 3)+
                                xlab(names(data[input$Z])) + coord_flip() +
                                ggtitle(paste("Notch boxplot for", (names(data[input$Z])), sep = " "))
    
    
    theGraph <- grid.arrange(theHisto, theBoxplot, theScatter,ncol = 2)
    print(theGraph)                                                          
  })
  
  output$MapIdwE1 <- renderPlot({
   
           library(gstat)
           
           if (is.null(data())) return(NULL)
           if (is.null(grid())) return(NULL)
           data <- data()
           grid <- grid()
           # Preparing for interpolation
           coordinates(data) <- data[c(input$X, input$Y)]
           coordinates(grid) <- grid[c("x" , "y")]
           var <- data[[input$Z]]
           e1.idw <- krige(var ~ 1, data, grid)
           TheGraph <- spplot(e1.idw["var1.pred"], main = 
                                paste((names(data[input$Z]@data)) ,  "IDW Interpolated Map", sep =" "))
           print(TheGraph)
    })

  output$MapOKE1 <- renderPlot({
    # if (inFile1 & inFile2) {
    library(gstat)
    library(grid)
    library(ggplot2)
    library(gridExtra)
    
    if (is.null(data())) return(NULL)
    if (is.null(grid())) return(NULL)
    data <- data()
    grid <- grid()
    # Preparing for interpolation
    coordinates(data) <- data[c(input$X, input$Y)]
    coordinates(grid) <- grid[c("x" , "y")]
    var <- data[[input$Z]]
    vgm <- variogram(var~1,data)
    fit <- fit.variogram(vgm,model=vgm(1, "Sph", 900, 1))
    kriged <- krige(var~1, data, grid, model=fit)
    
    df <- as.data.frame(kriged)
    br <- seq(min(df$var1.pred), max(df$var1.pred), len=8)
    
    theMap <- ggplot(data=df) + theme_bw()+
      geom_tile(aes(x, y, fill=var1.pred)) + 
      scale_fill_gradient(low="red", high="green", 
                          breaks=br, labels=sprintf("%.02f", br), 
                          guide=guide_colorbar(title=NULL, nbin=100, barheight=unit(0.75, "npc"), label.hjust=1)) + 
      scale_x_continuous(expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      ggtitle(paste("Kriging map for", names(data[input$Z]), sep= " "))
    
    theVario <- plot(vgm, fit, 
                     main= paste("Omnidirectional variogram for", names(data[input$Z]), "ppm", sep= " "))
    
    theGraph <- grid.arrange(theMap, theVario,ncol = 2)
    
    print(theGraph)   

  })
})