library(e1071)

mae <- function(pred,act)mean(abs(act-pred))

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

setwd("~/Dropbox/Monero/baggingMonero/")

#download("https://coinmetrics.io/data/all.zip", dest="dataset.zip", mode="wb") 
#unzip ("dataset.zip", exdir = "./")
#file.remove("dataset.zip")

file_vec <- list.files(pattern = ".csv")



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Predicción de cambio de signo de precios de Criptomonedas"),
   h5("Cómo usar: Se pueden afinar distintas variables respuesta y número de datos de entrenamiento en función de la tabla que se muestra. 1 es el caso cuando el día de mañana la variable respuesta aumentará y 0 cuando bajará"),
   h5("Creador: Rodrigo Díaz Lupanow"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("bins",
                     "Moneda:",
                     choices = file_vec,
                     selected = "xmr.csv"),
         sliderInput("tama","Número de días para entrenar",min = 100,max = 2500,value = 800),
         numericInput("col","Target (index de la columna)",value = 2)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("distPlot"),
         h5("Accuracy"),
         textOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderTable({
     
     expl<- file_vec[-which(file_vec==input$bins)]
     
     l <- lapply(expl, read.csv)
     
     nd=input$tama
     
     crip_to_remove <- sapply(l, function(x) nrow(x) < nd)
     
     l <- l[!crip_to_remove]
     l <- lapply(l, function(x) as.data.frame(x[nrow(x)-(nrow(x)-nd):nrow(x),  ]))
     l <- lapply(l, function(x) as.data.frame(x[,-1  ]))
     
     # create data.frame with input variables
     df_exp <- do.call(cbind, l)
     

     t <- read.csv(input$bins)
    
     ifelse(ncol(t)>1,target <- t[,input$col],target <- t[input$col])
     target <-  na.omit(target)
     target <- target[NROW(target)-(NROW(target)-nd):NROW(target)]
     
    
     
     x<- data.frame(target,df_exp)
     x<-x[sapply(x, function(x) !any(is.na(x)))] 
     x<-lapply(x, normalize)
     x<-as.data.frame(x)
     x<-na.omit(x)
     
     outcome = x$target
     
     # Create a binary outcome variable, the sign of change between today and tomorrow
     #vamos a transformar nuestra variable respuesta, en nuestro caso, el signo de cambio entre el hoy y mañana
     
     
     out<- vector()
     
     for (i in 1:NROW(x$target)) {
       
       
       out[i]<-ifelse(outcome[i]*1<outcome[i+1],1,0)
       
       
     }
     
  
     #hemos cambiado de variable continua a binomial o de dos valores (o categorias)
     
     # Create a dataframe to contain our explanatory variables.
     data = subset(x, select = -x$target)
     data[is.na(data)] <- 0
     
     # Set a seed for reproducibility in this random sampling.
     set.seed(1)
     
     # Reduce to a dataset of 150 observations to speed up model fitting.
     train_obs = sample(nrow(data), floor( nrow(data)/3))
     
     # X is our training sample.
     X_train = data[train_obs, ]
     
     # Create a holdout set for evaluating model performance.
     # Note: cross-validation is even better than a single holdout sample.
     X_holdout = data[-train_obs, ]
     
     
     outcome_bin = out
     
     Y_train = outcome_bin[train_obs]
     Y_holdout = outcome_bin[-train_obs]
     
     # Review the outcome variable distribution.CLASIFICACIOON
     
     table(Y_train)
     

     # Set the seed for reproducibility.
     set.seed(1)
     

     # Fit lasso model.
     sl <- svm(y = Y_train, x = X_train,scale = T,type  ="C-classification",kernel = "polynomial" )
     
 
     pred <- predict(sl, X_holdout, onlySL = T)

     
     cM<-confusionMatrix(as.factor(pred[-1200]),as.factor(Y_holdout[-1200]))
     
     cM$table
     
   })
   
   
   output$text <- renderText({
     
     expl<- file_vec[-which(file_vec==input$bins)]
     
     l <- lapply(expl, read.csv)
     
     nd=input$tama
     
     crip_to_remove <- sapply(l, function(x) nrow(x) < nd)
     
     l <- l[!crip_to_remove]
     l <- lapply(l, function(x) as.data.frame(x[nrow(x)-(nrow(x)-nd):nrow(x),  ]))
     l <- lapply(l, function(x) as.data.frame(x[,-1  ]))
     
     # create data.frame with input variables
     df_exp <- do.call(cbind, l)
     
     t <- read.csv(input$bins)
     
     ifelse(ncol(t)>1,target <- t[,input$col],target <- t[input$col])
     target <-  na.omit(target)
     target <- target[NROW(target)-(NROW(target)-nd):NROW(target)]
     
     x<- data.frame(target,df_exp)
     x<-x[sapply(x, function(x) !any(is.na(x)))] 
     x<-lapply(x, normalize)
     x<-as.data.frame(x)
     x<-na.omit(x)
     
     outcome = x$target
     
     # Create a binary outcome variable, the sign of change between today and tomorrow
     #vamos a transformar nuestra variable respuesta, en nuestro caso, el signo de cambio entre el hoy y mañana
     
     
     out<- vector()
     
     for (i in 1:NROW(x$target)) {
       
       
       out[i]<-ifelse(outcome[i]*1<outcome[i+1],1,0)
 
     }
     
     #hemos cambiado de variable continua a binomial o de dos valores (o categorias)
     
     # Create a dataframe to contain our explanatory variables.
     data = subset(x, select = -x$target)
     data[is.na(data)] <- 0
     
     # Set a seed for reproducibility in this random sampling.
     set.seed(1)
     
     # Reduce to a dataset of 150 observations to speed up model fitting.
     train_obs = sample(nrow(data), floor( nrow(data)/3))
     
     # X is our training sample.
     X_train = data[train_obs, ]
     
     # Create a holdout set for evaluating model performance.
     # Note: cross-validation is even better than a single holdout sample.
     X_holdout = data[-train_obs, ]
     
     outcome_bin = out
     
     Y_train = outcome_bin[train_obs]
     Y_holdout = outcome_bin[-train_obs]
     
     # Review the outcome variable distribution.CLASIFICACIOON
     
   
     # Set the seed for reproducibility.
     set.seed(1)
     
     
     # Fit lasso model.
     sl <- svm(y = Y_train, x = X_train,scale = T,type  ="C-classification",kernel = "polynomial" )
     
     
     pred <- predict(sl, X_holdout, onlySL = T)

     cM<-confusionMatrix(as.factor(pred[-1200]),as.factor(Y_holdout[-1200]))
     
     cM$overall[1]
     
     
     
     
     
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)


