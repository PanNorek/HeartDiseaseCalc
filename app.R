library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape)
library(gridExtra)
library(fastDummies)
library(randomForest)
require(tree)
require(caTools)
library(rattle)
library(caret)
library(randomForestExplainer)
library(data.table)
library(DALEX)

# Define UI

cat_cols <- c('Sex', 'ChestPainType', 'RestingECG', 'ExerciseAngina', 'ST_Slope')
num_cols <- c('Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak', 'HeartDisease')

#data preprocessing
raw_df = read.csv("heart.csv")
raw_df <- raw_df %>% transform(Sex=as.factor(Sex),
                       ChestPainType=as.factor(ChestPainType),
                       RestingECG=as.factor(RestingECG),
                       ExerciseAngina=as.factor(ExerciseAngina),
                       ST_Slope=as.factor(ST_Slope),
                       HeartDisease=as.factor(HeartDisease),
                       FastingBS = as.factor(FastingBS)
)
raw_df <- raw_df[!raw_df$RestingBP == 0, ]
raw_df[raw_df$Cholesterol == 0 , "Cholesterol"] = median(raw_df$Cholesterol)
raw_df %>% select(Age,RestingBP,Cholesterol,MaxHR,Oldpeak) %>% scale()
# raw_df[c('Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak')]<- scale(raw_df[c('Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak')])
data_first <- raw_df
new_df = data_first[c("ST_Slope","ChestPainType","Oldpeak","MaxHR","Cholesterol","HeartDisease","ExerciseAngina","Age","RestingBP")]

# sample for model
sample = sample.split(data_first$HeartDisease, SplitRatio = .75)
train = subset(raw_df, sample == TRUE)
test  = subset(raw_df, sample == FALSE)





#random forest for classification
rf <- randomForest(HeartDisease~., data=train, localImp = TRUE,ntree = 600)
# print(rf)
p1 <- predict(rf, train)
confusionMatrix(p1, train$HeartDisease)

p2 <- predict(rf, test)
confusionMatrix(p2, test$HeartDisease)

explainer_ranger <- explain(rf, data = raw_df, y = as.numeric(raw_df$HeartDisease), label = "Heart Disease")
fi_ranger <- model_parts(explainer_ranger)
# bd_ranger <- predict_parts(explainer_ranger, new_observation = raw_df[1,])
# cp_ranger <- predict_profile(explainer_ranger, new_observation = raw_df[1,])

# plot(rf)
# hist(treesize(rf),
#      main = "No. of Nodes for the Trees",
#      col = "green")
# varImpPlot(rf,
#            sort = T,
#            n.var = 10,
#            main = "Top 10 - Variable Importance")
# 
# load("min_depth_frame.rda")
# head(min_depth_frame, n = 10)
# plot_min_depth_distribution(min_depth_frame)
# 


# UI

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  
                  "HF Calculator",
                  tabPanel("Opis danych", 
                           
                           mainPanel(width=9,
                             p("Cardiovascular diseases (CVDs) are the number 1 cause of death globally,
                             taking an estimated 17.9 million lives each year, which accounts for 31% of all deaths worldwide. 
                             Four out of 5CVD deaths are due to heart attacks and strokes, and one-third of these deaths occur prematurely
                             in people under 70 years of age. 
                             Heart failure is a common event caused by CVDs and this dataset 
                             contains 11 features that can be used to predict a possible heart disease.
                             People with cardiovascular disease or who are at high cardiovascular risk 
                            (due to the presence of one or more risk factors such as hypertension, diabetes,
                               hyperlipidaemia or already established disease) need early detection and management 
                               wherein a machine learning model can be of great help."),
                         p( "There are 7 numerical features, including:
['Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak', 'HeartDisease']"),
                          p("There are 5 categorical features, including:
['Sex', 'ChestPainType', 'RestingECG', 'ExerciseAngina', 'ST_Slope']"),
                          
                           
                         h3("Podsumowanie danych kategorycznych"),
                          plotOutput("gmplot"),
                         h3("Podsumowanie danych numerycznych"),
                         plotOutput("plot2"),
                         plotOutput("num_melted_cat"),
                         h5("Wnioski"),
                         tags$ol(
                           tags$li("Niska korelacja między każdą cechą"), 
                           tags$li("Większe ryzyko zachorowania na chorobę serca, jeśli:",
                                   tags$ol(
                                   tags$li("gender == male"),tags$li("chest pain type == ASY"),
                                   tags$li("ExcerciseAngina == Y"),tags$li("ST_slope == Flat"))),
                            tags$li("Jest trochę danych odstających, szczególnie w kolumnie z cholesterolem.") )
                           )),
                  tabPanel("Przetwarzanie danych i opis modelu",width=9,
                           h3("W tej sekcji wstępnie przetworzymy dane przed wprowadzeniem danych do modelu."),
                           tags$ol(
                             tags$li("RestingBP ma skrajną wartość odstającą na poziomie 0"), 
                             tags$li("Cholesterol ma kilka wartości zerowych, zastąpimy je medianą"), 
                             tags$li("Standaryzacja danych numerycznych"), 
                             # textOutput("rf_conf_matrix")
                             ),
                           h3("Wyjaśnialność modelu"),
                           p("W projekcie zastosowaliśmy model lasu losowego (random forest) dla problemu klasyfikacyjnego - określenie
                             niewydolności serca na podstawie danych wejściowych pacjentów."),
                           
                           # plotOutput("plot02_01"),
                           # plotOutput('plot02_02'),
                           p("The Mean Decrease Accuracy plot expresses how much accuracy the model losses by excluding each variable. The more the accuracy suffers,
                             the more important the variable is for the successful classification. The variables are presented from descending importance."),
                           p("The mean decrease in Gini coefficient is a measure of how each variable contributes
                           to the homogeneity of the nodes and leaves in the resulting random forest.
                             The higher the value of mean decrease accuracy or mean decrease Gini score, the higher the importance of the variable in the model."),
                           plotOutput('plot02_03'),
                           plotOutput('plot02_05'),
                           # plotOutput('plot02_04'),
                           
                           
                           
                           
                           ),
        
                  
                  
                  
                  tabPanel("Wypróbuj model",
                           sidebarLayout(
                           sidebarPanel(width=3,
                            sliderInput(inputId = "Age",
                                        label = "Age:",
                                        min = 15,
                                        max = 100,
                                        value = 50),
                           selectInput(inputId="Sex", label = "Gender",
                                                    choices = levels(raw_df$Sex),
                                                    selected = "M"),
                           selectInput(inputId="ChestPainType", label = "ChestPainType",
                                       choices = levels(raw_df$ChestPainType),
                                       selected = "ASY"),
                           sliderInput(inputId = "RestingBP",
                                       label = "RestingBP",
                                       min = 40,
                                       max = 220,
                                       value = 130),
                           sliderInput(inputId = "Cholesterol",
                                       label = "Cholesterol",
                                       min = 60,
                                       max = 650,
                                       value = 200),
                           selectInput(inputId="FastingBS", label = "FastingBS",
                                       choices = levels(raw_df$FastingBS),
                                       selected = "0"),
                           selectInput(inputId="RestingECG", label = "RestingECG",
                                       choices = levels(raw_df$RestingECG),
                                       selected = "Normal"),
                           sliderInput(inputId = "MaxHR",
                                       label = "MaxHR",
                                       min = 50,
                                       max = 230,
                                       value = 100),
                           selectInput(inputId="ExerciseAngina", label = "ExerciseAngina",
                                       choices = levels(raw_df$ExerciseAngina),
                                       selected = "Y"),
                          sliderInput(inputId = "Oldpeak",
                                                    label = "Oldpeak",
                                                    min = -1,
                                                    max = 7,
                                                    value = 0,
                                                    step = 0.1),
                          selectInput(inputId="ST_Slope", label = "ST_Slope",
                                      choices = levels(raw_df$ST_Slope),
                                      selected = "Flat"),


                          actionButton("submitbutton", "Submit", class = "btn btn-primary")
                          ),
                           
                    mainPanel(
                             h1("Dane o pacjencie"),

                             
                             
                             verbatimTextOutput('contents'),
                             verbatimTextOutput('tabledata'),
                             verbatimTextOutput('score_infos'),
                             h1("Wykresy z modelu"),
                             plotOutput("explainer"),
                             
                             plotOutput("explainer_two"),

                           ))
                           ),
                           
                  
                  ) 
                  
                  
                  
                ) 



# Define server function  
server <- function(input, output) {
  
  # output$rf_conf_matrix <- renderPrint(confusionMatrix(p1, train$HeartDisease))
  
  meltData_01 <- raw_df %>%
    select(RestingBP,Cholesterol,MaxHR,Age) %>%
    melt() 
  
  
  meltData_02 <- raw_df %>%
    select(Oldpeak) %>%
    melt() 
  output$num_melted_cat <- renderPlot({
    
    
    pl1 <- ggplot(data = meltData_01, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))
    pl2 <- ggplot(data = meltData_02, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))
    grid.arrange(pl1,pl2, ncol=1)
    
  })

  
  
  #  create correlation matrix between variables and output
  M <- raw_df %>%
    select(Age,RestingBP,Cholesterol,MaxHR,Oldpeak,HeartDisease) %>%
    mutate(HeartDisease = as.numeric(HeartDisease)) %>% 
    cor()
  output$plot2<-renderPlot(corrplot(M, method="color"))
  
  
  
  # render boxplots for each variable
  meltData_big_numbers<-new_df %>%  select(MaxHR,Cholesterol,Age,RestingBP) %>% melt()
  
  output$plot3 <- renderPlot(ggplot(meltData_big_numbers,aes(x = variable,y = value))  + geom_boxplot() + xlab("Zmienne") + ylab("Wartości")
  )
    
  
  # magic 

  output$gmplot <-renderPlot({
    
    p1 <- ggplot(raw_df,aes(Sex, fill=HeartDisease)) +geom_bar(stat = "count")
    p2 <- ggplot(raw_df,aes(ChestPainType, fill=HeartDisease)) +geom_bar(stat = "count")
    p3 <- ggplot(raw_df,aes(RestingECG, fill=HeartDisease)) +geom_bar(stat = "count")
    p4 <- ggplot(raw_df,aes(ExerciseAngina, fill=HeartDisease)) +geom_bar(stat = "count")
    p5 <- ggplot(raw_df,aes(ST_Slope, fill=HeartDisease)) +geom_bar(stat = "count")
    grid.arrange(p1,p2,p3,p4,p5, ncol=2)
    
  })
    
  
    
  # tabPanel2 plots
  
  
  
  
  output$plot02_01 <- renderPlot(plot(rf))
  output$plot02_02 <- renderPlot(hist(treesize(rf),
                                            main = "Liczba węzłów dla drzew",
                                            col = "orange"))
  output$plot02_03 <- renderPlot(varImpPlot(rf,
                                                        sort = T,
                                                        n.var = 10,
                                                        main = "Najważniejsze zmienne"))
  output$plot02_04 <- renderPlot(plot_min_depth_distribution(min_depth_frame))
  
  
  load("min_depth_frame.rda")
  head(min_depth_frame, n = 10)
  output$plot02_05 <- renderPlot(plot_min_depth_distribution(min_depth_frame))
  
  # tabpanel3 logic


  datasetInput <- reactive({


    df <- data.frame(
      Name = colnames(raw_df)[1:11],
      Value = as.character(c(input$Age,
                             input$Sex,
                             input$ChestPainType,
                             input$RestingBP,
                             input$Cholesterol,
                             input$FastingBS,
                             input$RestingECG,
                             input$MaxHR,
                             input$ExerciseAngina,
                             input$Oldpeak,
                             input$ST_Slope
                             )),
      stringsAsFactors = FALSE)

    HeartDisease <- 'HeartDisease'
    df <- rbind(df, HeartDisease)
    # test_ed <- data.frame()
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    # write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)

    test_ed <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test_ed$Sex <- factor(test_ed$Sex, levels(raw_df$Sex))
    
    test_ed$ChestPainType <- factor(test_ed$ChestPainType, levels = levels(raw_df$ChestPainType))
    test_ed$FastingBS <- factor(test_ed$FastingBS, levels = levels(raw_df$FastingBS))
    test_ed$RestingECG <- factor(test_ed$RestingECG, levels = levels(raw_df$RestingECG))
    test_ed$ExerciseAngina <- factor(test_ed$ExerciseAngina, levels = levels(raw_df$ExerciseAngina))
    test_ed$ST_Slope <- factor(test_ed$ST_Slope, levels = levels(raw_df$ST_Slope))
    # test_ed$HeartDisease <- factor(test_ed$HeartDisease, levels = levels(raw_df$HeartDisease))
    # if(test_ed$Sex != factor("M", levels(raw_df$Sex))){test_ed$Sex<-factor("F", levels(raw_df$Sex))}
    test_ed$HeartDisease <- predict(rf,test_ed)
    

    
    
    # Output <- data.frame(Prediction=predict(rf,test_ed), Score = round(predict(rf,test_ed,type="prob"), 3))
    Output <- test_ed
    print(test_ed)
    

  })
  
  
  
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })

  output$tabledata <- renderPrint({
    
      datasetInput()
      
    
  
  })
  bd_ranger <- reactive({
    
    df <- data.frame(
      Name = colnames(raw_df)[1:11],
      Value = as.character(c(input$Age,
                             input$Sex,
                             input$ChestPainType,
                             input$RestingBP,
                             input$Cholesterol,
                             input$FastingBS,
                             input$RestingECG,
                             input$MaxHR,
                             input$ExerciseAngina,
                             input$Oldpeak,
                             input$ST_Slope
      )),
      stringsAsFactors = FALSE)
    
    HeartDisease <- 'HeartDisease'
    df <- rbind(df, HeartDisease)
    # test_ed <- data.frame()
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    # write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test_ed <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test_ed$Sex <- factor(test_ed$Sex, levels(raw_df$Sex))
    
    test_ed$ChestPainType <- factor(test_ed$ChestPainType, levels = levels(raw_df$ChestPainType))
    test_ed$FastingBS <- factor(test_ed$FastingBS, levels = levels(raw_df$FastingBS))
    test_ed$RestingECG <- factor(test_ed$RestingECG, levels = levels(raw_df$RestingECG))
    test_ed$ExerciseAngina <- factor(test_ed$ExerciseAngina, levels = levels(raw_df$ExerciseAngina))
    test_ed$ST_Slope <- factor(test_ed$ST_Slope, levels = levels(raw_df$ST_Slope))
    
    Output <- predict_parts(explainer_ranger, new_observation = test_ed)
  })
  output$explainer <- renderPlot(plot(bd_ranger()))
  
  cp_ranger <- reactive({
    df <- data.frame(
      Name = colnames(raw_df)[1:11],
      Value = as.character(c(input$Age,
                             input$Sex,
                             input$ChestPainType,
                             input$RestingBP,
                             input$Cholesterol,
                             input$FastingBS,
                             input$RestingECG,
                             input$MaxHR,
                             input$ExerciseAngina,
                             input$Oldpeak,
                             input$ST_Slope
      )),
      stringsAsFactors = FALSE)
    
    HeartDisease <- 'HeartDisease'
    df <- rbind(df, HeartDisease)
    
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    
    test_ed <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test_ed$Sex <- factor(test_ed$Sex, levels(raw_df$Sex))
    
    test_ed$ChestPainType <- factor(test_ed$ChestPainType, levels = levels(raw_df$ChestPainType))
    test_ed$FastingBS <- factor(test_ed$FastingBS, levels = levels(raw_df$FastingBS))
    test_ed$RestingECG <- factor(test_ed$RestingECG, levels = levels(raw_df$RestingECG))
    test_ed$ExerciseAngina <- factor(test_ed$ExerciseAngina, levels = levels(raw_df$ExerciseAngina))
    test_ed$ST_Slope <- factor(test_ed$ST_Slope, levels = levels(raw_df$ST_Slope))
    Output <- predict_profile(explainer_ranger, new_observation = test_ed)
    
  })
  output$explainer_two <- renderPlot(plot(cp_ranger()))
  
  score_info <- reactive({
    df <- data.frame(
      Name = colnames(raw_df)[1:11],
      Value = as.character(c(input$Age,
                             input$Sex,
                             input$ChestPainType,
                             input$RestingBP,
                             input$Cholesterol,
                             input$FastingBS,
                             input$RestingECG,
                             input$MaxHR,
                             input$ExerciseAngina,
                             input$Oldpeak,
                             input$ST_Slope
      )),
      stringsAsFactors = FALSE)
    
    HeartDisease <- 'HeartDisease'
    df <- rbind(df, HeartDisease)
    
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    
    test_ed <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    test_ed$Sex <- factor(test_ed$Sex, levels(raw_df$Sex))
    
    test_ed$ChestPainType <- factor(test_ed$ChestPainType, levels = levels(raw_df$ChestPainType))
    test_ed$FastingBS <- factor(test_ed$FastingBS, levels = levels(raw_df$FastingBS))
    test_ed$RestingECG <- factor(test_ed$RestingECG, levels = levels(raw_df$RestingECG))
    test_ed$ExerciseAngina <- factor(test_ed$ExerciseAngina, levels = levels(raw_df$ExerciseAngina))
    test_ed$ST_Slope <- factor(test_ed$ST_Slope, levels = levels(raw_df$ST_Slope))
    
    hf_prob<-mean(predict_profile(explainer_ranger, new_observation = test_ed)$`_yhat`)
    Output <- paste("There is a ", hf_prob , "probability that the patient has hearth disease")
    
    
  })
  output$score_infos <- renderText(score_info())
  
} 


# Create Shiny object
shinyApp(ui = ui, server = server)


# ['Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak', 'HeartDisease']
