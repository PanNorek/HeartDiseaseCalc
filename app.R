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
                       HeartDisease=as.factor(HeartDisease)
)
raw_df <- raw_df[!raw_df$RestingBP == 0, ]
raw_df[raw_df$Cholesterol == 0 , "Cholesterol"] = median(raw_df$Cholesterol)
raw_df %>% select(Age,RestingBP,Cholesterol,FastingBS,MaxHR,Oldpeak) %>% scale()
raw_df[c('Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak')]<- scale(raw_df[c('Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak')])
df <- raw_df
new_df = df[c("ST_Slope","ChestPainType","Oldpeak","MaxHR","Cholesterol","HeartDisease","ExerciseAngina","Age","RestingBP")]

# sample for model
sample = sample.split(df$HeartDisease, SplitRatio = .65)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)





#random forest for classification
rf <- randomForest(HeartDisease~., data=train, proximity=TRUE)
print(rf)
p1 <- predict(rf, train)
confusionMatrix(p1, train$HeartDisease)

p2 <- predict(rf, test)
confusionMatrix(p2, test$HeartDisease)


plot(rf)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


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
                            (due to the presence of one or more risk factors such as hypertension, diabetes, hyperlipidaemia or already established disease) need early detection and management wherein a machine learning model can be of great help."),
                         p( "There are 7 numerical features, including:
['Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak', 'HeartDisease']"),
                          p("There are 5 categorical features, including:
['Sex', 'ChestPainType', 'RestingECG', 'ExerciseAngina', 'ST_Slope']"),
                          # h3("Macierz korelacji pomiędzy zmiennymi"),
                           
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
                  tabPanel("Przetwarzanie danych i opis modelu",
                           h3("W tej sekcji wstępnie przetworzymy dane przed wprowadzeniem danych do modeli szkoleniowych."),
                           tags$ol(
                             tags$li("RestingBP ma skrajną wartość odstającą na poziomie 0"), 
                             tags$li("Cholesterol ma kilka wartości zerowych, zastąpimy je medianą"), 
                             tags$li("Standaryzacja danych numerycznych"), 
                             textOutput("rf_conf_matrix")
                             ),
                           
                           plotOutput("plot02_01"),
                           plotOutput('plot02_02'),
                           plotOutput('plot02_03'),
                           
                           
                           
                           
                           ),
                 
                  tabPanel("Wypróbuj model",
                           sidebarPanel(width=5,

                         
                             
                          sliderInput(inputId = "Oldpeak",
                                                    label = "Oldpeak",
                                                    min = -1,
                                                    max = 7,
                                                    value = 0,
                                                    step = 0.1), 
                          sliderInput(inputId = "MaxHR",
                                                    label = "MaxHR",
                                                    min = 50,
                                                    max = 250,
                                                    value = 100), 
                          sliderInput(inputId = "Cholesterol",
                                                    label = "Cholesterol",
                                                    min = 0,
                                                    max = 700,
                                                    value = 200), 
                          sliderInput(inputId = "Age",
                                                      label = "Age",
                                                      min = 15,
                                                      max = 100,
                                                      value = 50), 
                          sliderInput(inputId = "RestingBP",
                                      label = "RestingBP",
                                      min = 0,
                                      max = 220,
                                      value = 130),     
                          selectInput(inputId="ExerciseAngina", label = "ExerciseAngina", 
                                      choices = levels(new_df$ChestPainType), 
                                      selected = 1),
                          selectInput(inputId="ST_Slope", label = "ST_Slope", 
                                      choices = levels(new_df$ST_Slope), 
                                      selected = 1),
                          selectInput(inputId="ChestPainType", label = "ChestPainType", 
                                      choices = levels(new_df$ChestPainType), 
                                      selected = 1),

                             actionButton("Action", "Analyze")),
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ),
                           ), 
                           
                  
                  ) 
                  
                  
                  
                ) 



# Define server function  
server <- function(input, output) {
  
  output$rf_conf_matrix <- renderPrint(confusionMatrix(p1, train$HeartDisease))
  
  meltData_01 <- df %>%
    select(RestingBP,Cholesterol,MaxHR,Age) %>%
    melt() 
  
  
  meltData_02 <- df %>%
    select(FastingBS,Oldpeak) %>%
    melt() 
  output$num_melted_cat <- renderPlot({
    
    # pl1 <- boxplot(data=meltData_01, value~variable),
    pl1 <- ggplot(data = meltData_01, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))
    pl2 <- ggplot(data = meltData_02, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))
    # pl2 <- boxplot(data=meltData_02, value~variable)
    grid.arrange(pl1,pl2, ncol=1)
    
  })

  
  
  # output$plot1 <-renderPlot(
  #   boxplot(data=meltData, value~variable)
  # )
  
  #  create correlation matrix between variables and output
  M <- df %>%
    select(Age,RestingBP,Cholesterol,FastingBS,MaxHR,Oldpeak,HeartDisease) %>%
    mutate(HeartDisease = as.numeric(HeartDisease)) %>% 
    cor()
  output$plot2<-renderPlot(corrplot(M, method="color"))
  
  
  
  # render boxplots for each variable
  meltData_big_numbers<-new_df %>%  select(MaxHR,Cholesterol,Age,RestingBP) %>% melt()
  
  output$plot3 <- renderPlot(ggplot(meltData_big_numbers,aes(x = variable,y = value))  + geom_boxplot() + xlab("Zmienne") + ylab("Wartości")
  )
    
  
  # magic 

  output$gmplot <-renderPlot({
    
    p1 <- ggplot(df,aes(Sex, fill=HeartDisease)) +geom_bar(stat = "count")
    p2 <- ggplot(df,aes(ChestPainType, fill=HeartDisease)) +geom_bar(stat = "count")
    p3 <- ggplot(df,aes(RestingECG, fill=HeartDisease)) +geom_bar(stat = "count")
    p4 <- ggplot(df,aes(ExerciseAngina, fill=HeartDisease)) +geom_bar(stat = "count")
    p5 <- ggplot(df,aes(ST_Slope, fill=HeartDisease)) +geom_bar(stat = "count")
    grid.arrange(p1,p2,p3,p4,p5, ncol=2)
    
  })
    
  
    
  # tabPanel2 plots
  output$plot02_01 <- renderPlot(plot(rf))
  output$plot02_02 <- renderPlot(hist(treesize(rf),
                                            main = "No. of Nodes for the Trees",
                                            col = "green"))
  output$plot02_03 <- renderPlot(varImpPlot(rf,
                                                        sort = T,
                                                        n.var = 10,
                                                        main = "Top 10 - Variable Importance"))
  # plot(rf)
  # hist(treesize(rf),
  #      main = "No. of Nodes for the Trees",
  #      col = "green")
  # varImpPlot(rf,
  #            sort = T,
  #            n.var = 10,
  #            main = "Top 10 - Variable Importance")
  # 
} 


# Create Shiny object
shinyApp(ui = ui, server = server)


# ['Age', 'RestingBP', 'Cholesterol', 'FastingBS', 'MaxHR', 'Oldpeak', 'HeartDisease']





