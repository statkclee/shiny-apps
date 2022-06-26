
data_opts<-c("bulls_eye","worms","moon")
activation_opts<-c("sigmoid","tanh","relu")


shinyUI(fluidPage(
  fluidRow(headerPanel('신경망 모형 예제')),
  fluidRow(
    column(3,
           selectInput("data_sel", "데이터 선택", data_opts),
           sliderInput("hidden", "은닉층 노드 수",min=2, max=9, value=3, step=1),
           sliderInput("epochs","에포크 횟수(# Epochs):",min=1000, max=10000, value=3000, step=500),
           sliderInput("lr","학습율(Learning rate):",min=0.1, max=20.0, value=0.5, step=0.1),
           selectInput("activation_ftn", "활성화 함수(Activation Function)", activation_opts),
           actionButton("submit", "NN 모형 실행"),
           actionButton("reset", "초기화")
    ),
    column(9,
           plotOutput('plot1'),
           fluidRow(
             column(6,plotOutput('plot2')),
             column(6,verbatimTextOutput('nn_output'))
           )
    )
  ),
  tags$p("Mark Hodnett , Joshua F. Wiley , Yuxi (Hayden) Liu (2019), 'Deep Learning with R for Beginners' Packt 출판사")
))