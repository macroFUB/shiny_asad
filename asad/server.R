library(shiny)
library(tidyverse)

#Solving quadratic function
result <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
  }
  else {"There are no real roots."} # third case D<0
}

# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}

server <- function(input, output){
  dat <- reactive({

    P<-seq(0,input$P_e_anfang+3,0.001)

    Y<-(input$d_2_base*(input$c_0_base-input$c_1_base*input$T_base+
                                 input$b_0_base+input$G_base)+(input$b_1_base*input$M_bar_base)/
                 P)/(input$b_1_base*input$d_1_base+input$d_2_base*(1-input$c_1_base))

    P_lang_1<-input$P_e_anfang
    Y_bar<-(input$d_2_base*(input$c_0_base-input$c_1_base*input$T_base+
                              input$b_0_base+input$G_base)+(input$b_1_base*input$M_bar_base)/
              P_lang_1)/(input$b_1_base*input$d_1_base+input$d_2_base*(1-input$c_1_base))
    Y_star_1<-Y_bar
    r_star_1<-(input$d_1*(input$c_0-input$c_1*input$T+input$b_0+input$G)-
                    (1-input$c_1)*input$M_bar/P_lang_1)/((1-input$c_1)*input$d_2+input$d_1*input$b_1)

    Y_SRAS_1<-(1/input$alpha_base)*(P-input$P_e_anfang)+Y_bar

    Y_AD_2<-(input$d_2*(input$c_0-input$c_1*input$T+
                            input$b_0+input$G)+(input$b_1*input$M_bar)/P)/
      (input$b_1*input$d_1+input$d_2*(1-input$c_1))


    a<-(input$alpha_base*(1-input$c_1)/input$b_1)+((input$alpha_base*input$d_1)/input$d_2)
    b<-(input$P_e_anfang-input$alpha_base*Y_bar)*(input$d_1/input$d_2)+
      (input$P_e_anfang-input$alpha_base*Y_bar)*((1-input$c_1)/input$b_1)-
      (input$alpha_base/input$b_1)*(input$c_0-input$c_1*input$T+input$b_0+input$G)
    c<-(input$P_e_anfang-input$alpha_base*Y_bar)*(-(1/input$b_1)*(input$c_0-input$c_1*
      input$T+input$b_0+input$G))-input$M_bar/input$d_2

    Y_star_kurz<-result(a,b,c)[1]
    P_star_kurz<-input$alpha_base*(Y_star_kurz-Y_bar)+input$P_e_anfang
    r_star_kurz<-(input$d_1*(input$c_0-input$c_1*input$T+input$b_0+input$G)-
                    (1-input$c_1)*input$M_bar/P_star_kurz)/((1-input$c_1)*input$d_2+input$d_1*input$b_1)

    Y_star_lang<-Y_bar
    P_star_lang<-input$M_bar/(input$d_2*((input$d_1*Y_star_lang)/(input$d_2)+((1-input$c_1)/input$b_1)*Y_star_lang-
                  (1/input$b_1)*(input$c_0-input$c_1*input$T+input$b_0+input$G)))
    r_star_lang<-(input$d_1*(input$c_0-input$c_1*input$T+input$b_0+input$G)-
                    (1-input$c_1)*input$M_bar/P_star_lang)/((1-input$c_1)*input$d_2+input$d_1*input$b_1)

    P_e<-P_star_lang

    Y_SRAS_2<-(1/input$alpha_base)*(P-P_e)+Y_bar


    data_table <- data.frame(P,P_lang_1,Y,Y_AD_2,Y_bar,Y_star_1,Y_SRAS_1,Y_SRAS_2,
                             Y_star_kurz,P_star_kurz,r_star_kurz, Y_star_lang,P_star_lang,r_star_lang,r_star_1)
    data_table
  })
  output$graph <- renderPlot({
    ggplot(data = dat(), aes(x = Y, y= P)) + coord_cartesian(xlim=c(0, 3000))+
      geom_line(aes(x = Y_AD_2, colour = "AD2"), size = 1)+
      geom_line(aes(x = Y_SRAS_2, colour = "SRAS2"), size = 1)+
      geom_line(aes(x = Y, colour = "AD1"), size = 1)+
      geom_line(aes(x = Y_SRAS_1, colour = "SRAS1"), size = 1)+
      geom_line(aes(x = Y_bar, colour = "LRAS"), size = 1)+
      geom_segment(
        aes(x = Y_bar, y = 0, xend = Y_bar, yend = input$P_e_anfang),
        linetype = "dashed", colour = "black"
      ) +
      geom_segment(
        aes(x = 0, y = input$P_e_anfang, xend = Y_bar, yend = input$P_e_anfang),
        linetype = "dashed", colour = "black"
      ) +
      geom_point(aes(x=Y_bar, y=input$P_e_anfang), size=2)+
      geom_segment(
        aes(x = Y_star_kurz, y = 0, xend = Y_star_kurz, yend = P_star_kurz),
        linetype = "dashed", colour = "black"
      ) +
      geom_segment(
        aes(x = 0, y = P_star_kurz, xend = Y_star_kurz, yend = P_star_kurz),
        linetype = "dashed", colour = "black"
      ) +
      geom_point(aes(x=Y_star_kurz, y=P_star_kurz), size=2)+
      geom_segment(
        aes(x = Y_star_lang, y = 0, xend = Y_star_lang, yend = P_star_lang),
      linetype = "dashed", colour = "black"
      ) +
      geom_segment(
        aes(x = 0, y = P_star_lang, xend = Y_star_lang, yend = P_star_lang),
        linetype = "dashed", colour = "black"
      ) +
      geom_point(aes(x=Y_star_lang, y=P_star_lang), size=2)
  })
  output$table <- renderTable({
    tmp <- dat()
    equil <- data.frame(
      "Y_bar" = tmp$Y_bar,
      "Y_star_1"=tmp$Y_star_1,
      "P_star_1"=tmp$P_lang_1,
      "r_star_1"=tmp$r_star_1,
      "P_star_kurz" = tmp$P_star_kurz,
      "Y_star_kurz" = tmp$Y_star_kurz,
      "r_star_kurz" = tmp$r_star_kurz,
      "P_star_lang" = tmp$P_star_lang,
      "Y_star_lang" = tmp$Y_star_lang,
      "r_star_lang" = tmp$r_star_lang
    )[1,]
  },bordered = TRUE,sanitize.text.function=identity)
}
