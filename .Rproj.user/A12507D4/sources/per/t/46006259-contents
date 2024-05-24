library(shiny)
library(bslib)
library(DT)

source("codes/Modules.R")


function(input, output, session) {

  updateSelectizeInput(session, "sex", choices = unique(adsl$SEX))
  
  updateSelectizeInput(session, "sex_2", choices = unique(adsl$SEX))
  
  updateSelectizeInput(session, "arm", choices = unique(adsl$ARM))
  
  updateSelectizeInput(session, "race", choices = unique(adsl$RACE))
  
  updateSelectizeInput(session, "ethnic", choices = unique(adsl$ETHNIC))
  
  updateSelectizeInput(session, "position", choices = unique(na.omit(advs$ATPT)))
  
  updateSelectizeInput(session, "age_gr", choices = unique(adsl$AGEGR1))

  
  filtered_data <- reactive({
    filter_data(adsl, input$sex, input$age, input$arm, input$race, input$ethnic)
  })

  output$population_barchart <- renderPlot({
    
    plot_population_barchart(
      population_table(filtered_data())
                             )
  })
  
  output$placebo <- reactive({
    sum(filtered_data()$ARM == "Placebo")
  })
  
  output$highdose <- reactive({
    sum(filtered_data()$ARM == "Xanomeline High Dose")
  })
  
  output$lowdose <- reactive({
    sum(filtered_data()$ARM == "Xanomeline Low Dose")
  })
  
  output$total <- reactive({
    nrow(filtered_data())
  })
  
  output$demog_table <- renderDT({
    selected_adsl <- filtered_data()[, c("USUBJID","ARM","TRT01P","TRT01A"), drop = FALSE]
    datatable(selected_adsl)
  })
  
  output$vital_sign_plot_1 <- renderPlotly({
    print(input$visit_slider_1)
    
     vital_sign_plot(visit = input$visit_slider_1, 
                     param = "Diastolic Blood Pressure (mmHg)",
                     position = input$position,
                     sex = input$sex_2,
                     age_gr = input$age_gr) %>%
     ggplotly(tooltip = c("y","n")) %>%
     style(hoverlabel = list(font = list(family = "Roboto", size = 14))) %>%
     layout(showlegend = FALSE)
  })
  
  output$vital_sign_plot_2 <- renderPlotly({
    
    vital_sign_plot(visit = input$visit_slider_1, 
                    param = "Systolic Blood Pressure (mmHg)",
                    position = input$position,
                    sex = input$sex_2,
                    age_gr = input$age_gr) %>%
      ggplotly(tooltip = c("y","n")) %>%
      style(hoverlabel = list(font = list(family = "Roboto", size = 14))) %>%
      layout(showlegend = FALSE)
  })
  
  
  ae_pl_term <- reactive({
      adverse_end_data(input = adae,
                       trt = "Placebo",
                       color ="#ff46c8",
                       aesoc = input$organ_class_click$name)
  })
  
  ae_low_term <- reactive({
      adverse_end_data(input = adae,
                       trt = "Xanomeline Low Dose",
                       color ="#00beff",
                       aesoc = input$organ_class_click$name)
  })
  
  ae_high_term <- reactive({
    adverse_end_data(input = adae,
                     trt = "Xanomeline High Dose",
                     color ="#ffc80a",
                     aesoc = input$organ_class_click$name)
  })
  
  output$organ_class <- renderHighchart({ 
    organ_class()
  })
  
  output$aedecod_class_pl <- renderHighchart({ 
    aedecod_plot(
      ae_pl_term()
    )
  })
  
  output$aedecod_class_low <- renderHighchart({
    aedecod_plot(
      ae_low_term()
      )
  })

  output$aedecod_class_high <- renderHighchart({
    aedecod_plot(
      ae_high_term()
    )
  })
  
  output$organ_class_click <- renderText({
    if(is.null(input$organ_class_click$name)) {
      "ANY BODY SYSTEM" 
    } else {
      input$organ_class_click$name
    }
  })
  
  output$adverse_range_plot <- renderHighchart({
    
    adverse_range_plot(
      aesoc = input$organ_class_click$name, 
      aedecod = input$AedecodClicked
      )
    
  })
  
}
