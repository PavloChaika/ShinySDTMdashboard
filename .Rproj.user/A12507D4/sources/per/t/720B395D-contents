library(ggplot2)
library(haven)
library(dplyr)
library(plotly)
library(showtext)
library(highcharter)
library(lubridate)

adsl <- haven::read_xpt("data/adam/adsl.xpt")
dm <- haven::read_xpt("data/sdtm/dm.xpt")
advs <- haven::read_xpt("data/adam/advs.xpt")
adae <- haven::read_xpt("data/adam/adae.xpt")
adae <- adae %>%
  filter(AOCCFL == "Y") %>%
  mutate(
    AESOC = "ANY BODY SYSTEM"
  ) %>%
  rbind(adae)


font_add_google("Roboto", family = "roboto")
showtext_auto()

population_table <- function(data) {
  populations <- data %>%
    group_by(ARM) %>%
    summarise(
      ITT = sum(ITTFL == "Y"),
      Safety = sum(SAFFL == "Y"),
      Efficacy = sum(EFFFL == "Y"),
      `Complete Week 24` = sum(COMP24FL == "Y"),
      `Complete Study` = sum(EOSSTT == "COMPLETED")
    ) %>%
    tidyr::pivot_longer(cols = -ARM, names_to = "Measure", values_to = "Count")

  #populations$ARM <- gsub("Xanomeline", "Xanomeline\n", populations$ARM)
  return(populations)
}

filter_data <- function(data, sex, age, arm, race, ethnic) {
  output <- data
  
  if (!is.null(sex)) {
    output <- output %>% filter(SEX %in% sex)
  }
  
  if (!is.null(age)) {
    output <- output %>% filter(AGE >= age[1] & AGE <= age[2])
  }
  
  if (!is.null(arm)) {
    output <- output %>% filter(ARM %in% arm)
  }
  
  if (!is.null(race)) {
    output <- output %>% filter(RACE %in% race)
  }
  
  if (!is.null(ethnic)) {
    output <- output %>% filter(ETHNIC %in% ethnic)
  }
  
  return(output)
}


plot_population_barchart <- function(data){
  plot <- 
    data |>
    ggplot(aes(x = Measure, y = Count, fill = ARM)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(Count)), 
              position = position_stack(vjust = 0.5), 
              size = 6) +  
    scale_fill_manual(values = c("Placebo" = "#ff46c8AA", 
                                 "Xanomeline High Dose" = "#ffc80aAA", 
                                 "Xanomeline Low Dose" = "#00beffAA")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.ticks = element_blank()) + 
    guides(fill = "none") +
    ylim(0, 260)
  return(plot)
}

vital_sign_plot <- function(visit_custom, param, position, sex, age_gr){

data <- advs %>% 
  filter(ANL01FL == "Y") %>%
  filter(AVISITN == visit_custom) %>%
  filter(PARAM == param) 

data <- data %>%
  filter(if (!is.null(position)) ATPT %in% position else TRUE) %>%
  filter(if (!is.null(sex)) SEX %in% sex else TRUE) %>%
  filter(if (!is.null(age_gr)) AGEGR1 %in% age_gr else TRUE)

counts <- data %>% count(TRTP)

# Plotting
plot <- ggplot(data, aes(x = TRTP, y = AVAL, fill = TRTP, color = TRTP)) +
  geom_boxplot(size = 1, alpha = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("Placebo" = "#ff46c8AA", 
                               "Xanomeline High Dose" = "#ffc80aAA", 
                               "Xanomeline Low Dose" = "#00beffAA")) +
  scale_color_manual(values = c("Placebo" = "#ff46c8", 
                                "Xanomeline High Dose" = "#ffc80a", 
                                "Xanomeline Low Dose" = "#00beff")) +
  scale_x_discrete(labels = c("Placebo" = paste(counts[1,1],"\n",counts[1,2], sep =""), 
                              "Xanomeline High Dose" = paste(counts[2,1],"\n",counts[2,2], sep =""), 
                              "Xanomeline Low Dose" = paste(counts[3,1],"\n",counts[3,2], sep =""))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.ticks = element_blank(),
        text = element_text(family = "roboto")) + 
  stat_summary(
    fun = "mean", 
    geom = "point", 
    shape = 18, 
    size = 4, 
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  guides(fill = "none") 

return(plot)
}



organ_class <- function(data){
  data <- adae %>%
    filter(TRTEMFL == "Y" & SAFFL == "Y") %>%
    filter(AOCCSFL == "Y") %>%
    count(AESOC, TRTA) %>%
    group_by(AESOC) %>%
    mutate(N = sum(n)) %>%
    arrange(desc(N), TRTA, n)
     
plot <- data %>%
  hchart(
    "bar",
    hcaes(x = AESOC, y = n, group = TRTA)
  ) %>%
  hc_xAxis(
    labels = list(
      style = list(
        fontSize = '12',
        fontFamily = "Arial"
        )
      ),
    title = list(
      text = NULL
    )
  ) %>%
  hc_yAxis(
    labels = list(
      style = list(fontSize = '10')
    ),
    title = list(
      text = ""
    )
  ) %>%
  hc_colors(c("#ff46c8AA", "#ffc80aAA", "#00beffAA")) %>%
  hc_legend(
    layout = "vertical",
    align = "right",
    verticalAlign = "bottom",
    floating = TRUE,
    y = -20
  ) %>%
  hc_add_event_point(series = "series", event = "click")

return(plot)
}

adverse_end_data <- function(input,trt, color, aesoc){
  
  data <- input %>%
    filter(TRTEMFL == "Y" & SAFFL == "Y") %>%
    filter(AOCCPFL == "Y") %>%
    filter(TRTA == trt) %>%
    count(AESOC, AEDECOD, TRTA) %>%
    filter(if (!is.null(aesoc)) AESOC == aesoc else AESOC == "ANY BODY SYSTEM")
  
  alpha_hex = sprintf("%02X", round(seq(1, 0.1, length.out = nrow(data)) * 255))
  
  data$colors <- paste0(color, alpha_hex)

return(data)
  
}


aedecod_plot <- function(data){
  
  canvasClickFunction <- JS("function(event) {Shiny.setInputValue('AedecodClicked', [event.point.name]);}")
  
  if (nrow(data) == 0) {
    # If data is empty, create a placeholder plot or notification message
    plot <- highchart() %>%
      hc_title(text = "No data available")
  } else {
    plot <- highchart() %>%
      hc_add_series(
        data %>% arrange(desc(n)),
        "pie",
        hcaes(
          y = n,
          x = AEDECOD
        ),
        name = "Number of subjects",
        colors = as.list(data$colors)
      ) %>%
      hc_title(
        text = paste(sum(data$n)),
        style = list(
          color = "black", 
          useHTML = TRUE, 
          fontSize = '10', 
          fontFamily = "Arial"
          ),
        floating = TRUE,
        verticalAlign = "middle"
      ) %>%
      hc_plotOptions(
        series = list(
          innerSize = '60%',
          dataLabels = list(enabled = FALSE),
          events = list(
            click = canvasClickFunction
            )
        )
      ) %>%
      hc_add_event_point(series = "series", event = "click")
  }
  return(plot)
}


adverse_range_plot <- function(aesoc, aedecod){
  
  if (!is.null(aesoc)) if (aesoc == "ANY BODY SYSTEM")  aesoc = NULL

  data <- adae %>%
    filter(TRTEMFL == "Y" & SAFFL == "Y") %>%
    filter(AESOC != "ANY BODY SYSTEM") %>%
    filter(if (!is.null(aesoc)) AESOC == aesoc else TRUE) %>%
    filter(if (!is.null(aedecod)) AEDECOD == aedecod else TRUE) %>%
    mutate(
      name = paste(USUBJID, AEDECOD),
      low = ASTDY,
      high = AENDY
    ) %>%
    arrange(USUBJID, AEDECOD, ADURN) %>%
    select(name, high, low, TRTA) 
  
  plot <- highchart() %>%
    hc_chart(type = 'dumbbell', inverted = TRUE) %>%
    hc_colors(c("#ff46c8AA", "#ffc80aAA", "#00beffAA")) %>%
    hc_add_series(
      data %>% filter(TRTA == "Placebo") %>% select(-TRTA),
      name = "Placebo" ,
      connectorWidth = 3,
      lowColor = "#ff46c8AA",
      marker = list(
        enabled = TRUE,
        radius = 5
      )
    ) %>%
    hc_add_series(
      data %>% filter(TRTA == "Xanomeline High Dose") %>% select(-TRTA),
      name = "Xanomeline High Dose",
      connectorWidth = 3,
      lowColor = "#ffc80aAA",
      marker = list(
        enabled = TRUE,
        radius = 5
      )
    ) %>%
    hc_add_series(
      data %>% filter(TRTA == "Xanomeline Low Dose") %>% select(-TRTA),
      name = "Xanomeline Low Dose",
      lowColor = "#00beffAA",
      connectorWidth = 3,
      marker = list(
        enabled = TRUE,
        radius = 5
      )
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(animation = TRUE)) %>%
    hc_xAxis(
      type = "category",
      uniqueNames = TRUE,
      labels = list(
        style = list(
          color = "black", 
          useHTML = TRUE, 
          fontSize = '12', 
          fontFamily = "Arial"
        )
      )
    ) %>%
    hc_yAxis(
      title = list(
        text = "Day in Study",
        style = list(
          color = "black", 
          useHTML = TRUE, 
          fontSize = '12', 
          fontFamily = "Arial"
        )
      )
    ) 
  
 return(plot)
  
}



# 
# 
# library(lubridate)
# library(tidyverse)
# 
# N <- 6
# set.seed(1234)
# 
# df <- tibble(
#   start = sort(Sys.Date() + months(2 + sample(10:20, size = N))),
#   end = start + months(sample(1:3, size = N, replace = TRUE)),
#   name = c("Import", "Tidy", "Visualize", "Model", "Transform", "Import"),
#   id = tolower(name),
#   color = c("#7cb5ec", "#7cb5ec", "#7cb5ec", "#434348", "#434348", "#434348")
#   )
# 
# df <- mutate_if(df, is.Date, datetime_to_timestamp)
# 
# chart_colors <- c(
#   '#7cb5ec', '#434348'
# )
# 
# highchart(type = "gantt") %>% 
#   hc_add_series(
#     name = "Program",
#     data = df
#   ) %>% 
#   hc_navigator(
#     enabled = TRUE,
#     series = list(
#       type = 'gantt',
#       pointPlacement = 0.5,
#       pointPadding =  0.25
#     ),
#     yAxis = list(
#       min = 0,
#       max = N,
#       reversed = TRUE,
#       categories = data$TRTA
#     )
#   ) %>% 
#   hc_yAxis(
#     uniqueNames = TRUE
#   ) %>%
#   hc_colors(chart_colors) 
# 
# 
# df<-tibble(
#   name = sample(letters,10),
#   high = rnorm(10),
#   low = rnorm(10),
#   color = c("#7cb5ec", "#7cb5ec", "#7cb5ec", "#434348", "#434348", "#434348","#7cb5ec", "#7cb5ec", "#7cb5ec", "#434348"),
#   lowColor = c("blue", "red", "green", "blue", "red", "green","blue", "red", "green","blue")
# ) 
# 
#   
#   
# highchart() %>%
#   hc_chart(type = 'dumbbell', inverted = TRUE) %>%
#   hc_colors(c("#ff46c8AA", "#ffc80aAA", "#00beffAA")) %>%
#   hc_add_series(
#     df %>% filter(color == "#7cb5ec"),
#     name = 'Blue'
#   ) %>%
#   hc_add_series(
#     df %>% filter(color == "#434348"),
#     name = 'Red'
#   ) %>%
#   hc_exporting(enabled = TRUE) %>%
#   hc_plotOptions(series = list(animation = FALSE)) %>%
#   hc_navigator(
#     enabled = TRUE,
#     series = list(
#       type = 'gantt',
#       pointPlacement = 0.5,
#       pointPadding =  0.25
#     ),
#     yAxis = list(
#       min = 0,
#       max = N,
#       reversed = TRUE,
#       categories = df$name
#     )
#   ) %>% 
#   hc_xAxis(
#     type = "category",
#     uniqueNames = TRUE,
#     title = list(text = "Adverse events")
#   ) %>%
#   hc_yAxis(
#     title = list(text = "Day in Study")
#   ) %>%
#   hc_xAxis(
#     type = "category",
#     uniqueNames = TRUE,
#      title = list(text = "Adverse events")
#   ) 
# 














