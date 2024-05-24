library(shiny)
library(bslib)
library(bsicons)
library(thematic)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(highcharter)

thematic_shiny()

tagAppendAttributes(list(
  style = "zoom: 0.5;"
))

link_page <- tags$a(tags$img(src = "Avengas-link.png", height = 15, width = 15), "Avenga", href = "https://www.avenga.com/", target = "_blank")
link_linkedIn <- tags$a(tags$img(src = "LinkedIn-link.png", height = 15, width = 15), "LinkedIn", href = "https://www.linkedin.com/company/avenga/about/", target = "_blank")


page_navbar(
  
  theme = bslib::bs_theme(
    preset = "materia"
  ),
  window_title = "Test",
  title = span(
    img(src = "logo2.png", height = "70px", width = "200px", )
  ),
  fillable = TRUE,
  nav_panel("Populations", 
            layout_sidebar(
              sidebar = sidebar(
                  list(
                    selectizeInput(
                      "sex",
                      "Select sex",
                      choices = NULL,
                      multiple  = TRUE
                    ),
                    sliderInput(
                      "age", "Age:",
                      min = 0, max = 100,
                      value = c(0,100)
                    ),
                    selectizeInput(
                      "arm",
                      "Select Treatment",
                      choices = NULL,
                      multiple  = TRUE
                    ),
                    selectizeInput(
                      "race",
                      "Select Race",
                      choices = NULL,
                      multiple  = TRUE
                    ),
                    selectizeInput(
                      "ethnic",
                      "Select Ethnic",
                      choices = NULL,
                      multiple  = TRUE
                    )
                  )
              ),
              
              # Layout non-sidebar elements
              layout_columns(
                layout_columns(
                value_box(title = "Placebo",
                          textOutput("placebo"),
                          showcase = bs_icon("people-fill"),
                          showcase_layout = c("bottom"),
                          theme = value_box_theme(bg = "#ff46c8AA")),
                value_box(title = "Xanomeline High Dose",
                          textOutput("highdose"),
                          showcase = bs_icon("people-fill"),
                          showcase_layout = c("bottom"),
                          theme = value_box_theme(bg = "#ffc80aAA")),
                value_box(title = "Xanomeline Low Dose",
                          textOutput("lowdose"),
                          showcase = bs_icon("people-fill"),
                          showcase_layout = c("bottom"),
                          theme = value_box_theme(bg = "#00beffAA")),
                value_box(title = "Total",
                          textOutput("total"),
                          showcase = bs_icon("people-fill"),
                          showcase_layout = c("bottom"),
                          theme = value_box_theme(bg = "#00960aAA")),
                col_widths = c(6,6,6,6),
                row_heights = c(1,1)
                ),
                card(
                  full_screen = TRUE,
                  card_header("Summary of Populations"),
                  withSpinner(plotOutput("population_barchart"))
                ),
                card(
                  full_screen = TRUE,
                  card_header("ADSL"),
                  withSpinner(DT::dataTableOutput('demog_table'))
                ),
                col_widths = c(4,8,12),
                row_heights = c(3.6,1)
              )
            )
            
  ),
  nav_panel(
    "Vital Sign",
    layout_columns(
      card(
        card_header("Diastolic Blood Pressure (mmHg)"),
        withSpinner(plotlyOutput("vital_sign_plot_1"))
        ),
      card(
        card_header("Systolic Blood Pressure (mmHg)"),
        withSpinner(plotlyOutput("vital_sign_plot_2"))
      ),
      card(
        shinyWidgets::sliderTextInput(inputId = "visit_slider_1", 
                                      label = "Select Visit Number:", 
                                      choices = unique(na.omit(advs$AVISITN)),
                                      selected = 0
                                      )
      ),
      card(
        selectizeInput(
          "position",
          "Select Analysis Tiempoint",
          choices = NULL,
          multiple  = TRUE
        )
      ),
      card(
        selectizeInput(
          "age_gr",
          "Select Age Group",
          choices = NULL,
          multiple  = TRUE
        )
      ),
      card(
        selectizeInput(
          "sex_2",
          "Select sex",
          choices = NULL,
          multiple  = TRUE
        )
      ),
      col_widths = c(6,6,3,3,3,3),
      row_heights = c(3.2,1)
      )
    ),
  nav_panel(
    "Adverse Events",
    layout_columns(
      layout_columns(
        card(
          card_header(textOutput("organ_class_click")),
          highchartOutput("aedecod_class_pl"),
          highchartOutput("aedecod_class_low"),
          highchartOutput("aedecod_class_high"),
          full_screen = TRUE
        ),
        col_widths = c(12),
        col_heights = c(1),
        height = 630
      ),
      card(
        card_header("System Organ Class"),
        highchartOutput("organ_class"),
        fill = FALSE,
        height = 630
      ),
      card(
        card_header("Individual Adverse Events"),
        highchartOutput("adverse_range_plot"),
        fill = FALSE,
        full_screen = TRUE
      ),
      col_widths = c(3,9,12),
      col_heights = c(1),
      fill = FALSE
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_page),
    nav_item(link_linkedIn)
  )
)

