#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

df_trackviz <- readRDS("../Data/RDS Files/df_trackviz.rds") %>%
  rename(`Album`=album_name,
         `Duration (s)`=duration_s,
         `Track position in album (%)*`=track_starting_point_normalized,
         `Loudness`=loudness,
         `Tempo (bpm)`=tempo,
         `Sentiment (AFINN)`=net,
         `Number of words in lyrics`=num_of_words,
         `Mean word length in lyrics`=mean_word_length,
         `Album release date`=release_date)

album_colors <- c("A Sun Came!"="#7b7644", "Michigan"="#d32831",
                  "Seven Swans"="#010508", "Illinois"="#4d758c",
                  "The Avalanche"="#e8681b", "The Age of Adz"="#ae3625",
                  "All Delighted People"="#b37f61", "Carrie & Lowell"="#334249",
                  "The Ascension"="#f8a139", "A Beginner's Mind"="#1769a1",
                  "Javelin"="#e688a3")

library(DT)
library(tidyverse)
library(ggimage)
library(shiny)
library(bslib)
library(ggplot2)
library(ggExtra)

df_num <- df_trackviz %>% select(`Sentiment (AFINN)`,
                                 `Duration (s)`,
                                 `Track position in album (%)*`,
                                 `Loudness`,
                                 `Tempo (bpm)`,
                                 `Number of words in lyrics`,
                                 `Mean word length in lyrics`,
                                 `Album release date`)

ui <- navbarPage("SufjanViz",
  tabPanel("Scatterplot",
      page_sidebar(
        mainPanel(
          plotOutput("scatter"),
          h6("Summary Statistics", align="center"),
          div(DT::DTOutput("table"), style="font-size:75%")
        ),
        sidebar=sidebar(
        varSelectInput("xvar", "X axis", df_num, selected = "Duration (s)"),
        varSelectInput("yvar", "Y axis", df_num, selected = "Tempo (bpm)"),
        checkboxInput("exclude_instrumentals", "Exclude instrumental tracks", FALSE),
        checkboxGroupInput(
          "Album", "Filter by album",
          choices = unique(df_trackviz$Album),
          selected = unique(df_trackviz$Album)
        ),
        hr(), # Add a horizontal rule
        checkboxInput("by_albums", "Indicate album", TRUE),
        conditionalPanel(condition="input.by_albums==true",
                         checkboxInput("album_images", "Use album covers as points", FALSE)),
        checkboxInput("show_margins", "Show distributions", FALSE),
        checkboxInput("smooth", "Add smoothing"),
        conditionalPanel(condition="input.smooth==true",
                         selectInput("smooth_type", "Smoothing function",
                                     list("Linear"="lm", "Loess"="loess"), selected = "Linear"))
        )
      )),
  tabPanel("Barplot",
           page_sidebar(
           ),
           mainPanel(
           )
  ),
  tabPanel("Data sourcing",
           mainPanel())
)

server <- function(input, output, session) {

  observeEvent(input$by_albums, {
    if(!input$by_albums){
      updateCheckboxInput(session, "album_images", value=F)
    }
  })

  subsetted <- reactive({
    req(input$Album)
    if(input$exclude_instrumentals){
      df_trackviz %>% filter(Album %in% input$Album, text != "")
    }
    else{
      df_trackviz %>% filter(Album %in% input$Album)
    }
  })

  subsetted_table <- reactive({
    req(input$yvar)
    req(input$xvar)
    df_trackviz %>% select(input$xvar, input$yvar) %>%
      pivot_longer(everything(), names_to="variable", values_to="value") %>%
      group_by(variable) %>%
      summarize(Min=round(min(value, na.rm=T), 2),
                Q1=round(quantile(value, probs=0.25), 2),
                Mean=round(mean(value, na.rm=T), 2),
                Median=round(median(value, na.rm=T), 2),
                Q3=round(quantile(value, probs=0.75), 2),
                Max=round(max(value, na.rm=T), 2)) %>%
      pivot_longer(-variable, names_to="Statistic") %>%
      pivot_wider(names_from=variable)
  })

  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) +
      theme_bw() +
      list(
        theme(legend.position = "bottom"),
        if(input$by_albums) aes(color = Album),
        if(input$by_albums) scale_color_manual(values=album_colors),
        geom_point(),
        if(input$album_images) geom_image(aes(image=album_img_path, color=NULL), size=.03),
        if(input$album_images & !input$show_margins & !input$smooth) theme(legend.position="none"),
        if(input$smooth) geom_smooth(method=input$smooth_type),
        labs(color=""),
        if(input$xvar=="Track position in album (%)*" |
           input$yvar=="Track position in album (%)*") labs(caption="* Calculated as % of the way through the album's duration the track starts at.")
      )

    if (input$show_margins) {
      margin_type <- if (input$by_albums) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$by_albums, groupFill = input$by_albums)
    }

    p
  }, res = 100)

  output$table <- DT::renderDT({
    DT::datatable(subsetted_table(),
                  rownames=F,
                  options = list(dom = 't'))
  })
}

shinyApp(ui, server)
