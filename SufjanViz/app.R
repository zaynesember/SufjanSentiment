
library(tidyverse)
library(ggimage)
library(shiny)
library(bslib)
library(ggplot2)
library(ggExtra)
library(grid)
library(jpeg)

album_colors <- c("A Sun Came!"="#7b7644", "Michigan"="#d32831",
                  "Seven Swans"="#010508", "Illinois"="#4d758c",
                  "The Avalanche"="#e8681b", "The Age of Adz"="#ae3625",
                  "All Delighted People"="#b37f61", "Carrie & Lowell"="#334249",
                  "The Ascension"="#f8a139", "A Beginner's Mind"="#1769a1",
                  "Javelin"="#e688a3")

album_levels <- c("A Sun Came!", "Michigan",
                  "Seven Swans", "Illinois",
                  "The Avalanche", "The Age of Adz",
                  "All Delighted People", "Carrie & Lowell",
                  "The Ascension", "A Beginner's Mind",
                  "Javelin")

album_colors_accents <- c("A Sun Came!"="#c3a8a3", "Michigan"="#b9d3c6",
                          "Seven Swans"="#010508", "Illinois"="#fbe956",
                          "The Avalanche"="#7793a8", "The Age of Adz"="#1b1d1a",
                          "All Delighted People"="#ffffff", "Carrie & Lowell"="#b7b396",
                          "The Ascension"="#33323a", "A Beginner's Mind"="#f1d76e",
                          "Javelin"="#a78f6b")

df_trackviz <- readRDS("Data/df_trackviz.rds") %>%
  rename(`Album`=album_name,
         `Duration (s)`=duration_s,
         `Track position in album (%)*`=track_starting_point_normalized,
         `Loudness`=loudness,
         `Tempo (bpm)`=tempo,
         `Sentiment (AFINN)`=net,
         `Number of words in lyrics`=num_of_words,
         `Words per minute`=words_per_minute,
         `Mean word length in lyrics`=mean_word_length,
         `Album release date`=release_date) %>%
  mutate(Album=factor(Album, levels=album_levels))


df_num <- df_trackviz %>% select(`Duration (s)`,
                                 `Track position in album (%)*`,
                                 `Loudness`,
                                 `Tempo (bpm)`,
                                 `Number of words in lyrics`,
                                 `Words per minute`,
                                 `Mean word length in lyrics`,
                                 `Sentiment (AFINN)`,
                                 `Album release date`)

df_bar <- df_trackviz %>% select(`Duration (s)`,
                                 `Track position in album (%)*`,
                                 `Loudness`,
                                 `Tempo (bpm)`,
                                 `Number of words in lyrics`,
                                 `Words per minute`,
                                 `Mean word length in lyrics`,
                                 `Sentiment (AFINN)`)
# UI

ui <- navbarPage("SufjanViz",
                 tabPanel("Scatterplot",
                          page_sidebar(
                            tags$head(tags$style(HTML(".selectize-input,
                                                      .selectize-dropdown,
                                                      .checkbox,
                                                      #smooth_type-label,
                                                      #margin_type-label,
                                                      #xvar-label,
                                                      #yvar-label {font-size: 75%;}"))),
                            mainPanel(
                              plotOutput("scatter"),
                              hr(),
                              h6("Summary Statistics", align="center"),
                              div(DT::DTOutput("table"), style="font-size:75%")
                            ),
                            sidebar=
                                sidebar(
                                  h6("Plot options"),
                                  varSelectInput("xvar", "X variable", df_num, selected = "Duration (s)"),
                                  varSelectInput("yvar", "Y variable", df_num, selected = "Tempo (bpm)"),
                                  checkboxInput("exclude_instrumentals", "Exclude instrumental tracks", FALSE),
                                  checkboxInput("by_albums", "Group by album", TRUE),
                                  conditionalPanel(condition="input.by_albums==true",
                                                   checkboxInput("album_images", "Use album covers as points", FALSE),
                                                   conditionalPanel(condition="input.album_images==true",
                                                                    sliderInput("slider", "Point size",
                                                    min = 0.001, max = 0.5, value = .03))),
                                  checkboxInput("show_margins", "Show distributions", FALSE),
                                  conditionalPanel(condition="input.show_margins==true",
                                                   selectInput("margin_type", "Type", list("Density", "Histogram"), "Density")),
                                  checkboxInput("smooth", "Add smoothing"),
                                  conditionalPanel(condition="input.smooth==true",
                                                   selectInput("smooth_type", "Smoothing function",
                                                               list("Linear"="lm", "Loess"="loess"), selected = "Linear")),
                                  hr(), # Add a horizontal rule
                                  checkboxGroupInput(
                                    "Album", "Filter by album",
                                    choices = unique(df_trackviz$Album),
                                    selected = unique(df_trackviz$Album)
                                  )
                                )
                          )),
                 tabPanel("Barplot",
                          page_sidebar(
                            tags$head(tags$style(HTML(".selectize-input,
                                                      .selectize-dropdown,
                                                      .checkbox,
                                                      #barvar-label {font-size: 75%;}"))),
                            mainPanel(
                              plotOutput("bar"),
                              hr(),
                              h6("Summary Statistics", align="center")
                              #div(DT::DTOutput("table_bar"), style="font-size:75%")
                            ),
                            sidebar=sidebar(
                              h6("Plot options"),
                              varSelectInput("barvar", "Variable", df_bar, selected = "Duration (s)"),
                              hr(), # Add a horizontal rule
                              checkboxGroupInput(
                                "Album_bar", "Filter by album",
                                choices = unique(df_trackviz$Album),
                                selected = unique(df_trackviz$Album)
                              )
                            )
                          )
                 ),
                 tabPanel("About",
                          mainPanel())
)

# Server

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
    if(input$exclude_instrumentals){
      df_sub <- df_trackviz %>% filter(Album %in% input$Album, text != "") %>%
        select(input$xvar, input$yvar)
    }
    else{
      df_sub <- df_trackviz %>% select(input$xvar, input$yvar)
    }

    if("Album release date" %in% names(df_sub)) df_sub <- df_sub %>% select(-"Album release date")

    df_sub %>%
      pivot_longer(everything(), names_to="variable", values_to="value") %>%
      group_by(variable) %>%
      summarize(Min=round(min(value, na.rm=T), 2),
                Q1=round(quantile(value, probs=0.25, na.rm=T), 2),
                Mean=round(mean(value, na.rm=T), 2),
                Median=round(median(value, na.rm=T), 2),
                Q3=round(quantile(value, probs=0.75, na.rm=T), 2),
                Max=round(max(value, na.rm=T), 2),
                `Std. dev.`=round(sd(value, na.rm=T))) %>%
      pivot_longer(-variable, names_to="Statistic") %>%
      pivot_wider(names_from=variable)
  })

  # Scatter tab elements
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) +
      theme_bw() +
      list(
        theme(legend.position = "bottom",
              legend.text=element_text(size=8)),
        if(input$by_albums) aes(color = Album),
        if(input$by_albums) scale_color_manual(values=album_colors),
        geom_point(),
        if(input$album_images) geom_image(aes(image=album_img_path, color=NULL), size=input$slider),
        if(input$album_images & !input$show_margins & !input$smooth) theme(legend.position="none"),
        if(input$smooth) geom_smooth(method=input$smooth_type, se=F),
        labs(color=""),
        if(input$xvar=="Track position in album (%)*" |
           input$yvar=="Track position in album (%)*") labs(caption="* Calculated as % of the way through the album's duration the track starts at.")
      )

    if (input$show_margins) {
      #type <- if(input$margin_type=="Density") "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = tolower(input$margin_type), margins = "both",
                               size = 8, groupColour = input$by_albums, groupFill = input$by_albums)
    }

    p
  }, res = 100)

  output$table <- DT::renderDT({
    DT::datatable(subsetted_table(),
                  rownames=F,
                  options = list(dom = 't'))
  })

  # Bar tab elements

  subsetted_bar <- reactive({
    req(input$Album_bar)
    df_trackviz %>% filter(Album %in% input$Album_bar)
  })

  output$bar <- renderPlot({

    strip_text_size <- {
      if(length(input$Album_bar)==11) 6
      else if(length(input$Album_bar %in% 6:7)) 6.5
      else if(length(input$Album_bar %in% 4:6)) 7
      else if(length(input$Album_bar %in% 0:4)) 8
    }

    p2 <- ggplot(subsetted_bar(), aes(x=track_number, y=!!input$barvar,
                                      #color=factor(`Album`),
                                      #group=factor(`Album`),
                                      fill=factor(`Album`))) +
      theme_bw() +
      list(
        geom_bar(stat="identity",position="dodge", width=.55),
        scale_x_discrete(expand = c(0,0)),
        #scale_color_manual(values=album_colors),
        scale_fill_manual(values=album_colors),

        theme(
          axis.title.x=element_blank(),
          axis.text.y = element_text(size=7),
          axis.title.y = element_text(angle=90, size=9,
                                      margin=margin(0,5,0,0, unit="pt")),
          legend.position="none",
          strip.background = element_blank(),
          strip.text.x=element_text(size=strip_text_size, vjust=1, margin=margin(-0.1,0,0,0)),
          panel.spacing = unit(.05, "lines"),
          panel.border = element_blank(),
          plot.margin = margin(0,.05,.05,0),
          panel.grid.major = element_line(color="seashell3",
                                          linewidth=.25, linetype="dotted")
        )
      ) +
      facet_wrap(~Album, nrow=1, scales="free_x", strip.position="bottom",
                 labeller=label_wrap_gen(10))

    p2
  }, res = 100)

  # output$table2 <- DT::renderDT({
  #   DT::datatable(subsetted_table(),
  #                 rownames=F,
  #                 options = list(dom = 't'))
  # })
}

# TODO
# Add explanation to scatterplot that each observation is a track
# word clouds
# regression tool

shinyApp(ui, server)
