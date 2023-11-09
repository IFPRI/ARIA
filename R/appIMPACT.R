#' App for IMPACT (App foR ImpAct)
#'
#' @param folder Folder path where IMPACT outputs are stored.
#'
#' Typically of format \code{<model_folder>/OutputFiles/Scenarios}
#' @param base_year Base year for relative calculations. It will be calulcated automatically if not provided.
#'
#' @import shiny shinythemes ggplot2
#' @importFrom dplyr case_when %>% filter across group_by mutate
#' @importFrom utils select.list menu
#' @importFrom tools file_ext
#' @importFrom reportIMPACT getReport
#' @return Fires the app for Scenarios
#' @export
#'
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{appIMPACT(folder = <model_folder>/OutputFiles/Scenarios)}
#' @export

appIMPACT <- function(folder, base_year = NULL) {

    c <- flag <- NULL

    indicator <- region <- yrs <- unit2 <- value <- NULL

    choice <- gsub(pattern = "gdx", replacement = "rds",
                   x = rdsMaker(folder = folder, base_year = base_year))

    df_prep <- NULL

    for (file_vector in paste0(folder, "/", choice)) {
        cat("Reading ", basename(file_vector), "\n")
        df <- readRDS(file_vector)
        df$flag <- gsub(pattern = ".rds",
                        replacement = "",
                        x = basename(file_vector))
        df_prep <- rbind(df_prep, df)
    }

    df_prep$yrs <- as.numeric(as.character(df_prep$yrs))

    # Define UI for application that draws a histogram
    ui <- fluidPage(

        # Application title
        titlePanel("IMPACT Standard Results"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(

            sidebarPanel(

                selectInput(inputId = "s_indicator", label = strong("Indicator"),
                            choices = unique(df_prep$indicator),
                            selected = "Population"),

                selectInput(inputId = "s_region", "Regions",
                            choices = unique(df_prep$region),
                            multiple = TRUE,
                            selected = unique(df_prep$region)),

                sliderInput(inputId = "s_year", label = strong("Year"),
                            min = min(df_prep$yrs),
                            value = c(min(df_prep$yrs), max(df_prep$yrs)),
                            max = max(df_prep$yrs),
                            step = 1, sep = ""),


                fluidRow(column(4,
                                radioButtons(inputId = "line_plot_type",
                                             label = "Select the plot type",
                                             choices = c("None", "Relative", "Index"),
                                             selected = "None")),
                         column(4,
                                checkboxInput("free_y", label = strong("Free Y-axis"),
                                              value = FALSE,
                                              width = NULL))
                ),

                selectInput(inputId = "s_flag", label = strong("Scenarios"),
                            choices = unique(df_prep$flag),
                            multiple = TRUE,
                            selected = unique(df_prep$flag)),

                selectInput(inputId = "s_base_run", label = strong("BASE run"),
                            choices = c("", unique(df_prep$flag)),
                            selected = NULL,
                            multiple = FALSE),

                downloadButton(label = "Line plot", outputId = "downloadPlot"),
                downloadButton(label = "Area plot", outputId = "downloadPlotArea"),

                fluidRow(column(1,
                                imageOutput("preImage"))
                ),

                width = 3
            ),


            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Line plot",
                                     plotOutput(outputId = "plotgraph", width = "1400px", height = "900px")),
                            tabPanel("Area Plot",
                                     plotOutput(outputId = "areaplot", width = "1400px", height = "900px")),
                            tabPanel("Comparison to base RUN",
                                     DT::dataTableOutput(outputId = "compare_df")),
                            tabPanel("Comparison to base YEAR",
                                     DT::dataTableOutput(outputId = "compare_df_year"))
                ),
                textOutput(outputId = "Subtitle1"),
                tags$a(href = "https://github.com/abhimishr/ARIA",
                       "Powered by ARIA (App foR ImpAct)", target = "_blank")
            )
        )
    )

    # Define server logic
    server <- function(input, output, session) {

        # Send a pre-rendered image, and don't delete the image after sending it
        output$preImage <- renderImage({
            # When input$n is 3, filename is ./images/image3.jpeg
            filename <- system.file("extdata", "ifpri-logo.png", package = "ARIA")

            # Return a list containing the filename and alt text
            list(src = filename,
                 width = 188,
                 height = 105)

        }, deleteFile = FALSE)

        dfx <- reactive({
            class(df_prep) <- "data.frame"

            df_prep %>%
                filter(indicator == input$s_indicator) %>%
                filter(region %in% input$s_region) %>%
                filter(flag %in% input$s_flag) %>%
                filter(yrs %in% c(input$s_year[1]:input$s_year[2])) %>%
                filter(unit2 %in% case_when(input$line_plot_type == "None" ~ unique(grep("wrt", unit2, value = TRUE, invert = TRUE)),
                                            input$line_plot_type == "Relative" ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert = TRUE)), value = TRUE)),
                                            input$line_plot_type == "Index" ~ unique(grep("Index", unit2, value = TRUE)))
                )
        })

        dfx_bar <- reactive({
            class(df_prep) <- "data.frame"

            df_prep %>%
                filter(indicator == input$s_indicator) %>%
                filter(region %in% setdiff(input$s_region, "GLO")) %>%
                filter(flag %in% input$s_flag) %>%
                filter(yrs %in% c(input$s_year[1]:input$s_year[2])) %>%
                filter(unit2 %in% case_when(input$line_plot_type == "None" ~ unique(grep("wrt", unit2, value = TRUE, invert = TRUE)),
                                            input$line_plot_type == "Relative" ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert = TRUE)), value = TRUE)),
                                            input$line_plot_type == "Index" ~ unique(grep("Index", unit2, value = TRUE)))
                )
        })

        # Some pre-selection for ggplot arguments
        scales <- reactive({
            scales <- "fixed"
            if (input$free_y) scales <- "free_y"
        })

        plot_title <- reactive({
            append <- ""
            if (input$line_plot_type == "Relative") append <-  " (change)"
            if (input$line_plot_type == "Index") append <-  " (Index)"
            title <- ggtitle(paste0(unique(dfx()$indicator), append))
        })

        horizontal_line <- reactive({
            yintercept <- 0
            if (input$line_plot_type == "Index") yintercept <- 1
        })

        p_line <-  reactive({
            ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) +
                theme_bw(base_size = 25) +
                facet_wrap(region ~ ., scales = scales()) +
                geom_line(aes(color = dfx()$flag, group = dfx()$flag), linewidth = 1.3) +
                geom_point(shape = 1, size = 1.4) +
                ylab(unique(dfx()$unit2)) +
                xlab("Years") +
                ggtitle(plot_title()) +
                theme(legend.position = "bottom", legend.direction = "vertical") +
                geom_hline(yintercept = horizontal_line(), linetype = "longdash", linewidth = 1.2, color = "gray") +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(color = guide_legend(title = "Scenario"))
        })

        p_bar <-  reactive({
            ggplot(dfx_bar(), aes(x = dfx_bar()$yrs, y = dfx_bar()$value)) +
                theme_bw(base_size = 25) +
                facet_wrap(. ~ flag) +
                #    {if(free_y) facet_wrap(.~region, scales = "free_y")}+
                geom_area(position = "stack", aes(fill = dfx_bar()$region, group = dfx_bar()$region), color = "black") +
                stat_summary(fun = sum, geom = "line", size = 2) +
                ylab(unique(dfx_bar()$unit)) +
                xlab("Years") +
                ggtitle(plot_title()) +
                theme(legend.position = "bottom") +
                geom_hline(yintercept = horizontal_line(), linetype = "longdash", linewidth = 1.2, color = "gray") +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(fill = guide_legend(title = "Regions"))
        })

        output$help_text <- renderUI({
            HTML("<b>Cannot generate this table witthout selecting base year. Please wait after selecting base year. The table generation takes time.</b>")
        })

        output$compare_df <- DT::renderDataTable({

            DT::datatable(df_prep[, c("model", "region", "yrs", "indicator", "value", "unit", " unit2", "flag")] %>%
                              filter(indicator == input$s_indicator) %>%
                              filter(region %in% input$s_region) %>%
                              filter(flag %in% input$s_flag) %>%
                              filter(yrs %in% c(input$s_year[1]:input$s_year[2])) %>%
                              filter(unit2 %in% case_when(input$line_plot_type == "None" ~ unique(grep("wrt", unit2, value = TRUE, invert = TRUE)),
                                                          input$line_plot_type == "Relative" ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert = TRUE)), value = TRUE)),
                                                          input$line_plot_type == "Index" ~ unique(grep("Index", unit2, value = TRUE)))
                              ) %>%
                              group_by(across(c("model", "region", "indicator", "unit", "unit2", "yrs"))) %>%
                              mutate(delta_base_RUN = case_when(!is.null(input$s_base_run) ~ paste0(round(100 * ((value / value[flag == input$s_base_run]) - 1), 2), "%"),
                                                                input$s_base_run == "" ~ paste0("Select base run"))
                              ) %>%
                              filter(case_when(!is.null(input$s_base_run) ~ flag %in% setdiff(unique(df_prep$flag), input$s_base_run))) %>%
                              filter(yrs == input$s_year[2]) %>%
                              mutate(value = round(value, 2))
            )
        })

        output$compare_df_year <- DT::renderDataTable({
            DT::datatable(df_prep[, c("model", "region", "yrs", "indicator", "value", "unit", "unit2", "flag")] %>%
                              filter(indicator == input$s_indicator) %>%
                              filter(region %in% input$s_region) %>%
                              filter(flag %in% input$s_flag) %>%
                              filter(yrs %in% c(input$s_year[1]:input$s_year[2])) %>%
                              filter(unit2 %in% case_when(input$line_plot_type == "None" ~ unique(grep("wrt", unit2, value = TRUE, invert = TRUE)),
                                                          input$line_plot_type == "Relative" ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert = TRUE)), value = TRUE)),
                                                          input$line_plot_type == "Index" ~ unique(grep("Index", unit2, value = TRUE)))
                              ) %>%
                              group_by(across(c("model", "region", "indicator", "unit", "unit2", "flag"))) %>%
                              mutate(delta_base_YEAR = paste0(round(100 * ((value / value[yrs == input$s_year[1]]) - 1), 2), "%")) %>%
                              filter(yrs == input$s_year[2]) %>%
                              mutate(value = round(value, 2))
            )
        })


        output$plotgraph <- renderPlot({
            p_line()
        })
        output$areaplot <- renderPlot({
            p_bar()
        })

        output$downloadPlot <- downloadHandler(
            filename = reactive({
                paste0(dfx()$indicator, "_LP_", input$line_plot_type, ".png", sep = "")
            }),
            content = function(filename) {
                ggsave(filename = filename,
                       plot = p_line(),
                       width = 16,
                       height = 16)
            })

        output$downloadPlotArea <- downloadHandler(
            filename = reactive({
                paste0(dfx()$indicator, "_SA_", input$line_plot_type, ".png", sep = "")
            }),
            content = function(filename) {
                ggsave(filename = filename,
                       plot = p_bar(),
                       width = 16,
                       height = 16)
            })

        session$onSessionEnded(function() {
            stopApp()
        })

    }


    # Run the application
    runApp(shinyApp(ui = ui, server = server),
           launch.browser = getOption("shiny.launch.browser", interactive()),
           display.mode = "normal")

}
