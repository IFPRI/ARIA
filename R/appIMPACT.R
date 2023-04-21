#' App for IMPACT (App foR ImpAct)
#'
#' @param folder Folder path where IMPACT outputs are stored.
#'
#' Typically of format \code{<model_folder>/OutputFiles/Scenarios}
#' @param base_year Base year for relative calculations.
#' It will be calculated automatically if not provided.
#'
#' @import shiny shinythemes DOORMAT reportIMPACT DOORMAT reportIMPACT ggplot2
#' @importFrom dplyr case_when %>% filter across group_by mutate
#' @importFrom utils select.list menu
#' @importFrom tools file_ext
#' @return Fires the app for IMPACT runs
#' @export
#'
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{appIMPACT(folder = <model_folder>/OutputFiles/Scenarios)}
#' @export

appIMPACT <- function(folder, base_year = NULL) {

    cat(paste(rep("=", 50), collapse = ""))
    if (is.null(base_year)) cat(
        "\nNo base year provided.\n
        Using the first available datapoint for base year calculation(s).\n")
    cat(paste(rep("=", 50), collapse = ""))


    prep_flag <- as.vector(Sys.info()["effective_user"])

    indicator <- region <- yrs <- unit2 <- value <- NULL

    files <- grep(pattern = ".gdx", x = list.files(path = folder),value = TRUE)

    choice <- select.list(choices = files, title = "\nPlease Select IMPACT runs:",
                          multiple = TRUE,
                          graphics = getOption("menu.graphics"))

    skip_next_choice = FALSE

    for (file_vector in paste0(folder,"/", choice)) {
        if (file.exists(gsub(pattern = ".gdx", replacement = ".rds", x = (file_vector)))) {
            cat("A preprocessed RDS file already exists for ", basename(file_vector), "\n")
            cat("You might want to run getReport() on this gdx file manually if you want to run fresh reporting", "\n")
            cat("Porceeding with existing RDS file for this IMPACT run.", "\n")
        }
        if (!file.exists(gsub(pattern = ".gdx", replacement = ".rds", x = (file_vector)))) {
            cat("No preprocessed RDS file exists for ", basename(file_vector), "\n")
            if (!skip_next_choice) {
                user_choice <- menu(c("Yes", "No"),
                                    title="Would you like to convert this\nGDX file into a RDS file now?\nChoosing 'no' will stop the program.")
            }
            if (user_choice == 1) {
                skip_next_choice = TRUE
                user_choice = 1
                }
            if (user_choice == 2) stop("\nCould not convert GDX file to RDS.\nAborting ....... \nHint: Use RDS files if they exist or choose 'yes' at previous prompt")
            cat("Attempting to convert to RDS file ......", "\n")
            if (is.null(base_year)) getReport(gdx = file_vector, prep_flag = prep_flag)
            if (!is.null(base_year)) getReport(gdx = file_vector, prep_flag = prep_flag, base_year = base_year)
            choice[choice == basename(file_vector)] <- gsub(pattern = ".gdx", replacement = ".rds", x = basename(file_vector))
        }
        choice[choice == basename(file_vector)] <- gsub(pattern = ".gdx", replacement = ".rds", x = basename(file_vector))
    }

    df_prep <- NULL

    for (file_vector in paste0(folder,"/", choice)) {
        cat("Reading ", basename(file_vector), "\n")
        df <- readRDS(file_vector)
        df$flag <- gsub(pattern = ".rds", replacement = "", x = basename(file_vector))
        df_prep <- rbind(df_prep,df)
    }

    df_prep$yrs <- as.numeric(as.character(df_prep$yrs))

    # Define UI for application that draws a histogram
    ui <- fluidPage(

        # Application title
        titlePanel("IMPACT Standard Results"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(

            sidebarPanel(

                selectInput(inputId = "indicator", label = strong("Indicator"),
                            choices = unique(df_prep$indicator),
                            selected = "Population"),

                selectInput(inputId = "region", "Regions",
                            choices = unique(df_prep$region),
                            multiple = TRUE,
                            selected = unique(df_prep$region)),

                sliderInput(inputId = "year", label = strong("Year"),
                            min = min(df_prep$yrs),
                            value = c(min(df_prep$yrs),max(df_prep$yrs)),
                            max = max(df_prep$yrs),
                            step=1,sep = ""),

                # checkboxInput("donum1", "Line plot", value = T),
                # checkboxInput("donum2", "Bar plot", value = T),

                # checkboxInput("unit_def", "Line plot", value = T),
                # checkboxInput("unit_relative", "Line plot - relative", value = F),
                # checkboxInput("unit_index", "Line plot - index", value = F),

                fluidRow(column(4,
                                radioButtons(inputId = "line_plot_type", ,
                                             label = "Select the plot type",
                                             choices = c("None", "Relative", "Index", ),
                                             selected = "None")),
                         column(4,
                                checkboxInput("free_y", label = strong("Free Y-axis"),
                                              value = FALSE,
                                              width = NULL))
                         ),

                selectInput(inputId = "flag", label = strong("Scenarios"),
                            choices = unique(df_prep$flag),
                            multiple = TRUE,
                            selected = unique(df_prep$flag)),

                selectInput(inputId = "base_run", label = strong("BASE run"),
                            choices = c("", unique(df_prep$flag)),
                            selected = NULL,
                            multiple = FALSE),

                downloadButton(label = "Line plot", outputId = "downloadPlot"),
                downloadButton(label = "Area plot", outputId = "downloadPlotArea"),

                fluidRow(column(1,
                                imageOutput("preImage"))
                ),

                width=3
            ),


            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Line plot",
                                     plotOutput(outputId="plotgraph", width="1400px", height="900px")),
                            tabPanel("Area Plot",
                                     plotOutput(outputId="areaplot", width="1400px", height="900px")),
                            tabPanel("Comparison to base RUN",
                                     DT::dataTableOutput(outputId = 'compare_df')),
                            tabPanel("Comparison to base YEAR",
                                     DT::dataTableOutput(outputId = 'compare_df_year'))
                ),
                textOutput(outputId = "Subtitle1"),
                tags$a(href = "https://github.com/abhimishr/ARIA",
                       "Powered by ARIA (App foR ImpAct)", target = "_blank")
            )
        )
    )

    # Define server logic required to draw a histogram
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
                filter(indicator == input$indicator) %>%
                filter(region %in% input$region) %>%
                filter(flag %in% input$flag) %>%
                filter(yrs %in% c(input$year[1]:input$year[2])) %>%
                filter(unit2 %in% case_when(input$line_plot_type == "None", ~ unique(grep("wrt", unit2, value = TRUE,invert = TRUE)),
                                            input$line_plot_type == "Relative", ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),value=T)),
                                            input$line_plot_type == "Index", ~ unique(grep("Index", unit2, value = TRUE))
                )
                )
        })

        dfx_bar <- reactive({
            class(df_prep) <- "data.frame"

            df_prep %>%
                filter(indicator == input$indicator) %>%
                filter(region %in% setdiff(input$region,"GLO")) %>%
                filter(flag %in% input$flag) %>%
                filter(yrs %in% c(input$year[1]:input$year[2])) %>%
                filter(unit2 %in% case_when(input$line_plot_type == "None", ~ unique(grep("wrt", unit2, value = TRUE,invert = TRUE)),
                                            input$line_plot_type == "Relative", ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),value=T)),
                                            input$line_plot_type == "Index", ~ unique(grep("Index", unit2, value = TRUE))
                )
                )
        })


        p_line <-  reactive({
            ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) +
                theme_minimal(base_size = 25) +
                facet_wrap(region~.) +
                {if (input$free_y) facet_wrap(region~., scales = "free_y")}+
                geom_line(aes(color=dfx()$flag, group=dfx()$flag), linewidth =1.3) +
                geom_point(shape=1, size = 1.4) +
                ylab(unique(dfx()$unit2)) +
                xlab("Years") +
                ggtitle(unique(dfx()$indicator)) +
                {if (input$line_plot_type == "Relative") ggtitle(paste0(unique(dfx()$indicator), ", (change)"))} +
                {if (input$line_plot_type == "Index") ggtitle(paste0(unique(dfx()$indicator), ", (index)"))} +
                theme(legend.position = "bottom", legend.direction = "vertical") +
                {if (input$line_plot_type == "Relative") geom_hline(yintercept = 0, linetype="longdash", linewidth =1.2, color = "gray")} +
                {if (input$line_plot_type == "Index") geom_hline(yintercept = 1, linetype="longdash", linewidth = 1.2, color = "gray")} +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(color=guide_legend(title="IMPACT Run"))
        })

        p_bar <-  reactive({
            ggplot(dfx_bar(), aes(x = dfx_bar()$yrs, y = dfx_bar()$value)) +
                theme_minimal(base_size = 25) +
                facet_wrap(.~flag) +
                #    {if (free_y) facet_wrap(.~region, scales = "free_y")}+
                geom_area(position='stack',aes(fill=dfx_bar()$region,group=dfx_bar()$region),color="black") +
                stat_summary(fun = sum, geom = "line", size = 2) +
                ylab(unique(dfx_bar()$unit)) +
                xlab("Years") +
                ggtitle(unique(dfx_bar()$indicator)) +
                {if (input$line_plot_type == "Relative") ggtitle(paste0(unique(dfx_bar()$indicator), ", (change)"))} +
                {if (input$line_plot_type == "Index") ggtitle(paste0(unique(dfx_bar()$indicator), ", (index)"))} +
                theme(legend.position = "bottom") +
                {if (input$line_plot_type == "Relative") geom_hline(yintercept = 0, linetype="longdash", linewidth =1.2, color = "gray")} +
                {if (input$line_plot_type == "Index") geom_hline(yintercept = 0, linetype="longdash", linewidth = 1.2, color = "gray")} +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(fill=guide_legend(title="Regions"))
        })

        output$help_text <- renderUI({
            HTML("<b>Cannot generate this table witthout selecting base year. Please wait after selecting base year. The table generation takes time.</b>")
        })

        output$compare_df <- DT::renderDataTable({

            DT::datatable(df_prep[,c("model", "region", "yrs", "indicator", "value", "unit", "unit2", "flag")] %>%
                              filter(indicator == input$indicator) %>%
                              filter(region %in% input$region) %>%
                              filter(flag %in% input$flag) %>%
                              filter(yrs %in% c(input$year[1]:input$year[2])) %>%
                              filter(unit2 %in% case_when(input$line_plot_type == "None", ~ unique(grep("wrt", unit2, value = TRUE,invert = TRUE)),
                                                          input$line_plot_type == "Relative", ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),value=T)),
                                                          input$line_plot_type == "Index", ~ unique(grep("Index", unit2, value = TRUE))
                                                          )
                                     ) %>%
                              group_by(across(c("model", "region", "indicator", "unit", "unit2", "yrs"))) %>%
                              mutate(delta_base_RUN = case_when(!is.null(input$base_run) ~ paste0(round(100 * ((value / value[flag == input$base_run]) - 1),2),"%"),
                                                                input$base_run == "", ~ paste0("Select base run"))
                                     ) %>%
                              filter(case_when(!is.null(input$base_run) ~ flag %in% setdiff(unique(df_prep$flag),input$base_run))) %>%
                              filter(yrs == input$year[2]) %>%
                              mutate(value = round(value,2))
            )
        })

        output$compare_df_year <- DT::renderDataTable({
            DT::datatable(df_prep[,c("model", "region", "yrs", "indicator", "value", "unit", "unit2", "flag")] %>%
                              filter(indicator == input$indicator) %>%
                              filter(region %in% input$region) %>%
                              filter(flag %in% input$flag) %>%
                              filter(yrs %in% c(input$year[1]:input$year[2])) %>%
                              filter(unit2 %in% case_when(input$line_plot_type == "None", ~ unique(grep("wrt", unit2, value = TRUE,invert = TRUE)),
                                                          input$line_plot_type == "Relative", ~ unique(grep("wrt", unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),value=T)),
                                                          input$line_plot_type == "Index", ~ unique(grep("Index", unit2, value = TRUE)))
                                     ) %>%
                              group_by(across(c("model", "region", "indicator", "unit", "unit2", "flag"))) %>%
                              mutate(delta_base_YEAR = paste0(round(100 * ((value / value[yrs == input$year[1]]) - 1),2),"%")) %>%
                              filter(yrs == input$year[2]) %>%
                              mutate(value = round(value,2))
            )
        })


        output$plotgraph = renderPlot({
            p_line()
        })
        output$areaplot = renderPlot({
            p_bar()
        })

        output$downloadPlot <- downloadHandler(
            filename = reactive({
                paste0(dfx()$indicator,"_LP_", input$line_plot_type,'.png', sep='')
                }),
            content = function(filename) {
                ggsave(filename = filename,
                       plot = p_line(),
                       width=16,
                       height=16)
            })

        output$downloadPlotArea <- downloadHandler(
            filename = reactive({
                paste0(dfx()$indicator,"_SA_", input$line_plot_type,'.png', sep='')
            }),
            content = function(filename) {
                ggsave(filename = filename,
                       plot = p_bar(),
                       width=16,
                       height=16)
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

