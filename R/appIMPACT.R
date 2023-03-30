#' Title
#'
#' @param folder Folder path where IMPACT outputs are stored
#'
#' @import shiny shinythemes DOORMAT reportIMPACT DOORMAT reportIMPACT ggplot2
#' @importFrom dplyr case_when %>% filter
#' @importFrom utils select.list
#' @return Fires the app for IMPACT runs
#' @export
#'
#' @examples
#' \dontrun{appIMPACT()}
#' @export

appIMPACT <- function(folder){
    #folder="C:/EPTD/Modeling/IMPACT Non Mod Git/OutputFiles/Scenarios"

    #
    # This is a Shiny web application. You can run the application by clicking
    # the 'Run App' button above.
    #
    # Find out more about building applications with Shiny here:
    #
    #    http://shiny.rstudio.com/
    #

    indicator <- yrs <- unit2 <- NULL

    files <- grep(pattern = ".rds",x = list.files(path = folder),value = TRUE)

    choice <- select.list(choices = files, title = "Please Select IMPACT runs:",
                          multiple = TRUE,
                          graphics = getOption("menu.graphics"))

    df_prep <- NULL

    for(file_vector in paste0(folder,"/",choice)){
        cat("Reading ",basename(file_vector), "\n")
        df <- readRDS(file_vector)
        df$flag <- gsub(pattern = ".rds",replacement = "",x = basename(file_vector))
        df_prep <- rbind(df_prep,df)
    }

    df_prep$yrs <- as.factor(as.character(df_prep$yrs))

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

                sliderInput(inputId = "year", label = strong("Year"),
                            min = min(df$yrs),
                            value = c(min(df$yrs),max(df$yrs)),
                            max = max(df$yrs),
                            step=1,sep = ""),

                # checkboxInput("donum1", "Line plot", value = T),
                # checkboxInput("donum2", "Bar plot", value = T),

                # checkboxInput("unit_def", "Line plot", value = T),
                # checkboxInput("unit_relative", "Line plot - relative", value = F),
                # checkboxInput("unit_index", "Line plot - index", value = F),

                radioButtons(inputId = "line_plot_type" ,
                             label = "Select the plot type",
                             choices = c("Default", "Relative", "Index" )),

                # sliderInput("wt1","Lineplot Weight",min=1,max=3,value=3),
                # sliderInput("wt2","Barplot weight",min=1,max=3,value=3),
                checkboxInput("free_y", label = strong("Free Y-axis"),
                              value = FALSE,
                              width = NULL),

                width=3
            ),

            # Output: Description, lineplot, and reference
            # mainPanel(
            #     plotOutput(outputId = "impact_plot", height = "1200px"),
            #     # textOutput(outputId = "desc"),
            #     # tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
            #     #tableOutput("DataTable")
            #     ),

            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Line plot",
                                     plotOutput(outputId="plotgraph", width="1400px",height="900px")),
                            tabPanel("Area Plot",
                                     plotOutput(outputId="areaplot", width="1400px",height="900px"))
                )
            )
        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output) {

        dfx <- reactive({
            class(df_prep) <- "data.frame"
            #df_prep[df_prep$indicator == input$indicator,]
            df_prep %>%
                filter(indicator == input$indicator) %>%
                filter(yrs %in% c(input$year[1]:input$year[2])) %>%
                # filter(unit2 %in% case_when(input$unit_def ~ unique(grep("Default", unit2, value = TRUE)),
                #                             input$unit_relative ~ unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),
                #                             input$unit_index ~ unique(grep("Index", unit2, value = TRUE))
                #                             )
                #        ) %>%
                filter(unit2 %in% case_when(input$line_plot_type == "Default" ~ unique(grep("Default", unit2, value = TRUE)),
                                            input$line_plot_type == "Relative" ~ unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),
                                            input$line_plot_type == "Index" ~ unique(grep("Index", unit2, value = TRUE))
                )
                )

        })


        p_line <-  reactive({
            ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) +
                theme_minimal(base_size = 20) +
                facet_wrap(region~unit2) +
                {if(input$free_y) facet_wrap(region~unit2, scales = "free_y")}+
                geom_line(aes(color=dfx()$flag, group=dfx()$flag), linewidth =1.3) +
                geom_point(shape=1, size = 1.4) +
                ylab(unique(dfx()$unit)) +
                xlab("Years") +
                ggtitle(unique(dfx()$indicator)) +
                {if(input$line_plot_type == "Relative") ggtitle(paste0(unique(dfx()$indicator), " (change)"))} +
                {if(input$line_plot_type == "Index") ggtitle(paste0(unique(dfx()$indicator), " (index)"))} +
                theme(legend.position = "bottom",legend.direction = "vertical") +
                {if(input$line_plot_type == "Relative") geom_hline(yintercept = 0, linetype="longdash", linewidth =1.2, color = "gray")} +
                {if(input$line_plot_type == "Index") geom_hline(yintercept = 1, linetype="longdash", linewidth = 1.2, color = "gray")} +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(color=guide_legend(title="IMPACT Run"))
        })

        p_bar <-  reactive({
            ggplot(dfx(), aes(x = dfx()$yrs, y = dfx()$value)) +
                theme_minimal(base_size = 20) +
                facet_wrap(.~flag) +
                #    {if(free_y) facet_wrap(.~region, scales = "free_y")}+
                geom_area(position='stack',aes(fill=dfx()$region,group=dfx()$region),color="black") +
                ylab(unique(dfx()$unit)) +
                xlab("Years") +
                ggtitle(unique(dfx()$indicator)) +
                {if(input$line_plot_type == "Relative") ggtitle(paste0(unique(dfx()$indicator), " (change)"))} +
                {if(input$line_plot_type == "Index") ggtitle(paste0(unique(dfx()$indicator), " (index)"))} +
                theme(legend.position = "bottom") +
                theme(axis.text.x = element_text(angle = 90)) +
                guides(fill=guide_legend(title="Regions"))
        })

        # output$plotgraph = renderPlot({
        #     ptlist <- list(p_line(),p_bar())
        #     wtlist <- c(input$wt1,input$wt2)
        #     # remove the null plots from ptlist and wtlist
        #     to_delete <- !sapply(ptlist,is.null)
        #     ptlist <- ptlist[to_delete]
        #     wtlist <- wtlist[to_delete]
        #     if (length(ptlist)==0) return(NULL)
        #
        #     grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
        # })
        output$plotgraph = renderPlot({
            p_line()
        })
        output$areaplot = renderPlot({
            p_bar()
        })
    }


    # Run the application
    runApp(shinyApp(ui = ui, server = server),
           launch.browser = getOption("shiny.launch.browser", interactive()),
           display.mode = "normal")
}

