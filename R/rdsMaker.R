#' RDS maker
#'
#' @param folder IMPACT run output folder
#' @param base_year Base year for which the Reporting should be run
#' @importFrom reportIMPACT getReport
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{rdsMaker(folder = <model_folder>/OutputFiles/Scenarios)}
#' @export
rdsMaker <- function(folder, base_year = NULL) {

    files <- grep(pattern = ".gdx",
                  x = list.files(path = folder),
                  value = TRUE)

    prep_flag <- as.vector(Sys.info()["effective_user"]) # Pull user

    choice <- select.list(choices = files,
                          title = "\nPlease Select Scenarios:",
                          multiple = TRUE,
                          graphics = getOption("menu.graphics"))

    skip_next_choice <- FALSE
    skip_msg <- FALSE

    for (file_vector in paste0(folder, "/", choice)) {
        file_exists <- file.exists(gsub(pattern = ".gdx",
                                       replacement = ".rds",
                                       x = (file_vector)))
        if (file_exists) {
            if (!skip_msg) cat("A preprocessed RDS file already exists for ",
                               basename(file_vector), "\n")
            txt1 <- "You might want to run getReport() on this gdx file "
            txt2 <- "manually if you want to run fresh reporting \n"
            cat(paste0(txt1, txt2))
            skip_msg <- TRUE
        }
        if (!file_exists) {
            cat("No preprocessed RDS file for ", basename(file_vector), "\n")
            if (!skip_next_choice) {
                txt1 <- "Should I convert this GDX file into a RDS file now? "
                txt2 <- "Choosing 'no' will stop the program."
                user_choice <-
                    menu(c("Yes", "No"),
                         title = paste0(txt1, txt2))
            }
            if (user_choice == 1) {
                skip_next_choice <- TRUE
                cat("Attempting to convert to RDS file ......", "\n")
                if (is.null(base_year)) getReport(gdx = file_vector,
                                                  prep_flag = prep_flag)
                if (!is.null(base_year)) getReport(gdx = file_vector,
                                                   prep_flag = prep_flag,
                                                   base_year = base_year)
                choice[choice == basename(file_vector)] <-
                    gsub(pattern = ".gdx",
                         replacement = ".rds",
                         x = basename(file_vector))
                user_choice <- 1
            }

            choice[choice == basename(file_vector)] <-
                gsub(pattern = ".gdx",
                     replacement = ".rds",
                     x = basename(file_vector))

            if (user_choice == 2) {
                txt1 <- "Could not convert GDX file to RDS. Aborting ....... "
                txt2 <- "Hint: Use RDS files if they exist or choose 'yes' at previous prompt"
                stop(paste0(txt1, txt2))
            }
        }
    }
    return(choice)
}
