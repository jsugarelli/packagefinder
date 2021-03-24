#' @importFrom fs file_show
#' @title R News file
#' @description Returns the contents of the \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file in the standard browser installed on the operating system.
#' @param pdf If `FALSE` (default), the \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file will open in the browser, otherwise
#' \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.pdf}{NEWS.pdf} will be opened.
#' @param dev If `FALSE` (default), it will not show changes made to the language development version. 
#' To see changes in the development version, do `dev = TRUE`.
#' @details The \href{https://cran.r-project.org/doc/manuals/r-release/NEWS.html}{NEWS.html} file contains the main changes from the recently released versions of the \R language. 
#' The goal is to facilitate the query by invoking it directly from the \R command prompt. The \link{rnews} function is 
#' analogous to the \link{news} function of the **utils** package. However, using the \link{news} command in a terminal style 
#' bash shell is possible to receive a message like:
#' ```  
#' news()
#' starting httpd help server ... done
#' Error in browseURL(url) : 'browser' must be a non-empty character string
#' ```
#' This is an error that may occur depending on the installation of \R. Always prefer the use of the news
#' function but if you need to, use the \link{rnews} function.
#' @export
rnews <- function(pdf = FALSE, dev = FALSE) {
  
  extension <-
    ifelse(pdf,
           "pdf",
           "html")
  
  r_v <-
    ifelse(dev,
           "r-devel",
           "r-release")
  
  "https://cran.r-project.org/doc/manuals/{r_v}/NEWS.{extension}" %>%
    glue %>% 
    file_show
} 
