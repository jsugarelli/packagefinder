#'@title Package 'packagefinder'
#'
#'@description Comfortable search for R packages on CRAN directly from the R console
#'
#'@section What is packagefinder? How does it benefit me?:
#'
#'Currently, there are more than 15,000 R package contributions on CRAN providing R with an unparalleled wealth of features. The downside of the large and increasing amount of packages is that it becomes increasingly difficult to find the right tools to tackle a specific problem. Unfortunately, CRAN does not provide any good search functionality.
#'
#'\strong{packagefinder} is designed to search for CRAN packages right from the R console. The philosophy behind this package is that R users like using the R console and need a tool to do their day-to-day-work on CRAN without leaving their normal workspace, the console. In fact, the idea is that with \strong{packagefinder} you do not \emph{need} to leave the R console to work with CRAN effectively.
#'
#'\strong{packagefinder} is developed to save you time and energy finding the right packages to work with, thereby making your work with R more productive.
#'
#'
#'@section Where can I get more information?:
#'
#'\itemize{
#'\item Contact the \strong{author}:
#'\itemize{
#'\item E-mail: Joachim Zuckarelli (\email{joachim@@zuckarelli.de})
#'\item Twitter: \href{https://twitter.com/jsugarelli}{@@jsugarelli}
#'}
#'\item packagefinder \strong{introduction}: \href{http://www.zuckarelli.de/packagefinder/tutorial.html}{A Quick Tutorial}
#'\item packagefinder on \strong{GitHub}: \href{https://github.com/jsugarelli/packagefinder}{https://github.com/jsugarelli/packagefinder}
#'\item packagefinder video tutorial on \strong{YouTube}: \href{https://youtu.be/B96NMSo3nJI}{https://youtu.be/B96NMSo3nJI} (discusses version 0.1.5)
#'}
#'
#'@name packagefinder
NULL


###   PACKAGE PACKAGEFINDER
###
###   Author and maintainer: Joachim Zuckarelli (joachim@zuckarelli.de)
###   Version 0.2.0
###



.onAttach <- function(libname, pkgname){
  packageStartupMessage(crayon::blue("You are working with", crayon::bold("\npackagefinder"), "version 0.2.0\n"))
  pf<-tools::CRAN_package_db()
  if(numeric_version(pf$Version[pf$Package=="packagefinder"]) < numeric_version(utils::packageVersion("packagefinder"))) packageStartupMessage(crayon::red("Please update packagefinder to the newest version", numeric_version(pf$Version[pf$Package=="packagefinder"]), "!\n\n"))
  else packageStartupMessage("\n")
  packageStartupMessage(crayon::green("Getting started:\n\n"))
  packageStartupMessage(crayon::silver("* Use", crayon::cyan("findPackage(keywords, mode)"), "to search CRAN for packages, e.g.",crayon::italic("findPackage(c(\"meta\",\"regression\"), \"and\")")," or just ", crayon::italic("findPackage(\"meta and regression\")"),".\n\n"), sep="")
  packageStartupMessage(crayon::silver("* Use", crayon::cyan("exploreFields(fields, term)"),"to search a term in the specified fields, e.g.", crayon::italic("exploreFields(c(\"Package\", \"Title\"), \"logistic\").\n\n")), sep="")
  packageStartupMessage(crayon::silver("* Use", crayon::cyan("whatsNew()"),"to check for new packages on CRAN.\n\n"), sep="")

  packageStartupMessage(crayon::silver(crayon::cyan("Tip:"), "Check out the online help pages for more parameters as well as interesting functions like go(), packageDetails() or buildIndex().\n"))
}


.onDetach <-function(libpath) {
  options(packagefinder.lastresults = NULL)
  options(packagefinder.results.longdesc = NULL)
  options(packagefinder.skip.downloads = NULL)
  options(packagefinder.mode = NULL)
  options(packagefinder.keywords = NULL)
  options(packagefinder.call = NULL)
  options(packagefinder.timediff = NULL)
  options(packagefinder.num.results = NULL)
  options(packagefinder.num.cran = NULL)
}


urlExists <- function(test.url) {
  res<-tryCatch({ invisible(httr::http_status(httr::GET(test.url))) }, error=function(e) { invisible(FALSE) })
  if(!is.logical(res)) {
    if(res$category == "Success") return(TRUE)
    else return(FALSE)
  }
  else return(FALSE)
}



makeIndexAvailable <- function(address) {
  searchindex <- NULL
  if(is.list(address)) {
    searchindex <- address
  }
  else {
    if(!is.null(address)) {
      if(address != "") {
        if(file.exists(address)) {
          x <- load(address)
          searchindex <- get(x)
          rm(x)
        }
        else {
          if(urlExists(address)) {
            x <- load(url(address))
            searchindex <- get(x)
            rm(x)
          }
          else {
            return(0)
          }
        }
      }
    }
  }
  if (is.null(searchindex)) searchindex <- buildIndex()
  return(searchindex)
}



#' @title Creating a search index for findPackage()
#' @description Builds a search index that can be used with \code{\link{findPackage}()}.
#'
#' @param filename Name of .rdata file to which the new index is saved.
#' @param download.stats Indicates if the search index shall include download figures from the RStudio CRAN mirror logs.
#'
#' @return Returns the search index. As a side effect, the index is saved to a file, if a filename is provided via the \code{filename} argument.
#'
#' @details Functions like \code{\link{findPackage}()} or \code{\link{go}()} require a search index. This search index can either be created on-the-fly or be provided as a separate argument. In the latter case a search index can be built using \code{buildIndex()}. This index can include download figures for the packages
#' (this feature is turned on with \code{download.stats = TRUE}). Including download stats requires significant time (may well be > 1 hour) for the index to be built. Therefore, when \code{\link{findPackage}()} is called without
#' providing an index, the index that is created on-the-fly does not contain CRAN download figures.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{ index <- buildIndex(filename = file.path(tempdir(), "searchindex.rdata"), download.stats = FALSE) }
#'
#' @import tools
#' @export
buildIndex <- function(filename="", download.stats = FALSE) {
  ind <- tools::CRAN_package_db()
  df <- data.frame(NAME=rep("", nrow(ind)), stringsAsFactors = FALSE)
  df$NAME <- ind$Package
  df$URL <- paste0("https://CRAN.R-project.org/package=", df[,1])
  df$DESC_SHORT <- ind$Title
  df$DESC_LONG <- ind$Description
  df$DATE <- ind$Published
  df$DOWNL_MONTH <- NA  # Dowloads last month
  df$DOWNL_TOTAL <- NA  # Dowloads total
  df$MANUAL <- paste0("https://cloud.r-project.org/web/packages/", df[,1], "/", df[,1], ".pdf") # Link to manual

  df$VERSION <- ind$Version
  df$MAINTAINER <- ind$Maintainer
  df$AUTHORS <- ind$`Authors@R`
  df$AUTHOR <- ind$Author
  df$LICENSE <- ind$License
  df$IMPORTS <- ind$Imports
  df$ENHANCES <- ind$Enhances
  df$DEPENDS <- ind$Depends
  df$SUGGESTS <- ind$Suggests
  df$REVERSE.DEPENDS <- ind$`Reverse depends`
  df$REVERSE.SUGGESTS <- ind$`Reverse suggests`
  df$REVERSE.ENHANCES <- ind$`Reverse enhances`
  df$BUGREPORTS <- ind$BugReports
  df$URL <- ind$URL
  df$COPYRIGHT <- ind$Copyright
  df$CONTACT <- ind$Contact
  df$NOTE <- ind$Note
  df$MAILINGLIST <- ind$MailingList


  if(download.stats == TRUE) {
    prog <- utils::txtProgressBar(min=0, max=100, style=3)

    for(i in 1:nrow(df)) {
      if(i %% 100 == 0) utils::setTxtProgressBar(prog, i/nrow(df)*100)
      json <- jsonlite::fromJSON(paste0("https://cranlogs.r-pkg.org/downloads/total/last-month/", df[i,1]))
      if ("downloads" %in% names(json)) df[i,6]<-json$downloads
      json <- jsonlite::fromJSON(paste0("https://cranlogs.r-pkg.org/downloads/total/1970-01-01:", as.Date(Sys.time()), "/", df[i,1]))
      if ("downloads" %in% names(json)) df[i,7]<-json$downloads
    }

    utils::setTxtProgressBar(prog, 100)
    cat("\n")
  }

  date.time <- Sys.time()
  searchindex <- list(df, date.time)
  names(searchindex)<-c("index", "date.time")

  # Save index to file
  if(filename != "") {
    save(searchindex, file=filename)
    cat("\nIndex is ready.\n")
  }

  invisible(searchindex)
}



#' @title Searching for packages on CRAN
#' @description Searches for packages on CRAN based on the user's specification of search terms. Considers the package name, description as well as the long description, and prioritizes the results.
#'
#' @param keywords A vector of keywords to be searched for. Instead of separate search terms, \code{keywords} can also be a query like \code{"meta AND regression"}. In this case the \code{mode} argument is ignored. Only one type of logical operator (either \code{and} or \code{and}) may be used in a query; operators are not case-sensitive.
#' @param mode Indicates wthether the search terms in \code{keywords} shall be combined with a logical OR or with a logical AND; accordingly, permitted values are \code{"or"} (default) and \code{"and"}. In \code{"or"} mode, every package that contains \emph{at least one} of the keywords from the \code{keywords} argument is a search hit, in \code{"and"} mode generating a search hit requires \emph{all} search terms from the \code{keywords} argument to be found.
#' @param case.sensitive Indicates if the search shall be case sensitive, or not.
#' @param always.sensitive A vector of search terms for which capitalization is always considered relevant (even if \code{case.sensitive = FALSE}). This allows to better reflect abbreviations like 'GLM'.
#' @param weights A numeric vector describing how search hits in different fields of the a package's data shall be weighted. The first three elements of the vector are the weights assigned to hits in the package's \emph{title}, \emph{short description} and \emph{long description}, respectively. The fourth element is a factor applied to the overall score of a search hit if all search terms from the \code{keywords} argument are found (obviously only meaningful in \code{"or"} mode). All weights must be 1 or larger.
#' @param display Describes where the search results shall be shown. Either \code{"viewer"}, \code{"console"} or \code{"browser"}. If \code{"viewer"}, the results are shown in RStudio's Viewer pane if the RStudio IDE is being used. If \code{results = "console"} the search results are shown as a text table in the R console. \code{results = "browser"} shows the search results in the web browser.
#' @param results.longdesc Indicates whether the packages' long descriptions shall also be included in the search results. Given the length of some long decsriptions this may make the search results harder to read.
#' @param limit.results The maximum number of matches presented in the search results; choose a negative number to display all results
#' @param silent Indicates whether details of the user's search query are repeated in the console.
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{findPackage()} creates an ad hoc search index.
#' @param advanced.ranking Indicates if the ranking of search results shall be based on weights taking into account the inverse frequencies of the different search terms across all packages and the length of the matches relative to the texts they were found in. Usually, using advanced ranking (\code{advanced.ranking = TRUE}, default) gives more relevant results, especially in \code{"or"} mode when the search terms differ strongly in their frequency of occurrence across packages.
#' @param return.df If \code{TRUE}, \code{findPackage()} returns a dataframe with the results, otherwise there is no return value. Default is \code{FALSE}.
#'
#' @return The search results as a dataframe, if \code{df.return = TRUE}.
#'
#'
#' @details The \code{GO} column in the search results is an index number that can be used to address the found package easily with the \code{go()} function.
#' The \code{Total Downloads} column in the search results gives the overall number of downloads of the respective package since its submission to CRAN. The number is based on the figures for the RStudio CRAN mirror server.
#' This field is only provided if the search index contains download figures. Ad hoc indices (when \code{index = NULL}) never include download statistics. Please refer to \code{\link{buildIndex}()} for more details.
#'
#' \code{\link{fp}()} is a shorter alias for \code{\link{findPackage}()}.
#'
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{
#' search <- c("regression", "meta")
#' findPackage(search)
#'
#' findPackage(c("text", "tables"))
#'
#' searchindex <- buildIndex()
#' findPackage(keywords=c("regression", "linear"), mode="and",
#'    always.sensitive="GLM", index=searchindex)
#'
#' findPackage("meta and regression", display="browser")
#'
#' my.results <- findPackage("meta AND regression")
#' }
#' @export
findPackage<-function(keywords, mode = "or", case.sensitive = FALSE, always.sensitive = NULL, weights = c(2,2,1,2), display = "viewer", results.longdesc = FALSE, limit.results = 15, silent = FALSE, index = NULL, advanced.ranking = TRUE, return.df = FALSE) {

  if(!is.null(always.sensitive)) keywords = c(keywords, always.sensitive)
  if(sum(stringr::str_detect(keywords, "[:blank:]+[aA][nN][dD][:blank:]+"))>0) {
    keywords.list <- stringr::str_split(keywords, "[:blank:]+[aA][nN][dD][:blank:]+")
    keywords <- unlist(keywords.list)
    mode <- "and"
  } else {
    if(sum(stringr::str_detect(keywords, "[:blank:]+[oO][rR][:blank:]+"))>0) {
      keywords.list <-stringr::str_split(keywords, "[:blank:]+[oO][rR][:blank:]+")
      keywords <- unlist(keywords.list)
      mode <- "or"
    }
  }

  if(!silent) {
    if(mode=="and") {
      mode.param <- "and"
      other.mode <- "or"
    }
    else {
      mode.param <- "or"
      other.mode <- "and"
    }

    msg<-"Your are searching packages for the terms"
    if(length(keywords)==1) msg<-"Your are searching packages for the term"
    expl<-"(at least one occurence of any of the search terms)"
    if(mode.param=="and") expl<-"(matches must have at least one occurence of each of the search terms)."

    cat(crayon::cyan(paste(append(msg, paste(paste("'", keywords, "'", sep=""), collapse=", ")))))
    if(length(keywords)>1) cat(crayon::cyan(" in", crayon::bold(mode.param), "mode", expl),sep="")

    cat(crayon::cyan("\n\nPlease wait while index is being searched...\n"))
  }

  time.searchstart<-Sys.time()
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    score<-c()
    m = matrix(nrow=length(searchindex$index$NAME), ncol=length(keywords))
    m1.lengthratio <- 1
    m2.lengthratio <- 1
    m3.lengthratio <- 1

    num.keywords = length(keywords)

    for(i in 1:length(searchindex$index$NAME)) {

      for(f in 1:length(keywords)) {
        m1<-0
        m2<-0
        m3<-0

        if(f <= num.keywords) {
          cs <- !case.sensitive
        } else {
          cs <- FALSE
        }
        dm1 <- stringr::str_count(searchindex$index$NAME[i], stringr::fixed(keywords[f], ignore_case=cs))
        dm2 <- stringr::str_count(searchindex$index$DESC_SHORT[i], stringr::fixed(keywords[f], ignore_case=cs))
        dm3 <- stringr::str_count(searchindex$index$DESC_LONG[i], stringr::fixed(keywords[f], ignore_case=cs))
        if(advanced.ranking) {
          m1.lengthratio <- dm1 * nchar(keywords[f]) / nchar(searchindex$index$NAME[i])
          m2.lengthratio <- dm2 * nchar(keywords[f]) / nchar(searchindex$index$DESC_SHORT[i])
          m3.lengthratio <- dm3 * nchar(keywords[f]) / nchar(searchindex$index$DESC_LONG[i])
        }
        m1 <- dm1 * weights[1] * m1.lengthratio
        m2 <- dm2 * weights[2] * m2.lengthratio
        m3 <- dm3 * weights[3] * m3.lengthratio
        m[i,f] <- m1 + m2 + m3
      }
    }

    m <- m / max(m, na.rm=TRUE)

    inverse.keyword.weight <- c()
    if(!advanced.ranking) {
      inverse.keyword.weight <- rep(1, length(keywords))
    } else {
      for(f in 1:length(keywords)) {
        inverse.keyword.weight[f] <- sum(m[,f]>0, na.rm=TRUE)
      }
      inverse.keyword.weight <- 1 / (inverse.keyword.weight / max(inverse.keyword.weight, na.rm=TRUE))
    }

    if(mode == "or") {
      for(i in 1:length(searchindex$index$NAME)) {
        score[i] <- (sum(m[i,] * inverse.keyword.weight, na.rm=TRUE) * (1 + (weights[4] - 1) * (sum(m[i,]==0, na.rm=TRUE)==0)))
      }
    } else {
      for(i in 1:length(searchindex$index$NAME)) {
        score[i] <- (sum(m[i,] * inverse.keyword.weight, na.rm=TRUE) * (sum(m[i,]==0, na.rm=TRUE)==0))
      }
    }

    score <- score / max(score, na.rm=TRUE) * 100
    searchindex$index$SCORE <- as.numeric(score)

    if(sum(searchindex$index$SCORE, na.rm=TRUE) > 0) {
      outp <- searchindex$index[searchindex$index$SCORE>0, c(26,1,3,4,7)]
      if(sum(outp$DOWNL_TOTAL, na.rm=TRUE) == 0) {
        skip.downloads <- TRUE
        outp$DOWNL_TOTAL <- rep(0, NROW(outp))
      }
      else  skip.downloads <- FALSE
      colnames(outp) <- c("Score", "Name", "Short Description", "Long Description", "Total Downloads")

      # Show results
      cols <- rep(TRUE, 6)
      if(results.longdesc == FALSE) cols[4]<-FALSE
      cols[5]<-!skip.downloads

      outp$Score <- round(outp$Score, 1)
      outp <- outp[order(-outp$Score, -outp[,"Total Downloads"], tolower(outp$Name), decreasing = FALSE),]
      outp <- cbind(outp, data.frame(GO = row.names(outp)))
      row.names(outp) <- NULL
      num.results <- NROW(outp)

      options(packagefinder.lastresults.full = outp)

      res <- outp[,cols]
      if(skip.downloads) outp[, NCOL(outp)-1] <- NULL


      if(limit.results > 0 & limit.results < NROW(res) & tolower(display) != "browser") {
        res<-res[(1:limit.results),]
        top.results.msg <- paste0("Top ", limit.results, " results are shown. Use parameter 'limit.results' to increase number of results shown.")
      }
      else top.results.msg <- ""

      time.searchend<-Sys.time()

      cat("\nResults:", crayon::bold(num.results), "out of", crayon::bold(NROW(searchindex$index)), "CRAN packages found in", round(as.numeric(time.searchend - time.searchstart, units="secs"),0), "seconds.", top.results.msg,"\n")

      options(packagefinder.lastresults = res)
      options(packagefinder.results.longdesc = results.longdesc)
      options(packagefinder.skip.downloads = skip.downloads)
      options(packagefinder.mode = toupper(mode.param))
      options(packagefinder.keywords = keywords)
      options(packagefinder.call = sys.call())
      options(packagefinder.timediff = round(as.numeric(time.searchend - time.searchstart, units="secs"),0))
      options(packagefinder.num.results = num.results)
      options(packagefinder.num.cran = NROW(searchindex$index))
      lastResults(display)
      if(return.df) return(outp)
    } else {
      cat("\nNo results found.")
    }
  } else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}




#' @title Searching for packages on CRAN
#' @description Shorter alias for function \code{\link{findPackage}()}.
#'
#' @param ... Arguments as in \code{\link{findPackage}()}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{
#' fp(c("meta", "regression"))
#' }
#' @export
fp <- function(...) { findPackage(...) }



#' @title Searching for packages on CRAN
#' @description Searches for packages on CRAN by scanning a specified set of information fields for a user-provided search term.
#'
#' @param term Search term to look for; character vector must have one element.
#' @param fields The list of fields to be scanned for the search term; must be a character vector with one or more field names. Allowed field names are: \code{"Name"}, \code{"Description"}, \code{"LongDescription"}, \code{"Maintainer"}, \code{"Authors@R"}, \code{"Author"}, \code{"License"}, \code{"Imports"}, \code{"Enhances"}, \code{"Depends"}, \code{"Suggests"}, \code{"Reverse depends"}, \code{"Reverse suggests"}, \code{"Reverse enhances"}, \code{"Copyright"}, \code{"Contact"}, \code{"Note"}, \code{"MailingList"}.
#' @param mode Indicates whether matches in the field shall be combined with a logical OR or with a logical AND; accordingly, permitted values are \code{"or"} (default) and \code{"and"}. In \code{"or"} mode, every package that has the search term in any of the fields from \code{fields} generates a hit, in \code{"and"} mode the search term must be found in all fields provided to make that package a search hit.
#' @param match Either \code{"like"} (default) or \code{"exact"}. Determines if the field content must match the search term exactly or only needs to contain it.
#' @param display Describes where the search results shall be shown. Either \code{"viewer"} or \code{"console"}. If \code{"viewer"}, the results are shown in RStudio's Viewer pane if the RStudio IDE is being used). If \code{results = "console"} the search results are shown as a text table in the R console.
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{exploreFields()} creates an ad hoc search index.

#' @return No return value.
#'
#' @details Found packages are listed in alphabetical order, there is no prioritization of search hits as in \code{\link{findPackage}()}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{ exploreFields("Hadley", c("Maintainer", "Authors@R", "Author")) }

#' @export
exploreFields <- function(term, fields=c("Name", "Description", "LongDescription"), mode="or", match="like", display="viewer", index = NULL) {
  fields.df <- data.frame(list(names=c("Name", "Description", "LongDescription", "Maintainer", "Authors@R", "Author", "License", "Imports", "Enhances", "Depends", "Suggests", "Reverse depends", "Reverse suggests", "Reverse enhances", "Copyright", "Contact", "Note", "MailingList"), index.pos=c(1,3,4,10,11,12,13,14,15,16,17,18,19,20,22,23,24,25)))
  chk<-tolower(fields) %in% tolower(fields.df$names)
  if(sum(chk) == length(fields)) {
    searchindex <- makeIndexAvailable(index)
    if(class(searchindex) == "list"){
      if(match %in% c("like", "exact")) {

        time.searchstart<-Sys.time()

        if(mode=="or") match.pkg <- rep(FALSE, NROW(searchindex$index))
        else match.pkg <- rep(TRUE, NROW(searchindex$index))
        for(i in 1:NROW(fields)) {
          col <- fields.df$index.pos[fields.df$names==fields[i]]
          if(mode=="or") {
            if(match=="like") match.pkg <- match.pkg | stringr::str_detect(tidyr::replace_na(searchindex$index[,col], ""),paste0(".*", term, ".*"))
            else match.pkg <- match.pkg | (tidyr::replace_na(searchindex$index[,col])==term)
          }
          else {
            if(match=="like") match.pkg <- match.pkg & stringr::str_detect(tidyr::replace_na(searchindex$index[,col], ""),paste0(".*", term, ".*"))
            else match.pkg <- match.pkg & (tidyr::replace_na(searchindex$index[,col])==term)
          }
        }
        res <- searchindex$index[match.pkg,]

        if(NROW(res)>0) {

          time.searchend<-Sys.time()
          cat("\nResults:", crayon::bold(NROW(res)), "out of", crayon::bold(NROW(searchindex$index)), "CRAN packages found in", round(as.numeric(time.searchend - time.searchstart, units="secs"),0), "seconds.\n")

          skip.downloads<-FALSE
          if(is.na(unique(res$DOWNL_TOTAL)[1]) & length(unique(res$DOWNL_TOTAL))==1) skip.downloads <- TRUE

          col <- fields.df$index.pos[fields.df$names %in% unique(append(c("Name", "Description", "LongDescription"), fields))]
          if(!skip.downloads) {
            res <- res<-cbind(res[,col], DOWNL_TOTAL=res[,7], GO=as.integer(row.names(res)))
            res <- res[order(res$DOWNL_TOTAL, decreasing=TRUE),]
          }
          else {
            res<-cbind(res[,col], GO=as.integer(row.names(res)))
            res <- res[order(res$NAME),]
          }
          row.names(res)<-NULL

          text.align.formattable <- rep("l", NCOL(res))
          text.align.pandoc <- rep("left", NCOL(res))

          if(tolower(display) == "viewer") {
            bold <- formattable::formatter("span", style = x ~ formattable::style("font-weight:bold; color:#4d4d4d"))
            light.color <- formattable::formatter("span", style = x ~ formattable::style("color:#8c8c8c"))
            if("DOWNL_TOTAL" %in% names(res)) res$DOWNL_TOTAL <- formattable::comma(res$DOWNL_TOTAL, format="d", big.mark=",")
            if(skip.downloads == FALSE & "DOWNL_TOTAL" %in% names(res)) {
              formattable::formattable(res, align=text.align.formattable, list(
                NAME = bold,
                GO = light.color,
                SCORE = formattable::color_tile("white", "chartreuse3"),
                formattable::area(col = DOWNL_TOTAL) ~ formattable::normalize_bar("lightblue", 0.4)
              ))
            }
            else {
              if("DOWNL_TOTAL" %in% names(res)) res$DOWNL_TOTAL<-NULL
              formattable::formattable(res, align=text.align.formattable, list(
                NAME = bold,
                GO = light.color,
                SCORE = formattable::color_tile("white", "chartreuse3")
              ))
            }
          }
          else {
            if(skip.downloads == TRUE) {
              if("DOWNL_TOTAL" %in% names(res)) res$DOWNL_TOTAL<-NULL
            }
            pander::pandoc.table(res, split.table=Inf,justify=text.align.pandoc, style="grid")
          }
        }
        else {
          cat("\nNo results found.")
        }
      }
      else {
        stop(paste0("'", mode,"' is not a valid value for argument 'match'. Choose either 'like' or 'exact' depending on your search strategy."))
      }
    } else {
      stop("Search index is not available. Create a search index with buidIndex().")
    }
  } else {
    fld <- "fields "
    isare <-" are "
    if(sum(chk) - length(fields) == 1) {
      fld <- "field "
      isare <- " is "
    }
    stop(paste0("\n\nThe ", fld, paste0(paste0("'", fields[!chk], "'"), collapse = ", "), isare, "no valid package fields that can be searched. \n\nValid package fields are: \n", paste0(paste0("- '", fields.df$names, "'"), collapse="\n"), "."))
  }
}



showResults <- function(res, display, results.longdesc, skip.downloads) {
  if(!is.null(res)) {
    text.align.formattable <- c("r", rep("l", NCOL(res)-1))
    text.align.pandoc <- c("right", rep("left", NCOL(res)-1))

    if(tolower(display) == "viewer") {
      bold <- formattable::formatter("span", style = x ~ formattable::style("font-weight:bold; color:#4d4d4d"))
      light.color <- formattable::formatter("span", style = x ~ formattable::style("color:#8c8c8c"))
      if("Total Downloads" %in% names(res)) res[,"Total Downloads"] <- formattable::comma(res[,"Total Downloads"], format="d", big.mark=",")
      if(skip.downloads == FALSE) {
        print(formattable::formattable(res, align=text.align.formattable, list(
          Name = bold,
          GO = light.color,
          Score = formattable::color_tile("white", "chartreuse3"),
          formattable::area(col = "Total Downloads") ~ formattable::normalize_bar("lightblue", 0.4)
        )))
      }
      else {
        print(formattable::formattable(res, align=text.align.formattable, list(
          Name = bold,
          GO = light.color,
          Score = formattable::color_tile("white", "chartreuse3")
        )))
      }
    } else {
      if(tolower(display)=="browser") {
        html.viewHTML()
      } else {
          pander::pandoc.table(res, split.table=Inf,justify=text.align.pandoc, style="grid")
      }
    }
  }
  else cat("No results available.")
}


#' @title Searching for packages on CRAN
#' @description Shows the results of the last search with \code{\link{findPackage}()}.
#'
#' @param display Describes where the search results shall be shown. Either \code{"viewer"}, \code{"console"} or \code{"browser"}. If \code{"viewer"}, the results are shown in RStudio's Viewer pane if the RStudio IDE is being used. If \code{results = "console"} the search results are shown as a text table in the R console. \code{results = "browser"} shows the search results in the web browser.
#'
#' @return No return value.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}

#' @export
lastResults <- function(display = "viewer") {
  res <- getOption("packagefinder.lastresults", NULL)
  if(!is.null(res)) {
    results.longdesc <- getOption("packagefinder.results.longdesc", FALSE)
    skip.downloads <- getOption("packagefinder.skip.downloads", TRUE)

    showResults(res, display, results.longdesc, skip.downloads)
  }
  else cat("No last results available.")
}



#' @title Staying up-to-date on CRAN packages
#' @description Shows information on the latest package additions to CRAN.
#'
#' @param last.days The length of the period (in days) for which package additions to CRAN shall be presented. \code{last.days=0} means only today's additions are shown.
#' @param brief Determines if all avalilable package description fields shall be shown (\code{brief=FALSE}) or only a summary covering the most important fields (\code{brief=TRUE}, the default).
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{whatsNew()} creates an ad hoc search index.
#'
#' @return Number of packages covered by the period specified in \code{last.days}.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#' @examples
#' \donttest{
#' whatsNew(last.days = 3)
#'}
#'
#' @export
whatsNew <- function(last.days=0, brief = TRUE, index = NULL) {
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    if(last.days>=0) {
      num <- 0
      date.to <- lubridate::today()-last.days
      ind <- searchindex$index[as.Date(lubridate::parse_date_time(searchindex$index$DATE, "%Y-%m-%d"))>=date.to,]
      ind <- ind[order(ind$DATE,decreasing=TRUE),]
      for(i in unique(ind$DATE)) {
        ind.date <- ind[ind$DATE==i,]
        header <- "\nPublished "
        if(i==lubridate::today()) header <- paste0(header, "today")
        else {
          if(i== lubridate::today()-1) header <- paste0(header, "yesterday")
          else header <- paste0(header, "on ", i)
        }

        cat(crayon::magenta(crayon::bold(header)), crayon::magenta(paste0(" (",NROW(ind.date), " packages):\n")), sep="")

        for(f in 1:NROW(ind.date)) {
          packageDetails(as.integer(row.names(ind.date)[f]), brief=brief, show.tip = FALSE, index=searchindex)
          num <- num + 1
        }
      }
      invisible(num)
    }
    else {
      stop("Argument last.days must be equal to or larger than zero.")
    }
  }
  else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}



#' @title Showing information about a package
#' @description Allows to inspect a package found with \code{\link{findPackage}()} by showing detailed CRAN information on the package, opening its manual (PDF) or pulling up the package's website. Also allows to install the package right away.
#'
#' @param package Either the name of the package (capitalization does generally not matter) or the search result number shown in the results of \code{\link{findPackage}()} (the number in the \code{GO} column).
#' @param where.to Either \code{"details"} (default), \code{"manual"}, \code{"website"}, or \code{"install"}. With \code{"details"}, \code{go()} presents a set of CRAN information on the package, \code{where.to = "manual"} opens the package's PDF manual from the web and \code{where.to = "website"} pulls up the package's website(s), if any website is provided by the package maintainer. \code{where.to = "install"} installs the package (including dependencies).
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{go()} creates an ad hoc search index.
#'
#' @return No return value.
#'
#' @details \code{go()} is made to inspect a package found with \code{\link{findPackage}()} and to decide whether or not this package serves the intended purposes.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @export
go <- function(package, where.to = "details", index = NULL) {
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    go.num <- checkPackageValidity(searchindex, package)
    if(!is.null(go.num)) {
      if(!(tolower(where.to) %in% c("install", "website", "manual", "details"))) {
        stop("Argument go.num must be either 'install', 'website', 'manual' or 'details'.")
      } else {
        if(is.numeric(go.num)) {
          if(go.num %% 1 == 0) {
            df <- searchindex$index[go.num,]
            if(is.na(df$NAME[1])) stop(paste0("No go.num with index ", go.num, " could be found."))
          }
        }
        else {
          df <- searchindex$index[tolower(searchindex$index$NAME)==tolower(go.num),]
          if(NROW(df)>1) df<- searchindex$index[searchindex$index$NAME==go.num,]
          if(NROW(df)==0) stop(paste0("go.num '", go.num, "' could not be found."))
        }
        if(tolower(where.to)=="install") {
          utils::install.packages(go.num, dependencies=TRUE)
        }
        else {
          if(tolower(where.to)=="website") {
            urls <- stringr::str_trim(stringr::str_split(df$URL, ",")[[1]])
            for(i in 1:NROW(urls)) utils::browseURL(urls[i])
          }
          else {
            if(tolower(where.to)=="manual") {
              utils::browseURL(as.character(df$MANUAL))
            }
            else
              if(tolower(where.to)=="details") {
                packageDetails(index = index, go.num, show.tip = TRUE)
              }
          }
        }
      }
    } else {
      stop(paste0("Package ", as.character(package), " does not exist. Please check the package GO number / name."))
    }
  } else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}



checkPackageValidity <- function(index, package) {
  if(is.numeric(package)) {
    if(package %in% rownames(index$index)) return(package)
  }
  else {
    if(package %in% index$index$NAME) {
      return(as.integer(rownames(index$index[index$index$NAME==package,])))
    }
  }
  return(NULL)
}



packageElement <- function(field, value, bold.field=FALSE, bold.value=FALSE, line.break=80) {
  lb <- ""
  if(nchar(value[1])>80) lb <- "\n"

  if(bold.field==FALSE) cat(crayon::silver(field), crayon::silver(": "), lb, sep="")
  else cat(crayon::silver(crayon::bold(field)), crayon::silver(": "), lb, sep="")

  if(bold.value==FALSE) cat(value, "\n\n", sep="")
  else cat(crayon::bold(value), "\n\n", sep="")
}



#' @title Showing information about a package
#' @description Shows detailed CRAN information for a package.
#'
#' @param package Either the name of a package (capitalization does generally not matter) or the search result number shown in the results of \code{\link{findPackage}()} (the number in the \code{GO} column). Only one package is allowed here.
#' @param brief If \code{"TRUE"}, only title, short and long description as well as the maintainer of the package are shown, otherwise all available fields are displayed.
#' @param show.tip If \code{"TRUE"}, tips for getting additional information on the package are shown.
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{packageDetails()} creates an ad hoc search index.
#'
#' @return No return value.
#'
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{ packageDetails("ggplot2") }
#'
#' @export
packageDetails <- function(package, brief=FALSE, show.tip=TRUE, index=NULL) {
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    go.num <- checkPackageValidity(searchindex, package)
    if(!is.null(go.num)) {
      cat(crayon::cyan("\nPackage", crayon::bold(searchindex$index$NAME[go.num]), "\n\n"), sep="")
      packageElement("Title", searchindex$index$DESC_SHORT[go.num])
      packageElement("Long description", searchindex$index$DESC_LONG[go.num])
      if(brief == FALSE) packageElement("Publish date", searchindex$index$DATE[go.num])
      if(brief == FALSE) packageElement("Version", searchindex$index$VERSION[go.num])
      if(brief == FALSE) packageElement("License", searchindex$index$LICENSE[go.num])
      if(brief == FALSE) if(!is.na(searchindex$index$COPYRIGHT[go.num])) packageElement("Copyright", searchindex$index$COPYRIGHT[go.num])
      packageElement("Maintainer", searchindex$index$MAINTAINER[go.num])
      if(brief == FALSE) {
        if(!is.na(searchindex$index$CONTACT[go.num])) packageElement("Contact", searchindex$index$CONTACT[go.num])
        if(!is.na(searchindex$index$MAILINGLIST[go.num])) packageElement("Mailing list", searchindex$index$MAILINGLIST[go.num])
        if(!is.na(searchindex$index$URL[go.num])) packageElement("URL", searchindex$index$URL[go.num])
        if(!is.na(searchindex$index$BUGREPORTS[go.num])) packageElement("Bug reports", searchindex$index$BUGREPORTS[go.num])
        if(!is.na(searchindex$index$AUTHOR[go.num])) packageElement("Authors", searchindex$index$AUTHOR[go.num])
        if(!is.na(searchindex$index$AUTHORS[go.num])) packageElement("Authors@R", searchindex$index$AUTHORS[go.num])
        if(!is.na(searchindex$index$ENHANCES[go.num])) packageElement("Enhances", searchindex$index$ENHANCES[go.num])
        if(!is.na(searchindex$index$IMPORTS[go.num])) packageElement("Imports", searchindex$index$IMPORTS[go.num])
        if(!is.na(searchindex$index$DEPENDS[go.num])) packageElement("Depends", searchindex$index$DEPENDS[go.num])
        if(!is.na(searchindex$index$SUGGESTS[go.num])) packageElement("Suggests", searchindex$index$SUGGESTS[go.num])
        if(!is.na(searchindex$index$REVERSE.DEPENDS[go.num])) packageElement("Reverse depends", searchindex$index$REVERSE.DEPENDS[go.num])
        if(!is.na(searchindex$index$REVERSE.SUGGESTS[go.num])) packageElement("Reverse suggests", searchindex$index$REVERSE.SUGGESTS[go.num])
        if(!is.na(searchindex$index$REVERSE.ENHANCES[go.num])) packageElement("Reverse enhances", searchindex$index$REVERSE.ENHANCES[go.num])
        if(!is.na(searchindex$index$NOTE[go.num])) packageElement("Note", searchindex$index$NOTE[go.num])
      }
      if(!is.numeric(package)) package <- paste0("\"", package, "\"")
      if(show.tip == TRUE) cat(crayon::magenta(crayon::bold("\nTip:"), "Use", crayon::bold(paste0("go(", package,",\"manual\")")), paste0("to view the manual of package '", searchindex$index$NAME[go.num],"' and"), crayon::bold(paste0("go(", package,",\"website\")")), paste0("to visit its website (if any is provided).")),sep="")
    }
    else {
      stop(paste0("Package ", as.character(package), " does not exist. Please check the package GO number / name."))
    }
  }
  else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}
