###   PACKAGE PACKAGEFINDER
###
###   Author and maintainer: Joachim Zuckarelli (joachim@zuckarelli.de)
###   Version 0.0.7
###


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
#' @description Builds a search index that can be used with \code{\link{findPackage}}.
#'
#' @param filename Name of .rdata file to which the new index is saved.
#' @param download.stats Indicates if the search index shall include download figures from the RStudio CRAN mirror logs.
#'
#' @return Returns the search index. As a side effect, the index is saved to a file, if a filename is provided via the \code{filename} argument.
#'
#' @details \code{buildIndex} creates the index that is needed by the \code{\link{findPackage}} function to search CRAN. Optionally, this index can include download figures for the packages
#' (this feature is turned on with \code{download.stats = TRUE}). Including download stats requires significant time (may be > 1 hour) for the index to be built. Therefore, when \code{\link{findPackage}} is called without
#' providing an index, the index that is created ad hoc does not contain CRAN download figures.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' index <- buildIndex(filename = file.path(tempdir(), "searchindex.rdata"), download.stats = FALSE)
#'
#' @export
buildIndex <- function(filename="", download.stats = FALSE) {
  ind <- tools::CRAN_package_db()
  df <- data.frame(NAME=rep("", nrow(ind)), stringsAsFactors = FALSE)
  df$NAME <- ind$Package
  df$URL <- paste0("https://CRAN.R-project.org/package=", df[,1])
  df$DESC_SHORT <- ind$Title
  df$DESC_LONG <- ind$Description
  df$DATE <- ind$Date
  df$DOWNL_MONTH <- NA  # Dowloads last month
  df$DOWNL_TOTAL <- NA  # Dowloads total
  df$MANUAL <- paste0("https://cloud.r-project.org/web/packages/", df[,1], "/", df[,1], ".pdf") # Link to manual

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
#' @param keywords A vector of keywords to be searched for.
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{findPackage} creates an ad hoc search index.
#' @param mode Indicates wthether the search terms in \code{keywords} shall be combined with a logical OR or with a logical AND; accordingly, permitted values are \code{"or"} (default) and \code{"and"}. In \code{"or"} mode, every package that contains \emph{at least one} of the keywords from the \code{keywords} argument is a search hit, in \code{"and"} mode generating a search hit requires \emph{all} search terms from the \code{keywords} argument to be found.
#' @param case.sensitive Indicates if the search shall be case sensitive, or not.
#' @param always.sensitive A vector of search terms for which capitalization is always considered relevant (even if \code{case.sensitive = FALSE}). This allows to better reflect abbreviations like 'GLM'.
#' @param weights A numeric vector describing how search hits in different fields of the a package's data shall be weighted. The first three elements of the vector are the weights assigned to hits in the package's \emph{title}, \emph{short description} and \emph{long description}, respectively. The fourth element is a factor applied to the overall score of a search hit if all search terms from the \code{keywords} argument are found (obviously only meaningful in \code{"or"} mode). All weights must be 1 or larger.
#' @param display Describes where the search results shall be shown. Either \code{"viewer"} or \code{"console"}. If \code{"viewer"}, the results are shown as a formatted web page in the browser (or in RStudio's Viewer pane if the RStudio IDE is being used). If \code{results = "console"} the search results are shown as a text table in the R console.
#' @param results.longdesc Indicates whether the packages' long descriptions shall also be included in the search results. Given the length of some long decsriptions this may make the search results harder to read.
#'
#' @return No return value.
#'
#' @details The \code{GO} column in the search results is an index number that can be used to address the found package easily with the \code{go()} function.
#' The \code{DOWNL_TOTAL} column in the search results gives the overall number of downloads of the respective package since its submission to CRAN. The number is based on the figures for the RStudio CRAN mirror server.
#' This field is only provided if the search index contains download figures. Ad hoc indices (when \code{index = NULL}) never include download statistics. Please refer to \code{\link{buildIndex}} for more details.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @examples
#' \donttest{
#' search <- "regression"
#' findPackage(search)
#'
#' findPackage(c("text", "tables"), mode="and")
#'
#' searchindex <- buildIndex()
#' findPackage(keywords=c("regression", "linear"), index=searchindex,
#'    mode="and", always.sensitive="GLM")
#' }
#' @export
findPackage<-function(keywords, index = NULL, mode = "or", case.sensitive = FALSE, always.sensitive = NULL, weights = c(1,2,1,2), display = "viewer", results.longdesc = FALSE) {
  time.searchstart<-Sys.time()
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    score<-c()
    for(i in 1:length(searchindex$index$NAME)) {
      found<-c()
      m1<-0
      m2<-0
      m3<-0
      for(f in 1:length(keywords)) {
        dm1 <- stringr::str_count(searchindex$index$NAME[i], stringr::fixed(keywords[f], ignore_case=!case.sensitive))
        dm2 <- stringr::str_count(searchindex$index$DESC_SHORT[i], stringr::fixed(keywords[f], ignore_case=!case.sensitive))
        dm3 <- stringr::str_count(searchindex$index$DESC_LONG[i], stringr::fixed(keywords[f], ignore_case=!case.sensitive))
        m1 <- m1 + dm1 * weights[1]
        m2 <- m2 + dm2 * weights[2]
        m3 <- m3 + dm3 * weights[3]
        found[f] <- !(dm1 + dm2 + dm3 == 0)
      }

      if(!is.null(always.sensitive)) {
        for(f in 1:length(always.sensitive)) {
          dm1 <- stringr::str_count(searchindex$index$NAME[i], stringr::fixed(always.sensitive[f], ignore_case=FALSE))
          dm2 <- stringr::str_count(searchindex$index$DESC_SHORT[i], stringr::fixed(always.sensitive[f], ignore_case=FALSE))
          dm3 <- stringr::str_count(searchindex$index$DESC_LONG[i], stringr::fixed(always.sensitive[f], ignore_case=FALSE))
          m1 <- m1 + dm1 * weights[1]
          m2 <- m2 + dm2 * weights[2]
          m3 <- m3 + dm3 * weights[3]
          found[length(keywords)+f] <- !(dm1 + dm2 + dm3 == 0)
        }
      }

      if(mode == "or") score[i] <- (m1 + m2 + m3) * (weights[4] - (weights[4]-1) * (length(found[found==FALSE])!=0))
      else score[i] <- (m1 + m2 + m3) * weights[4] * (length(found[found==FALSE])==0)
    }
    score <- score / max(score, na.rm=TRUE) * 100
    searchindex$index$SCORE <- as.numeric(score)

    if(sum(searchindex$index$SCORE, na.rm=TRUE) > 0) {

      # Show results
      if(results.longdesc == TRUE) {
        # res.cols <- c("SCORE", "NAME", "DESC_SHORT", "DESC_LONG", "DOWNL_TOTAL")
        res.cols <-c(9,1,3,4,7)
        text.align.formattable <- c("r", "l", "l", "l", "l", "l")
        text.align.pandoc <- c("right", "left", "left", "left", "left", "left")
      } else {
        # res.cols <- c("SCORE", "NAME", "DESC_SHORT", "DOWNL_TOTAL", "GO")
        res.cols <-c(9,1,3,7)
        text.align.formattable <- c("r", "l", "l", "l", "l")
        text.align.pandoc <- c("right", "left", "left", "left", "left")
      }

      res<-searchindex$index[searchindex$index$SCORE>0, res.cols]

      res$SCORE <- round(res$SCORE,1)
      res<-res[order(-res$SCORE, -res$DOWNL_TOTAL, tolower(res$NAME), decreasing = FALSE),]
      res<-cbind(res,data.frame(GO = row.names(res)))
      row.names(res)<-NULL

      time.searchend<-Sys.time()
      cat("\n", NROW(res), " out of ", NROW(searchindex$index), " CRAN packages found in", round(as.numeric(time.searchend - time.searchstart, units="secs"),0), "seconds.\n")

      if(tolower(display) == "viewer") {
        bold <- formattable::formatter("span", style = x ~ formattable::style("font-weight:bold; color:#4d4d4d"))
        light.color <- formattable::formatter("span", style = x ~ formattable::style("color:#8c8c8c"))
        res$DOWNL_TOTAL <- formattable::comma(res$DOWNL_TOTAL, format="d", big.mark=",")
        formattable::formattable(res, align=text.align.formattable, list(
          NAME = bold,
          GO = light.color,
          SCORE = formattable::color_tile("white", "chartreuse3"),
          formattable::area(col = DOWNL_TOTAL) ~ formattable::normalize_bar("lightblue", 0.4)
        ))
      } else {
        pander::pandoc.table(res, split.table=Inf,justify=text.align.pandoc, style="grid")
      }
    } else {
      cat("\nNo results found.")
    }
  } else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}



#' @title Showing information about a package
#' @description Allows to inspect a package found with \code{\link{findPackage}} by showing its manual (PDF) or go to its CRAN webpage.
#'
#' @param package Either the name of the package (capitalization does generally not matter) or the search result number shown in the results of \code{\link{findPackage}} (the number in the \code{GO} column).
#' @param index Either a path (or URL) to a search index, or a search index that is already loaded. If no index is provided, \code{findPackage} creates an ad hoc search index.
#' @param where.to Either \code{"manual"}, \code{"cran"} or \code{"install"}. With \code{where.to = "manual"} \code{go()} opens the package's PDF manual from the web, \code{where.to = "cran"} pulls up the package's CRAN website and \code{where.to = "install"} installs the package (including dependencies).
#'
#' @return No return value.
#'
#' @details \code{go()} is made to inspect a package found with \code{findPackage()} and to decide whether or not this package serves the intended purposes.
#'
#' @author Joachim Zuckarelli \email{joachim@@zuckarelli.de}
#'
#' @export
go <- function(package, index = NULL, where.to = "manual") {
  searchindex <- makeIndexAvailable(index)
  if(class(searchindex) == "list"){
    if(!(tolower(where.to) %in% c("install", "cran", "manual"))) {
      stop("Argument package must be either 'install', 'cran' or 'manual'.")
    } else {
      if(is.numeric(package)) {
        if(package %% 1 == 0) {
          df <- searchindex$index[package,]
          if(is.na(df$NAME[1])) stop(paste0("No package with index ", package, " could be found."))
        }
      }
      else {
        df <- searchindex$index[tolower(searchindex$index$NAME)==tolower(package),]
        if(NROW(df)>1) df<- searchindex$index[searchindex$index$NAME==package,]
        if(NROW(df)==0) stop(paste0("Package '", package, "' could not be found."))
      }
      if(tolower(where.to)=="install") {
        utils::install.packages(package, dependencies=TRUE)
      }
      else {
        if(tolower(where.to)=="cran") {
          utils::browseURL(as.character(df$URL))
        }
        else {
          if(tolower(where.to)=="manual") {
            utils::browseURL(as.character(df$MANUAL))
          }
        }
      }
    }
  } else {
    stop("Search index is not available. Create a search index with buidIndex().")
  }
}
