buildLink <- function(link.url, image.url, image.height=32, tooltip = "") {
  return(as.character(shiny::tags$a(href = link.url,
                                    target = "_blank",
                                    shiny::tags$img(src = image.url,
                                                    style = paste0("height: ", image.height, "px;"),
                                                    title = tooltip
                                    ))
  )
  )
}


getPackageNameFromHTML <- function(html) {
  return(stringr::str_replace(stringr::str_replace(stringr::str_extract(html, ">(.*)<"),
                                                   "<", ""),
                              ">", ""))
}


getPackageDetailsHTML <- function(df) {
  fields <- c(
    "Package",
    "Title",
    "Description",
    "Version",
    "License",
    "License", # Actually downloads but needs to be an existing field name
    "Author",
    "Authors@R",
    "Maintainer",
    "BugReports",
    "URL",
    "Depends",
    "Imports",
    "Suggests",
    "Reverse depends",
    "Reverse imports",
    "Reverse suggests"
  )
  
  headers <- c(
    "Name",
    "Short description",
    "Long description",
    "Version",
    "License",
    "Total Downloads",
    "Authors",
    "Authors", # Actually, the Authors@R field
    "Maintainer",
    "BugReports",
    "URLs",
    "Depends",
    "Imports",
    "Suggests",
    "Reverse depends",
    "Reverse imports",
    "Reverse suggests"
  )
  
  placeholders <- c(
    NA,
    NA,
    NA,
    NA,
    NA,
    {
      paste0("<img src='https://cranlogs.r-pkg.org/badges/grand-total/", df[1, "Package"], "'/>")
    },
    { if(!is.na(df[1, "Authors@R"])) {
      ""
    }
      else {
        paste0(unlist(strsplit(df[1, "Author"], ",")), collapse="<br>")
      }
    },
    {
      if(!is.na(df[1, "Authors@R"])) {
        pers <- eval(parse(text = df[1, "Authors@R"]))
        stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(paste0(pers, collapse = "#"), ">", "&gt;"), "<", "&lt;"), "#", "<br>")
      }
      else {
        ""
      }
    },
    { stringr::str_replace_all(stringr::str_replace_all(df[1,"Maintainer"], ">", "&gt;"), "<", "&lt;") },
    { if(!is.na(df[1, "BugReports"])) {
      paste0("<a href='", df[1, "BugReports"], "'>", df[1, "BugReports"], "</a>")
    }
      else {
        NA
      }
    },
    {
      if(!is.na(df[1, "URL"])) {
        urls <- unlist(strsplit(df[1, "URL"], ","))
        html.urls<-""
        first <- TRUE
        for(i in 1:NROW(urls)) {
          if(first) {
            first <- FALSE
          }
          else {
            html.urls <- paste0(html.urls, "<br>")
          }
          html.urls <- paste0(html.urls, "<a href='", urls[i], "'>", urls[i],"</a>")
        }
        html.urls
      }
      else {
        NA
      }
    },
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  )
  
  html <- "<div style=\"background-color: #FCFAFA; padding-left: 20px\">"
  for(i in 1:NROW(fields)) {
    if(!is.na(df[1, fields[i]])) {
      if(is.na(placeholders[i])) {
        html <- paste0(html,"<span style=\"color:#CDC9C9; font-size:90%; font-style:bold\">", headers[i], "</span><br>")
        html <- paste0(html,"<p>", df[1, fields[i]], "</p>")
        html <- paste0(html,"<p></p>")
      }
      else {
        if(placeholders[i] != "") {
          html <- paste0(html,"<span style=\"color:#CDC9C9; font-size:90%; font-style:bold\">", headers[i], "</span><br>")
          html <- paste0(html, "<p>", eval(placeholders[i]), "</p>")
          html <- paste0(html,"<p></p>")
        }
      }
    }
  }
  html <- paste0(html, "<div>")
  return(html)
}


js <- "
    $(document).keyup(function(event) {
      if ($(\"#txt_search\").is(\":focus\") && (event.keyCode == 13)) {
          $(\"#btn_search\").click();
      }
      if ($(\"#txt_alwayscase\").is(\":focus\") && (event.keyCode == 13)) {
          $(\"#btn_search\").click();
      }
    });
  "

optwidth <- function() return("60%")


inline <- function(widget, label) {
  return(
    shiny::div(style = "display: inline-block; vertical-align: middle; margin-top:0px; margin-bottom:0px; padding-top:0px; padding-bottom:0px",
               shiny::HTML(paste0("<span style = 'middle; margin-top:0px; margin-bottom:0px; padding-top:0px; padding-bottom:0px'>", label, "&nbsp;&nbsp;</span>")),
               shiny::div(style = "display: inline-block; vertical-align: middle; margin-top:0px; margin-bottom:0px; padding-top:0px; padding-bottom:0px",
                          widget
               )
    )
  )
}


processSearch <- function(search = TRUE, input, package.list) {
  if(search) {
    if(!is.null(input$txt_search)) {
      if(stringr::str_replace_all(input$txt_search, " ", "") !="") {
        options("packagefinder.lst_searchterms" = unique(append(getOption("packagefinder.lst_searchterms", c()), shiny::isolate(input$txt_search))))
        if(!is.null(input$chk_case) & input$chk_case != FALSE) {
          case.sensitive <- TRUE
        }
        else {
          case.sensitive <- FALSE
        }
        if(input$txt_alwayscase != "") {
          always.sensitive <- stringr::str_replace_all(unlist(strsplit(input$txt_alwayscase,",")), " ", "")
        }
        else {
          always.sensitive = NULL
        }
        mode <- tolower(input$rad_mode)
        terms <- scan(text = input$txt_search, what = "character")
        if(!input$chk_regex) res <- findPackage(terms, silent = TRUE, return.df = TRUE, mode = mode, case.sensitive = case.sensitive, always.sensitive = always.sensitive, index = getOption("packagefinder.index", NULL))
        else res <- findPackage(query=terms, silent = TRUE, return.df = TRUE, mode = mode, case.sensitive = case.sensitive, always.sensitive = always.sensitive, index = getOption("packagefinder.index", NULL))
      }
    }
  }
  else {
    newoncran <- package.list
    newoncran <- newoncran[lubridate::ymd(newoncran$Published) >= lubridate::today()-getOption("packagefinder.num_optcrandays", 3),]
    newoncran <- newoncran[order(lubridate::ymd(newoncran$Published), decreasing = TRUE),]
    newoncran <- newoncran[, c("Package", "Title", "Description")]
    names(newoncran) <- c("Name", "Short Description", "Long Description")
    res <- cbind(Score = rep(100, NROW(newoncran)), newoncran)
    res <- cbind(res, GO = rep(NA, NROW(newoncran)))
    newoncran <- newoncran[sapply(unique(newoncran$Name), function(x) {min(which(newoncran$Name == x))}),]
  }
  if(!is.null(res)) {
    num.results <- NROW(res)
    
    res[,"Long Description"] <- NULL
    orig.name <- res$Name
    res$GO <- NULL
    res$Installed <- rep("", NROW(res))
    res$ActionPDF <- rep("", NROW(res))
    res$ActionWeb <- rep("", NROW(res))
    res$ActionGitHub <- rep("", NROW(res))
    res$Favorite <- rep("", NROW(res))
    inst <- utils::installed.packages()
    datetime <- as.Date
    df_ext <- package.list
    df_ext <- df_ext[sapply(unique(df_ext$Package), function(x) {min(which(df_ext$Package == x))}),]
    df_ext$AllURLs <- tolower(stringr::str_replace(paste0(df_ext$URL, ",", df_ext$BugReports), " ", ","))
    df_ext$GitHub <- NA
    for(i in 1:NROW(res)) {
      urls.split <- unlist(strsplit(df_ext$AllURLs[df_ext$Package == orig.name[i]], ","))
      if(NROW(urls.split)>0) {
        match.git <- stringr::str_detect(urls.split, "github.com")
        if(sum(match.git, na.rm=TRUE) > 0) {
          df_ext$GitHub[df_ext$Package == orig.name[i]] <- urls.split[which(match.git == TRUE)][1]
        }
      }
    }
    
    for(i in 1:NROW(res)) {
      if(orig.name[i] %in% inst[,"Package"]) {
        res$Installed[i] = "Installed"
      }
      else {
        res$Installed[i] <- paste0("<img src=\"https://www.zuckarelli.de/files/download-col.png\" style=\"height:32px\"
                title = \"Install package '", orig.name[i] , "' (with dependencies)\"/>")
      }
      
      res$ActionPDF[i] <- buildLink(
        link.url = paste0("https://cran.r-project.org/web/packages/", res$Name[i], "\\", res$Name[i], ".PDF"),
        image.url = "https://www.zuckarelli.de/files/PDF-col.png",
        tooltip = paste0("PDF manual of package '", res$Name[i], "'")
      )
      
      res$ActionWeb[i] <- buildLink(
        link.url = paste0("https://cran.r-project.org/web/", res$Name[i]),
        image.url = "https://www.zuckarelli.de/files/r-col.png",
        tooltip = paste0("CRAN website of package '", res$Name[i], "'")
      )
      
      github.url <- df_ext$GitHub[which(df_ext$Package == res$Name[i])]
      if(!is.na(github.url)) {
        res$ActionGitHub[i] <- buildLink(
          link.url = github.url,
          image.url = "https://www.zuckarelli.de/files/social-github-col.png",
          tooltip = paste0("GitHub repository of package '", res$Name[i], "'")
        )
      }
      else {
        res$ActionGitHub[i] <- ""
      }
    }
    
    res$Name = paste0("<span style=\"font-weight:bold\">", res$Name, "</span>")
  }
  else {
    num.results <- 0
  }
  
  return(list(df = res, df_ext = df_ext, num.results = num.results))
}




getPackageFinderCode <- function(input, search = TRUE, cran.days = 3) {
  if(search) {
    if(tolower(input$rad_mode) == "and") mode <- ", mode = \"and\""
    else mode <- ""
    if(!is.null(input$chk_case) & input$chk_case != FALSE) case.sensitive <- ", case.sensitive = TRUE"
    else case.sensitive <- ""
    if(input$txt_alwayscase != "") {
      always.sensitive <- stringr::str_replace_all(unlist(strsplit(input$txt_alwayscase,",")), " ", "")
      if(NROW(always.sensitive) > 1) always.sensitive <- paste0(", always.sensitive = c(", paste0(always.sensitive, collapse = ", "), ")")
      else always.sensitive <- paste0(", always.sensitive = \"", always.sensitive, "\"")
    }
    else always.sensitive <- ""
    terms <- scan(text = input$txt_search, what = "character")
    if(NROW(terms) > 1) terms <- paste0("c(", paste0(paste0("\"", terms, "\""), collapse = ", "), ")")
    else terms <- paste0("\"", terms, "\"")
    if(input$chk_regex) terms <- paste0("query = ", terms)
    code <- paste0("findPackage(", terms, mode, case.sensitive, always.sensitive, ")")
  }
  else {
    code <- paste0("whatsNew(last.days = ", cran.days, ")")
  }
  return(code)
}



waitUI <- function(code) {
  return(
    as.list(shiny::tagList(shiny::HTML(paste0("<table id='msg' style='width:100%'>
                                <tr>
                                  <td>
                                    <p><span style='font-weight: bold'>While we are searching ... Did you know?</span><span> You can also search from the R console:</span></p>
                                    <span style='font-family:Courier; font-size:120%'>", code, "</span>&nbsp;&nbsp;",
                                              shiny::actionButton("copy", "Copy R code"), "
                                  </td>
                                  <td>
                                   <a href= \"https://github.com/jsugarelli/packagefinder\"><img src='https://www.zuckarelli.de/files/hexagon-packagefinder.png' style='width:120px'></a>
                                  </td>
                                </tr>
                              </table><p id='p1'>&nbsp;</p><p id='p2'>&nbsp;</p>"))
    ),
    ))
}