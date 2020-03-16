html.viewHTML <- function() {
  page.size <- 50
  res <- getOption("packagefinder.lastresults.full", "")
  num.pages <-  NROW(res) %/% page.size
  if(NROW(res) %% page.size > 0) num.pages <- num.pages + 1
  files <- rep("", num.pages)
  for(i in 1:num.pages) files[i] <- tempfile(fileext=".html")
  for(i in 1:num.pages) {
    page.start <- 1+(i-1)*page.size
    page.end <- min(i*page.size, NROW(res))
    res.page <- res[c(page.start:page.end),]
    pagination <- "<span style=\"align:center;\">"
    for(f in 1:num.pages) {
      if(f==i) zoom <- "font-size:24px;"
      else zoom <- ""
      pagination <- paste0(pagination, "<a style=\"padding-right:10px;", zoom,"\" href=\"", files[f], "\">",1+(f-1)*page.size,"-", min(f*page.size, NROW(res)),"</a>")
    }
    pagination <- paste0(pagination, "</span>")
    if(num.pages>1) html.code <- html.buildDoc(paste0(html.header(), html.formatdf(res.page), pagination, html.footer()))
    else html.code <- html.buildDoc(paste0(html.header(), html.formatdf(res.page), html.footer()))
    write(html.code, file=files[i])
    if(i==1) shell.exec(files[1])
    # utils::browseURL(files[1]))
  }
}


html.header <- function() {
 func.call <- deparse(getOption("packagefinder.call", ""))
 call.info <- paste0("<form>Original call: <input type=\"text\" readonly size=", round(nchar(func.call)*1.5,0)," style=\"border:0px;font-family:'Cutive Mono';font-size:17px\"  value='", func.call,"' id=\"code\"><br><input type=\"button\" value=\"Copy to clipboard\" class=\"button\" onclick=\"var copyText=document.getElementById('code');copyText.select(); document.execCommand('copy')\"></form>")
 keywords.info <- paste(paste0("<span style=\"font-weight:bold;\">", getOption("packagefinder.keywords", ""), "</span>"), collapse=paste0(" <span style=\"color:#cfcfcf;\">", getOption("packagefinder.mode", ""), "</span> "))
 html.code <- paste0("
    <table>
      <tr>
        <td style=\"width:20%\">
          <a href=\"http://www.zuckarelli.de/packagefinder/tutorial.html\"><img src=\"http://www.zuckarelli.de/files/hexagon-packagefinder.png\" style=\"width:164\"></a>
        </td>
        <td style=\"padding-left:20px; width:80%\">
          <p style=\"font-size:36px;\">Search Results</p>
          <p>", call.info, "</p>
          <p>Search query: ", keywords.info, "</p>
          <p><span style=\"font-weight:bold\">", getOption("packagefinder.num.results", "") ,"</span> of <span style=\"font-weight:bold\">", getOption("packagefinder.num.cran", ""), "</span> CRAN packages found in <span style=\"font-weight:bold\">", getOption("packagefinder.timediff", ""), "</span> seconds.</p>
        </td>
      </tr>
    </table>
    <p>&nbsp;</p>")
  return(html.code)
}


html.footer <- function(){
  html.code <- paste0("<p style=\"text-align:right; font-size:13px\">Created by <a href=\"http://www.zuckarelli.de/packagefinder/tutorial.html\">packagefinder</a> version ", utils::packageVersion("packagefinder"), "</p>")
  return(html.code)
}


html.colorscore <- function(scores) {
  red <- format(as.hexmode(255-round(255 * scores/100,0)), width=2)
  green <- format(as.hexmode(255), width=2)
  blue <- format(as.hexmode(255-round(255 * scores/100,0)), width=2)

  return(paste0("#", red, green, blue))
}


html.getbar <- function(scores) {
  res <- c(rep(0, NROW(scores)))
  for(i in 1:NROW(scores)) {
    res[i] <- paste0(rep("&#9610;", (scores[i] %/% 20)), collapse="")
  }
  return(res)
}


html.formatdf <- function(df) {
  keywords <- getOption("packagefinder.keywords", NULL)
  df[,"Long Description"] <- textutils::HTMLencode(df[,"Long Description"])
  df[,NCOL(df)] <- NULL    # Cut-off GO number
#  df[, "Score"] <- paste0(format(df[, "Score"], nsmall=1), "<span style=\"padding-left:10px; color:", html.colorscore(df[, "Score"]),"\">", html.getbar(df[, "Score"]), "</span>")
  df[, "Score"] <- paste0(format(df[, "Score"], nsmall=1), "<span style=\"padding-left:10px; color:#1ddb1d\">", html.getbar(df[, "Score"]), "</span>")

  if("Total Downloads" %in% colnames(df)) df[,"Total Downloads"] <- NULL
  df[,"Total Downloads"] <- paste0("<img src=\"https://cranlogs.r-pkg.org/badges/grand-total/", df$Name,"\"/>")
  linkspart.manual <- paste0("<a target=\"_blank\" href=\"https://cran.r-project.org/web/packages/", df$Name, "/", df$Name, ".pdf\"><img style=\"align-vertical:middle; padding-right:5px; width:28px\" src=\"http://www.zuckarelli.de/files/pdf-col.png\" /></a>")
  linkspart.cranpage <- paste0("<a target=\"_blank\" href=\"https://cran.r-project.org/package=", df$Name, "\"><img style=\"align-vertical:middle;  padding-right:10px; width:32px\" src=\"http://www.zuckarelli.de/files/r-col.png\" /></a>")
  linkspart.google <- paste0("<a target=\"_blank\" href=\"https://www.google.com/search?q=r+package+", df$Name, "\"><img style=\"align-vertical:middle; padding-right:5px; width:26px\" src=\"http://www.zuckarelli.de/files/google-col.png\" /></a>")
  df$Links <- paste0("<div>", linkspart.manual, linkspart.cranpage, linkspart.google, "</div>")
  df[,"Install code"] <- paste0("<form> <input type=\"button\" class=\"button\" value=\"Copy\" onclick=\"var copyText=document.getElementById('install_",df$Name,"');copyText.select(); document.execCommand('copy')\"><input type \"text\" style=\"position: relative; left: -10000px;\" id=\"install_", df$Name,"\" value=\'install.packages(\"",df$Name,"\", dependencies = TRUE)'></form>")
  df$Name <- paste0("<a href=\"https://cran.r-project.org/package=", df$Name, "\">", df$Name, "</a>")

  for(i in 1:NROW(keywords)) {
    df$Name <- gsub(paste0("(.*^=",keywords[i],")"), "<span style=\"background-color:#e6f1ff\">\\1</span>", df$Name, ignore.case = TRUE, perl=TRUE)
    df[,"Short Description"] <- gsub(paste0("(",keywords[i],")"), "<span style=\"background-color:#e6f1ff\">\\1</span>", df[,"Short Description"], ignore.case = TRUE, perl=TRUE)
    df[,"Long Description"] <- gsub(paste0("(",keywords[i],")"), "<span style=\"background-color:#e6f1ff\">\\1</span>", df[,"Long Description"], ignore.case = TRUE, perl=TRUE)
  }

  css.cols <- rep("", NCOL(df))
  css.cols[1] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; vertical-align:top"
  css.cols[2] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; width:8%; vertical-align:top"
  css.cols[3] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; width:15%; vertical-align:top"
  css.cols[4] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; width:45%; vertical-align:top"
  css.cols[5] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; vertical-align:top; text-align:center"
  css.cols[6] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; width:10%; vertical-align:top; text-align:center"
  css.cols[7] <- "border-bottom:1pt solid #d1d1d1; padding: 7px; vertical-align:top; text-align:center"

  html.code <- htmlTable::htmlTable(
    df,
    align = paste0(c(rep("l", NCOL(df)-2),"c", "c")),
    align.header = paste0(c(rep("l", NCOL(df)-2),"c", "c")),
    css.cell = css.cols,
    rnames = FALSE,
    css.class = "hover"
  )
  return(html.code)
}


html.buildDoc <-function(body.html) {
  html.code <- paste0("
    <html>
      <head>
        <meta charset=\"utf-8\">
        <link rel=\"stylesheet\" type=\"text/css\" href=\"http://fonts.googleapis.com/css?family=Hind+Madurai|Cutive+Mono\">
        <title>packagefinder Search results</title>
        <style style=\"text/css\">
          .hover{ width:100%; border-collapse:collapse; }
          .hover tr{ background: #ffffff;}
          .hover tr:hover { background-color: #ededed; }
          input[type=button] { background-color: #ED5036; border:none; color:#FFFFFF; padding: 5px 15px 5px 15px; text-decoration:none; margin: 0px; border-radius:5px; }
        </style>
      </head>
      <body style=\"font-family:'Hind Madurai'; color:#494949; backgroundcolor:#FFFFFF;\">",
        body.html,
      "</body>
    <html>")
  return(html.code)
}
