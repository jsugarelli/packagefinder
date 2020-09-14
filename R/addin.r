packagefinderAddIn <-function() {

  # -------------------------- User Interface -------------------------- #


  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML(js)),
    shiny::tags$head(

      shiny::tags$style(
        shiny::HTML(
          "
          #inputs-table {
            border-collapse: collapse;
          }

          #inputs-table td {
            padding-top: 0px;
            padding-bottom: 0px;
            padding-left: 10px;
            padding-right: 10px;
            vertical-align: bottom;
          }
          "
        )
      )
    ),

    # titlePanel("packagefinder"),
    shiny::HTML("<p></p>"),
    shiny::tabsetPanel(
      shiny::tabPanel(title = "Search",
            shiny::wellPanel(style = "padding: 0px; margin-top: 15px; margin-bottom: 0px;",
                         shiny::tags$table(id = "inputs-table", style = "width: 100%; margin-top: 0px; padding: 0px",
                                    shiny::tags$tr(style = "vertical-align:middle",
                                            # shiny::tags$td(textInput(inputId = "txt_search", label = "", value = "")),
                                            shiny::tags$td(style = "vertical-align:middle", shiny::tags$span(style = "margin-top:30px",
                                                                                               shiny::selectizeInput(inputId = "txt_search",
                                                                                                              label = "",
                                                                                                              choices = getOption("packagefinder.lst_searchterms", c()),
                                                                                                              options = list(create = TRUE,
                                                                                                                             createOnBlur = TRUE,
                                                                                                                             onChange = I("function(value) { Shiny.setInputValue('SearchTxtChange', Math.random()); }"),
                                                                                                                             render = I("{ option_create: function(data, escape) { return('<div class=\"create\"><strong>' + escape(data.input) + '</strong></div>');} }")
                                                                                                              )
                                                                                               ))),
                                            shiny::tags$td(style = "vertical-align:middle", shiny::actionButton(inputId = "btn_search", label = "Search"), shiny::actionButton(inputId = "btn_new", label = "New on CRAN")),
                                            shiny::tags$td(width="5%"),
                                            shiny::tags$td(style="text-align: right; vertical-align:middle",  shiny::actionButton(inputId = "btn_installsel", label = "Install Selected"),
                                                           shiny::actionButton(inputId = "btn_close", label = "Close")),
                                    ),
                                    shiny::tags$tr(
                                      shiny::tags$td(shiny::radioButtons(inputId = "rad_mode", label = "Connect search terms with", choices = c("OR", "AND"), selected = { if(getOption("packagefinder.chk_optlog", FALSE) == FALSE) "OR" else "AND" }, inline = TRUE)),
                                      shiny::tags$td(shiny::checkboxInput(inputId = "chk_case", label = "Case sensitive search", value = getOption("packagefinder.chk_optcase", FALSE))),
                                      shiny::tags$td(),
                                      shiny::tags$td()
                                    ),
                                    shiny::tags$tr(
                                      shiny::tags$td(),
                                      shiny::tags$td("Always case-sensitive (comma-sep.):", shiny::textInput(inputId = "txt_alwayscase", label = NULL, value = "", width = "100%")),
                                      shiny::tags$td(),
                                      shiny::tags$td()
                                    )
                         )
            ),
            shiny::wellPanel(style="padding: 5px; margin-top: 15px; margin-bottom: 10px;",
                         shiny::htmlOutput(outputId = "message")
               ),
               shinybusy::add_busy_spinner(spin = "fading-circle", color = "#ED5036"),
               reactable::reactableOutput(outputId = "tbl_results")
      ),
      shiny::tabPanel(title = "Options",
                      shiny::wellPanel(style = "margin-top: 5px; margin-bottom: 5px; padding-left: 10px; padding-right: 10px; padding-top: 0px",
                                       shiny::HTML("<H3>Options</H2>"),

                                       shiny::HTML("<H4 style = 'margin-top: 30px'>Search</H3>"),
                                       inline(shiny::numericInput("num_opthistentries", label = "", value = getOption("packagefinder.num_opthistentries", 10), min = 1, step = 1, width = "100px"), "Number entries in search history:"),
                                       shiny::checkboxInput("chk_optlog", label = "Use 'AND' as default logical operator to combine search terms", value = getOption("packagefinder.chk_optlog", FALSE), width = optwidth()),
                                       shiny::checkboxInput("chk_optcase", label = "Use case-sensitive search per default", value = getOption("packagefinder.chk_optcase", FALSE), width = optwidth()),
                                       shiny::checkboxInput("chk_optdf", label = "Automatically save search results as datafame 'packagefinder.results' in the global environment", value = getOption("packagefinder.chk_optdf", FALSE), width = optwidth()),


                                       shiny::HTML("<H4 style = 'margin-top: 30px'>New on CRAN</H3>"),
                                       inline(shiny::numericInput("num_optcrandays", label = "", value = getOption("packagefinder.num_optcrandays", 3), min = 1, step = 1, width = "100px"), "Number of days for 'New on CRAN' search:"),
                                       shiny::HTML("<H4 style = 'margin-top: 30px'>Search results</H3>"),
                                       shiny::checkboxInput("chk_optzebra", label = "Striped table (zebra-style)", value = getOption("packagefinder.chk_optzebra", FALSE), width = optwidth()),
                                       inline(shiny::selectInput("sel_optnumresults", label = "", choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100), selected = getOption("packagefinder.sel_optnumresults", 10), width = "100px"), "Number of results per page:")
                       )
      )
    )
  )




  # -------------------------- Server -------------------------- #


  server <- function(input, output, session) {

    if(!exists("df", inherits = FALSE)) df <- data.frame(list(Score = NA, Name = NA, "Short Description" = NA, ActionPDF = NA, ActionWeb = NA, Favorite=NA), stringsAsFactors = FALSE)
    r <- shiny::reactiveValues(df = df, num.results = 0, cran.days = 3)

    package.list = tools::CRAN_package_db()
    if(is.null(getOption("packagefinder.index", NULL))) options("packagefinder.index" = buildIndex())

    shiny::observeEvent(input$btn_close, { shiny::stopApp() })
    shiny::observeEvent(input$done, { shiny::stopApp() })

    shiny::observeEvent(input$btn_search, {
      r$code <- getPackageFinderCode(input, TRUE)
      shiny::insertUI(
        selector = "#tbl_results",
        where = "beforeBegin",
        ui = waitUI(shiny::isolate(r$code)),
        immediate=TRUE
      )
      ui.elems <- c("#msg", "#copy", "#p1", "#p2")
      lst <- processSearch(TRUE, input, package.list)
      r$df <- lst$df
      r$df_ext <- lst$df_ext
      r$num.results <- lst$num.results
      for(i in 1:NROW(ui.elems)) shiny::removeUI(selector = ui.elems[i], immediate=TRUE)
    })

    shiny::observeEvent(input$SearchTxtChange, {
      if(input$txt_search != "" & !is.null(input)) {
        r$code <- getPackageFinderCode(input, TRUE)
        shiny::insertUI(
          selector = "#tbl_results",
          where = "beforeBegin",
          ui = waitUI(shiny::isolate(r$code)),
          immediate=TRUE
        )
        ui.elems <- c("#msg", "#copy", "#p1", "#p2")
        lst <- processSearch(TRUE, input, package.list)
        r$df <- lst$df
        r$df_ext <- lst$df_ext
        r$num.results <- lst$num.results
        for(i in 1:NROW(ui.elems)) shiny::removeUI(selector = ui.elems[i], immediate=TRUE)
      }
    })

    shiny::observeEvent(input$copy, {
      clipr::write_clip(shiny::isolate(r$code))
    })

    shiny::observeEvent(input$btn_new, {
      r$code <- getPackageFinderCode(input, FALSE)
      shiny::insertUI(
        selector = "#tbl_results",
        where = "beforeBegin",
        ui = waitUI(shiny::isolate(r$code)),
        immediate=TRUE
      )
      ui.elems <- c("#msg", "#copy", "#p1", "#p2")
      lst <- processSearch(FALSE, input, package.list)
      r$df <- lst$df
      r$df_ext <- lst$df_ext
      r$num.results <- lst$num.results
      for(i in 1:NROW(ui.elems)) shiny::removeUI(selector = ui.elems[i], immediate=TRUE)
    })


    output$message <- shiny::renderUI({
      html.found <- ""
      html.sels <- ""
      html.favs <- ""
      if(!is.na(r$df$Score[1])) {
        if(r$num.results > 0) {
          if(r$num.results == 1) package.num <- "package"
          else package.num <- "packages"
          html.found <- shiny::HTML(paste0("<span style='font-weight:bold'>", r$num.results, "</span> ", package.num, " found of <span style='font-weight:bold'>", NROW(r$df_ext), " </span>packages on CRAN."))
          favs.num <- NROW(r$df[r$df$Favorite != "",])
          sels.num <- NROW(reactable::getReactableState("tbl_results", "selected"))
          if(sels.num > 0) {
            if(sels.num == 1) package.num <- "package"
            else package.num <- "packages"
            html.sels <- paste0("&nbsp;	&nbsp; &nbsp;<span style='font-weight:bold'>", sels.num, "</span> ", package.num, " selected.")
          }
          if(favs.num > 0) {
            if(favs.num == 1) package.num <- "package"
            else package.num <- "packages"
            html.favs <- paste0("&nbsp;	&nbsp; &nbsp;<span style='font-weight:bold'>", favs.num, "</span> ", package.num, " marked as favorites.")
          }
        }
        else {
          html.found <- "<span style='font-weight: bold; color: #C70039'>No results found.</span>"
        }
      }
      else {
        html.found <- "<span style='font-weight: bold; color: #000000'>&nbsp;</span>"
      }
      shiny::HTML(paste0(html.found, html.sels, html.favs))
    })


    shiny::observeEvent(r$num.results, {
      if(r$num.results > 0) {
        shinyjs::show("tbl_results", animType = "fade")
      }
      else {
        shinyjs::hide("tbl_results", animType = "fade")
      }
    })


    output$tbl_results <- reactable::renderReactable({
      if(!is.na(r$df$Score[1])) {
        reactable::reactable(data = r$df,
                  details = reactable::colDef(
                    name = "Details",
                    html = TRUE,
                    width = 80,
                    details = function(index) {
                      getPackageDetailsHTML(shiny::isolate(r$df_ext[which(r$df_ext$Package == getPackageNameFromHTML(r$df[index, "Name"])),]))
                    }
                  ),
                  columns = list(
                    .selection = reactable::colDef(show = FALSE),
                    Score = reactable::colDef(
                      format = reactable::colFormat(separators = TRUE, digits = 1),
                      width = 100,
                      style = function(value) {
                        if(NROW(r$df) == 1 | NROW(unique(r$df$Score)) == 1) {
                          color = "#05B905"
                        }
                        else {
                          normalized <- (value - min(r$df$Score)) / (max(r$df$Score) - min(r$df$Score))
                          color <- grDevices::rgb(grDevices::colorRamp(c("#FFFFFF", "#05B905"))(normalized), maxColorValue = 255)
                        }
                        return(list(background = color))
                      }
                    ),
                    Name = reactable::colDef(html = TRUE, width = 220, style="background: #FCFBFA;"),
                    ActionPDF = reactable::colDef(name = "", width = 50, filterable = FALSE, sortable = FALSE, html = TRUE),
                    ActionWeb = reactable::colDef(name = "", width = 50, filterable = FALSE, sortable = FALSE, html = TRUE),
                    ActionGitHub = reactable::colDef(name = "", width = 50, filterable = FALSE, sortable = FALSE, html = TRUE),
                    Installed = reactable::colDef(name = "Installed", filterable = FALSE, html = TRUE, width = 200),
                    Favorite = reactable::colDef(name = "Favorite", width = 100, filterable = FALSE, sortable = TRUE, align = "center", html = TRUE)
                  ),
                  filterable = TRUE,
                  selection = "multiple",
                  showPageSizeOptions = TRUE,
                  defaultPageSize = as.numeric(input$sel_optnumresults),
                  striped = input$chk_optzebra,
                  showSortable = TRUE,
                  highlight = TRUE,
                  rowStyle = list(cursor = "pointer"),
                  theme = reactable::reactableTheme(
                    rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ed5036")
                  ),
                  onClick = reactable::JS(
                    "
                      function(rowInfo, colInfo) {
                        if (window.Shiny) {
                          Shiny.onInputChange('clicked', { column: colInfo.id, index: rowInfo.index + 1, rand: Math.random() })
                        }
                      }
                    "
                  )
        )
      }
      else {
      }
    })



    shiny::observeEvent(input$btn_installsel, {
      sel <- reactable::getReactableState("tbl_results", name = "selected")
      if(NROW(sel) > 0) {
        for(i in 1:NROW(sel)) {
          utils::install.packages(getPackageNameFromHTML(shiny::isolate(r$df$Name[sel[i]])), dependencies = TRUE)
          r$df$Installed[sel[i]] <- "Installed"
        }
      }
    })


    shiny::observeEvent(input$clicked, {
      if(!(input$clicked$column %in% c("ActionPDF", "ActionWeb", "Favorite"))) {
        sel <- reactable::getReactableState("tbl_results", name = "selected")
        if(input$clicked$index %in% sel) {
          sel <- sel[-which(sel == input$clicked$index)]
          reactable::updateReactable("tbl_results", selected = sel)
        }
        else {
          reactable::updateReactable("tbl_results", selected = append(sel, input$clicked$index))
        }
      }

      if(input$clicked$column == "Installed") {
        if(shiny::isolate(r$df$Installed[input$clicked$index]) != "Installed") {
          utils::install.packages(getPackageNameFromHTML(shiny::isolate(r$df$Name[input$clicked$index])), dependencies = TRUE)
          r$df$Installed[input$clicked$index] <- "Installed"
        }
      }

      fav = "<span style = \"color: #ed5036; font-size: 18px\">&#10687;</span>"
      if(input$clicked$column == "Favorite") {
        if(shiny::isolate(r$df[as.numeric(input$clicked$index), "Favorite"]) != fav) {
          r$df[as.numeric(input$clicked$index), "Favorite"] <- fav
        }
        else {
          r$df[as.numeric(input$clicked$index), "Favorite"] <- ""
        }
      }
    })

    # Options
    shiny::observeEvent(input$num_optcrandays, { options("packagefinder.num_optcrandays" = input$num_optcrandays) })
    shiny::observeEvent(input$num_opthistentries, { options("packagefinder.num_opthistentries" = input$num_opthistentries) })
    shiny::observeEvent(input$chk_optcase, {
      shiny::updateCheckboxInput(session, "chk_case", value = input$chk_optcase)
      options("packagefinder.chk_optcase" = input$chk_optcase) })
    shiny::observeEvent(input$chk_optdf, { options("packagefinder.chk_optdf" = input$chk_optdf) })
    shiny::observeEvent(input$chk_optlog, {
      shiny::updateRadioButtons(session, "rad_mode", selected = { if(input$chk_optlog == FALSE) "OR" else "AND" })
      options("packagefinder.chk_optlog" = input$chk_optlog) })
    shiny::observeEvent(input$chk_optzebra, { options("packagefinder.chk_optzebra" = input$chk_optzebra) })
    shiny::observeEvent(input$sel_optnumresults, { options("packagefinder.sel_optnumresults" = input$sel_optnumresults) })
  }

  viewer <- shiny::dialogViewer(dialogName = "packagefinder - Search for packages", width=1800, height=1000)
  shiny::runGadget(ui, server, viewer = viewer)
}
