# # book-specific code to include on every page


glossary_table <- function(as_kable = TRUE) {
  glossary <- glossary_options("table")
  if (is.null(glossary))
    glossary <- list()
  term <- names(glossary)
  linked_term <- term
  if (!is.null(glossary_path()) && glossary_path() == "psyteachr") {
    lcterm <- gsub(" ", "-", tolower(term), fixed = TRUE)
    first_letter <- substr(lcterm, 1, 1)
    linked_term <- paste0(" [", lcterm, "](https://psyteachr.github.io/glossary/",
                          first_letter, "#", lcterm, "){target='_blank' class='glossary'} ")
  }
  if (is.null(term)) {
    data.frame()
  } else if (as_kable) {
    the_list <- data.frame(term = linked_term, definition = unlist(glossary))
    kable(the_list[order(term), ],
          escape = FALSE,
          row.names = FALSE, )
  } else {
    the_list <- data.frame(term = term, definition = unlist(glossary))
    the_list[order(term), ]
  }
}


# library(knitr)
#
# # useful function for options
# `%||%` <- function(l, r) {
#   if (is.null(l)) r else l
# }
#
# # super-customised table printing ----
# knit_print.data.frame <- function (x, options, ...) {
#   # get options
#   digits <- options$digits %||% getOption("digits")
#   rownames <- options$rownames %||% FALSE
#   pageLength <- options$pageLength %||% 10
#   escape <- options$escape %||% TRUE
#   caption <- options$table.cap
#
#   # use paged for longer tables in html
#   if (nrow(x) > pageLength & knitr::is_html_output()) {
#     numeric_cols <- sapply(x, is.numeric) |> which() |> names()
#     dt <- DT::datatable(x,
#                         rownames = rownames,
#                         caption = caption,
#                         escape = escape,
#                         width = "100%",
#                         height = "auto",
#                         options = list(pageLength = pageLength),
#                         selection = "none")
#     if (length(numeric_cols) > 0) {
#       dt <- DT::formatRound(dt,
#                             columns = numeric_cols,
#                             digits = digits)
#     }
#     knitr::knit_print(dt, options)
#   } else {
#     # use kableExtra::kable for PDFs or shorter tables
#     k <- kableExtra::kable(x,
#                            digits = digits,
#                            row.names = rownames,
#                            caption = caption,
#                            escape = escape) |>
#       kableExtra::kable_styling(
#         full_width = options$full_width,
#         bootstrap_options = c("striped", "hover")
#       )
#
#     if (knitr::is_html_output()) {
#       k <- c("<div class=\"kable-table\">", k, "</div>") |>
#         paste(collapse = "\n")
#     }
#
#     knitr::asis_output(k)
#   }
# }
# registerS3method("knit_print", "data.frame", knit_print.data.frame)
