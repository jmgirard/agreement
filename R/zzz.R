## Quiet R CMD check about global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "Category", "Estimate", "Term", "categories", "weighting"
    )
  )
}
