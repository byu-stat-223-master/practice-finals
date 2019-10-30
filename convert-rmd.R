# This script converts the README.Rmd files in all child directories into
# README.md and README.html files


rmd_files <- fs::dir_ls(recurse = TRUE, regexp = "(README.Rmd)")

purrr::walk(rmd_files, rmarkdown::render, output_format = c("github_document", "html_document"))
