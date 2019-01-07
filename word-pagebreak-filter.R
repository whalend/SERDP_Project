#!/usr/bin/env Rscript

json_in <- file('stdin', 'r')
lat_newp <- '{"t":"RawBlock","c":["latex","\\\\newpage"]}'
doc_newp <- '{"t":"RawBlock","c":["openxml","<w:p><w:r><w:br w:type=\\"page\\"/></w:r></w:p>"]}'
ast <- paste(readLines(json_in, warn=FALSE), collapse="\n")
ast <- gsub(lat_newp, doc_newp, ast, fixed=TRUE)
write(ast, "")

# Obtained from Noam Ross' answer to this question on stack overflow: https://stackoverflow.com/questions/24672111/how-to-add-a-page-break-in-word-document-generated-by-rstudio-markdown

# Save this as page-break-filter.R or something like that and make it executable by running chmod +x page-break-filter.R in the terminal.

# Then include this filter the R Markdown YAML like so:
#
# ---
# title: "Title
# author: "Author"
# output:
# word_document:
# pandoc_args: [
# "--filter", "/path/to/pandoc-newpage-filter.R"
# ]
# ---
