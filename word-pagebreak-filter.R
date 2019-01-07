#!/usr/bin/env Rscript

<<<<<<< HEAD
# Obtained from Noam Ross' answer to this question on stack overflow: https://stackoverflow.com/questions/24672111/how-to-add-a-page-break-in-word-document-generated-by-rstudio-markdown
# Copied to my Dropbox/R_Stats_resources (WWD)

# From the post by Noam: Here is an R script that can be used as a pandoc filter to replace LaTeX breaks (\pagebreak) with word breaks, per @JAllen's answer above. With this you don't need to compile a pandoc script. Since you are working in R Markdown I assume one has R available in the system.
=======
json_in <- file('stdin', 'r')
lat_newp <- '{"t":"RawBlock","c":["latex","\\\\newpage"]}'
doc_newp <- '{"t":"RawBlock","c":["openxml","<w:p><w:r><w:br w:type=\\"page\\"/></w:r></w:p>"]}'
ast <- paste(readLines(json_in, warn=FALSE), collapse="\n")
ast <- gsub(lat_newp, doc_newp, ast, fixed=TRUE)
write(ast, "")

# Obtained from Noam Ross' answer to this question on stack overflow: https://stackoverflow.com/questions/24672111/how-to-add-a-page-break-in-word-document-generated-by-rstudio-markdown
>>>>>>> f6382e06dcfb9e8b184681a9f1d54aabc27d41ef

# Save this as page-break-filter.R or something like that and make it executable by running chmod +x page-break-filter.R in the terminal.

# Then include this filter the R Markdown YAML like so:
#
<<<<<<< HEAD
#       ---
#       title: "Title
# author: "Author"
# output:
#   word_document:
#     pandoc_args: [
#       "--filter", "/path/to/pandoc-newpage-filter.R"
#     ]
# ---

json_in <- file('stdin', 'r')
lat_newp <- '{"t":"RawBlock","c":["latex","\\\\newpage"]}'
doc_newp <- '{"t":"RawBlock","c":["openxml","<w:p><w:r><w:br w:type=\\"page\\"/></w:r></w:p>"]}'
ast <- paste(readLines(json_in, warn=FALSE), collapse="\n")
ast <- gsub(lat_newp, doc_newp, ast, fixed=TRUE)
write(ast, "")
=======
# ---
# title: "Title
# author: "Author"
# output:
# word_document:
# pandoc_args: [
# "--filter", "/path/to/pandoc-newpage-filter.R"
# ]
# ---
>>>>>>> f6382e06dcfb9e8b184681a9f1d54aabc27d41ef
