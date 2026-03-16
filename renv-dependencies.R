
# This file exists so renv detects packages required to render the site that
# are not referenced directly in project code (e.g., Quarto features invoked
# through YAML such as `code-link: true`). Without this file, renv may mark
# these packages as unused and omit them from snapshots/restores.

# Dependencies needed for Quarto HTML code-linking
requireNamespace("downlit", quietly = TRUE)
requireNamespace("xml2", quietly = TRUE)