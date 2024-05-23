# virtus

`virtus` is an R package that measures values-based language in text. It allows for the analysis of texts to measure human values based on a predefined dictionary based on the paper 'Development and Validation of the Personal Values Dictionary: A Theory-Driven Tool for Investigating References to Basic Human Values in Text' by Ponizovskiy et. al (2020). The package includes functions for a basic count of word occurrences, as well as functions that adjust scores based on modifying words.

## Installation

You can install the `virtus` package directly from GitHub using the `devtools` package.

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install virtus from GitHub
devtools::install_github("yourusername/virtus")
