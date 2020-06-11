#! /usr/bin/env bash
# Render all examples, copy output to example directory, and clean up.

Rscript -e 'rmarkdown::render("harmonized-variable-reproduction-height.Rmd")' > /dev/null
Rscript -e 'rmarkdown::render("harmonized-variable-reproduction-weight.Rmd")' > /dev/null
Rscript -e 'rmarkdown::render("harmonized-variable-reproduction-bmi.Rmd")' > /dev/null

mv example_height_1.tsv example_data/harmonized/
mv example_weight_1.tsv example_data/harmonized/
mv example_bmi_1.tsv example_data/harmonized/
