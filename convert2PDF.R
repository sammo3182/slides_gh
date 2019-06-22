library(pagedown)
pagedown::chrome_print("slides/conference/2019-ACPS/migrant_language.Rmd")

library(webshot)
# install_phantomjs()

## For Tsinghua Political Confernece

file_name <- paste0("file:///", normalizePath("slides/conference/2018-MPSA/proficiencyTrust.html", winslash = "/"))
# webshot(file_name, "./slides/conference/2018-MPSA/proficiencyTrust.pdf", zoom = 0.28)

webshot(file_name, "./slides/conference/2018-MPSA/proficiencyTrust.pdf", vwidth = 272, vheight = 204, delay = 2)


file_name <- paste0("file:///", normalizePath("slides/teachingDemo/languagePolicy.html", winslash = "/"))
# webshot(file_name, "./slides/conference/2018-MPSA/proficiencyTrust.pdf", zoom = 0.28)

webshot(file_name, "./slides/teachingDemo/languagePolicy.pdf", vwidth = 272, vheight = 204, delay = 2)

