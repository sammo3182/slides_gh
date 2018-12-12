library(webshot)
# install_phantomjs()

file_name <- paste0("file:///", normalizePath("slides/conference/2018-MPSA/proficiencyTrust.html", winslash = "/"))
# webshot(file_name, "./slides/conference/2018-MPSA/proficiencyTrust.pdf", zoom = 0.28)

webshot(file_name, "./slides/conference/2018-MPSA/proficiencyTrust.pdf", vwidth = 272, vheight = 204, delay = 2)

