# Define Data Path
root <- "D:/Users/gmohanna/SkyDrive/"
data_path <- "Documents/HW/Coursera/Data Science Specialization/7 - Regression Models/Project"
setwd(paste0(root, data_path))

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("all_analysis.Rmd")
markdownToHTML('all_analysis.md', 'all_analysis.html', options=c("use_xhml"))
system("pandoc -s all_analysis.html -o all_analysis.pdf")
