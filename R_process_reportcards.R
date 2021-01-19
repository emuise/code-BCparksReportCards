library(rmarkdown)
library(knitr)

inPark = "Spats"
reportCardGen <- function(inPark){
  parameters <- list(inPark = inPark)
  rmarkdown::render("reportCard.rmd", 
                    output_format = "html_document", 
                    output_file = paste0(inPark, "_Reportcard"),
                    output_dir = "reportCards",
                    params = parameters)
}

parks <- c("Gar", "Spats", "Strath", "StuartRiverParkLowerSite", "MountSeymourPark", "JoffreLakesPark")

map(parks, reportCardGen)
