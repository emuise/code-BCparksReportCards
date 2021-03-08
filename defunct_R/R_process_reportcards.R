library(rmarkdown)
library(knitr)
library(magick)

#place this background in the params r file
background <- image_read_pdf("report_cards/background_template.pdf")


reportCardGen <- function(inPark){
  parameters <- list(inPark = inPark)
  rmarkdown::render("R_generate_reportcard_figures.rmd", 
                    output_format = "html_document", 
                    output_file = paste0(inPark, "_Reportcard"),
                    output_dir = "reportCards",
                    params = parameters)
}

valid_parks <- read_csv("joinTables/parknames.csv") %>% pull(processing_name)

valid_parks <- valid_parks[file.exists(file.path("outputs", valid_parks, "rasters"))]

map(valid_parks, reportCardGen)


