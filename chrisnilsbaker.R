d <- read.csv("41330723.csv", header = TRUE, stringsAsFactors = FALSE)
View(d)
head(d[["Inside.Outside.Footprint"]])
head(d[["Inside.Outside.Footprint"]][d[["Inside.Outside.Footprint"]] == "Outside"])


