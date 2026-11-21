url <- "https://archive.ics.uci.edu/static/public/292/wholesale+customers.zip"
archive <- tempfile(fileext = ".zip")
download.file(url, archive, mode = "wb", quiet = TRUE)
file <- unzip(archive, list = TRUE)$Name[1]
wholesale_customers <- read.csv(unz(archive, file))
names(wholesale_customers) <- tolower(names(wholesale_customers))
wholesale_customers$channel <- factor(wholesale_customers$channel,
                                      labels = c("hotel_restaurant_cafe", "retail"))
wholesale_customers$region <- factor(wholesale_customers$region,
                                     labels = c("lisbon", "porto", "other"))
save(wholesale_customers, file = "data/wholesale_customers.rda", compress = "xz", version = 2)
