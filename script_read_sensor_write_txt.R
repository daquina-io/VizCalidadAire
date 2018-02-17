require(influxdbr)

args <- commandArgs(trailingOnly = TRUE)  ## will use arg[1] for sensor id

con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",
                         port = 8086, group = NULL, verbose = FALSE,
                         config_file = "~/.influxdb.cnf")

points <- influx_query(con, db = "aqa", query =  paste("SELECT mean(\"pm25\") AS \"mean_pm25\", mean(\"lat\") AS \"mean_lat\", mean(\"lng\") AS \"mean_lng\" FROM \"aqa\".\"autogen\".",args[1]," WHERE time > now() - 7d GROUP BY time(10m) FILL(none)"),timestamp_format = c("n", "u", "ms", "s", "m", "h"))

points <- as.data.frame(points)

## quita NA
points <- points[complete.cases(points), ]

## just one value
points <- head(points,1)

## change range between 0 and 1
x <- points$mean_pm25 ## 
# x <- points$mean_pm25 
newMax <- 1
newMin <- 0
oldMin <- 0
oldMax <- 500
y <- unlist(lapply(x, function(x)(((x - oldMin) * newMax) / oldMax) + newMin))
y <- paste0("0,",y)
print(y)

write(y, file= paste(args[1],".txt", sep = ""), ncolumns = 1)
## write file

