require(influxdbr)

con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",
                         port = 8086, group = NULL, verbose = FALSE,
                         config_file = "~/.influxdb.cnf")

points <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"mean_pm25\", mean(\"lat\") AS \"mean_lat\", mean(\"lng\") AS \"mean_lng\" FROM \"aqa\".\"autogen\".\"volker0001\" WHERE time > now() - 1h GROUP BY time(1h)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
points <- as.data.frame(points)

## quita NA
points <- points[complete.cases(points), ]

## change range between 0 and 1
x <- points$mean_pm25
newMax <- 100
newMin <- 0
oldMin <- 0
oldMax <- 500
y <- unlist(lapply(x, function(x)(((x-oldMin)*newMax)/oldMax)+newMin))
print(y)

## write file
write(y, file="pm25.txt", ncolumns = 1)

