rm(list=ls())
if(!require(proftools)) install.packages('proftools')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(lubridate)) install.packages('lubridate')
if(!require(influxdbr)) install.packages('influxdbr')

#### DEMO
pd <- readProfileData(system.file("samples", "glmEx.out", package="proftools"))
flameGraph(pd)
calleeTreeMap(pd)

#### QUERY INFLUXDB
## conexión remota
host <- ## "gblabs.co"
        "aqa.unloquer.org"
        ## "aireciudadano.servehttp.com"
db <-   ## "canairio"
        "aqa"
        ## "ENVdataDB"
con <- influx_connection(scheme = c("http", "https"), host = host,port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

measurements <- unlist( show_measurements(con = con,
                                          db = db
                                          ))

sensores <- function(db, measurements) {
    paste0("\"",db,"\"",".autogen.","\"",measurements,"\"",collapse=",")
}

sensores_ts <- function(db, measurements) {
    map(measurements, function(ms) paste0("\"",db,"\"",".autogen.","\"",ms,"\"",collapse=","))
}

## "aqa"."autogen"."volker0004"
query_time_series <- "SELECT mean(\"pm25\") AS \"pm25\" FROM %s WHERE time > now() - 1h GROUP BY time(10s) FILL(none)"
query_1h_mean <- "SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s WHERE time > now() - 1h GROUP BY time(1h) FILL(none) LIMIT 1"

query <- function(snsrs, type = "ts") {
    if(type == "1h") sprintf(query_1h_mean, snsrs)
    else sprintf(query_time_series, snsrs)
}

data <- influx_query(con, db = db, query = query(sensores(db,measurements),"1h"),timestamp_format = c("n", "u", "ms", "s", "m", "h"), return_xts = FALSE)[[1]]

## add ICApm25 colors
data$color <- (lapply(data$pm25, function(x)(
  ifelse(x < 12 , "green",
  ifelse(x < 35 && x >= 12 , "gold",
  ifelse( x < 55 && x >= 35, "orange",
  ifelse( x < 150 && x >= 55, "red",
  ifelse( x < 250 && x >= 150, "purple",
         "maroon"))))))) %>% enframe %>% unnest)$value

## write_tsv(x,"/tmp/aireciudadano.txt")

#### PREPARE THE STACKS TO BE GRAPHED
## pm2.5 measures
x <- list()
x$counts <- as.integer(data$pm25)
boliv <- paste0("b_", as.character(x$counts[1]))
parq <- paste0("parq_", as.character(x$counts[2]))
fatim <- paste0("fat_", as.character(x$counts[3]))
molin <- paste0("molin_", as.character(x$counts[4]))
mont <- paste0("mont_", as.character(x$counts[5]))
pila <- paste0("pila_", as.character(x$counts[6]))
lad <- paste0("lad_", as.character(x$counts[7]))
se <- paste0("se_", as.character(x$counts[8]))
la80 <- paste0("la80_", as.character(x$counts[9]))
c3p <- paste0("c3p_", as.character(x$counts[10]))
## sensor names and measures
o1 <- list(c("valle"   ,"med","belen", boliv ))
o2 <- list(c("valle"   ,"med","conquista",parq ))
o3 <- list(c("valle"   ,"med","conquista", fatim))
o4 <- list(c("valle"   ,"med","belen", molin))
o5 <- list(c("valle"   ,"med","sanL", mont))
o6 <- list(c("valle"   ,"med","pilarica", pila))
o7 <- list(c("valle"   ,"med","laLadera", lad))
o8<- list(c("staElena","staElena","staElena", se))
o9 <- list(c("valle"   ,"med","calazans", la80))
#o10 <- list(c("valle"  ,"med","prad", c3p))
x$stacks <- cbind(o1,o2,o3,o4,o5,o6,o7,o8,o9)
flameGraph(x)

