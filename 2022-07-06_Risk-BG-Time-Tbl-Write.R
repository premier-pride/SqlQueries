library(pool)
library(tidyverse)
library(RMySQL)
library(lubridate)


loadData <- function(databaseName,table) {
    # Connect to the database
    db <- dbPool(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                 port = options()$mysql$port, user = options()$mysql$user, 
                 password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    poolClose(db)
    data
}

options(mysql = list(
    "user" = Sys.getenv('MUFASA_USER'),
    "password" = Sys.getenv('MUFASA_PASSWORD'),
    "host" = "mufasa.cxcfcs0uwqfn.us-east-2.rds.amazonaws.com",
    "port" = 3306
))

statusData <- loadData('staffing-data','Risk_BackgroundStatusChange')


test <- statusData |> 
    group_by(emp_id) |> 
    filter(emp_id != '') |> 
    arrange(emp_id,timestamp) |> 
    mutate(timestamp = as_datetime(timestamp),
           last_time = lag(timestamp,n = 1)) |> 
    ungroup() |> 
    mutate(min = (timestamp - last_time),
           stage_length = seconds_to_period(min),
           sec = as.integer(min)) |> 
    filter(!is.na(sec)) |> 
    group_by(emp_id) |> 
    arrange(emp_id,timestamp) |> 
    mutate(total_length = cumsum(sec)) |> 
    filter(new_status == "Complete") |> 
    ungroup() |> 
    mutate(overall_length = seconds_to_period(total_length)) |> 
    select(id:emp_id,total_length,overall_length)
    

db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
             port = options()$mysql$port, user = options()$mysql$user,
             password = options()$mysql$password)

dbWriteTable(db,'Risk_BGOverallTurnaroundTimes',
             row.names = FALSE,
             test,overwrite = TRUE, append = FALSE,
             field.types = c('id'= 'bigint',
                             'timestamp' = 'datetime',
                             'emp_id' = 'bigint',
                             'total_length' = 'bigint',
                             'overall_length' = 'text'))
poolClose(db)


