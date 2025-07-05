library("RSQLite")
library(tidyverse)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="/home/atulya/Desktop/MTH208/mth-208-course-project-group24/Data/data.db")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

companies2 <- as.tibble(lDataFrames[[2]])
students <- as.tibble(lDataFrames[[3]])

save(companies2,file = "companies.Rdata")
save(students,file = "students.Rdata")









