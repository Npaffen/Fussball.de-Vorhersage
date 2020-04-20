## How to use RSQLite

require(RSQLite)

# open db connection
db_path <- paste0(getwd(), "/data/test_db") # path to db
db <- dbConnect(SQLite(), db_path) # load connection to db

# list all tables in db
dbListTables(db) 

# create table
data <- data.frame(test_data = rnorm(10,0,1))
dbCreateTable(conn = db, name = "example_table",
              fields = data, overwrite=TRUE)

# save table
dbWriteTable(value = data, conn = db,
             name = "example_table", overwrite=TRUE)

# load table
query <- sprintf("SELECT * FROM %s", "example_table") #  get all from example_table
data <- dbGetQuery(db, query)

# do something to data
data <- data*rnorm(10,0,1)

# save changes
dbWriteTable(value = data, conn = db,
             name = "example_table", overwrite=TRUE)

# close connection
dbDisconnect(db) 

