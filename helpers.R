
dbConnector <- function(session, dbname) {
  require(RSQLite)
  
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}


dbGetVenues <- function(conn, tblname) {
  query_venues <- paste("SELECT * FROM", tblname)
  
  as.data.frame(dbGetQuery(conn = conn,
                           statement = query_venues))
}


dbGetCoins <- function(conn, tblname) {
  query_coins <- paste("SELECT c.*, date(c.date) as date2 FROM", tblname, 'c')
  #query_coins <- paste("SELECT * FROM", tblname)
  
  as.data.frame(dbGetQuery(conn = conn,
                           statement = query_coins))
}




