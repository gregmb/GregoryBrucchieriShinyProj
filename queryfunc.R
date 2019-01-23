dbConnector<-function(session, dbname){
    require(RSQLite)
    ##setup connection to database
    conn <- dbConnect(drv = SQLite(), dbname = dbname)
    ##disconnect on session end
    session$onSessionEnded(function(){
        dbDisconnect(conn)
    })
    ##return connection
    conn
}

#Query database to create data table
dbGetData <- function(conn, tblname, slect, borough, params){
    #create query
    query <- paste('SELECT', slect, 'FROM', tblname, paste0('WHERE BOROUGH = "', borough, '"'))
    if(params !='') {
      query <- paste(query, 'AND', params)
    }
    #create data table
    as.data.table(dbGetQuery(conn = conn, statement = query))
}
