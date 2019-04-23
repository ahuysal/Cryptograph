############
###SQLite###
############

#connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)

#Drop table
dbRemoveTable(conn, tbl_venues)
dbRemoveTable(conn, tbl_coins)


#SQLite DB name
dbname = "./cryptograph.sqlite"

#SQLite venues table name
tbl_venues = "venues"

#SQLite coins table name
tbl_coins = "coins"


#Write to tables begin


#write table
dbWriteTable(conn = conn,
             name = tbl_venues,
             value = df_venues)

#write table
dbWriteTable(conn = conn,
             name = tbl_coins,
             value = all_coins)

#List tables
dbListTables(conn)

#Disconnect from DB
dbDisconnect(conn)

#Write to tables ends


#SQLite DB and table definitions


#SQLite DB name
dbname = "../data/cryptograph.sqlite"

#SQLite venues table name
tbl_venues = "venues"

#SQLite coins table name
tbl_coins = "coins"