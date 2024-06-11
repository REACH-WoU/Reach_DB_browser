library(odbc)
source('www/src/stuff.R')
# my_connection <- dbConnect(odbc::odbc(), 
#                            Driver = "SQL Server", 
#                            server = Sys.getenv('name1'), 
#                            database = Sys.getenv('name2'), 
#                            uid = Sys.getenv('name3'), 
#                            pwd = Sys.getenv('name4'), 
#                            Port = 1433)


my_connection <- if(Sys.info()[['user']]== 'shiny'){
    dbConnect(odbc::odbc(),
                   Driver   = "FreeTDS",
                   Database = Sys.getenv('name2'),
                   Uid      = Sys.getenv('name3'),
                   Pwd      = Sys.getenv('name4'),
                   Server   = Sys.getenv('name1'),
                   Port     = 1433,
                   TDS_Version=8.0
    )
  }else{
    dbConnect(odbc::odbc(), 
                   driver = "SQL Server",
                   server = Sys.getenv('name1'), 
                   database = Sys.getenv('name2'), 
                   uid = Sys.getenv('name3'), 
                   pwd = Sys.getenv('name4')
    )
  }
