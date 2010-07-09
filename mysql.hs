module TestMysql () where
  import Control.Monad
  import Database.HDBC
  import Database.HDBC.MySQL

  main = do conn <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost     = "localhost",
                        mysqlUser     = "root",
                        mysqlPassword = "doc1291",
                        mysqlDatabase = "cltc_cms_development",
                        mysqlUnixSocket = "/tmp/mysql.sock"
                      }
            rows <- quickQuery conn "SELECT id, cltc_number FROM clients LIMIT 10" []
            forM_ rows $ putStrLn . show
