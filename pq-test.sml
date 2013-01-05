
val c = PgClient.conn "dbname=test user=test password=test"

val _ = PgClient.close c


