# Opt-in integration tests. Set RSQLCLIENTNET_TEST_CONNECTION_STRING to a test
# instance only; never use on clinical production systems.
library(RSqlClientNET)
cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
if (nzchar(cs)) {
  for (transport in "rcpp") {
    con <- sql_connect(connection_string=cs, transport=transport)
    tryCatch({
      x <- sql_query(con, "SELECT CAST(1 AS int) AS id",
                     result_format="csv", precision="native")
      stopifnot(identical(x$id,1L), sql_is_valid(con))
      # A disposable local temporary table; not a clinical/production table.
      sql_execute(con, "CREATE TABLE #sqlclient024_tx (id int NOT NULL)")
      sql_begin(con)
      sql_execute(con, "INSERT INTO #sqlclient024_tx (id) VALUES (1)")
      sql_rollback(con)
      zero <- sql_query(con, "SELECT CAST(COUNT(*) AS int) AS n FROM #sqlclient024_tx",
                        result_format="csv")
      stopifnot(identical(zero$n[1], 0L))
      sql_begin(con)
      sql_execute(con, "INSERT INTO #sqlclient024_tx (id) VALUES (2)")
      sql_commit(con)
      one <- sql_query(con, "SELECT CAST(COUNT(*) AS int) AS n FROM #sqlclient024_tx",
                       result_format="csv")
      stopifnot(identical(one$n[1], 1L))
      # An ordinary SQL syntax error must NOT invalidate the physical session.
      err <- tryCatch(sql_query(con, "SELEC id FROM #sqlclient024_tx", timeout=10),
                      error=function(e) conditionMessage(e))
      stopifnot(is.character(err), sql_is_valid(con))
      intact <- sql_query(con, "SELECT CAST(COUNT(*) AS int) AS n FROM #sqlclient024_tx",
                          result_format="csv")
      stopifnot(identical(intact$n[[1L]], 1L))
    }, finally=sql_disconnect(con))
    # Explicit reconnect remains available, but is not required for syntax errors.
    con <- sql_connect(connection_string=cs, transport=transport)
    tryCatch(stopifnot(sql_is_valid(con),
                       sql_ping(con)$ok), finally=sql_disconnect(con))
  }
  message("PASS: SQL syntax error recovery, #temp persistence and fresh connection")
} else {
  message("SKIP optional SQL Server integration; set RSQLCLIENTNET_TEST_CONNECTION_STRING")
}
