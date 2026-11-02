testthat::test_that("UNIQUEIDENTIFIER and DATETIMEOFFSET text bulk writes round-trip", {
  .rsqlclientnet_skip_no_sql()
  conn <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"), tz = "UTC"
  )
  on.exit(if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn), add = TRUE)
  tab <- "#sbin_guid_offset_03438"
  data <- data.frame(
    id = 1:4,
    gid = c("00000000-0000-4000-8000-000000000001", NA_character_,
            "00112233-4455-6677-8899-aabbccddeeff",
            "00000000-0000-4000-8000-000000000004"),
    happened = c("2024-05-01T12:34:56.1234567+02:00", NA_character_,
                 "2024-05-02T05:00:00.0000000-05:00",
                 "2024-05-03T22:00:00.0000000+00:00")
  )
  testthat::expect_true(DBI::dbWriteTable(conn, tab, data, batch_rows = 2L,
    field.types = c(id = "INT", gid = "UNIQUEIDENTIFIER",
                    happened = "DATETIMEOFFSET(7)")))
  query <- paste("SELECT id, CAST(gid AS varchar(36)) AS gid,",
                 "DATEPART(TZOFFSET,happened) AS offset_minutes,",
                 "CASE WHEN happened IS NULL THEN 1 ELSE 0 END AS null_time",
                 "FROM", tab, "ORDER BY id")
  out <- DBI::dbGetQuery(conn, query)
  testthat::expect_identical(nrow(out), 4L)
  # SQL Server may format the hex digits in UNIQUEIDENTIFIER as uppercase.
  # UUIDs identify the same value irrespective of letter case.
  testthat::expect_identical(tolower(as.character(out$gid[c(1L, 3L, 4L)])),
    tolower(data$gid[c(1L, 3L, 4L)]))
  testthat::expect_true(is.na(out$gid[2L]))
  testthat::expect_equal(out$offset_minutes[c(1L, 3L, 4L)], c(120, -300, 0))
  testthat::expect_equal(out$null_time, c(0, 1, 0, 0))
  testthat::expect_true(DBI::dbIsValid(conn))

  testthat::expect_equal(DBI::dbAppendTable(conn, tab, data.frame(
    id = 5L, gid = "00000000-0000-4000-8000-000000000005",
    happened = "2024-05-04T01:02:03.1234567+03:00")), 1)
  testthat::expect_equal(DBI::dbGetQuery(conn,
    paste("SELECT DATEPART(TZOFFSET,happened) AS offset_minutes FROM", tab,
          "WHERE id=5"))$offset_minutes, 180)
  testthat::expect_true(DBI::dbRemoveTable(conn, tab))
  testthat::expect_true(DBI::dbIsValid(conn))
})
