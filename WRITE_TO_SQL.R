write_df_to_sql_append <- function(db_server, db_name, schema.tablename, df, vartypes = NULL) {
  conn <- odbcDriverConnect(paste0("driver={SQL Server}; server=", db_server, "; database=", db_name, "; trusted_connection = true"))
  sqlSave(conn, df, tablename = schema.tablename, rownames = FALSE, append = TRUE, varTypes = vartypes)
  odbcClose(conn)
}

write_df_to_sql_overwrite <- function(db_server, db_name, schema.tablename, df, ...) {
  conn <- odbcDriverConnect(paste0("driver={SQL Server}; server=", db_server, "; database=", db_name, "; trusted_connection = true"))
  sqlDrop(conn, schema.tablename) 
  sqlSave(conn, df, tablename = schema.tablename, rownames = FALSE, append = FALSE)
  odbcClose(conn)
}