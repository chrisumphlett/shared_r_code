md1 <- "select * from dev.qualtrics.responses_metadata"
md2 <- "select * from dev.qualtrics.question_metadata"
md3 <- "select * from dev.qualtrics.survey_metadata"
resp_md <- query_sql_server(db_server, db_name, md1)
question_md <- query_sql_server(db_server, db_name, md2)
survey_md <- query_sql_server(db_server, db_name, md3)

rm(md1, md2, md3)


get_survey_resp <- function(survey_id) {
  resp_qry <- paste0("select * from dev.qualtrics.", survey_id)
  survey_resp <- query_sql_server(db_server, db_name, resp_qry)
  return(survey_resp)
}