md1 <- "select * from dev.qualtrics.response_metadata"
md2 <- "select * from dev.qualtrics.question_metadata"
md3 <- "select * from dev.qualtrics.survey_metadata"
resp_md <- query_sql_server(db_server, db_name, md1)
question_md <- query_sql_server(db_server, db_name, md2)
survey_md <- query_sql_server(db_server, db_name, md3)

rm(md1, md2, md3)


get_survey_resp <- function(survey_id) {
  resp_md2 <- resp_md %>%
    select(response_id, startdate)
  question_md2 <- question_md %>%
    select(question_name, survey_id, question_text, question_type, response_type) %>%
    distinct()
  resp_qry <- paste0("select * from dev.qualtrics.", survey_id)
  survey_resp <- query_sql_server(db_server, db_name, resp_qry) %>%
    mutate(question_name = toupper(word(question_id, sep = fixed("_"))),
           question_id = toupper(question_id)) %>%
    left_join(resp_md2) %>%
    left_join(question_md2) %>%
    mutate(survey_start_dt = as.POSIXct(startdate)) %>%
    select(-startdate)
  return(survey_resp)
}
