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
    select(question_name, survey_id, question_text, question_type, response_type, response_choices) %>%
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

prep_response_data <- function(chosen_survey_id) {
  
  survey_resp <- get_survey_resp(chosen_survey_id)
  
  resp_md1 <- resp_md %>% filter(survey_id == chosen_survey_id) 
  
  question_md1 <- question_md %>% filter(survey_id == chosen_survey_id)
  
  # survey overview
  print(paste0("Total survey responses = ", nrow(survey_resp)))
  print(paste0("First response: ", min(survey_resp$survey_start_dt)))
  print(paste0("Last response: ", max(survey_resp$survey_start_dt)))
  
  # distinct questions and type
  qs <- survey_resp %>% select(question_name, question_type, response_type, question_text) %>% distinct()
  
  # eliminate questions that are not real questions
  survey_resp2 <- survey_resp %>%
    filter(
      !response_type %in% c("TB", "PTB"), # Text instructions we give to the respondent
      substr(question_name, 1, 4) != "DO-Q", # Display order information - Qualtrics records actual order shown to the respondent
      question_type != "Meta" # Browser info, this is stored in the response metadata
    )
  
  # separate into 3 different types: single answer, multiple answer, and verbatim
  single <- survey_resp2 %>%
    filter(question_type %in% c("MC") & response_type %in% c("SAVR", "SAHR"))
  mult <- survey_resp2 %>%
    filter(question_type %in% c("MC") & response_type %in% c("MAVR", "MAHR"))
  verb <- survey_resp2 %>%
    filter(question_type %in% c("TE"))
  
  ## should be same # of rows
  samerows <- nrow(survey_resp2) == (nrow(single) + nrow(mult) + nrow(verb))
  print(paste0("Does total rows responses split by type = total original rows? Should be TRUE:", " - ", samerows))
  
  
  mult2 <- mult %>% filter(row_number() < 500) %>%
    mutate(response_num = as.numeric(word(question_id, 2, sep = fixed("_"))),
           choice = word(response_choices, response_num, sep = fixed(" | ")),
           new_name = paste0(question_id, "-", choice),
           response2 = if_else(is.na(response), 0, 1)) %>%
    select(response_id, new_name, response2) %>%
    pivot_wider(names_from = new_name, values_from = response2) %>%
    janitor::clean_names()
}
