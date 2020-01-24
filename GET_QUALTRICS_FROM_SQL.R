md1 <- "select * from dev.qualtrics.response_metadata"
md2 <- "select * from dev.qualtrics.question_metadata"
md3 <- "select * from dev.qualtrics.survey_metadata"
resp_md <- query_sql_server(db_server, db_name, md1)
question_md <- query_sql_server(db_server, db_name, md2)
survey_md <- query_sql_server(db_server, db_name, md3)

rm(md1, md2, md3)


get_survey_resp <- function(chosen_survey_id) {
  resp_md2 <- resp_md %>%
    select(response_id, startdate)
  question_md2 <- question_md %>%
    select(question_name, survey_id, question_text, question_type, response_type, response_choices) %>%
    distinct()
  resp_qry <- paste0("select * from dev.qualtrics.", chosen_survey_id)
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
  
  survey_title <- survey_md %>% filter(survey_id == chosen_survey_id) %>% select(survey_name) %>% pull()
  
  # survey overview
  print(paste0("Survey title: ", survey_title))
  print(paste0("First response: ", min(survey_resp$survey_start_dt)))
  print(paste0("Last response: ", max(survey_resp$survey_start_dt)))
  
  # distinct questions and type
  qs <- survey_resp %>% select(question_name, question_type, response_type, question_text) %>% distinct()
  
  # eliminate questions that are not real questions
  survey_resp2 <- survey_resp %>%
    filter(
      !response_type %in% c("TB", "PTB", "ESTB"), # Text instructions we give to the respondent
      !substr(question_name, 1, 4) %in% c("DO-Q", "RO-B"), # Display order information - Qualtrics records actual order shown to the respondent
      question_type != "Meta" # Browser info, this is stored in the response metadata
    )
  
  a <- survey_resp2 %>% select(question_name, question_type, response_type, question_text) %>% distinct()
  
  # separate into 3 different types: single answer, multiple answer, and verbatim
  single_resp <- survey_resp2 %>%
    filter(question_type %in% c("MC") & response_type %in% c("SAVR", "SAHR", "NPS", "DL", "SACOL") & str_detect(question_id, "_TEXT") == FALSE)
  single_resp2 <- single_resp %>%
    mutate(new_name = paste0(question_id, "-", question_text)) %>%
    select(response_id, new_name, response) %>%
    pivot_wider(names_from = new_name, values_from = response) %>%
    janitor::clean_names()
    
  mult_resp <- survey_resp2 %>%
    filter(question_type %in% c("MC") & response_type %in% c("MAVR", "MAHR", "MACOL") & str_detect(question_id, "_TEXT") == FALSE) 
  mult_resp2 <- mult_resp %>%
    mutate(response_num = as.numeric(word(question_id, 2, sep = fixed("_"))),
           choice = word(response_choices, response_num, sep = fixed(" | ")),
           new_name = paste0(question_id, "-", choice),
           response2 = if_else(is.na(response), 0, 1)) %>%
    select(response_id, new_name, response2) %>%
    pivot_wider(names_from = new_name, values_from = response2) %>%
    janitor::clean_names()
  
  verbatim_resp <- survey_resp2 %>%
    filter(question_type %in% c("TE") | str_detect(question_id, "_TEXT") == TRUE)
  verbatim_resp2 <- verbatim_resp %>%
    mutate(new_name = paste0(question_id, "-", question_text)) %>%
    select(response_id, new_name, response) %>%
    pivot_wider(names_from = new_name, values_from = response) %>%
    janitor::clean_names()
  
  # these are not friendly to what we have in our architecture... don't have the question text. 
  ## they are "nested" in that there are multiple questions listed under a single header
  nested_resp <- survey_resp2 %>%
    filter(question_type %in% c("Matrix", "CS")  & str_detect(question_id, "_TEXT") == FALSE)
  nested_resp2 <- nested_resp %>%
    # mutate(question_num = as.numeric(word(question_id, 2, sep = fixed("_"))),
    #        choice = word(response_choices, response_num, sep = fixed(" | ")),
    #        new_name = paste0(question_id, "-", choice),
    #        response2 = if_else(is.na(response), 0, 1)) %>%
    select(response_id, question_id, response) %>%
    pivot_wider(names_from = question_id, values_from = response) %>%
    janitor::clean_names()
  
  # verify same # of rows
  samerows <- nrow(survey_resp2) == (nrow(single_resp) + nrow(mult_resp) + nrow(verbatim_resp) + nrow(nested_resp))
  print(paste0("T/F Do we have same # of rows after splitting by question/response type? ", samerows))
  print("If not, need to update the filters to include new question/response types, OR, there are multiple questions with the same name")
  
  print(nrow(survey_resp2))
  print(nrow(single_resp) + nrow(mult_resp) + nrow(verbatim_resp) + nrow(nested_resp))
  
  result_list <- list(single_resp2, mult_resp2, verbatim_resp2, nested_resp2, a)

  return(result_list)
}

# 
# # check for new question/response types
# a<-as.data.frame(result_list[5])
