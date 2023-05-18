#' Get the variables used to determine student type
#'
#' @inheritParams   get_data_from_sql_query
#' @return   A cleaned-up data.frame containing the student-type-determination-variables dataset.
#'
#' @export

get_student_type_variables <- function(dsn = "***REMOVED***") {
  sql_query <- get_query_string("student_type_determination_variables")

  get_data_from_sql_query(sql_query, dsn = dsn)
}

#' Determine the different student types (High school, Continuing Graduate ...)
#'
#' @param   student_type_determination_variables   Data-frame. Contains all variables required to
#' determine the type of each student.
#' @param   parameter_term,term_two_terms_ago   Term
#'
#' @importFrom   rlang   .data
#' @export

calculate_student_types <- function(student_type_determination_variables,
                                    parameter_term,
                                    term_two_terms_ago) {
  return_df <- student_type_determination_variables %>%
    dplyr::mutate(calculated_student_type = dplyr::case_when(
      # GROUP: High School ####
      # if the calculated high school graduation term is greater than the passed in parameter term,
      # then the student is a high school student on the provided parameter term
      # if calculated_high_school_graduation_term > parameter_term
      #    return "High School"
      .data[["calculated_high_school_graduation_term"]] > parameter_term
      ~ "High School",

      # GROUP Graduate ####
      # first_term_enrolled and last_term_enrolled only looking at GRADUATE LEVEL transcript
      # records for local institution

      ## Continuing Graduate ####
      # if last_term_enrolled_as_graduate >= (parameter_term - two_terms)
      #    return "Continuing Graduate"
      (.data[["last_term_enrolled_on_or_after_calculated_hs_graduation_term"]] >=
        term_two_terms_ago) &
        .data[["student_level"]] == "GR"
      ~ "Continuing Graduate",

      ## Readmit Graduate ####
      # if last_term_enrolled_as_graduate < (parameter_term - two_terms)
      #    return "Readmit Graduate"
      (.data[["last_term_enrolled_on_or_after_calculated_hs_graduation_term"]] <
        term_two_terms_ago) &
        .data[["student_level"]] == "GR"
      ~ "Readmit Graduate",

      ## Transfer Graduate ####
      # if has_transfer_graduate_credits & ( first_term_enrolled_as_graduate == parameter_term )
      #    return "Transfer Graduate"
      .data[["has_transfer_credits_on_or_after_calculated_hs_graduation_term"]] &
        (.data[["first_term_enrolled_on_or_after_calculated_hs_graduation_term"]] ==
          parameter_term) &
        .data[["student_level"]] == "GR" &
        .data[["transfer_credits_level"]] == "GR"
      ~ "Transfer Graduate",

      ## New Graduate ####
      # if first_term_enrolled_as_graduate == parameter_term:
      #    return "New Graduate"
      .data[["first_term_enrolled_on_or_after_calculated_hs_graduation_term"]] == parameter_term &
        .data[["student_level"]] == "GR"
      ~ "New Graduate",

      # GROUP Undergraduate ####
      # first_term_enrolled, last_term_enrolled after calculated_high_school_graduation_date
      # and only looking at UNDERGRADUATE LEVEL transcript records for local institution.

      ## Continuing ####
      # if last_term_enrolled_as_undergraduate_after_hs_grad >= (parameter_term - two_terms):
      #  return "Continuing Undergraduate"
      (.data[["last_term_enrolled_on_or_after_calculated_hs_graduation_term"]] >=
        term_two_terms_ago) &
        .data[["student_level"]] == "UG"
      ~ "Continuing Undergraduate",

      ## Readmit ####
      # if last_term_enrolled_as_undergraduate_after_hs_grad < (parameter_term - two_terms):
      #  return "Readmit Undergraduate"
      (.data[["last_term_enrolled_on_or_after_calculated_hs_graduation_term"]] <
        term_two_terms_ago) &
        .data[["student_level"]] == "UG"
      ~ "Readmit Undergraduate",

      ## Transfer ####
      # has_transfer_undergraduate_credits after calculated_high_school_graduation_date.
      # if has_transfer_undergraduate_credits_after_hs_grad
      #   AND (first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term):
      #  return "Transfer Undergraduate"
      .data[["has_transfer_credits_on_or_after_calculated_hs_graduation_term"]] &
        (.data[["first_term_enrolled_on_or_after_calculated_hs_graduation_term"]] ==
          parameter_term) &
        .data[["student_level"]] == "UG" &
        .data[["transfer_credits_level"]] == "UG"
      ~ "Transfer Undergraduate",

      ## Freshman ####
      # if first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term:
      #  return "Freshman"
      .data[["first_term_enrolled_on_or_after_calculated_hs_graduation_term"]] == parameter_term &
        .data[["student_level"]] == "UG"
      ~ "Freshman",

      ## Unknown ####
      # else:
      # return "Unknown"
      TRUE
      ~ "Unknown"
    ), ) %>%
    # set term for what is used in calculations
    dplyr::mutate(term_id = parameter_term) %>%
    # only return relevant variables from calculation
    dplyr::select("sis_system_id", "term_id", "calculated_student_type")

  return(return_df)
}
