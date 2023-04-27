get_summarized_faculty_workload_df <- function(instructional_faculty_workload_df,
                                               non_instructional_faculty_workload_df,
                                               time_aggregator) {
  # options for time_aggregator are:
  # 1. "term"
  # 2. "academic_year"
  grouping <- c("faculty_info", "sis_id", "full_name", time_aggregator)

  # Summarize instructional workload.
  summarized_instructional_faculty_workload_df <- instructional_faculty_workload_df %>%
    dplyr::mutate(contracted_workload = tidyr::replace_na(contracted_workload, 0)) %>%
    dplyr::group_by_at(grouping) %>%
    dplyr::summarize(
      credit_hours = sum(course_credit_hours),
      students = sum(course_student_count),
      student_credit_hours = sum(course_student_credit_hours),
      fte = sum(course_fte),
      instructional_workload = sum(course_workload),
      # For courses_taught we count distinct course_crn if the course is NOT a cross listed course,
      # otherwise we count all cross listed course sections as one single course.
      courses_taught = (
        dplyr::n_distinct(course_crn[is.na(course_cross_list_group)]) +
          dplyr::n_distinct(course_cross_list_group[!is.na(course_cross_list_group)])
      ),
      faculty_status = dplyr::first(na.omit(faculty_status)),
      faculty_rank = dplyr::first(na.omit(faculty_rank)),
      faculty_college = dplyr::first(na.omit(faculty_college)),
      faculty_department = dplyr::first(na.omit(faculty_department)),
      contracted_workload = min(contracted_workload)
    ) %>%
    dplyr::ungroup()

  # Summarize non-instructional workload.
  summarized_non_instructional_faculty_workload_df <- non_instructional_faculty_workload_df %>%
    dplyr::mutate(contracted_workload = tidyr::replace_na(contracted_workload, 0)) %>%
    dplyr::group_by_at(grouping) %>%
    dplyr::summarize(
      non_instructional_workload = sum(non_instructional_workload),
      faculty_status = dplyr::first(na.omit(faculty_status)),
      faculty_rank = dplyr::first(na.omit(faculty_rank)),
      faculty_college = dplyr::first(na.omit(faculty_college)),
      faculty_department = dplyr::first(na.omit(faculty_department)),
      contracted_workload = min(contracted_workload)
    ) %>%
    dplyr::ungroup()

  # full join instructional and non-instructional workload data frames
  workload_exploration_df <- dplyr::full_join(
    x = summarized_instructional_faculty_workload_df,
    y = summarized_non_instructional_faculty_workload_df,
    by = grouping
  ) %>%
    dplyr::mutate(
      faculty_status = dplyr::coalesce(faculty_status.x, faculty_status.y),
      faculty_rank = dplyr::coalesce(faculty_rank.x, faculty_rank.y),
      faculty_college = dplyr::coalesce(faculty_college.x, faculty_college.y),
      faculty_department = dplyr::coalesce(faculty_department.x, faculty_department.y),
      contracted_workload = dplyr::coalesce(contracted_workload.x, contracted_workload.y)
    ) %>%
    dplyr::select(-c(
      faculty_status.x, faculty_status.y,
      faculty_rank.x, faculty_rank.y,
      faculty_college.x, faculty_college.y,
      faculty_department.x, faculty_department.y,
      contracted_workload.x, contracted_workload.y
    )) %>%
    dplyr::mutate(
      instructional_workload = tidyr::replace_na(instructional_workload, 0),
      non_instructional_workload = tidyr::replace_na(non_instructional_workload, 0),
      contracted_workload = tidyr::replace_na(contracted_workload, 0)
    ) %>%
    # Final workload and overload calculations. ####
    dplyr::mutate(
      total_workload = instructional_workload + non_instructional_workload,
      overload = dplyr::case_when(
        contracted_workload == 0 ~ 0,
        TRUE ~ total_workload - contracted_workload
      )
    ) %>%
    # set the order of columns
    dplyr::relocate(c(
      "faculty_info", "sis_id", "full_name", time_aggregator,
      "faculty_college", "faculty_department", "faculty_status", "faculty_rank",
      "instructional_workload", "non_instructional_workload", "total_workload",
      "contracted_workload", "overload",
      "courses_taught", "credit_hours", "students", "student_credit_hours", "fte"
    ))

  return(workload_exploration_df)
}
