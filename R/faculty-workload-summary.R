get_summarized_faculty_workload_df <- function(instructional_faculty_workload_df,
                                               non_instructional_faculty_workload_df,
                                               time_aggregator) {
  # options for time_aggregator are:
  # 1. "term"
  # 2. "academic_year"
  grouping <- c("faculty_info", "sis_id", "full_name", time_aggregator)

  # Summarize instructional workload.
  summarized_instructional_faculty_workload_df <- instructional_faculty_workload_df %>%
    dplyr::mutate(contracted_workload = tidyr::replace_na(.data[["contracted_workload"]], 0)) %>%
    dplyr::group_by_at(grouping) %>%
    dplyr::summarize(
      credit_hours = sum(.data[["course_credit_hours"]]),
      students = sum(.data[["course_student_count"]]),
      student_credit_hours = sum(.data[["course_student_credit_hours"]]),
      fte = sum(.data[["course_fte"]]),
      instructional_workload = sum(.data[["course_workload"]]),
      # For courses_taught we count distinct course_crn if the course is NOT a cross listed course,
      # otherwise we count all cross listed course sections as one single course.
      courses_taught = (
        dplyr::n_distinct(
          .data[["course_crn"]][is.na(.data[["course_cross_list_group"]])]
        ) +
          dplyr::n_distinct(
            .data[["course_cross_list_group"]][!is.na(.data[["course_cross_list_group"]])]
          )
      ),
      faculty_status = dplyr::first(na.omit(.data[["faculty_status"]])),
      faculty_rank = dplyr::first(na.omit(.data[["faculty_rank"]])),
      faculty_college = dplyr::first(na.omit(.data[["faculty_college"]])),
      faculty_department = dplyr::first(na.omit(.data[["faculty_department"]])),
      contracted_workload = min(.data[["contracted_workload"]])
    ) %>%
    dplyr::ungroup()

  # Summarize non-instructional workload.
  summarized_non_instructional_faculty_workload_df <- non_instructional_faculty_workload_df %>%
    dplyr::mutate(contracted_workload = tidyr::replace_na(.data[["contracted_workload"]], 0)) %>%
    dplyr::group_by_at(grouping) %>%
    dplyr::summarize(
      non_instructional_workload = sum(.data[["non_instructional_workload"]]),
      faculty_status = dplyr::first(na.omit(.data[["faculty_status"]])),
      faculty_rank = dplyr::first(na.omit(.data[["faculty_rank"]])),
      faculty_college = dplyr::first(na.omit(.data[["faculty_college"]])),
      faculty_department = dplyr::first(na.omit(.data[["faculty_department"]])),
      contracted_workload = min(.data[["contracted_workload"]])
    ) %>%
    dplyr::ungroup()

  # full join instructional and non-instructional workload data frames
  workload_exploration_df <- dplyr::full_join(
    x = summarized_instructional_faculty_workload_df,
    y = summarized_non_instructional_faculty_workload_df,
    by = grouping
  ) %>%
    dplyr::mutate(
      faculty_status = dplyr::coalesce(.data[["faculty_status.x"]], .data[["faculty_status.y"]]),
      faculty_rank = dplyr::coalesce(.data[["faculty_rank.x"]], .data[["faculty_rank.y"]]),
      faculty_college = dplyr::coalesce(
        .data[["faculty_college.x"]], .data[["faculty_college.y"]]
      ),
      faculty_department = dplyr::coalesce(
        .data[["faculty_department.x"]], .data[["faculty_department.y"]]
      ),
      contracted_workload = dplyr::coalesce(
        .data[["contracted_workload.x"]], .data[["contracted_workload.y"]]
      )
    ) %>%
    dplyr::select(-c(
      .data[["faculty_status.x"]], .data[["faculty_status.y"]],
      .data[["faculty_rank.x"]], .data[["faculty_rank.y"]],
      .data[["faculty_college.x"]], .data[["faculty_college.y"]],
      .data[["faculty_department.x"]], .data[["faculty_department.y"]],
      .data[["contracted_workload.x"]], .data[["contracted_workload.y"]]
    )) %>%
    dplyr::mutate(
      instructional_workload = tidyr::replace_na(.data[["instructional_workload"]], 0),
      non_instructional_workload = tidyr::replace_na(.data[["non_instructional_workload"]], 0),
      contracted_workload = tidyr::replace_na(.data[["contracted_workload"]], 0)
    ) %>%
    # Final workload and overload calculations. ####
    dplyr::mutate(
      total_workload = .data[["instructional_workload"]] + .data[["non_instructional_workload"]],
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
