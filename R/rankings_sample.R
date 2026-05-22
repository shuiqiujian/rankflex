#' Sample university ranking dataset
#'
#' A cleaned and merged dataset of university rankings from multiple sources
#' (QS, THE, ARWU) covering 2023-2025. Each row represents one university
#' from one source in one year.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{entity}{University name}
#'   \item{year}{Year of the ranking (2023, 2024, or 2025)}
#'   \item{source}{Ranking source: "QS", "THE", or "ARWU"}
#'   \item{rank}{Numeric rank (integer)}
#'   \item{score_overall}{Overall score from the source (0-100)}
#'   \item{score_academic_reputation}{QS: Academic reputation score}
#'   \item{score_employer_reputation}{QS: Employer reputation score}
#'   \item{score_faculty_student}{QS: Faculty-to-student ratio score}
#'   \item{score_citations_faculty}{QS: Citations per faculty score}
#'   \item{score_intl_faculty}{QS: International faculty score}
#'   \item{score_intl_students}{QS: International students score}
#'   \item{score_intl_research_network}{QS: International research network score}
#'   \item{score_employment_outcomes}{QS: Employment outcomes score}
#'   \item{score_sustainability}{QS: Sustainability score (2024-2025 only)}
#'   \item{score_teaching}{THE: Teaching score}
#'   \item{score_research_environment}{THE: Research environment score}
#'   \item{score_research_quality}{THE: Research quality score}
#'   \item{score_industry}{THE: Industry impact score}
#'   \item{score_intl_outlook}{THE: International outlook score}
#'   \item{score_alumni}{ARWU: Alumni winning major awards score}
#'   \item{score_award}{ARWU: Staff winning major awards score}
#'   \item{score_hici}{ARWU: Highly cited researchers score}
#'   \item{score_ns}{ARWU: Nature and Science papers score}
#'   \item{score_pub}{ARWU: Publications score}
#'   \item{score_pcp}{ARWU: Per capita academic performance score}
#' }
#'
#' @note Columns from sources other than the row's own \code{source} will be
#'   \code{NA}. For example, \code{score_teaching} is only populated for THE rows.
#'
#' @source
#' QS World University Rankings (2023-2025), Times Higher Education World
#' University Rankings (2023-2025), Academic Ranking of World Universities
#' (ARWU, 2023-2025).
"rankings_sample"
