library(dplyr)

safe_utf8 <- function(x) {
  iconv(x, from = "", to = "UTF-8", sub = "?")
}

clean_rank <- function(x) {
  suppressWarnings(as.integer(gsub("^[^0-9]*(\\d+).*$", "\\1", as.character(x))))
}

qs_2023 <- read.csv("data-raw/qs_2023.csv",
                    encoding = "UTF-8", stringsAsFactors = FALSE)
qs_2024 <- read.csv("data-raw/qs_2024.csv",
                    encoding = "UTF-8", stringsAsFactors = FALSE)
qs_2025 <- read.csv(
  "data-raw/qs_2025.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)
the_raw  <- read.csv("data-raw/THE_2016_to_2026.csv",
                     encoding = "UTF-8", stringsAsFactors = FALSE)
arwu_raw <- read.csv("data-raw/all_years_combined.csv",
                     encoding = "UTF-8", stringsAsFactors = FALSE)


# QS
qs_2023_clean <- qs_2023 %>%
  transmute(
    entity                        = safe_utf8(trimws(institution)),
    year                          = 2023L,
    source                        = "QS",
    rank                          = clean_rank(Rank),
    score_academic_reputation     = as.numeric(ar.score),
    score_employer_reputation     = as.numeric(er.score),
    score_faculty_student         = as.numeric(fsr.score),
    score_citations_faculty       = as.numeric(cpf.score),
    score_intl_faculty            = as.numeric(ifr.score),
    score_intl_students           = as.numeric(isr.score),
    score_intl_research_network   = as.numeric(irn.score),
    score_employment_outcomes     = as.numeric(ger.score),
    score_sustainability          = NA_real_,
    score_overall                 = as.numeric(score.scaled)
  )

qs_2024_clean <- qs_2024 %>%
  transmute(
    entity                        = safe_utf8(trimws(Institution.Name)),
    year                          = 2024L,
    source                        = "QS",
    rank                          = clean_rank(X2024.RANK),
    score_academic_reputation     = as.numeric(Academic.Reputation.Score),
    score_employer_reputation     = as.numeric(Employer.Reputation.Score),
    score_faculty_student         = as.numeric(Faculty.Student.Score),
    score_citations_faculty       = as.numeric(Citations.per.Faculty.Score),
    score_intl_faculty            = as.numeric(International.Faculty.Score),
    score_intl_students           = as.numeric(International.Students.Score),
    score_intl_research_network   = as.numeric(International.Research.Network.Score),
    score_employment_outcomes     = as.numeric(Employment.Outcomes.Score),
    score_sustainability          = as.numeric(Sustainability.Score),
    score_overall                 = as.numeric(Overall.SCORE)
  )

qs_2025_clean <- qs_2025 %>%
  transmute(
    entity                        = safe_utf8(trimws(Institution_Name)),
    year                          = 2025L,
    source                        = "QS",
    rank                          = clean_rank(RANK_2025),
    score_academic_reputation     = as.numeric(Academic_Reputation_Score),
    score_employer_reputation     = as.numeric(Employer_Reputation_Score),
    score_faculty_student         = as.numeric(Faculty_Student_Score),
    score_citations_faculty       = as.numeric(Citations_per_Faculty_Score),
    score_intl_faculty            = as.numeric(International_Faculty_Score),
    score_intl_students           = as.numeric(International_Students_Score),
    score_intl_research_network   = as.numeric(International_Research_Network_Score),
    score_employment_outcomes     = as.numeric(Employment_Outcomes_Score),
    score_sustainability          = as.numeric(Sustainability_Score),
    score_overall                 = as.numeric(Overall_Score)
  )

qs_clean <- bind_rows(qs_2023_clean, qs_2024_clean, qs_2025_clean)


# THE
the_clean <- the_raw %>%
  filter(Year %in% c(2023, 2024, 2025)) %>%
  transmute(
    entity                       = safe_utf8(trimws(Name)),
    year                         = as.integer(Year),
    source                       = "THE",
    rank                         = clean_rank(Rank),
    score_teaching               = as.numeric(Teaching),
    score_research_environment   = as.numeric(Research.Environment),
    score_research_quality       = as.numeric(Research.Quality),
    score_industry               = as.numeric(Industry.Impact),
    score_intl_outlook           = as.numeric(International.Outlook),
    score_overall                = as.numeric(Overall.Score)
  )


# ARWU
arwu_clean <- arwu_raw %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  transmute(
    entity        = safe_utf8(trimws(name)),
    year          = as.integer(year),
    source        = "ARWU",
    rank          = clean_rank(rank),
    score_alumni  = as.numeric(Alumni),
    score_award   = as.numeric(Award),
    score_hici    = as.numeric(HiCi),
    score_ns      = as.numeric(N.S),
    score_pub     = as.numeric(PUB),
    score_pcp     = as.numeric(PCP),
    score_overall = as.numeric(total_score)
  )


rankings_sample <- bind_rows(qs_clean, the_clean, arwu_clean) %>%
  filter(!is.na(entity), entity != "", !is.na(rank)) %>%
  arrange(year, source, rank)


usethis::use_data(rankings_sample, overwrite = TRUE, compress = "xz")

message("Done! rankings_sample: ", nrow(rankings_sample), " rows x ",
        ncol(rankings_sample), " cols")
message("Years: ", paste(sort(unique(rankings_sample$year)), collapse = ", "))
message("Sources: ", paste(unique(rankings_sample$source), collapse = ", "))



usethis::use_data(rankings_sample, overwrite = TRUE)
