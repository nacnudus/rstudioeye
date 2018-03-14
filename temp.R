#' ---
#' title: Exploratory analysis of tidyverse activity
#' author: "Duncan Garmonsway"
#' output: github_document
#' ---

library(tidyverse)
library(gh)
library(git2r)
library(fs)
library(lubridate)
library(tibbletime)
library(here)

dir_create(here::here("repos"))

tidyverse_members <- gh("/orgs/:org/members", org = "tidyverse")
member_names <- map_chr(tidyverse_members, ~ .x[["login"]])

tidyverse_repos <- gh("/orgs/:org/repos", org = "tidyverse")

#' Clone/pull all the repos locally
tidyverse_repos %>%
  map_chr(~ .x[["ssh_url"]]) %>%
  tibble(name = str_extract(basename(.), "^.*(?=\\.)"),
         url = .,
         local_path = path("repos", name)) %>%
  # pwalk(~ clone(..2, ..3)) %>%
  # pwalk(~ pull(repository(..3))) %>%
  print(n = Inf)

repo_dirs <- dir_ls(here::here("repos"))

#' Structure of a commit
commits(repository(as.character(repo_dirs[1]))) %>%
  .[[1]] %>% str()

tidyverse_commits <-
  repo_dirs %>%
  tibble(repo_dir = .,
         commit = map(repo_dir, ~ commits(repository(as.character(.x))))) %>%
  unnest() %>%
  # slice(1:10) %>%
  mutate(repo = basename(repo_dir),
         committed = ymd_hms(map_chr(commit, when)),
         committer = map_chr(commit, ~ .x@committer@email))
tidyverse_commits

periodically <-
  tidyverse_commits %>%
  arrange(committed, repo, committer) %>%
  as_tbl_time(index = committed) %>%
  select(repo, committed, committer) %>%
  collapse_by("weekly", start_date = min(floor_date(.$committed, "week"))) %>%
  count(repo, committed, committer)
periodically

committers <- count(tidyverse_commits, committer, sort = TRUE)
committers

#' Plot a committer's commits over time, by package
periodically %>%
  filter(committer == "jenny@stat.ubc.ca") %>%
  ggplot(aes(committed, n)) +
  geom_bar(stat = "identity") +
  facet_grid(repo ~ .) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank())
