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

rate_limit <- function() gh("/rate_limit")
rate_limit()

dir_create(here::here("repos"))

tidyverse_members <- gh("/orgs/:org/members", org = "tidyverse")
member_names <- map_chr(tidyverse_members, ~ .x[["login"]])

tidyverse_repos <-
  gh("/orgs/:org/repos", org = "tidyverse") %>%
  map_chr(~ .x[["ssh_url"]]) %>%
  tibble(name = str_extract(basename(.), "^.*(?=\\.)"),
         url = .,
         local_path = path_abs(path("repos", name)))

repo_dirs <- dir_ls(here::here("repos"))

existing_repos <- filter(tidyverse_repos, local_path %in% repo_dirs)
new_repos <- anti_join(tidyverse_repos, existing_repos, by = "local_path")

#' Clone new repos locally
new_repos %>%
  pwalk(~ clone(..2, ..3)) %>%
  print(n = Inf)

#' Pull existing repos locally
existing_repos %>%
  pwalk(~ pull(repository(..3))) %>%
  print(n = Inf)

repo_dirs <- dir_ls(here::here("repos"))

#' Structure of a commit
commits(repository(as.character(repo_dirs[1]))) %>%
  .[[1]] %>% str()

#' Import all commits
tidyverse_commits <-
  repo_dirs %>%
  tibble(repo_dir = .,
         commit = map(repo_dir, ~ commits(repository(as.character(.x))))) %>%
  unnest() %>%
  mutate(repo = basename(repo_dir),
         committed = ymd_hms(map_chr(commit, when)),
         name = map_chr(commit, ~ .x@committer@name),
         email = map_chr(commit, ~ .x@committer@email)) %>%
  print()

#' People have used multiple email addresses
committer_emails <- count(tidyverse_commits, email, sort = TRUE)
print(committer_emails, n = 20)

#' People have used multiple names
committer_names <- count(tidyverse_commits, name, sort = TRUE)
print(committer_names, n = 20)

#' Look up a commit's user's username via the SHA and the API
commit_login <- function(repo, sha) {
  commit <- gh("/repos/:owner/:repo/commits/:sha",
               owner = "tidyverse",
               repo = repo,
               sha = sha)
  out <- commit$committer$login
  if(is.null(out)) {
    out <- commit$commit$committer$name
  }
  if(is.null(out)) return(NA)
  cat(out, "\n")
  out
}
logins <-
  tidyverse_commits %>%
  mutate(sha = map_chr(commit, ~ .x@sha)) %>%
  group_by(email) %>%
  summarise(n = n(),
            last_repo = last(repo),
            last_sha = last(sha)) %>%
  arrange(desc(n)) %>%
  mutate(login = pmap_chr(list(last_repo, last_sha), commit_login)) %>%
  print()

#' Resolve commits to logins
named_commits <-
  logins %>%
  select(email, login) %>%
  right_join(tidyverse_commits, by = "email") %>%
  select(repo, login, committed, commit)

#' Distribution of commits by login
named_commits %>%
  count(login, sort = TRUE) %>%
  slice(1:20) %>%
  mutate(login = fct_reorder(login, n)) %>%
  ggplot(aes(login, n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Construct tibbletime
periodically <-
  named_commits %>%
  arrange(committed, repo, login) %>%
  as_tbl_time(index = committed) %>%
  select(repo, committed, login) %>%
  collapse_by("weekly", start_date = min(floor_date(.$committed, "week"))) %>%
  count(repo, committed, login)
periodically

#' Plot a committer's commits over time, by package
committer <- "hadley"
periodically %>%
  filter(login == committer) %>%
  group_by(repo) %>%
  mutate(max_n = max(n)) %>%
  distinct(repo, max_n) %>%
  arrange(desc(max_n)) %>%
  slice(1:min(10, nrow(.))) %>%
  select(repo) %>%
  inner_join(periodically, by = "repo") %>%
  filter(committed >= Sys.time() - years(1)) %>%
  mutate(login = fct_rev(fct_other(as_factor(login), committer)),
         repo = as.factor(repo)) %>%
  ggplot(aes(committed, n, fill = login)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("grey70", "blue")) +
  facet_grid(repo ~ .) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "grey95"))
