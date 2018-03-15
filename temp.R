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
         name = map_chr(commit, ~ .x@committer@name),
         email = map_chr(commit, ~ .x@committer@email))
tidyverse_commits

periodically <-
  tidyverse_commits %>%
  arrange(committed, repo, email) %>%
  as_tbl_time(index = committed) %>%
  select(repo, committed, email) %>%
  collapse_by("weekly", start_date = min(floor_date(.$committed, "week"))) %>%
  count(repo, committed, email)
periodically

#' People have used multiple email addresses
committers <- count(tidyverse_commits, email, sort = TRUE)
print(committers, n = 20)

#' People have used multiple names
committers <- count(tidyverse_commits, name, sort = TRUE)
print(committers, n = 20)

#' Distribution of commits by committers
committers %>%
  slice(1:20) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Plot a committer's commits over time, by package
periodically %>%
  filter(email %in% c("jenny@stat.ubc.ca", "jenny.f.bryan@gmail.com  ")) %>%
  ggplot(aes(committed, n)) +
  geom_bar(stat = "identity") +
  facet_grid(repo ~ .) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank())

#' Look up a commit's user's username via the SHA and the API
first_commits <-
  tidyverse_commits %>%
  mutate(sha = map_chr(commit, ~ .x@sha)) %>%
  group_by(email) %>%
  summarise(n = n(), first_sha = first(sha)) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%
  print(n = Inf)

xcommit <- gh("/search/commits?q=hash:2bd18518ad35776f7daada5c3badc10005d69ba4",
              .send_headers = c("Accept" = "application/vnd.github.cloak-preview"))
xcommit$items[[1]]$committer$login

xcommit <- gh("/search/commits?q=hash:0078d75da7b4af5adfd754a1cf59216f34870d0d",
              .send_headers = c("Accept" = "application/vnd.github.cloak-preview"))
xcommit$items[[1]]$committer$login
