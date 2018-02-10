#' @title Update olsrr package
#' @description Check whether olsrr is up-to-date, and will install
#' after confirmation.
#' @param owner repository owner
#' @param repo name of the package
#' @examples
#' \dontrun{
#' ols_update()
#' }
#' @export
ols_update <- function(owner = "rsquaredacademy", repo = "olsrr") {
  deps <- ols_deps(owner = owner, repo = repo)
  behind <- dplyr::filter(deps, behind)

  if (nrow(behind) == 0) {
    cli::cat_line(glue::glue(repo, " is up-to-date."))
    return(invisible())
  }

  cli::cat_line("The following sources are out of date:")
  cli::cat_line()
  cli::cat_bullet(format(behind$location), " (", deps$version[1], " -> ", behind$version, ")")

  invisible()
}

#' @importFrom utils available.packages packageVersion
#' @importFrom gh gh
#' @importFrom stringr str_locate str_sub str_split
#' @importFrom magrittr subtract
#' @title Check olsrr Version
#' @description  Checks the version of olsrr on local, cran,
#' github release and devel.
#' @param owner repository owner
#' @param repo name of the package
#' @return a tibble with package versions
#' @examples \dontrun{
#' ols_deps()
#' }
#' @export
ols_deps <- function(owner = "rsquaredacademy", repo = "olsrr") {
  pkgs <- utils::available.packages()
  cran_version <- pkgs %>%
    extract(repo, "Version") %>%
    package_version()

  local_version <- packageVersion(repo)
  behind_cran <- cran_version > local_version

  # github release
  gh_release <- gh(
    "GET /repos/:owner/:repo/releases/latest",
    owner = owner, repo = repo
  ) %>%
    use_series(tag_name)

  if (str_detect(gh_release, "v")) {
    gh_release_version <- str_split(gh_release, "v") %>%
      purrr::map_chr(2)
  } else {
    gh_release_version <- gh_release
  }


  behind_github_release <- gh_release_version > local_version

  # github development
  gh_devel <- gh(
    "GET /repos/:owner/:repo/contents/:path",
    owner = owner, repo = repo,
    path = "DESCRIPTION",
    .send_headers = c("Accept" = "application/vnd.github.raw")
  )

  start <- str_locate(gh_devel$message, pattern = "Version") %>%
    extract(1) + 9
  gh_trim <- str_sub(gh_devel, start = start)
  end <- str_locate(gh_trim, "\n") %>%
    extract2(1) %>%
    subtract(1)

  gh_devel_version <- str_sub(gh_trim, start = 0, end = end) %>%
    package_version()

  behind_github_dev <- gh_devel_version > local_version

  tibble(
    location = c("local", "cran", "gh_release", "gh_devel"),
    version = c(
      as.character(local_version), as.character(cran_version),
      as.character(gh_release_version), as.character(gh_devel_version)
    ),
    behind = c(NA, behind_cran, behind_github_release, behind_github_dev)
  )
}
