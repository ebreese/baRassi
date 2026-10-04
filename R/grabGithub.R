# ---------------------------------------------------------------------------
# Configuration: edit these for your package
# ---------------------------------------------------------------------------
.gh_owner <- "ebreese"
.gh_repo  <- "baRassi-data"
.gh_ref   <- "main"          # branch, tag, or commit to track
.gh_dir   <- ""          # base folder in the repo ("" for the repo root)
.pkg_name <- "baRassi"

# Session-level memo: ask GitHub about each file at most once per session
.fetch_env <- new.env(parent = emptyenv())


#' Fetch a dataset, using a local cache when it is up to date
#'
#' Checks the current version (git blob SHA) of `<subdir>/<name>.csv` on
#' GitHub. If a cached copy with that SHA exists it is returned; otherwise the
#' file is downloaded, cached, and returned. The cache mirrors the repo's
#' folder structure, so files with the same name in different subdirectories
#' are kept separate. If GitHub can't be reached, the most recent cached copy
#' is used with a warning.
#'
#' @param name Dataset name (file name without `.csv`).
#' @param subdir Optional subdirectory under the base data folder, e.g.
#'   `"afl/2025"`. `NULL` (default) means the base folder itself.
#' @param ref Branch, tag, or commit SHA to fetch from.
#' @param force Re-download even if the cache looks current.
#' @param quiet Suppress progress messages.
#' @return A data.frame.
fetch_data <- function(name, subdir = NULL, ref = .gh_ref,
                       force = FALSE, quiet = FALSE) {
  check_name(name)
  parts <- subdir_parts(subdir)

  repo_path <- paste(c(nonempty(.gh_dir), parts, paste0(name, ".csv")), collapse = "/")
  label     <- paste(c(parts, name), collapse = "/")   # for messages
  dir       <- cache_path(parts, create = TRUE)
  cached    <- cached_versions(dir, name)

  # 1. What version is current on GitHub? (falls back to cache if offline)
  sha <- tryCatch(
    remote_sha(repo_path, ref, refresh = force),
    error = function(e) {
      if (length(cached) == 0) {
        stop("Couldn't reach GitHub and no cached copy of '", label, "' exists.\n",
             conditionMessage(e), call. = FALSE)
      }
      warning("Couldn't reach GitHub; using cached copy of '", label,
              "', which may be out of date.", call. = FALSE)
      NULL
    }
  )
  if (is.null(sha)) {
    return(readRDS(cached[which.max(file.mtime(cached))]))
  }

  # 2. Cache hit?
  target <- file.path(dir, paste0(name, "_", sha, ".rds"))
  if (file.exists(target) && !force) {
    return(readRDS(target))
  }

  # 3. Cache miss: download exactly this version, cache it atomically
  if (!quiet) message("Downloading '", label, "' (", substr(sha, 1, 7), ")...")
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  download_blob(sha, tmp_csv)

  df <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)

  tmp_rds <- tempfile(tmpdir = dir, fileext = ".tmp")
  saveRDS(df, tmp_rds)
  if (!file.rename(tmp_rds, target)) {
    unlink(tmp_rds)
    stop("Couldn't write cache file: ", target, call. = FALSE)
  }
  unlink(setdiff(cached, target))         # drop superseded versions

  df
}


#' Delete cached datasets
#'
#' @param name Optional dataset name. If given, only that dataset is removed
#'   from `subdir`.
#' @param subdir Optional subdirectory. With no `name`, everything under it
#'   (including nested folders) is removed.
#' @return The deleted file paths, invisibly.
clear_cache <- function(name = NULL, subdir = NULL) {
  parts <- subdir_parts(subdir)
  dir   <- cache_path(parts, create = FALSE)
  if (!dir.exists(dir)) return(invisible(character(0)))

  if (is.null(name)) {
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
  } else {
    check_name(name)
    files <- cached_versions(dir, name)
  }
  unlink(files)

  # Forget session memo entries so the next fetch re-checks GitHub
  rm(list = ls(.fetch_env), envir = .fetch_env)
  invisible(files)
}


# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

cache_root <- function() {
  # Option lets tests / CRAN checks redirect the cache to tempdir()
  getOption(paste0(.pkg_name, ".cache_dir"),
            tools::R_user_dir(.pkg_name, which = "cache"))
}

# Cache folder mirroring the repo subdirectory, e.g. <root>/afl/2025
cache_path <- function(parts, create = TRUE) {
  dir <- do.call(file.path, as.list(c(cache_root(), parts)))
  if (create && !dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

cached_versions <- function(dir, name) {
  list.files(dir, pattern = paste0("^", name, "_[0-9a-f]{40}\\.rds$"),
             full.names = TRUE)
}

# Split and validate a subdirectory into safe path segments.
# Accepts "a/b", "a/b/", "/a/b" or "a\\b"; rejects "..", "." and odd characters
# so a subdir can never point the cache outside its own folder.
subdir_parts <- function(subdir) {
  if (is.null(subdir)) return(character(0))
  if (!is.character(subdir) || length(subdir) != 1 || is.na(subdir)) {
    stop("`subdir` must be a single string or NULL.", call. = FALSE)
  }
  parts <- strsplit(gsub("\\\\", "/", subdir), "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  bad <- parts %in% c(".", "..") | !grepl("^[A-Za-z0-9_.-]+$", parts)
  if (any(bad)) {
    stop("`subdir` may only contain folder names made of letters, digits, ",
         "'_', '-' or '.', separated by '/'.", call. = FALSE)
  }
  parts
}

check_name <- function(name) {
  if (!is.character(name) || length(name) != 1 || !grepl("^[A-Za-z0-9_-]+$", name)) {
    stop("`name` must be a single string of letters, digits, '_' or '-'.",
         call. = FALSE)
  }
}

nonempty <- function(x) x[nzchar(x)]

gh_request <- function(...) {
  req <- httr2::request("https://api.github.com")
  req <- httr2::req_url_path_append(req, ...)
  req <- httr2::req_user_agent(req, paste0(.pkg_name, " R package"))
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_retry(req, max_tries = 3)
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) req <- httr2::req_auth_bearer_token(req, pat)
  req
}

# Git blob SHA of a file: changes if and only if the file's content changes
remote_sha <- function(path, ref, refresh = FALSE) {
  key <- paste(ref, path, sep = ":")
  if (!refresh && !is.null(.fetch_env[[key]])) return(.fetch_env[[key]])

  req  <- gh_request("repos", .gh_owner, .gh_repo, "contents", path)
  req  <- httr2::req_url_query(req, ref = ref)
  req  <- httr2::req_headers(req, Accept = "application/vnd.github+json")
  info <- httr2::resp_body_json(httr2::req_perform(req))

  if (is.null(info$sha) || !identical(info$type, "file")) {
    stop("'", path, "' is not a file in the repository.", call. = FALSE)
  }
  .fetch_env[[key]] <- info$sha
  info$sha
}

# Download the exact blob identified by `sha` (avoids raw-CDN staleness)
download_blob <- function(sha, dest) {
  req <- gh_request("repos", .gh_owner, .gh_repo, "git", "blobs", sha)
  req <- httr2::req_headers(req, Accept = "application/vnd.github.raw+json")
  httr2::req_perform(req, path = dest)
  invisible(dest)
}
