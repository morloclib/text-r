morloc_split <- function(delim, s) {
  if (delim == "") {
    return(strsplit(s, "")[[1]])
  }
  strsplit(s, delim, fixed = TRUE)[[1]]
}

morloc_lines <- function(s) {
  strsplit(s, "\n", fixed = TRUE)[[1]]
}

morloc_words <- function(s) {
  parts <- strsplit(trimws(s), "\\s+")[[1]]
  parts[parts != ""]
}

morloc_chars <- function(s) {
  strsplit(s, "")[[1]]
}

morloc_join <- function(delim, xs) {
  paste0(xs, collapse = delim)
}

morloc_unlines <- function(xs) {
  paste0(xs, collapse = "\n")
}

morloc_unwords <- function(xs) {
  paste0(xs, collapse = " ")
}

morloc_contains <- function(needle, haystack) {
  grepl(needle, haystack, fixed = TRUE)
}

morloc_startsWith <- function(prefix, s) {
  startsWith(s, prefix)
}

morloc_endsWith <- function(suffix, s) {
  endsWith(s, suffix)
}

morloc_toUpper <- function(s) {
  toupper(s)
}

morloc_toLower <- function(s) {
  tolower(s)
}

morloc_trim <- function(s) {
  trimws(s)
}

morloc_trimStart <- function(s) {
  trimws(s, which = "left")
}

morloc_trimEnd <- function(s) {
  trimws(s, which = "right")
}

morloc_replace <- function(old_str, new_str, s) {
  sub(old_str, new_str, s, fixed = TRUE)
}

morloc_replaceAll <- function(old_str, new_str, s) {
  gsub(old_str, new_str, s, fixed = TRUE)
}

morloc_padLeft <- function(width, fill, s) {
  n <- nchar(s)
  if (n >= width || nchar(fill) == 0) return(s)
  pad_needed <- width - n
  fill_len <- nchar(fill)
  full_reps <- pad_needed %/% fill_len
  partial <- pad_needed %% fill_len
  padding <- paste0(paste0(rep(fill, full_reps), collapse = ""), substr(fill, 1, partial))
  paste0(padding, s)
}

morloc_padRight <- function(width, fill, s) {
  n <- nchar(s)
  if (n >= width || nchar(fill) == 0) return(s)
  pad_needed <- width - n
  fill_len <- nchar(fill)
  full_reps <- pad_needed %/% fill_len
  partial <- pad_needed %% fill_len
  padding <- paste0(paste0(rep(fill, full_reps), collapse = ""), substr(fill, 1, partial))
  paste0(s, padding)
}

morloc_isBlank <- function(s) {
  nchar(trimws(s)) == 0
}
