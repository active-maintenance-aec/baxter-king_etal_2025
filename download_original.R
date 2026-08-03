# baxter-king_etal_2025/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from OSF and verify every file. Run
#   this once before running anything in maintained/. Re-running is free: files
#   already present with the right checksum are not downloaded again.
#
#   The deposit is an OSF project rather than a Dataverse dataset, which changes
#   two things. OSF stores exactly the bytes that were uploaded and derives no
#   alternative representation of any file, so there is no ?format=original
#   distinction and no separate checksum for a derived tabular copy. And OSF
#   publishes both an MD5 and a SHA-256 for every file. The manifest therefore
#   carries md5_served, the MD5 of the bytes the download endpoint returned when
#   this code was written, alongside md5_published and sha256_published from the
#   file metadata. Every file is verified against the MD5, the SHA-256 and the
#   published byte size, all three of which describe the same stored bytes.
#
#   The deposit has directory structure, which the manifest records in full, so
#   the directories are created before anything is written.

library(tidyverse)
library(here)

here::i_am("download_original.R")

osf_node <- "bwv3c"
base_url <- str_glue("https://files.osf.io/v1/resources/{osf_node}/providers/osfstorage")

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

planned <-
  manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{osf_file_id}"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk(unique(dirname(planned$path)), function(d) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
})

# A file that is absent has not been fetched yet. A file that is present and
# does not match has been damaged since it was fetched, which is the thing this
# gate exists to catch, so it stops here rather than being quietly refetched.
# run_all.R sources this file last as well as first, and a silent repair there
# would hide a mid-run write into original/.
damaged <- planned |> filter(!is.na(md5_local), md5_local != md5_served)

if (nrow(damaged) > 0) {
  stop("Files in original/ no longer match the manifest: ",
       paste(damaged$file, collapse = ", "),
       ". Something has written to original/ since it was fetched. ",
       "Delete the offending files and re-run to refetch them from OSF.")
}

# Download what is missing or wrong ----
walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
# OSF publishes two hashes of one set of stored bytes, so unlike the two MD5s a
# Dataverse deposit carries, these are two independent algorithms and both are gates.
# Byte size is checked alongside them.
sha256_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  as.character(openssl::sha256(con))
}

verified <-
  planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    sha256_downloaded = map_chr(path, sha256_file),
    bytes_on_disk = file.size(path),
    md5_ok = md5_downloaded == md5_served,
    sha256_ok = sha256_downloaded == sha256_published,
    bytes_ok = bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_ok, sha256_ok, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$md5_ok)) {
  stop("MD5 mismatch in original/: ",
       paste(verified$file[!verified$md5_ok], collapse = ", "),
       ". Delete the offending files and re-run to refetch them from OSF.")
}

if (!all(verified$sha256_ok)) {
  stop("SHA-256 mismatch in original/: ",
       paste(verified$file[!verified$sha256_ok], collapse = ", "), ".")
}

if (!all(verified$bytes_ok)) {
  stop("Byte size mismatch in original/: ",
       paste(verified$file[!verified$bytes_ok], collapse = ", "), ".")
}

# The deposit and nothing else ----
# A name check alone would pass a deposited file that had been renamed by hand, so the
# emptiness check runs on paths and the checksum check above runs on bytes and comes
# first. all.files = TRUE is not optional: without it a stray dotfile passes unseen, and
# a deposit can ship dotfiles of its own, which the manifest then has to list.
unexpected <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)

if (length(unexpected) > 0) {
  stop("original/ holds files the manifest does not list: ",
       paste(unexpected, collapse = ", "),
       ". Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("All {nrow(verified)} files match on MD5, SHA-256 and byte size, and ",
               "original/ holds nothing else. {sum(!verified$published_agrees)} carry a ",
               "published MD5 that disagrees with what the download endpoint served."))
print(str_glue("Archive: https://osf.io/{osf_node}/"))
