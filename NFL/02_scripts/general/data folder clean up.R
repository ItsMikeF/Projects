# install.packages(c("fs","dplyr"))  # if needed
library(fs)
library(dplyr)

# >>> set your root folder
root <- "C:/Users/mikef/OneDrive/Documents/GitHub/Projects/NFL/01_data/"

pretty_bytes <- function(bytes) {
  u <- c("B","KB","MB","GB","TB","PB")
  if (is.na(bytes) || bytes <= 0) return("0 B")
  p <- min(floor(log(bytes, 1024)), length(u) - 1)
  sprintf("%.2f %s", bytes / (1024 ^ p), u[p + 1])
}

folder_size_bytes <- function(path) {
  info <- fs::dir_info(path, recurse = TRUE, type = "file", fail = FALSE)
  sum(info$size, na.rm = TRUE)
}

get_folder_sizes <- function(top_dir, include_root_files = TRUE) {
  stopifnot(fs::dir_exists(top_dir))
  subdirs <- fs::dir_ls(top_dir, type = "directory", recurse = FALSE, fail = FALSE)
  
  sizes <- tibble(
    folder_path = subdirs,
    folder_name = fs::path_file(subdirs),
    size_bytes  = vapply(subdirs, folder_size_bytes, numeric(1))
  ) |>
    arrange(desc(size_bytes)) |>
    mutate(size_readable = vapply(size_bytes, pretty_bytes, character(1)))
  
  if (include_root_files) {
    root_files <- fs::dir_info(top_dir, recurse = FALSE, type = "file", fail = FALSE)
    root_bytes <- sum(root_files$size, na.rm = TRUE)
    sizes <- bind_rows(
      tibble(
        folder_path   = top_dir,
        folder_name   = "(files in root)",
        size_bytes    = root_bytes,
        size_readable = pretty_bytes(root_bytes)
      ),
      sizes
    )
  }
  
  sizes
}

sizes <- get_folder_sizes(root)
print(sizes[, c("folder_name","size_readable","folder_path")], n = nrow(sizes))

