
# ---------------------------------------------------------
# Example R script to parse the text you showed into a long
# data frame with columns: date, variable, value
# ---------------------------------------------------------

library(tidyverse)
library(zoo)

dfair <- r"(E:\R_projects\projects\fairmini\data_raw\FMFP)"

fn <- "fmage.txt"
fpath <- fs::path(dfair, fn)


# start non-function code ----

# 1. Read all lines from the file
txt <- readLines(fpath)

# 2. Extract the SMPL line to find the start/end periods
smpl_line <- grep("^\\s*SMPL\\s+", txt, value = TRUE)
# Example: "SMPL    1952.1   2027.4 ;"

# Simple regex to pull out the two numbers after "SMPL"
smpl_pattern <- "^\\s*SMPL\\s+([0-9]+\\.[0-9]+)\\s+([0-9]+\\.[0-9]+).*"
smpl_match   <- regexec(smpl_pattern, smpl_line)
smpl_capt    <- regmatches(smpl_line, smpl_match)[[1]]

start_str <- smpl_capt[2]  # e.g. "1952.1"
end_str   <- smpl_capt[3]  # e.g. "2027.4"

# Helper to split something like "1952.1" into (year=1952, quarter=1)
parse_year_quarter <- function(x) {
  parts   <- strsplit(x, "\\.")[[1]]
  year    <- as.integer(parts[1])
  quarter <- as.integer(parts[2])
  list(year = year, quarter = quarter)
}

start_yq <- parse_year_quarter(start_str)
end_yq   <- parse_year_quarter(end_str)

# 3. Function to create a vector of year-quarter labels
#    from e.g. (1952,1) to (2027,4). This is intentionally
#    lenient: we’ll generate as many year-quarters as we
#    actually have numeric data for each variable (see below).
make_yearq_labels <- function(start_y, start_q, n) {
  # Start at (start_y, start_q). Generate 'n' consecutive quarters.
  yrs <- numeric(n)
  qtr <- numeric(n)
  y   <- start_y
  q   <- start_q
  for(i in seq_len(n)) {
    yrs[i] <- y
    qtr[i] <- q
    q <- q + 1
    if(q > 4) {
      q <- 1
      y <- y + 1
    }
  }
  # Return character labels like "1952Q1", "1952Q2", ...
  paste0(yrs, "Q", qtr)
}

# 4. Identify each LOAD ... block, read numeric values until 'END'
load_lines_idx <- grep("^\\s*LOAD\\s+", txt)  # lines where "LOAD varname"
all_dfs <- list()

for(i in seq_along(load_lines_idx)) {
  
  # Current LOAD line's index
  start_idx <- load_lines_idx[i]
  
  # The variable name is right after "LOAD", e.g. "AG1"
  # Using a regex capture:
  load_line <- txt[start_idx]
  var_pattern <- "^\\s*LOAD\\s+([A-Za-z0-9_]+)"
  var_match   <- regexec(var_pattern, load_line)
  var_capt    <- regmatches(load_line, var_match)[[1]]
  varname     <- var_capt[2]
  
  # Find the line index of the matching 'END' that terminates this block
  # We look for the next line matching '^\\s*END' after start_idx
  # (In the sample text, each variable block ends with a line that begins with 'END')
  # end_idx_candidates <- grep("^\\s*END", txt)  # djb -- doesn't find the single quote
  end_idx_candidates <- grep("^\\s*'END'\\s*$", txt)
  end_idx_candidates <- end_idx_candidates[end_idx_candidates > start_idx]
  end_idx <- end_idx_candidates[1]  # the first 'END' after LOAD
  
  # Grab all lines between LOAD ... and the 'END'
  block_lines <- txt[(start_idx + 1):(end_idx - 1)]
  
  # Those lines contain EViews float data in scientific notation. 
  # We can flatten them into one long string, then split on whitespace:
  flat <- paste(block_lines, collapse = " ")
  nums <- scan(text = flat, what = numeric(), quiet = TRUE)
  
  # The length of 'nums' is how many observations for this variable
  n_obs <- length(nums)
  
  # Build a vector of date labels for exactly n_obs quarters
  # starting from the user-specified start date (1952.1 in sample).
  # Note: If you want the *entire* range 1952.1 to 2027.4 for each variable,
  # you can build all quarters and verify that n_obs matches,
  # or slice accordingly. For simplicity, let's just assume each block
  # has the correct number of consecutive quarters from 1952Q1 onward:
  date_labels <- make_yearq_labels(start_yq$year, start_yq$quarter, n_obs)
  date_real <- as.Date(zoo::as.yearqtr(date_labels, format = "%YQ%q"), frac = 0)
  
  # Create a data.frame for this variable
  df_var <- tibble(
    date = date_real,
    date_label = date_labels,
    variable = varname,
    value    = nums
  )
  
  all_dfs[[ varname ]] <- df_var
}

# 5. Combine all into one long data frame
df_long <- do.call(rbind, all_dfs)

# Check result
head(df_long, 12)
#    date     variable      value
# 1  1952Q1   AG1          0.000000e+00
# 2  1952Q2   AG1          0.000000e+00
# 3  1952Q3   AG1          3.732097e-01
# 4  1952Q4   AG1          3.742911e-01
# 5  1953Q1   AG1          3.753679e-01
# 6  1953Q2   AG1          3.764400e-01
# ...

df_long |> 
  ggplot(aes(date, value, colour = variable)) +
  geom_line()

# AG1 exog Percent of 16+ population 26-55 minus percent 16-25. BLS data. 1, 2, 3 -- working age minus college age
# AG2 exog Percent of 16+ population 56-65 minus percent 16-25. BLS data. 1, 2, 3 -- early retirement minus college age
# AG3 exog Percent of 16+ population 66+ minus percent 16-25. BLS data. 1, 2, 3   -- elderly minus college age

# get eviews as a function ----

qtm <- function(quarter) {(quarter - 1) * 3 + 1} # qtm(1:4)

get_smpl <- function(smpl_line){
  # parse the SMPL line such as "SMPL    1952.1   2027.4 ;" to find start/end
  parts <- stringr::str_split(smpl_line, "\\s+")[[1]]
  parts <- parts[parts != ""]
  
  # get the smpl dates as a matrix first row is start next is end
  smpl_mat <- str_split(c(parts[2:3]), "\\.", simplify=TRUE) |> 
    apply(2, as.numeric)
  
  # first element of smpl_dates is start as a date, second element is end
  smpl_dates <- apply(smpl_mat, 1, 
                      \(x) make_date(year = x[1], month = qtm(x[2]))) |>
    as.Date()
  smpl <- setNames(as.list(smpl_dates), c("start", "end"))
  
  return(smpl)
}

get_varnames <- function(lines, txt){
  load_lines <- txt[lines]
  str_match(load_lines, "^\\s*LOAD\\s+([^\\s;]+)")[, 2]
}

txt

get_eviews <- function(fpath){
  
  # assume only ONE smpl line, at start
  txt <- readLines(fpath)
  smpl_line <- grep("^\\s*SMPL\\s+", txt, value = TRUE) # example: "SMPL    1952.1   2027.4 ;"
  smpl <- get_smpl(smpl_line)
  
  load_lines_idx <- grep("^\\s*LOAD\\s+", txt)  # lines where "LOAD varname"
  end_lines_idx <- grep("^\\s*'END'\\s*$", txt)
  stopifnot(length(load_lines_idx) == length(end_lines_idx))
  vnames <- get_varnames(load_lines_idx, txt)
  # block_lines <- txt[(start_idx + 1):(end_idx - 1)]
  #   flat <- paste(block_lines, collapse = " ")
  #   nums <- scan(text = flat, what = numeric(), quiet = TRUE)
  #     date_labels <- make_yearq_labels(start_yq$year, start_yq$quarter, n_obs)
  #       date_real <- as.Date(zoo::as.yearqtr(date_labels, format = "%YQ%q"), frac = 0)
  #       mutate(text_segment = pmap(list(start, end, offset), ~ text[.x + .z:.y + .z]))
  f <- function(start, end){
    block_lines <- txt[start:end]
    flat <- paste(block_lines, collapse = " ")
    nums <- scan(text = flat, what = numeric(), quiet = TRUE)
    nums
  }
  
  dates <- seq.Date(smpl$start, smpl$end, by = "quarter")
  
  df1 <- tibble(vname=vnames, 
                date_start=smpl$start, 
                date_end=smpl$end, 
                start=load_lines_idx + 1, 
                end=end_lines_idx - 1)
  
  df2 <- df1 |> 
    mutate(date = list(dates),
           value = purrr::map2(start, end, \(start, end) f(start, end)))
  
  df2
  
}


fn1 <- "fmage.txt"; fpath <- fs::path(dfair, fn1)
fn2 <- "fmdata.txt"; fpath <- fs::path(dfair, fn2)

# fpath <- fs::path(dfair, fn1)
# 
seq.Date(as.Date("1952-01-01"), as.Date("2027-10-01"), by = "quarter")

tmp <- get_eviews(fpath)
tmp

tmp |> unnest(c(date, value))
tmp |> mutate(nd=length(date), nv=length(value)) |> filter(nd != nv)

# [1,]              2            76
# [2,]             77           151
# [3,]            152           226
# [4,]            227           301
# [5,]            302           376
# [310,]          23177         23251
# [311,]          23252         23326
# [312,]          23327         23401




txt <- readLines(fpath)

tmp <- get_eviews(fpath) |> 
  mutate(value=purrr::map(vname, \(x) rep(x, 5)))

tmp |> 
  unnest(cols=value)

get_varname(2, txt)
get_varname(23327, txt)
get_varname2(c(2, 77, 152, 23327), txt)


block_lines <- txt[(2 + 1):(79 - 1)]





txt_line <- "SMPL    1952.1   2027.4 ;"
get_smpl(txt_line)



