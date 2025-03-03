
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
           value = purrr::map2(start, end, \(start, end) f(start, end)),
           data = pmap(list(date, value), \(date, value) tibble(date, value))) |> 
    select(vname, start=date_start, end=date_end, data)
  
  df2
}

