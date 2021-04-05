zip_this <- function(out_file, .object){
  
  if ('data.frame' %in% class(.object)){
    filepath <- basename(out_file) %>% tools::file_path_sans_ext() %>% paste0('.csv') %>% file.path(tempdir(), .)
    write_csv(.object, path = filepath)
    zip_this(out_file = out_file, .object = filepath)
  } else if (class(.object) == 'character' & file.exists(.object)){
    # for multiple files?
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd(dirname(.object))
    zip::zip(file.path(curdir, out_file), files = basename(.object))
  } else {
    stop("don't know how to zip ", .object)
  }
}

zip_obs <- function(out_file, in_file){
  if (grepl('csv', in_file)) {
    zip_this(out_file, .object = readr::read_csv(in_file))
    
  } else if (grepl('rds', in_file)) {
    zip_this(out_file, .object = readRDS(in_file))
  } else {
    message('There is no reader for this filetype. Please modify function zip_obs.')
  }
  
}

get_distance_matrix <- function(out_file, in_file) {
  distance <- readRDS(in_file)
  from <- rownames(distance$updown)

  out <- as_tibble(distance$updown) %>%
    mutate(from = from) %>%
    select(from, everything())
  
  readr::write_csv(out, path = out_file) 
}

get_sntemp_output <- function(out_file, in_file){
  sntemp <- feather::read_feather(in_file)
  readr::write_csv(sntemp, out_file)
}


extract_reach_attributes <- function(res_file, attr_file, out_file) {
  res <- readRDS(res_file) %>% select(-subseg_seg) %>% rename(reach_class = type_res)
  attr <- readRDS(attr_file)
  attr <- attr$edges %>% st_drop_geometry()

  out <- left_join(res, select(attr, -subseg_updown, -start_pt, -end_pt, -to_subseg))
  
  readr::write_csv(out, out_file)
  
}

get_sites <- function(in_dat) {
  
  sites <- in_dat$edges %>%
    filter(!is.na(seg_id_nat))
  return(unique(sites$seg_id_nat))
}

filter_reservoirs <- function(in_dat, keep, out_file) {
  dat <- readr::read_csv(in_dat) %>%
    filter(reservoir %in% keep)
  
  readr::write_csv(dat, out_file)
}

filter_to_subset <- function(out_file, in_file, segments) {
  dat <- readr::read_csv(in_file) %>%
    filter(seg_id_nat %in% segments)
  
  readr::write_csv(dat, out_file)
}

get_grands <- function(in_file) {
  dat <- readr::read_csv(in_file)
  return(unique(dat$GRAND_ID))
}

filter_res_ids <- function(in_file, out_file, res_keep) {
  dat <- readr::read_csv(in_file) %>%
    filter(GRAND_ID %in% res_keep)
  
  readr::write_csv(dat, out_file)
}
