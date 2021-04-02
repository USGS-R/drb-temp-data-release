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


##### Reservoir modeling file utils #####
#' Call gd_get (without any scipiper target builds) on a file in another repo on
#' this file system, downloading that file from a shared Drive cache into that
#' other repo
#'
#' @param ind_file the indicator file of the data file in the other repo, to be
#'   downloaded to the corresponding location in that other repo
#' @param repo the relative file path of the other repo. Usually it'll be right
#'   alongside the current repo, e.g., '../lake-temperature-model-prep'
gd_get_elsewhere <- function(ind_file, repo, ...) {
  # switch to the elsewhere repo, with plans to always switch back to this one before exiting the function
  this_repo <- getwd()
  on.exit(setwd(this_repo))
  setwd(repo)
  
  # fetch the file down to that elsewhere repo
  gd_get(ind_file, ...)
}

# Write a layer of an sf object as a zipped-up shapefile
sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}