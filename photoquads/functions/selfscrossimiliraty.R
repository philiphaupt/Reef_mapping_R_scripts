
get_composition <- function(x, field){
  comp_data <- table(x[, field]) %>% as.data.frame()
  names(comp_data) <- c(field, "freq")
  comp_data %>% 
    dplyr::mutate(prop = freq/sum(freq))
}

get_similarity <- function(x, y){
  x %<>%  dplyr::select(1, prop)
  y %<>%  dplyr::select(1, prop)
  names(x) <- c("categ", "x")
  names(y) <- c("categ", "y")
  suppressWarnings({
    categ <- union(as.character(x[, 1]), as.character(y[, 1]))
    categ1 <- data.frame(categ = categ) %>%
      dplyr::left_join(x, by = "categ") %>%
      dplyr::mutate(x = replace(x, is.na(x), 0))
    categ2 <- data.frame(categ = categ) %>%
      dplyr::left_join(y, by = "categ") %>%
      dplyr::mutate(y = replace(y, is.na(y), 0))
  })
  o <- cbind(categ1[, 2], categ2[, 2]) 
  numerator <- apply(o, 1, min)
  denominator <- apply(o, 1, max)
  sum(numerator)/sum(denominator)
}

subsample <- function(x, n_i, n_p){
  pic_subset <- sample(unique(x$image), round(dplyr::n_distinct(x$image)*n_i))
  x %>%
    dplyr::filter(image %in% pic_subset) %>%
    dplyr::group_by(image) %>% 
    dplyr::sample_n(n_p)
}