# function for reading more than one excel sheet
read_excel_allsheets <- function(filename, tibble = T) {

  require(readxl)

  sheets <- readxl::excel_sheets(filename)

  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))

  if(!tibble){
    x <- lapply(x, as.data.frame)
  }

  names(x) <- sheets
  x
}

# extract datasets from lists of lists
extract_euklems <- function(data_list, variable, countries, years, output = T,
                            time0 = "1995/01/01", nace_cod = "TOT"){

  require(dplyr)
  require(reshape2)
  require(stringr)

  if (output == F){
    nace2 <- data_list[[1]]$I_IT$code
    M <- dim(data_list[[1]]$I_IT)[1]      # number of NACE
    N <- length(years)                    # number of years
    C <- length(countries)                # number of countries
  } else if (output == T){
    nace2 <- data_list[[1]]$VA_Q$code
    M <- dim(data_list[[1]]$VA_Q)[1]      # number of NACE
    N <- length(years)                    # number of years
    C <- length(countries)                # number of countries
  }

  x = lapply(data_list, `[`,c(variable)) %>%
    data.frame(.) %>%
    select(names(.)[!grepl("*.code", names(.))]) %>%
    select(names(.)[!grepl("*.desc", names(.))]) %>%
    setNames(gsub("^.*\\.","", names(.))) %>%
    setNames(str_sub(names(.),-4)) %>%
    as.matrix(.) %>%
    t(.) %>%
    data.frame(.) %>%
    mutate(., country = rep(countries, each = N)) %>%
    setNames(c(nace2, "country")) %>%
    melt(., id.vars = "country") %>%
    mutate(., year = rep(years,C*M)) %>%
    filter(., variable == nace_cod) %>%
    filter(., year > time0) %>%
    mutate(., l_value = log(value)) %>%
    group_by(country) %>%
    mutate(., dl_value = c(NA,diff(l_value))) %>%
    ungroup(.)

}

# extract datasets from lists of lists with 2 digits
extract_euklems_2digits <- function(data_list, variable, countries, years, output = T,
                                    nace_cod = a_nace2, rca = F){

  require(dplyr)
  require(reshape2)
  require(stringr)

  if (output == F){
    nace2 <- data_list[[1]]$I_IT$code
    M <- dim(data_list[[1]]$I_IT)[1]      # number of NACE
    N <- length(years)                    # number of years
    C <- length(countries)                # number of countries
  } else if (output == T){
    nace2 <- data_list[[1]]$VA_Q$code
    M <- dim(data_list[[1]]$VA_Q)[1]      # number of NACE
    N <- length(years)                    # number of years
    C <- length(countries)                # number of countries
  }

  x = lapply(data_list, `[`,c(variable))

  y <- list()
  for (i in 1:C){
    y[[i]] <- x[[i]]$VA_QI %>%
      filter(., code %in% nace_cod) %>%
      setNames(c("code", "desc", as.character(c(1970:2015))))

    if (rca == T){
      y[[i]] <- y[[i]][,c(28:48)]
      y[[i]] <- as.matrix(y[[i]])
    } else if (rca == F){
      y[[i]] <- y[[i]][,c(28:48)]
      y_csum <- colSums(y[[i]])

      for (j in 1:dim(y[[i]])[1]){
        y[[i]][j,] <- y[[i]][j,]/y_csum
      }
      y[[i]] <- as.matrix(y[[i]])

    }
  }

  names(y) <- countries

  # rm(x, y_csum)

  y
}
