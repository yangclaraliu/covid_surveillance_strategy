label_K <- function(X){
  unlist(lapply(X,  FUN =
         function(x){
           
           if (is.na(x)){return(x)}
           
           if (x < 1e3){
             sprintf("%i",x)
           } else {
             sprintf("%iK",x/1e3)
           }
         }
  ))}

### need to parse (100, 1e+3] as (100 &lt; tests < 1000)
relabel_categories <- function(s, label = "x"){
  lapply(as.character(s), function(x){unlist(strsplit(x, split = ","))}) %>%
    lapply(., parse_number) %>%
    lapply(., function(x){paste(x[1], "<", label, "\u2264", x[2])}) %>%
    lapply(., function(x){sub(pattern = " < NA", replacement = "", x = x)}) %>%
    unlist
  
}

scientific_10 <- function(x){
  scales::scientific_format()(x) %>%
    sub(pattern = "e\\+",
        x = .,
        replacement = "e") %>%
    sub(pattern = "e00", 
        x = ., 
        replacement = "") %>%
    gsub(pattern = "e",
         replacement = " %*% 10^",
         x = .) %>%
    sub(pattern = "1 %*% ",
        replacement = "",
        x = ., fixed = T) %>%
    parse(text = .)
}




color_strip <- function(plot){
  g <- ggplot_gtable(ggplot_build(plot))
  stripr <- which(grepl('strip-t', g$layout$name))
  k <- 1
  for (i in stripr) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    lab <- g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label %>%
      sub(x = ., pattern = "Departments", replacement = "Dept") %>%
      sub(x = ., pattern = "Fever Clinics", replacement = "Fever Clinic") %>%
      sub(x = ., pattern = "Adults", replacement = "Younger Adults") %>%
      sub(x = ., pattern = "Elderly", replacement = "Older Adults")
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-
      r_palette[lab]
    g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col <-
      ifelse(lab == "Community - Older Adults", "white", "grey10")
    k <- k+1
  }
  return(grid::grid.draw(g))
}