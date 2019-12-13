# bmarketing <- read.csv2("bmarketing.csv",dec=".")
# str(bmarketing)

relevel_days <- function(vec){
  
  stopifnot(is.factor(vec))
  stopifnot(all(levels(vec) %in% c("mon","tue","wed","thu","fri")))
  
  factor(as.character(vec),
         levels=c("mon","tue","wed","thu","fri"))
}

# vec <- bmarketing$day_of_week
# relevel_days(vec)

relevel_months <- function(vec){
  
  stopifnot(is.factor(vec))
  stopifnot(all(levels(vec) %in% c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")))
  
  
  factor(as.character(vec),
         levels=c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
}

# vec <- bmarketing$month
# relevel_months(vec)