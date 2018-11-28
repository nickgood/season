#' Converts date to season
#' @param date an object lubridate recongnizes as a date
#' @return The season the date is in.
#' @examples
#' date_to_season("2018-05-11")

date_to_season <- function(date){
# empty output factor
  season <- factor(levels = c("spring", "summer", "fall", "winter"))
# check if a month can be extracted
  moy <- tryCatch(lubridate::month(date),
           error = function(e){print(paste("non-date argument", date));NaN})
# calculate season
 if(is.na(moy)){NA}
 else{
  if(moy > 2 & moy < 6){season <- "spring"}
  if(moy > 5 & moy < 9){season <- "summer"}
  if(moy > 8 & moy < 12){season <- "fall"}
  if(moy > 11 | moy < 3){season <- "winter"}
 }
# output
 return(season)
}
