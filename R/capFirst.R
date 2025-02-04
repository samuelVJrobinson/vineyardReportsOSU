#Capitalize the first letter
capFirst <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
