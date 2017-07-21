## Legacy function
ffirst.lagm.SD <- function(PKindex,...,MD=FALSE) {
  ffirst.lag(PKindex,...,Tlag=TRUE,MMe=TRUE,MD=FALSE)
}
ffirst.lagm.MD <- function(PKindex,...,MD=TRUE) {
  ffirst.lag(PKindex,...,Tlag=TRUE,MMe=TRUE,MD=TRUE)
}
## Legacy function
ffirst.nolagm.SD <- function(PKindex,...,MD=FALSE) {
  ffirst.nolag(PKindex,...,Tlag=FALSE,MMe=TRUE,MD=FALSE)  ### original was 'ffirst.lag(PKindex,...,Tlag=FALSE,MMe=TRUE)'; I wondered if it's correct.
}
ffirst.nolagm.MD <- function(PKindex,...,MD=TRUE) {
  ffirst.nolag(PKindex,...,Tlag=FALSE,MMe=TRUE,MD=TRUE)
}
## Legacy function
fzero.nolagm.SD <- function(PKindex,...,MD=FALSE) {
  fzero.nolag(PKindex,...,MMe=TRUE,MD=FALSE)
}
fzero.nolagm.MD <- function(PKindex,...,MD=TRUE) {
  fzero.nolag(PKindex,...,MMe=TRUE,MD=TRUE)
}

## Legacy function
finfu.mm.SD <- function(PKindex,...,MD=FALSE) {
  finfu1(PKindex,...,MMe=TRUE,MD=FALSE)
}
finfu.mm.MD <- function(PKindex,...,MD=TRUE) {
  finfu1(PKindex,...,MMe=TRUE,MD=TRUE)
}
## Legacy function
fbolus.mm.SD <- function(PKindex,...,MD=FALSE) {
  fbolus1(PKindex,...,MMe=TRUE,MD=FALSE)
}
fbolus.mm.MD <- function(PKindex,...,MD=TRUE) {
  fbolus1(PKindex,...,MMe=TRUE,MD=TRUE)
}