posReport <- function(raw, team) {
  Pos <- subset(raw, Team==team)[,4:8]
  Wt <- subset(raw, Team==team)[,"MP"]
  Wt <- Wt/sum(Wt)*5
  rownames(Pos) <- subset(raw, Team==team)[,"Name"]
  round(100*Pos*Wt)
}
