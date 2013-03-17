load("../data/ncaa/2011/ncaa2011.RData")
par <- setPar("ncaa",2011)
makeTeam(ncaa.raw,ncaa.tf,ncaa.vc,type="pdf",teams=c("sandiegost","connecticut","florida","brighamyoung","duke","arizona","wisconsin","butler","floridast","vcu","kentucky","ohiost","kansas","richmond","northcarolina","marquette"),filename="sweet16.tex")
system("pdflatex sweet16.tex")
