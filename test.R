

av <- tt$adl_initial
ar <- tt$deces

tabcont <- function(av,ar, titre = "", lab=""){
zz <- table(av,ar)
tfin <- zz
zzp <- proportions(zz,1)*100
for (c in 1:2){
  for (l in 1:2){
    tfin[c,l] <- paste0(zz[c,l],"(",round(zzp[c,l],1),"%)")
  }
}
kable(tfin,caption=titre,label=lab, row.names = TRUE)%>% 
                kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE,
                                          position = "center")

}
