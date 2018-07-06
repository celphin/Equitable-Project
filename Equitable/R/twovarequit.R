twovarequit <-
function(a,at){
# s[i,j]<- s<-uniroot(function(x) x^4-a[i,j]*x^3+a[j,i]*x-1, c(a[i,j]-?,a[i,j]+?))
ma<-max(abs(a),abs(at))

#v<-uniroot(function(x) x^4-a*x^3+at*x-1, c(0,ma))
z<-c(-1,at,-a,1)
#z<-c(-1,a,-a,1)
#z<-c(-1,at,-at,1)
sr<-polyroot(z)
 cat("\n",sr,"\n")
# cat("\n",Im(sr),"\n")
# cat("\n ",which(abs(Im(sr))<1e-10))
realroots<-Re(sr[which(abs(Im(sr))<1e-10)])
cat("\nreal roots",realroots)
min(realroots-a)
s<-realroots[which((realroots-a)==min(realroots-a))]
 st<-1/s[1]
slope<-list(s=s,st=st)
cat("\n slope=",slope$s,slope$st)
z<-c(-1,a,-at,1)
sr<-polyroot(z)
# cat("\n",sr,"\n")
# cat("\n",Im(sr),"\n")
# cat("\n ",which(abs(Im(sr))<1e-10))
realroots<-Re(sr[which(abs(Im(sr))<1e-10)])
#cat("\nreal roots",realroots)
min(realroots-at)
s<-realroots[which((realroots-at)==min(realroots-at))]
st<-1/s[1]
cat("\n reverse s,st",s,st,"\n")

return(slope)
}
