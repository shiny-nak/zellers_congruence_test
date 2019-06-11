#曜日
week_chr<-c("月","火","水","木","金","土","日")
#y年m月d日

y<-2019
m<-5
m_fanc<-function(y,m){
  if(m==1){
    m<-13
    y<-y-1
  }else if(m==2){
    m<-14
    y<-y-1
  }
  return(list(y,m))
}
y<-m_fanc(y,m)[[1]]
m<-m_fanc(y,m)[[2]]
d<-1

C<-floor(y/100)
Y<-y%%100

gamma_func<-function(y,C){
  if(1582<=y){
    (5*C)+(C/4)
  }else{
    6*C+5
  }
}

D<-((d+floor((26*(m+1))/10)+Y+floor(Y/4)+floor(gamma_func(y,C)/4)-2*gamma_func(y,C))%%7)+1

print(week_chr[D])
