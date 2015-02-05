y<-sample(x = 1:20,size = 100,replace = TRUE)
e<-rnorm(n = 100,mean = 3,sd = 1)
x<-((2*(y-6))+e)

SampleData<-data.frame(y,x,e)
str(SampleData)

regSample<-glm(formula = y~x,data = SampleData)
str(regSample)

CV<-cv.glm(data = SampleData,glmfit = regSample,K = 10)
CV$delta

cv.error=rep(0,10)
for (i in 1:10){
  glmfit=glm(y~poly(x,i),data = SampleData)
  cv.error[i]<-cv.glm(data = SampleData,glmfit = glmfit,K = 10)$delta[1]
}