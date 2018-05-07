pageRank <- function(H, V, noIteration){
  Hnew <- H
  for(i in 1:(noIteration-1)){
    Hnew <- H %*% Hnew
  }
  finalV <- Hnew %*% V
  return(finalV)
}


pageData <- read.csv("/home/soumen/Desktop/PhDCourseWork/sample-large1.csv", header = FALSE)
pageData[is.na(pageData)] <- 0
N <- nrow(pageData)
pageMatrix <- matrix(0, nrow = N, ncol = N)

for(i in 1:N){
  v = as.numeric(pageData[i,-1])  
  #print(v)
  for(j in v){
    index <- which(pageData[,1] == j)
    pageMatrix[index, i] <- 1/length(v)
  }
}

#H <- matrix(c(0,0,1/3,0,1/2,0,1/3,0,1/2,0,0,1,0,1,1/3,0), nrow = 4, byrow = TRUE)
H <- pageMatrix
n <- nrow(H)
V <- rep(1/n, n)

noIteration <- readline(prompt="Enter the Number of Iterations: ")
noIteration <- as.integer(noIteration)

rankVector <- pageRank(H, V, noIteration)
#print(rankVector)

result <- sort(rankVector, decreasing = TRUE, index.return=TRUE)
resultIndex <- result$ix
#print("The rank of the pages:")
#print(pageData[resultIndex, 1])
write.csv(file = "pageWithRanking.csv", pageData[resultIndex, 1])

