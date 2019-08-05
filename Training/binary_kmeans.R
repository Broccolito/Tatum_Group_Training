rm(list = ls())

car = cars

car$speed[car$speed > 15] = car$speed[car$speed > 15] + 4

plot(car)

# x[x > 15] = x[x > 15] + 4
# y[y > 60] = y[y > 60] + 20

# plot(x,y,main = "Title", xlab = "speed", ylab = "distance")
# abline(lm(y~x))

# Initial x, y values
x = car$speed
y = car$dist
x1value = quantile(x,.25)
x2value = quantile(x,.75)
yvalue = mean(y)

#Define two centers
center1 = c(x1value, yvalue)
center2 = c(x2value, yvalue)

distance = function(p1, p2){
  xdist = p1[1]-p2[1]
  ydist = p1[2]-p2[2]
  dist = sqrt(xdist^2 + ydist^2)
  return(dist)
}
for (k in 1:100){
  
  xcenter1 = vector()
  xcenter2 = vector()
  ycenter1 = vector()
  ycenter2 = vector()
  
  flaglist = vector()
  for (i in 1:dim(car)[1]){
    x1 = car[i,1]
    y1 = car[i,2]
    xcenter1 = center1[1]
    ycenter1 = center1[2]
    xcenter2 = center2[1]
    ycenter2 = center2[2]
    
    dist1 = distance(p1 = c(x1,y1), p2 = c(xcenter1,ycenter1))
    dist2 = distance(p1 = c(x1,y1), p2 = c(xcenter2,ycenter2))
    
    if(dist1 > dist2){
      flag = TRUE
    }else{
      flag = FALSE
    }
    
    flaglist = c(flaglist, flag)
    
  }
  
  cluster1 = car[flaglist, ]
  cluster2 = car[!flaglist,]
  
  xcenter1 = mean(cluster1[,1])
  ycenter1 = mean(cluster1[,2])
  xcenter2 = mean(cluster2[,1])
  ycenter2 = mean(cluster2[,2])
  center1 = c(xcenter1, ycenter1)
  center2 = c(xcenter2, ycenter2)
  
}
#mark color
for (it in 1:dim(cluster1)[1]){
  points(cluster1[it,1], cluster1[it,2], col = 2, pch = 16)
}
for (it in 1:dim(cluster2)[1]){
  points(cluster2[it,1], cluster2[it,2], col = 3, pch = 16)
}
points(xcenter1,ycenter1, col = 1)
points(xcenter2, ycenter2, col = 1)

