x = seq(-10, 10, 1)
y = x ^ 2

plot(x, y, type = "l")

plot(cars$dist ~ cars$speed, main = "title", xlab = "speed", sub = "subtitle", xlim = c(10,20), 
     col = round(cars$speed, 0), pch = 16, cex = 2)
abline(lm(data = cars, dist ~ speed))
sm = summary(lm(data = cars, dist ~ speed))
eqn = paste0("y = ", round(sm$coefficients[2,1], 2), "x + ", round(sm$coefficients[1,1], 2))
legend(15,40, eqn)

library(ggplot2)

car = cars
car = cbind.data.frame(car, color = seq(1, dim(car)[1]))

car$color = as.factor(car$color)

ggplot(data = car, aes(x = speed, y = dist)) + 
  geom_point(aes(x = speed, y = dist, color = color)) + 
  geom_smooth(method = "loess") + 
  ggtitle("title", sub = "sub") 
  
  