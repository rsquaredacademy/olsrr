model <- lm(formula = oxygen ~ runtime + age + runpulse + maxpulse + weight + 
                restpulse, data = fitness)

k <- stepaic_bo(model)
k <- stepaic_bo(model, details = T)
plot(k)
