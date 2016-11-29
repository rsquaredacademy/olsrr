model <- lm(formula = oxygen ~ runtime + age + runpulse + maxpulse + weight + 
                restpulse, data = fitness)

k <- stepaic_b(model)
k <- stepaic_b(model, details = T)
plot(k)
