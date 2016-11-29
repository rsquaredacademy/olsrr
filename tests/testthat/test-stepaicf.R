model <- lm(formula = oxygen ~ runtime + age + runpulse + maxpulse + weight + 
            restpulse, data = fitness)

k <- stepaic(model)
k <- stepaic(model, details = T)
plot(k)
