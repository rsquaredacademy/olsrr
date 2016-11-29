# import fitness data
fitness <- read.csv("data/fitness.csv")

# full model
model <- lm(oxygen ~ runtime + age + runpulse + maxpulse +
                weight + restpulse, data = fitness)

# test 1
m1 <- lm(oxygen ~ runtime, data = fitness)
rline(fitness$oxygen, fitness$runtime)
cline(m1)

# test 2
m2 <- lm(oxygen ~ age, data = fitness)
rline(fitness$oxygen, fitness$age)
cline(m2)

# test 3
m3 <- lm(oxygen ~ runpulse, data = fitness)
rline(fitness$oxygen, fitness$runpulse)
cline(m3)

# test 4
m4 <- lm(oxygen ~ maxpulse, data = fitness)
rline(fitness$oxygen, fitness$maxpulse)
cline(m4)

# test 5
m5 <- lm(oxygen ~ weight, data = fitness)
rline(fitness$oxygen, fitness$weight)
cline(m5)

# test 1
m6 <- lm(oxygen ~ restpulse, data = fitness)
rline(fitness$oxygen, fitness$restpulse)
cline(m6)

