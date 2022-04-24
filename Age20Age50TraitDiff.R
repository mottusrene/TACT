## This R code simulates and plots trait differences in between two ages, assuming a difference ~ 0.55 standard deviations

age = sample(1:2, 10^5, replace=T)
score = scale(.5 * age + sqrt(1 - .5^2) * rnorm(length(age)))
tapply(score, age, mean)

# Randomly create 10^4 pairs
t1t2 = data.frame(t1 = score[age==1][1:10000], t2=score[age==2][1:10000])

# Trisect the variable
cut3 <- function(x,c1,c2) cut(x,quantile(x,c(0,c1,c2,1)),1:3, include.lowest=TRUE)
cor(score, age)
score = as.numeric(cut3(score, 1/3, 2/3))
cor(score, age)
t1t2new = data.frame(t1 = score[age==1][1:10000], t2=score[age==2][1:10000])
col = sign(t1t2new$t2 - t1t2new$t1)
prop.table(table(col))

# Plot pairs with older scoring higher (green), younger scoring higher (red), or both scoring the same (grey)
col[col == -1] = "red"
col[col == 0] = "grey90"
col[col == 1] = "green"

#pdf("ageChanges.pdf", height=5, width=5)
plot(1:2, t1t2[1,], type="l", axes=F, xlab="", ylab="", col="white", ylim=c(-2.5,3.8))
for(i in 1:100) lines(1:2, t1t2[i,], type="l", col = col[i])
text(1.1, 2.8, "Age about 20")
text(1.9, 2.8, "Age about 50")
text(1.5, 3.8, "Individuals' expected trait changes", cex=1.5)
#dev.off()
