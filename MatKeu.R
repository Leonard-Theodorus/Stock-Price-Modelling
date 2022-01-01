sahamawal = 5000
miu = 0.05
sigma = 0.1
deltaT = 1/250
epsilon = rnorm(1,mean = 0, sd = 1)
hargasaham <- c(sahamawal)
dailyreturn <- rep(NA,999)
dailylogreturn <- rep(NA,999)
index <- c(1)
index2 <-c()

for (i in 2:1000){
  productpangkat = (miu-(sigma*sigma)/2) * deltaT + 
    sigma * rnorm(1,mean = 0, sd = 1) * 
    sqrt(deltaT)
  sahamakhir = hargasaham[i-1] * exp(productpangkat)
  print(sahamakhir)
  hargasaham[i] = sahamakhir
}

for(w in 1 : 999){
  dailylogret = (hargasaham[w+1] - hargasaham[w]) / hargasaham[w]
  print(dailylogret)
  dailylogreturn[w] = dailylogret
}

for(q in 1 : 999){
  dailyret = hargasaham[q+1] - hargasaham[q]
  print(dailyret)
  dailyreturn[q] = dailyret
}
for(g in 1 : 1000){
  index[g] <- c(g)
}
for (j in 1 : 999){
  index2[j] <- c(j)
}
print(hargasaham)

df <- data.frame(index,hargasaham)
ggplot(df, aes(x = factor(index), y = hargasaham)) + geom_point() +
  scale_x_discrete(breaks = index[c(T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)])+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  xlab("t") + ylab("Harga Saham") + ggtitle("Grafik Pergerakan Harga Saham")

df1 <- data.frame(index2,dailyreturn)
ggplot(df1, aes(x = factor(index2), y = dailyreturn)) + geom_point() +
  scale_x_discrete(breaks = index[c(T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)]) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  xlab("t") + ylab("Daily Return") + ggtitle("Plot Daily Return")

df2 <- data.frame(index2,dailylogreturn)
ggplot(df2, aes(x = factor(index2), y = dailylogreturn)) + geom_point() +
  scale_x_discrete(breaks = index[c(T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)]) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  xlab("t") + ylab("Daily Log Return") + ggtitle("Plot Daily Log Return")

hist(dailylogreturn,
     main = 'Histogram Daily Log Return',
     freq = FALSE,
     col = 'green',
     ylab = 'Kepadatan',
     las = 1,
     breaks = 12)
x <- seq(-3,3,length = 999)
plot(x,dnorm(x,mean = 0, sd = 1), type = 'l', ylab = 'Kepadatan', main = 'Kurva Fungsi Kepadatan Peluang
     Distribusi Normal')

sigmakuadrat = sigma * sigma
meanteoritis = (miu-sigmakuadrat/2) * 1
variansiteoritis = sigmakuadrat
print(meanteoritis)
print(variansiteoritis)

meanempiris = mean(dailylogreturn)
variansiempiris = var(dailylogreturn)

print(meanempiris)
print(variansiempiris)


