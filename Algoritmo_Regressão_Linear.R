library(ggplot2) 
# Importa pacote ggplot2
peso <- c(45,50,60,55,58,56,48,53)
altura <- c(1.54,1.56,1.65,1.60,1.65,1.63,1.58,1.59)
plot(peso, altura)
lm(altura ~ peso)
# Executar lm antes para obter amostras dos coeficientes angular e linear.
# O valor “Intercept”, que é o coeficiente linear, e o coeficiente angular 
# é apresentado junto a sua respectiva variável, no caso a peso. 
# Realiza o ajuste da reta ou modelo de regressão linear no R é a lm().
abline(1.200575, 0.007519)
# Adiciona linha reta ao plot

ggplot(mapping = aes(peso, altura)) +
  geom_point() +
  geom_smooth(method = "lm")
# Função aes(): que receberá a indicação das variáveis a serem utilizadas 
# nos respectivos eixos, com a indicação do visual a ser utilizado, como cor, 
# formas (pontos, triângulos, quadrados, etc.), tamanho, etc.

# Função geom_(): definir o tipo de gráfico que iremos utilizar.
# Função geom_smooth(): geom_smooth() – Meios condicionais suavizados.
# Função geom_point(): Pontos.
retas <- ggplot(mapping = aes(peso, altura)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") + 
  geom_hline(yintercept = mean(altura))
# Função geom_hline(): cria uma Reta de Interceptação do Eixo Y.
# Função geom_segment(): Segmentos de linhas e curvas.
retas
retas +
  geom_segment(aes(x = peso, y = altura,
                   xend = peso, yend = mean(altura)), color="red")
retas +
  geom_segment(aes(x = peso, y = altura,
                   xend = peso, yend = predict(lm(altura ~ peso))), color="red")
SQt = sum((mean(altura) - altura)**2)
# Função mean(): calcula a Média.
# Função data.frame(): cria coleções de variáveis com propriedades de matrizes
# e listas usadas como estruturas de dados pelo software. 
SQres = sum((predict(lm(altura ~ peso)) - altura)**2)
R2 = (SQt - SQres) / SQt
summary(lm(altura ~ peso))
# Função summary(): podemos obter resumos na linguagem R, o que se aplica 
# também a objetos criados através da função lm().
predict(lm(altura ~ peso))
pesos <- data.frame(peso = c(48, 51, 62))
predict(lm(altura ~ peso), pesos)
# Função predict(): Esta função é utilizada para realizar previsões.