# Ambiente local ---------------------------------------------------------------------------------------------------------------------------
library(MASS)
library(earth)
library(ggplot2)
library(magrittr)

# Cargar datos -----------------------------------------------------------------------------------------------------------------------------
data(Boston)
Boston %>% head

# Declarar modelo lineal y MARS ------------------------------------------------------------------------------------------------------------
lreg <- lm(medv ~ lstat, data=Boston)
mars <- earth(x=Boston$lstat, y=Boston$medv)

# Predicciones -----------------------------------------------------------------------------------------------------------------------------
y_lreg <- predict(lreg, Boston)
y_mars <- predict(mars, Boston$lstat)

df <- data.frame(Boston$lstat, Boston$medv, y_lreg, y_mars)
colnames(df) <- c('x','y','lreg','mars')

# Resultados -------------------------------------------------------------------------------------------------------------------------------
ggplot(data=df) + geom_point(mapping=aes(x=x, y=y), alpha=0.3) +
  geom_line(mapping=aes(x=x, y=y_lreg), color='blue') +
  geom_line(mapping=aes(x=x, y=y_mars), color='green') +
  labs(title='Regersión lineal VS MARS', x='Hogares pobres en vecindad (%)', y='Precio de casa (USD, miles)') +
  theme_light()

# Comparación de RMSE ----------------------------------------------------------------------------------------------------------------------
rmse_lreg <- sum((df$y - df$lreg)^2) / nrow(df)
rmse_mars <- sum((df$y - df$mars)^2) / nrow(df)

print(paste0('RMSE de regresión lineal: ', rmse_lreg %>% round))
print(paste0('RMSE de MARS: ', rmse_mars %>% round))