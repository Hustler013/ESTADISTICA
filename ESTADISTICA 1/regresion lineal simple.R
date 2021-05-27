#SESION 6#######################################################3
#REGRESION LINEAL SIMPLE (2 VARIABLES)
#(QUIERO PREDECIR UNA VARIABLE CUANTITATIVA CONTINUA)

#diagrama de dispercion - verificasi hay relacion entre las variables (creciente o decreciente)

#prediccion de ventas
#vericar ventas trimestrales re restaurantes 
#estudiantil

x = c(2,6,8,8,12,16,20,20,22,26) #vector de poblaci???n de estudiantes / independiente
y = c(58,105,88,118,117,137,157,169,149,202) #vector de ventas / dependendiente
plot(x,y)

#medias
xmed = mean(x)
ymed = mean(y)

#(diferencias/restas) vector menos las medias 
x_xmed = x-xmed
y_ymed = y-ymed

#producto
prod = x_xmed*y_ymed

#(sumatorias) s1 y s2 
s1 = sum(prod)
s2 = sum(x_xmed^2)

#(coeficientes de regresion= b1 y b0 
b1 = s1/s2 
b0 = ymed-b1*xmed 

#para la formula y=b0 + b1x + epsilon
y_est = b0+b1*x #yo predigo que esto va a suceder (lo comparo mi estimacion de Y con Y)

#SESION 7#########################################################
#ERROR
error = y - y_est #error/residual (valor real menos mi estimacion)

SCE = sum(error^2) #SCE sumatoria de error elevadas al cuadrado
STC = sum(y_ymed^2) #SUMATOTAL DE CUADRADOS
SCR = STC-SCE #Suma de cuadrados debido a la regresion

#BONDAD DE AJUSTE
r2 = SCR/STC #0.9027 entre 0 y 1 (mas cerca de 1 dice que no tengo error)

#COEFICIENTE DE CORRELACION #TRESHOLD/UMBRAL
RXY = sign(b1)*sqrt(r2) #+0.95 = +1(directamente proporcional) 0 -1(inversamente proporcional)

#PRUEBAS DE SIGNIFICANCIA
#VARIANZA
n = length(x)
ECM = SCE/(n-2) #191.25 ERROR CUADRADO MEDIO/varianza
s = sqrt(ECM) #13.82932 Residual standard error

#SESION 8###################################################################
#PRUEBA T
sb1 = s/sqrt(s2) #0.5802652
t = b1/sb1 #8.616749  valor de t

#si rechazo la hipotesis nula
#no mirar tabla t (0.01 nivel de significancia/2)(8 grados de libertad)
qt(0.01/2,8, lower.tail = FALSE) #(3.355 buscamos en la tabla = 0.005) si (cola superior)
#por lo tanto t<0.005 (8.617<0.005) 
8.617<0.005 #=FALSE por lo tanto rechazo h0 hipotesis nula y acepto la hipotesis alternativa
#o (p value del b1) (Pr(>|t|)2.55e-05) y alfa=0.01
2.55e-05 < 0.01 #TRUE por lo tanto rechazo hipotesis nula
#intervalo de confianza (para ver si es significativo)
#5+-3.355((0.5803)) = 5 +- 1.19
#intervalo 3.05 a 6.95

#Prueba F
CMR = SCR/1 #14200 donde 1 son los numeros de variable independientes (x , x1,x2,x3...etc)
F = CMR/ECM #74.24837 (F-stadistic) Si se acerca al 1 se acepta hipotesis nula| si es mayor a 1 se acepta hipotesis alternativa
#veo tabla F y encuentro que 11.26 < 74.24 REZHACO HIPOTESIS NULA Y ACEPTO HIPOTESIS ALTERNATIVA
qf(0.001,1,8, lower.tail = FALSE) #11.26

#ANOVA en funciones directas
anova(reg)




#########################FUNCIONES DIRECTAS##########################################
#resumen de todo lo hecho
x = c(2,6,8,8,12,16,20,20,22,26) #vector de poblaci???n de estudiantes
y = c(58,105,88,118,117,137,157,169,149,202) #vector de ventas
plot(x,y)
reg = lm(y~x)
abline(reg)
summary(reg)
anova(reg)
#residuals son los errores (min=-21 max=18)
#coefficients: (ESTIMATE b0=60 b1=5)
#(BONDAD DE AJUSTE/Multiple R-squared=0.9027)
#Residual standard error = 13.83 on 8 degrees of freedom (8 grados de libertad n-2)
#PRUEBA T (b0 y Std. Error = 0.5803)
#Valor de t (area o probabilidad)(b1 t value 8.617)
#p value (Pr(>|t|)=2.255e-05 < alfa) = TRUE rechazo hipotesis nula
#PRUEBA F F-statistic: 74.25 (on 1 and 8 DF veo 1 y 8 en la tabla F) o hago qf(0.001,1,8, lower.tail = FALSE)
qf(0.01,1,8, lower.tail = FALSE) # 11.26 < 74.25 = TRUE (entonces rechazo h0 y acepto ha)
#correlacion
cor(x,y) # 0.950123
##################COMO SELECCIONAR LOS VALORES######
reg$coefficients[1] #b0 = 60
reg$coefficients[2] #b1 = 5
#por lo cual podemos calcular y_est
#y_est = b0+b1*x
y_est2 = reg$coefficients[1] + reg$coefficients[2] * x #X pondria los valores de los estudiantes que comen pizza EJM:6
y_est2 = reg$coefficients[1] + reg$coefficients[2] * 6 #90mil ventas tendria por 6mil estudiantes
#####################################################################################



