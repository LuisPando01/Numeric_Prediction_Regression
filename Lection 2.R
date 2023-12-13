insurance <- read.csv("/Users/USUARIO.DESKTOP-7RETBCV/Documents/
                      MEGA/ESTUDIOS PERSONALES/INTERCAMBIO/Goethe Universität/
                      CURSOS/Informationssysteme für das Management/DataSets/
                      insurance.csv",stringsAsFactors = TRUE)

#DATOS Y DISTRIBUCIÓN
str(insurance)
summary(insurance)
summary(insurance$expenses)
hist(insurance$expenses)

#EXPLORAR VARIABLES CATEGÓRICAS
table(insurance$region)

#CORRELACIÓN ENTRE VARIABLES
cor(insurance[c("age","bmi","children","expenses")])

#EXCELENTE DIAGRAMA PARA EXPLORAR TODAS LAS CORRELACIONES
pairs.panels(insurance[c("age","bmi","children","expenses")])
#Mientras más redondo el círculo de la imagen, menos correlación habrá



#REGRESIÓN LINEAR MÚLTILPE
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,data =
                  insurance)
ins_model

#EMPEZAR A PROBAR EL MODELO
set.seed(123)
#SE TOMA UN 5% DE LAS FILAS COMO TEST
test_sample<-sample(nrow(insurance),0.05*nrow(insurance))
#DATASET DE TEST
insurance_test<-insurance[test_sample,]
str(insurance_test)
head(insurance_test)
#row 415
str(test_sample)
insurance_test[1,]

#PRIMERA PRUEBA DEL MODELO
predict(ins_model, insurance_test[1,])
#SE CREA VECTOR CON LAS PREDICCIONES SEGÚN EL MODELO Y LAS FILAS TOMADAS ALEATORIAMENTE
ins_pred <- predict(ins_model, insurance_test)
#SE QUITA EL NOMBRE DE FILA
ins_p <- unname(ins_pred)

#CORRELACIÓN ENTRE LOS GASTOS REALES Y LA PREDICCIÓN
cor(insurance_test$expenses,ins_p)

#GRÁFICO COMPARATIVO ENTRE PREDICCIÓN Y REALIDAD
plot(insurance_test$expenses, type="l", main="Linear Regression:
Predicted versus Actual Expenses", xlab="record",
     ylab="expenses in $",axes=FALSE, frame.plot=TRUE)
axis(1, at=c(0,10,20,30,40,50,60),labels=FALSE)
lines(ins_p,col="blue",lty=2)
legend("topleft",inset=.02,c("actual","predicted"),lty=c(1,2),
       col=c("black","blue"))


#EVALUAR LOS RESULTADOS DEL MODELO
summary(ins_model)
#La edad y el BMI (a partir de 30 años probablemente) están más relacionados al resultado
#Fumar también está relacionado, pero fumar y ser obeso puedo ser aún peor

###VAMOS A MEJORAR EL MODELO###
#Introducir columna nueva por la edad (ya que incrementa como una F cuadrática)
insurance$age2 <- insurance$age^2

#Introducir columna nueva: si tiene o no más de 30 años
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

#NUEVO MODELO MEJORADO
ins_model2 <- lm(expenses ~ age + age2 + children + bmi +
                   sex + bmi30*smoker + region, data = insurance)
#Verificar modelo
summary(ins_model2)

#Al ver los resultados, se ve que el modelo mejoró
head(insurance)
predict(ins_model2, insurance[3,])
