library(nnet)

datos = data.frame(
  estudio = c(2,7,1,0,5,3,4,5,7), #input
  suenio = c(10,7,15,3,5,2,8,7,7), #input
  aprueba = c(0,1,0,0,1,1,1,0,0) #output

)
View(datos)

##Escalar de datos/normalizar (solo los predictores)
datos$estudio = scale(datos$estudio)
datos$suenio = scale(datos$suenio)

##Creamos el modelo 
modelo = nnet(aprueba ~ estudio + suenio, data=datos,
   size=3, maxit=500, decay=0.01, linout=FALSE)

# Ver los pesos de la red
print(modelo)

# Hacer predicciones
predicciones = predict(modelo, datos, type = "raw")
print(predicciones)

# Comparar con los valores reales
tabla = table(Predicho = predicciones, Real =
                datos$aprueba)

print(tabla)

print(modelo)

# Medir precisión
Precision = sum(diag(tabla)) / sum(tabla)
cat("Precisiónn del modelo:", round(Precision * 100, 2), "%\n")
