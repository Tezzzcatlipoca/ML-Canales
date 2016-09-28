
# La otra idea sería sacar todas las palabras e identificar todas las que se repiten más de X Veces
# Hacer una variable para cada una
library(stringr)

training<-read.csv("C:/Users/franro04/Documents/Work Log/z9. Sep 2016/ID-Cells-Types/Git/ML-Canales/TrainingSet-Canales.csv")
texto<-paste(training$Cell_name, collapse = " ")
separadas<-str_extract_all(texto,"\\S+")
sep.array<-separadas[[1]]     # Convierte lista en Array
unicas<-unique(sep.array)

numunicas<-length(unicas)
for (i in 1:numunicas) {
     stringo<-paste0(" ",unicas[i]," ")
     apariciones<-sum(str_count(texto, pattern = stringo))
     if (i==1) {
          veces<-apariciones
     } else {
          
          veces<-c(veces,apariciones)
     }
}

tabla<-data.frame(unicas=unicas,veces=veces)
orden<-order(tabla$veces,decreasing = TRUE)
ordenada<-tabla[orden,]

# Escoger variables a ser utilizadas
importantes<-as.character(ordenada[ordenada$veces>10,'unicas'])

# Crear las variables
lineas<-dim(training)[1] # Lineas
variables<-length(importantes) # Columnas
m<-matrix(nrow=lineas,ncol=variables)
tabla<-as.data.frame(m)
names(tabla)<-importantes

# Contar las apariciones en cada celda
for (vars in 1:variables) {
     for (lins in 1:lineas) {
          if(importantes[vars]=='+'){importantes[vars]<-'\\+'}
          stringo<-paste0("( ",importantes[vars]," )|(^",importantes[vars]," )|(",importantes[vars],"$)")
          apariciones<-sum(str_count(training$Cell_name[lins], pattern = stringo))
          if(apariciones>0) {
               tabla[lins,vars]<-TRUE
          } else {
               tabla[lins,vars]<-FALSE
          }
     }
}

### CREA EL MODELO LINEAL
library(caret)
# Crear un ID para cada ShopType
tipos<-training$ShopType
tipos.unicos<-unique(tipos)
tipos.unicos<-sort(tipos.unicos)
ids<-seq(1,length(tipos.unicos))
tabla.ids<-data.frame(type_id=ids,ShopType=tipos.unicos)

# Crear tabla a utilizar en creación del modelo
train.ids<-merge(training,tabla.ids,by="ShopType",all.x=TRUE)
# Se necesita una nueva tabla con nombres diferentes (válidos)
tabla2<-tabla
nums<-dim(tabla2)[2]
names(tabla2)<-paste0("X", seq(1,nums))
#train.final<-cbind(ID=train.ids$ShopType,tabla2)
train.final<-cbind(ID=train.ids$type_id,tabla2)

# IMPORTANTE!!!! ID debe ser de tipo FACTOR
train.final<-as.factor(train.final)

# Crear modelo lineal
fit<-train(ID ~ .,method="rpart",data=train.final)

## -------------------------- TESTING ----------------------------------

# Abrir archivo de Testing
testing<-read.csv("C:/Users/franro04/Documents/Work Log/z9. Sep 2016/ID-Cells-Types/Git/ML-Canales/TestingSet.csv")

# Generar las mismas variables para el archivo de testing

lineas<-dim(testing)[1] # Lineas
variables<-length(importantes) # Columnas
m<-matrix(nrow=lineas,ncol=variables)
tabla.test<-as.data.frame(m)
names(tabla.test)<-importantes

# Contar las apariciones en cada celda
for (vars in 1:variables) {
     for (lins in 1:lineas) {
          if(importantes[vars]=='+'){importantes[vars]<-'\\+'}
          stringo<-paste0("( ",importantes[vars]," )|(^",importantes[vars]," )|(",importantes[vars],"$)")
          apariciones<-sum(str_count(testing$Cell_name[lins], pattern = stringo))
          if(apariciones>0) {
               tabla[lins,vars]<-TRUE
          } else {
               tabla[lins,vars]<-FALSE
          }
     }
}

# Agregar las variables creadas
tabla.test2<-tabla.test
nums<-dim(tabla.test2)[2]
names(tabla.test2)<-paste0("X", seq(1,nums))
test.final<-cbind(Nome=testing$Cell_name,tabla.test2)

# Obtener respuesta
out<-predict(fit,test.final)

#nombres2<-c('Cell_name',importantes)
test.final.2<-test.final
names(test.final.2)<-nombres2
write.csv(test.final.2,file = "Variables.csv", row.names = FALSE)

# También, se puede buscar cada una de las palabras contenidas en los canales que buscamos


# Aquí irían todas las palabras identificadas por mí
training$CK<-0
training$CK[grepl("CK|ck|Ck",training$Cell_name)>0]<-1

training$AS<-0
training$CK[grepl(" AS ",training$Cell_name)>0]<-1
training$CK[grepl("^AS ",training$Cell_name)>0]<-1

training$Indep<-0
training$Indep[grepl("indep|INDEP|Indep",training$Indep)>0]<-1

training$NCK<-0
training$NCK[grepl(" [0-9]+[ ]{0,1}(CK|ck|Ck)")>0]<-

#gregexpr
#substr(aaa,regexec("[0-9]+[ ]{0,1}(CK|ck|Ck)",aaa)[[1]][1]-3,regexec("[0-9]+[ ]{0,1}(CK|ck|Ck)",aaa)[[1]][1]+1)

