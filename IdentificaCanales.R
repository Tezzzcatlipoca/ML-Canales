
# La otra idea sería sacar todas las palabras e identificar todas las que se repiten más de X Veces
# Hacer una variable para cada una

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

