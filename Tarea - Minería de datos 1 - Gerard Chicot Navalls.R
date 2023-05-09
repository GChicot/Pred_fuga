##1) Lectura e inspección de datos. Tipo de variables, distribuciones, valores extraños, etc.

getwd()
setwd("C:/Users/xikix/Downloads")

source("Funciones_R.R")

datos <- readRDS("FugaClientes_Training.RDS")

paquetes(c('questionr','psych','car','corrplot','ggplot2',"gridExtra",
           'kableExtra','dplyr','DMwR2','lmSupport','pROC','caret'))

str(datos)

names(datos)

summary(datos)

sapply(Filter(is.numeric, datos),function(x) length(unique(x))) 

psych::describe(Filter(is.numeric, datos))
psych::describe(Filter(is.factor, datos))

listaHist <- dfplot_his(datos)

gridExtra::marrangeGrob(listaHist, nrow = 3, ncol = 2)


##2) Depuración de los datos.

varObjBin <- datos$Fuga

input <- as.data.frame(datos[,-(21)])
input$ID <- as.numeric(datos$ID)


str(input)
summary(input)

sapply(Filter(is.numeric, input),
       function(x) atipicosAmissing(x)[[2]]/nrow(input))

       
t <- data.frame(sort(
  round(sapply(Filter(
    is.numeric, input), function(nOut) atipicosAmissing(
    nOut)[[2]])/nrow(input)*100,3), decreasing = T))
names(t) <- "% Outliers por variable"

t

outlier.scores <- lofactor(na.omit(Filter(is.numeric, input)), k=20)
plot(density(outlier.scores))

outliers <- order(outlier.scores, decreasing = T)[1:20]
(na.omit(Filter(is.numeric,input))[outliers,]->out5)

out5$ID -> ID_out5

data.frame(t(round(apply(na.omit(Filter(is.numeric, input)),2,mean),3)))

data.frame(t(round(apply(na.omit(Filter(is.numeric, input)),2,median),3)))

input[,as.vector(which(sapply(
  input, class)=="numeric"))]<-sapply(Filter(
    is.numeric, input),function(x) atipicosAmissing(x)[[1]])

input[input$ID %in% ID_out5,]

##

corrplot(cor((Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "number",type = "upper")

psych::scatter.hist(input$FacturaTotal,input$FacturaMes*input$Antig.fc.edad)

input[is.na(input$FacturaTotal),] #--> Qué pasa aquí?? 

input[is.na(input$FacturaMes),] #--> Qué pasa aquí?? 

input[is.na(input$Antig.fc.edad),] #--> Qué pasa aquí?? 

input[is.na(input$FacturaMes),]$FacturaMes <- input[is.na(input$FacturaMes),]$FacturaTotal/input[is.na(input$FacturaMes),]$Antig.fc.edad 

##

input$prop_missings<-apply(is.na(input),1,mean) # Por observación
summary(input$prop_missings)

input %>% arrange(desc(prop_missings)) %>% slice_head(n=5)

input %>% filter(prop_missings< 0.5) %>% select(!(names(
  prop_missingsVars)[prop_missingsVars>0.5]))-> imput 

varObjBin<-varObjBin[input$prop_missings<0.5] 

input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(
  Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio"))

input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(
  Filter(is.factor, input),function(x) ImputacionCuali(x,"aleatorio"))

input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(
  input[,as.vector(which(sapply(input, class)=="character"))] , factor)

summary(input)

if (any(is.na(input))){
  input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(
    Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio"))
  summary(input)
}

saveRDS(cbind(varObjBin,input),"datosVinoDep.RDS")

##3) Estudio de variables y relaciones con la variable objetivo

datos2 <- readRDS("C:/Users/xikix/Downloads/datosFugaDep.RDS")

varObjBin<-datos2$varObjBin
input<-datos2[,-(1)]

input$aleatorio<-runif(nrow(input))
input$aleatorio2<-runif(nrow(input))

graficoVcramer(input,varObjBin)

h1<-hist_targetbinaria(input$Contrato,varObjBin,"Contrato")
h2<-hist_targetbinaria(input$Antig.fc.edad,varObjBin,"Antig.fc.edad")
h3<-hist_targetbinaria(input$Int_serv,varObjBin,"Int_serv")
h4<-hist_targetbinaria(input$MetodoPago,varObjBin,"MetodoPago")
h5<-hist_targetbinaria(input$FacturaMes,varObjBin,"FacturaMes")
h6<-hist_targetbinaria(input$Fact_sinPapel,varObjBin,"Fact_sinPapel")
h7<-hist_targetbinaria(input$FacturaTotal,varObjBin,"FacturaTotal")
h8<-hist_targetbinaria(input$Seguridad,varObjBin,"Seguridad")
h9<-hist_targetbinaria(input$Soporte_tecnico,varObjBin,"Soporte_tecnico")
h10<-hist_targetbinaria(input$PersCargo,varObjBin,"PersCargo")

marrangeGrob(list(h1,h2,h3,h4),nrow = 2,ncol = 2)


###4) Modelado manual

todo<-data.frame(input, varObjBin)

freq(todo$varObj)

names(todo)

set.seed(123456)
trainIndex <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]
modeloInicial<-glm(varObjBin~.,data=data_train[,-c(1)],family=binomial)
summary(modeloInicial)


table(todo$Contrato,todo$varObjBin)
table(data_train$Contrato,data_train$varObjBin)

pseudoR2(modeloInicial,data_train,"varObjBin")
pseudoR2(modeloInicial,data_test,"varObjBin")
modeloInicial$rank
impVariablesLog(modeloInicial,"varObjBin") 

modelo2<-glm(varObjBin~Contrato+Antig.fc.edad+Seguridad+Telf_serv+Fact_sinPapel+
               Soporte_tecnico+Mayor65+Int_serv+VariasLineas+CopiaSeguridad+
               FacturaMes+FacturaTotal,
             data=data_train,family=binomial)
summary(modelo2)

pseudoR2(modelo2,data_train,"varObjBin")
pseudoR2(modelo2,data_test,"varObjBin")
modelo2$rank
impVariablesLog(modelo2,"varObjBin") 

modelo3<-glm(varObjBin~Contrato+Antig.fc.edad+Seguridad+Telf_serv+Fact_sinPapel+
               Soporte_tecnico+Mayor65+Int_serv+VariasLineas+CopiaSeguridad+
               FacturaMes+FacturaTotal+MetodoPago,
             data=data_train,family=binomial)
summary(modelo3)

pseudoR2(modelo3,data_train,"varObjBin")
pseudoR2(modelo3,data_test,"varObjBin")
modelo3$rank


modelo4<-glm(varObjBin~Contrato+Antig.fc.edad+Seguridad+Telf_serv+Fact_sinPapel+
               Soporte_tecnico+FacturaMes+Mayor65,
             data=data_train,family=binomial)
summary(modelo4)
pseudoR2(modelo4,data_train,"varObjBin")
pseudoR2(modelo4,data_test,"varObjBin")
modelo4$rank


modelo5<-glm(varObjBin~Contrato+Antig.fc.edad+Seguridad+Telf_serv+Fact_sinPapel+
               Soporte_tecnico+FacturaMes+FacturaTotal+Mayor65,
             data=data_train,family=binomial)
summary(modelo5)
pseudoR2(modelo5,data_train,"varObjBin")
pseudoR2(modelo5,data_test,"varObjBin")
modelo5$rank

modelo6<-glm(varObjBin~Contrato+Antig.fc.edad+Seguridad+Telf_serv+Fact_sinPapel+
               Soporte_tecnico+Mayor65+Int_serv, data=data_train,family=binomial)
summary(modelo6)
pseudoR2(modelo6,data_train,"varObjBin")
pseudoR2(modelo6,data_test,"varObjBin")
modelo6$rank


##5) Modelado por selección de variables (clásica, aleatoria, Lasso..)

null<-glm(varObjBin~1, data=data_train, family = binomial) #Modelo minimo
full<-glm(varObjBin~., data=data_train, family = binomial) #Modelo maximo, le quitamos las transformaciones

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), trace=0, direction="both")
summary(modeloStepAIC)

psr_clas1<-pseudoR2(modeloStepAIC,data_test,"varObjBin")

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), trace=0, direction="backward")
summary(modeloBackAIC)

psr_clas2<-pseudoR2(modeloBackAIC,data_test,"varObjBin") #mismo modelo

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), trace=0, direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)

psr_clas3<-pseudoR2(modeloStepBIC,data_test,"varObjBin")

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), trace=0, direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)

psr_clas4<-pseudoR2(modeloBackBIC,data_test,"varObjBin")
param_Clas<-c(modeloStepAIC$rank,modeloBackBIC$rank,
              modeloStepBIC$rank,modeloBackBIC$rank)

psR_Clas<-c(psr_clas1,psr_clas2,psr_clas3,psr_clas4)
tibble(Modelo=c('StepAIC','BackAIC', 'StepBIC', 'BAckBIC'),
       parametros=param_Clas,
       pseudoR=psR_Clas)


##6) Comparación de modelos por validación cruzada repetida

auxVarObj<-todo$varObjBin
todo$varObjBin<-make.names(todo$varObjBin) 

total<-c()
modelos<-sapply(list(modeloInicial,modelo2,modelo3,modelo4,modelo5,modelo6),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = todo,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
boxplot(roc~modelo,data=total,main="Área bajo la curva ROC") 

aggregate(roc~modelo, data = total, mean) 

aggregate(roc~modelo, data = total, sd) 

todo$varObjBin<-auxVarObj

modeloInicial$rank
modelo2$rank 
modelo3$rank
modelo4$rank
modelo5$rank
modelo6$rank


##7) Elección justificada del modelo final


## Mi elección es el model 4 ya que su PseudoR2 es de los más elevados y con el menor número de interacciones
## Si visualizamos el ROC todos estan entre el 0.83 y el 0.84, el model 4 es el que tiene los parámetros más
## elevados con menos interacciones. A nivel de desviación no es el más alto pero al diferencia es muy poca.


##8) Evaluación e interpretación de parámetros (¿Cuáles son las variables que más 
##influyen? ¿En qué sentido lo hacen?
  
## 
## 
## 
## 

                                               
##9) Búsqueda del punto de corte óptimo para la probabilidad estimada

hist_targetbinaria(predict(modelo5, newdata=data_test,type="response"),data_test$varObjBin,"probabilidad")
sensEspCorte(modelo5,data_test,"varObjBin",0.5,"1")
sensEspCorte(modelo5,data_test,"varObjBin",0.75,"1")
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo5,data_test,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)

plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]
sensEspCorte(modelo5,data_test,"varObjBin",0.49,"1")

sensEspCorte(modelo5,data_test,"varObjBin",0.76,"1")

coef(modelo5)

pseudoR2(modelo4,data_train,"varObjBin")

pseudoR2(modelo4,data_test,"varObjBin")

roc(data_train$varObjBin, predict(modelo5,data_train,type = "response"), direction="<")
roc(data_test$varObjBin, predict(modelo5,data_test,type = "response"), direction="<")

sensEspCorte(modelo5,data_train,"varObjBin",0.76,"1")

sensEspCorte(modelo5,data_test,"varObjBin",0.76,"1")

pred_test<-factor(ifelse(predict(modelo5,data_test,type = "response")>0.76,1,0))

table(pred_test)

table(data_test$varObjBin)

confusionMatrix(pred_test,data_test$varObjBin, positive = '1')


##10) Predicción para los datos de test (utilizando el punto de corte óptimo hallado!!)

modeloC<-glm(formula(modelo4),
             data=todo,family=binomial)
summary(modeloC)

epiDisplay::logistic.display(modeloC)

##11) Construcción del dataset de entrega con el ID y Fuga_pred

