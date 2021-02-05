#Libraries----
library(ineq)
library(ggplot2)
library(broom)

set.seed(2020)

#Funcion----
ABM2020<-function(Iteraciones,Time,POP,SIw,SIor,SIm,SWw,SWor,SWm,umbralinc,tasabaja,tasainc,tu,robjetivo,uobjetivo,expectadap,paciencia,inventariodeseado,ajusteprecios,renuncias,exitoinn,exitoimi,aversionc,aversionk){
  
  #Matrices para recabar la informacion en cada iteracion de Time periodos----
  
  #precios
  PC<<-matrix(nrow = Iteraciones,ncol = Time)
  PK<<-matrix(nrow = Iteraciones,ncol = Time)
  #umbral
  UMBRAL<<-matrix(nrow = Iteraciones,ncol = Time)
  #Tasa
  TASA<<-matrix(nrow = Iteraciones,ncol = Time)
  #Producto real
  Y<<-matrix(nrow = Iteraciones,ncol = Time)
  #Producto nominal
  GDP<<-matrix(nrow = Iteraciones,ncol = Time)
  #Inflacion
  PI<<-matrix(nrow = Iteraciones,ncol = Time)
  #Deficit Fiscal
  DF<<-matrix(nrow = Iteraciones,ncol = Time)
  #Deficit Fiscal sobre producto
  DFGDP<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gini
  IG<<-matrix(nrow = Iteraciones,ncol = Time)
  #gini2
  IG2<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gini despues de impuestos
  IGAT<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gini despues del impuesto al ingreso
  IGAIT<<-matrix(nrow = Iteraciones,ncol = Time)
  #Produccion de bienes de consumo
  YC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Produccion de bienes de capital
  YK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Inversion
  IN<<-matrix(nrow = Iteraciones,ncol = Time)
  #Consumo
  CO<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto al ingreso
  TI<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto al ingreso real
  TIR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto a la riqueza
  TW<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto a la riqueza real
  TWR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto a las ganancias
  TP<<-matrix(nrow = Iteraciones,ncol = Time)
  #Recaudacion impuesto a las ganancias real
  TPR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Riqueza
  W<<-matrix(nrow = Iteraciones,ncol = Time)
  #Riqueza real
  WR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Desempleo
  UN<<-matrix(nrow = Iteraciones,ncol = Time)
  #Masa salarial
  MS<<-matrix(nrow = Iteraciones,ncol = Time)
  #Masa salarial real
  MSR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Dividendos
  V<<-matrix(nrow = Iteraciones,ncol = Time)
  #Dividendos reales
  VR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Quiebras
  Q<<-matrix(nrow = Iteraciones,ncol = Time)
  #Empresas que lograron innovar
  INN<<-matrix(nrow = Iteraciones,ncol = Time)
  #Empresas que lograron imitar
  IMI<<-matrix(nrow = Iteraciones,ncol = Time)
  #Inflacion minorista
  PIC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Inflacion mayorista
  PIK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gasto publico en salarios
  MSG<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gasto publico en salarios real
  MSGR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gasto en subsidio de desempleo
  DO<<-matrix(nrow = Iteraciones,ncol = Time)
  #Gasto en subsidio de desempleo real
  DOR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Utilizacion en las firmas de consumo
  UT<<-matrix(nrow = Iteraciones,ncol = Time)
  #Deuda publica nominal
  DG<<-matrix(nrow = Iteraciones,ncol = Time)
  #Deuda publica real
  DGR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Deuda publica sobre producto
  DGGDP<<-matrix(nrow = Iteraciones,ncol = Time)
  #Proporcion de creditos rechazados
  LRTOT<<-matrix(nrow = Iteraciones,ncol = Time)
  #Proporcion de creditos rechazados a las firmas de capital
  LRC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Proporcion de creditos rechazados a las firmas de capital
  LRK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Credito vigente
  LOAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #Credito vigente a las firmas de consumo
  LOANC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Credito vigente a las firmas de capital
  LOANK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Depositos
  DEPO<<-matrix(nrow = Iteraciones,ncol = Time)
  #Depositos de las firmas de consumo
  DEPOC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Depositos de las firmas de capital
  DEPOK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Apalancamiento de los bancos
  LEVB<<-matrix(nrow = Iteraciones,ncol = Time)
  #Apalancamiento de las firmas de consumo (deuda/valor neto)
  LEVCNW<<-matrix(nrow = Iteraciones,ncol = Time)
  #Apalancamiento de las firmas de consumo (activo/deuda)
  LEVCAD<<-matrix(nrow = Iteraciones,ncol = Time)
  #Apalancamiento de las firmas de capital (deuda/valor neto)
  LEVKNW<<-matrix(nrow = Iteraciones,ncol = Time)
  #Apalancamiento de las firmas de capital (activo/deuda)
  LEVKAD<<-matrix(nrow = Iteraciones,ncol = Time)
  #activo de las firmas de consumo
  ACTC<<-matrix(nrow = Iteraciones,ncol = Time)
  #pasivo de las firmas de consumo
  PASC<<-matrix(nrow = Iteraciones,ncol = Time)
  #patrimoio de las firmas de consumo
  PATC<<-matrix(nrow = Iteraciones,ncol = Time)
  #activo de las firmas de capital
  ACTK<<-matrix(nrow = Iteraciones,ncol = Time)
  #pasivo de las firmas de capital
  PASK<<-matrix(nrow = Iteraciones,ncol = Time)
  #patrimonio de las firmas de capital
  PATK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Productividad del trabajo en las firmas de consumo
  MUFIC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Beneficios del capital OFC/K=r
  VK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Ahorro
  SH<<-matrix(nrow = Iteraciones,ncol = Time)
  #Ahorro real
  SHR<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up de las firmas de consumo
  MUPC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up de las firmas de capital
  MUPK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up de la economia
  MUP<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up sobre ventas de las firmas de consumo
  MUPSC<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up sobre ventas de las firmas de capital
  MUPSK<<-matrix(nrow = Iteraciones,ncol = Time)
  #Mark-up sobre ventas de la economia
  MUPS<<-matrix(nrow = Iteraciones,ncol = Time)
  #Ingreso de los hogares en t=5
  YH5<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares en t=25
  YH25<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares en t=50
  YH50<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=5
  YHIT5<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=25
  YHIT25<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=50
  YHIT50<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=5
  YHAT5<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=25
  YHAT25<<-matrix(nrow = Iteraciones,ncol = POP)
  #Ingreso de los hogares after tax en t=50
  YHAT50<<-matrix(nrow = Iteraciones,ncol = POP)
  #Riqueza de los hogares en t=5
  WH5<<-matrix(nrow = Iteraciones,ncol = POP)
  #Riqueza de los hogares en t=25
  WH25<<-matrix(nrow = Iteraciones,ncol = POP)
  #Riqueza de los hogares en t=50
  WH50<<-matrix(nrow = Iteraciones,ncol = POP)
  #Para controlar el tiempo
  HORA<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los trabajadores empleados
  WMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los trabajadores empleados
  WMEANR<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los operarios empleados
  WWMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los operarios empleados
  WWMEANR<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los oficinistas empleados
  WOMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los oficinistas empleados
  WOMEANR<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los investigadores empleados
  WRMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los investigadores empleados
  WRMEANR<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los manager empleados
  WMMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #salario promedio de los manager empleados
  WMMEANR<<-matrix(nrow = Iteraciones,ncol = Time)
  #consumo deseado promedio de los trabajadores empleados
  CWMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #consumo deseado promedio de los operarios empleados
  CWWMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #consumo deseado promedio de los oficinistas empleados
  CWOMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #consumo deseado promedio de los investigadores empleados
  CWRMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  #consumo deseado promedio de los manager empleados
  CWMMEAN<<-matrix(nrow = Iteraciones,ncol = Time)
  
for(i in 1:Time){
  assign(paste0('WAGE',i),matrix(ncol=POP,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('DIVH',i),matrix(ncol=POP,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('YGINI',i),matrix(ncol=POP,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('MUC',i),matrix(ncol=100,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('R',i),matrix(ncol=100,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('S',i),matrix(ncol=110,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('MU',i),matrix(ncol=110,nrow=Iteraciones),envir = .GlobalEnv)
  assign(paste0('DIVX',i),matrix(ncol=120,nrow=Iteraciones),envir = .GlobalEnv)
}

  #ITERACIONES----   
  for (it in 1:Iteraciones) {  
    
  #Parametros----
  
  
  #disenio progresivo
  taoi10<-0
  taoi20<-0.08
  umbral0<-0
  
  #tasa de crecimiento nominal del producto en estado estacionario
  gss<-0.0075
  
  #parametros de expectativas adaptativas
  lambda<-0.25
  
  #poblacion de trabajadores
  
  #operarios
  nw<-2400
  #oficinistas
  no<-1119
  #investigadores
  nr<-81
  #managers
  nm<-400
  
  #poblacion total
  POP<-nw+no+nr+nm
  
  #cantidad de firmas de consumo
  fic<-100
  #cantidad de firmas de capital
  fik<-10
  #cantidad de bancos
  fib<-10
  
  #empleo inicial en las firmas de capital
  Nk1<-500
  #empleo inicial en las firmas de consumo
  Nc1<-2500
  
  #cantidad de empleados publicos
  Ngt<-680
  
  #proporcion de operarios en las firmas de consumo
  sharecw<-0.6
  #proporcion de operarios en las firmas de capital
  sharekw<-0.6
  #proporcion de oficinistas en las firmas de consumo
  shareco<-0.3
  #proporcion de oficinistas en las firmas de capital
  shareko<-0.15
  #proporcion de investigadores en las firmas de consumo
  sharecr<-0
  #proporcion de investigadores en las firmas de capital
  sharekr<-0.15
  #proporcion de managers en las firmas de consumo
  sharecm<-0.1
  #proporcion de managers en las firmas de capital
  sharekm<-0.1
  
  #LOS VOLVi A ESCRIBIR, ME OLVIDe QUE LOS TENiA
  #participacion inicial de los operarios en el ingreso bruto
  siw0<-0.3
  #participacion inicial de los oficinistas e investigadore en el ingreso bruto
  sior0<-0.4
  #participacion inicial de los managers en el ingreso bruto
  sim0<-0.3
  
  #participacion inicial de los operarios en la riqueza antes de impuestos
  sww0<-0.3
  #participacion inicial de los oficinistas e investigadores en la riqueza antes de impuestos
  swor0<-0.4
  #participacion inicial de los managers en la riqueza antes de impuestos
  swm0<-0.3
  
  #propension a consumir de los operarios
  alfaw<-0.99
  #propension a consumir de los oficinistas e investigadores
  alfaor<-0.9815
  #propension a consumir de los managers
  alfam<-0.975
  
  #persistencia del consumo
  beta<-0.9
  
  #desempleo inicial
  u0<-0.08
  
  #productividad inicial del trabajo en las firmas de capital
  muN0<-6.67
  #productividad inicial del capital
  mu0<-1
  
  #stock de capital inicial
  k0<-40000
  
  #ratio capital/operarios
  lk<-20
  
  #partners potenciales en el mercado de consumo
  chic<-5
  #partners potenciales en el mercado de capital
  chik<-5
  #partners potenciales en el mercado de depositos
  chidep<-3
  #partners potenciales en el mercado de creditos
  chicred<-3
  #partners potenciales en el mercado de trabajo de operarios por cada vacante
  chiw<-10
  #partners potenciales en el mercado de trabajo de oficinistas por cada vacante
  chio<-10
  #partners potenciales en el mercado de trabajo de investigadores por cada vacante
  chir<-10
  #partners potenciales en el mercado de trabajo de managers por cada vacante
  chim<-10
  
  #intensidad de eleccion en el mercado de depositos
  epsilondep<-4.62
  #intensidad de eleccion en el mercado de creditos
  epsiloncred<-4.62
  #intensidad de eleccion en el mercado de consumo
  epsilonc<-4.62
  #intensidad de eleccion en el mercado de capital
  epsilonk<-13.86
  
  #participacion objetivo del inventario de las firmas
  nu<-0.1
  
  #ratio de rotaccion en el mercado de trabajo
  tita<-0.05
  
  #mark-up inicial sobre el costo unitario laboral de las firmas de consumo
  muc0<-0.318857 
  #mark-up inicial sobre el costo unitario laboral de las firmas de capital
  muk0<-0.075 
  
  #parametro de ajuste del salario de reserva y mark-up
  #media de la distribucion folded normal 1
  muFN1<-0
  #desvio estandar de la distribucion folded normal 1
  sigmaFN1<-0.015
  
  
  #trimestres desempleado para la revision del salario de reserva
  tu<-2
  tum<-2
  
  #tasa impositiva a la renta
  taopi0<-0.18
  #tasa impositiva al ingreso
  taoi0<-0.08
  #tasa impositiva a la riqueza
  taow0<-0.05
  
  #progresividad del sistema impositivo
  titaminuscula<-0
  
  #umbral inferior del DF en la regla de revision de la tasa impositiva
  def0<-0.02
  #umbral superior del DF en la regla de revision de la tasa impositiva
  def1<-0.05
  
  #parametro de ajuste en la regla de revision de la tasa impositiva
  v<-0.05
  
  #duracion de los prestamos
  eta0<-20
  #duracion de los bienes de capital
  kapa0<-20
  
  #tasa de ganancias objetivo en la funcion de inversion
  rtecho0<-0.04345
  
  #objetivo de capacidad utilizada de las firmas de consumo
  utecho0<-0.8
  
  #ponderacion de las ganancias en la funcion de inversion
  gama1<-0.015
  #ponderacion de la capacidad utilizada en la funcion de inversion
  gama2<-0.015
  
  #parametro de probabilidad de exito en la innovacion
  xiinn0<-0.015
  #parametro de probabilidad de exito en la imitacion
  xiimi0<-0.045
  
  #Ganancia de productividad de la innovacion
  #media de la distribucion folded normal 3
  muFN3<-0
  #desvio estandar de la distribucion folded normal 3
  sigmaFN3<-0.01
  
  #depositos de precaucion de las firmas como porcion de WB
  sigma<-1
  
  #porcion de las ganancias de las firmas de consumo distribuidas como dividendos
  roc0<-0.9
  #porcion de las ganancias de las firmas de capital distribuidas como dividendos
  rok0<-0.9
  #porcion de las ganancias de los bancos distribuidas como dividendos
  rob0<-0.75
  
  #tasa de interes inicial de los prestamos
  ilb0<-0.0075
  #tasa de interes inicial de los depositos
  idb0<-0.0025
  
  #ratio objetivo de capital inicial de los bancos
  CRT0<-0.18
  #ratio objetivo de liquidez inicial de los bancos
  LRT0<-0.26
  
  #aversion al riesgo de los bancos respecto a prestarle a las firmas de consumo
  stigmac<-3.9
  
  #aversion al riesgo de los bancos respecto a prestarle a las firmas de capital
  stigmak<-21.5
  
  #media de la distribucion folded normal 2
  muFN2<-0
  #desvio estandar de la distribucion folded normal 2
  sigmaFN2<-0.015
  
  #haircut sobre el valor de las firmas defaulteadas
  iota<-0.5
  
  #salario promedio inicial
  wn0<-5
  
  #subsidio de desempleo como porcion del salario del operario REVISAR SI ES EL SALARIO DEL OPERARIO O SALARIO PROMEDIO
  omega<-0.4
  
  #tasa de interes del banco central sobre los avances
  itechocb0<-0.005 
  
  #tasa de interes de los bonos
  itechob0<-0.005
  
  #precio de los bonos
  ptechob0<-1
  
  #cantidad de agentes
  Ni<-nw+no+nr+nm+fic+fik+fib+2
  
  
  #parametros de la funcion----
  
  umbral0<-umbralinc
  xiinn0<-exitoinn
  xiimi0<-exitoimi
  if(it > 1000 & it<=10000){
    if(it <=2000 ){
    umbral0<-2
    }else{
      if(it <=3000 ){
        umbral0<-3
      }else{
        if(it <=4000 ){
          umbral0<-4
        }else{
          if(it <=5000 ){
            umbral0<-5
          }else{
            if(it <=6000 ){
              umbral0<-6
            }else{
              if (it<=7000){
                xiinn0<-exitoinn-0.005
              }else{
                if (it<=8000){
                  xiinn0<-exitoinn+0.005
                }else{
                  if (it<=9000){
                    xiimi0<-exitoimi-0.005
                  }else{
                    if (it<=10000){
                      xiimi0<-exitoimi+0.005
                    }
                  }
                }
              } 
            }
          }
        }
      }
    }
  }
  
  
    
  #Matrices para trabajar----
  
  #tipo de agente
  x<-cbind(Id=c(1:Ni),"Tipo de agente"=NA)
  x<-as.data.frame(x)
  
  #firmas de consumo -> c
  x[1:fic,2]<-"c"
  #firmas de capital -> k
  x[(fic+1):(fic+fik),2]<-"k"
  #bancos -> b
  x[(fic+fik+1):(fic+fik+fib),2]<-"b"
  #operarios -> w
  x[(fic+fik+fib+1):(fic+fik+fib+nw),2]<-"w"
  #oficinistas -> o
  x[(fic+fik+fib+nw+1):(fic+fik+fib+nw+no),2]<-"o"
  #investigadores -> r
  x[(fic+fik+fib+nw+no+1):(fic+fik+fib+nw+no+nr),2]<-"r"
  #managers -> m
  x[(fic+fik+fib+nw+no+nr+1):(fic+fik+fib+nw+no+nr+nm),2]<-"m"
  #gobierno -> g
  x[(fic+fik+fib+nw+no+nr+nm+1),2]<-"g"
  #banco central -> cb
  x[(fic+fik+fib+nw+no+nr+nm+2),2]<-"cb"
  
  #nivel de produccion deseado
  yd<-matrix(ncol=Time,nrow=(fic+fik))
  #nivel de produccion
  y<-matrix(ncol=Time,nrow=(fic+fik))
  #productividad del trabajo de operarios SE MODIFICA EN FIRST PERIOD
  mun<-matrix(data=muN0,ncol = Time,nrow = fic+fik)
  #productividad del trabajo de operarios de la economia
  mufic<-vector(mode = "numeric",length = Time)
  #ventas esperadas
  se<-matrix(ncol=Time,nrow=(fic+fik))
  #ventas (en terminos reales) TIENE QUE ARRANCAR EN DATA=0
  s<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #inventario
  inv<-matrix(ncol=Time,nrow=(fic+fik))
  #demanda de operarios
  Ndw<-matrix(ncol=Time,nrow=(fic+fik+1))
  #demanda de oficinistas
  Ndo<-matrix(ncol=Time,nrow=(fic+fik+1))
  #demanda de cientificos
  Ndr<-matrix(ncol=Time,nrow=(fic+fik+1))
  #demanda de managers
  Ndm<-matrix(ncol=Time,nrow=(fic+fik+1))
  #demanda total de trabajadores
  Nd<-matrix(ncol=Time,nrow=(fic+fik+1))
  #trabajadores operarios
  Nw<-matrix(ncol=Time,nrow=(fic+fik+1))
  #trabajadores oficinistas
  No<-matrix(ncol=Time,nrow=(fic+fik+1))
  #trabajadores investigadores
  Nr<-matrix(ncol=Time,nrow=(fic+fik+1))
  #trabajadores managers
  Nm<-matrix(ncol=Time,nrow=(fic+fik+1))
  #trabajadores totales
  Nx<-matrix(ncol=Time,nrow=(fic+fik+1))
  
  
  #para las firmas de consumo es capital (bienes de uso), para las firmas de capital es mercaderia en stock
  #ver valores iniciales
  capital<-cbind(Id=c(1:(fic+fik)),Variedad=1,matrix(data=c(1,1,0,1),nrow = (fic+fik),ncol=4, dimnames = list(c(),c("k", "ud", "age", "pcompra")),byrow = TRUE),matrix(data=c(mu0, kapa0),nrow = fic+fik, ncol=2, dimnames = list(c(),c("mu", "kapa")),byrow = T)) #cada nueva variedad creara una fila superior dentro de las filas de la firma.
  #La variedad 1 dentro de cada firma sera la mas productiva, disminuyendo de productividad a medida que aumenta el numero de variedad.
  #k -> cantidad de capital
  #ud -> capacidad de utilizacion deseada
  #age -> antig?edad
  #pcompra -> precio al que se compro
  #mu -> productividad
  #kapa -> vida util
  
  
  
  #stock total de capital real
  K<-matrix(data=0,nrow = fic,ncol = Time)
  
  #Amortizacion del capital
  kamor<-matrix(data=0,ncol=Time,nrow=fic)
  
  #Stock real de capital (se descuenta la amortizacion)
  kreal<-matrix(data=1,ncol=Time,nrow=fic)
  
  #Proveedor de cada firma de consumo, es el numero de la firma de k que le vende en t
  kmarket<-matrix(nrow=fic,ncol=Time)
  kmarket[,1]<-sample(1:fik,fic,replace=TRUE)
  #costo unitario asociado a la variedad de capital que vende el antiguo proveedor en t
  pkold<-matrix(data=1,nrow=fic,ncol=Time)
  #costo unitario asociado a la variedad de capital que vende el candidato a nuevo proveedor en t
  pknew<-matrix(data=1,nrow=fic,ncol=Time)
  
  #Inversion de las firmas de consumo (en unidades monetarias)
  inversion<-matrix(data=0,nrow=fic,ncol=Time)
  
  #probabilidad de que la firma de consumo cambie de proveedor de capital
  Prsk<-matrix(data=0,nrow=fic,ncol=Time)
  
  #capital que se vuelve obsoleto en cada periodo TIENE QUE ARRANCAR EN DATA=0
  kobsoleto<-matrix(data=0,nrow=fic,ncol=Time)
  
  #ventas de capital canceladas por la firma de consumo
  vecanc<-matrix(data=0,nrow=fik,ncol=Time)
  #ventas de capital canceladas por la firma de capital
  vecank<-matrix(data=0,nrow=fik,ncol=Time)
  #compras de capital canceladas por la firma de consumo (en unidades)
  cocanc<-matrix(data=0,nrow=fic,ncol=Time)
  #compras de capital canceladas por la firma de capital (en unidades)
  cocank<-matrix(data=0,nrow=fic,ncol=Time)
  
  #pricing
  
  #precio del bien ofertado (consumo o capital)
  px<-matrix(ncol=Time,nrow=(fic+fik))
  #inflacion
  pi<-vector(mode = "numeric",length = Time)
  #inflacion minorista
  pic<-vector(mode = "numeric",length = Time)
  #inflacion mayorista
  pik<-vector(mode = "numeric",length = Time)
  #indice de precios al consumo
  ipc<-vector(mode = "numeric",length = Time)
  #indice de precios esperados del consumidor (PONDERA POR INGRESO DISPONIBLE)
  ipeh<-vector(mode = "numeric",length = Time)
  #indice general de precios
  ipg<-vector(mode = "numeric",length = Time)
  #deflactor del pib
  igdp<-vector(mode = "numeric",length = Time)
  #aumento del endeudamiento
  deltad<-vector(mode = "numeric",length = Time)
  #mark-up
  mu<-matrix(ncol=Time,nrow=(fic+fik))
  #mark-up de las firmas de consumo
  mu[1:fic,]<-muc0
  #mark-up de las firmas de capital
  mu[(fic+1):(fic+fik),]<-muk0
  #mark-up del sector consumo
  mupc<-vector(mode = "numeric",length = Time)
  #mark-up del sector consumo
  mupk<-vector(mode = "numeric",length = Time)
  #mark-up de la economia
  mup<-vector(mode = "numeric",length = Time)
  #mark-up sobre ventas del sector consumo
  mupsc<-vector(mode = "numeric",length = Time)
  #mark-up sobre ventas del sector consumo
  mupsk<-vector(mode = "numeric",length = Time)
  #mark-up sobre ventas de la economia
  mups<-vector(mode = "numeric",length = Time)
  #expectativas de salario promedio a pagar
  we<-matrix(data=wn0,ncol=Time,nrow=(fic+fik))
  #ratio de capacidad utilizada de la firma
  ux<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #ratio de capacidad utilizada del sector
  ufic<-vector(mode = "numeric",length = Time)
  #ratio de capacidad utilizada deseado de la firma 
  ud<-matrix(data=1,ncol=Time,nrow=(fic+fik))
  #ratio de capacidad utilizada normal
  utecho<-utecho0
  
  #inversion
  
  #tasa de crecimiento deseada de la capacidad productiva de la firma
  gd<-matrix(data=1,ncol=Time,nrow=(fic+fik))
  #tasa de gananacia
  r<-matrix(data=rtecho0,ncol=Time,nrow=(fic+fik))
  #tasa de gananacia de la economia
  vk<-vector(mode = "numeric",length = Time)
  #tasa de ganancia normal
  rtecho<-rtecho0
  #flujo de fondos operativos neto
  OCF<-matrix(data=1,ncol=Time,nrow=(fic+fik))

  OCFe<-matrix(data=1,ncol=Time,nrow=(fic+fik))
  
  #productividad total(promedio,agregada) del capital utilizado
  muc<-matrix(data=mu0,nrow = fic,ncol = Time)
  
  #Banco prestamista de cada firma, es el numero del banco que le presta en t
  loanmarket<-matrix(data=sample(1:fib,fic+fik,replace=TRUE),nrow=fic+fik,ncol=Time)
  
  #probabilidad de que la firma cambie de proveedor de credito (prestamista)
  Prscred<-matrix(data=0,nrow=fic+fik,ncol=Time)
  
  #matriz de creditos
  loans<-matrix(data=0,ncol=6, dimnames = list(c(),c("id","b" ,"L", "i", "eta", "t1"))) 
  #id<- firma prestataria
  #b <- banco
  #L <- monto
  #i <- tasa
  #eta <-plazo
  #t1 <- fecha del primer pago (el prestamo se otorga en t0)
  #voy a crear una fila cada vez que se otorga un credito
  
  #intereses pagados en t=1
  int1<-vector(mode = "numeric",length = fib)
  #deuda total de cada firma
  deudax<-matrix(data=0,nrow=fic+fik,ncol=Time)
  #matriz para control de ventanillas visitadas por la firma
  ventanilla<-matrix(data=0,nrow = fic+fik,ncol = fib)
  #monto recuperado ante default de un deudor
  recupero<-matrix(data=0,nrow = fib,ncol = Time)
  #monto perdido ante default de un deudor
  perdida<-matrix(data=0,nrow = fib,ncol = Time)
  #prestamos concedidos y defaulteados en el mismo periodo (perdida total para el banco)
  loandefhoy<-matrix(data=0,nrow = fic+fik,ncol = Time)
  #vida util del capital
  kapa<-matrix(data=kapa0,ncol=Time,nrow=(fic+fik))
  #inversion deseada medida en unidades de capital
  idx<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #inversion nominal deseada 
  Idx<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  
  #I+D
  
  #probabilidad de exito de la actividad de innovacion
  Prinn<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #parametro de probabilidad de exito de la innovacion
  xiinn<-matrix(data=xiinn0,ncol=Time,nrow=(fic+fik))
  #probabilidad de exito de la actividad de imitacion
  Primi<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #parametro de probabilidad de exito de la imitacion
  xiimi<-matrix(data=xiimi0,ncol=Time,nrow=(fic+fik))
  #Toma valor 1 en el periodo donde la firma innovo
  Nuevavariedad<-matrix(data=0,ncol=Time,nrow=fik)
  #Toma valor 1 en el periodo donde la firma imito
  Imitacion<-matrix(data=0,ncol=Time,nrow=fik)
  
  
  #ganancias
  
  #ganancias antes de impuestos
  pix<-matrix(data=0,ncol=Time,nrow=(fic+fik+fib))
  #tasa de interes del deposito en el banco b
  idb<-matrix(data=idb0,ncol=Time,nrow=fib)
  #costo unitario
  uc<-matrix(data=1,ncol=Time,nrow=(fic+fik))
  #salario promedio
  wn<-matrix(data=wn0,ncol=Time,nrow=(fic+fik+1))
  #salario promedio pagado
  wnpago<-matrix(ncol=Time,nrow=(fic+fik+1))
  #tasa de interes del prestamo ofrecido por cada banco
  ilb<-matrix(data=ilb0,ncol=Time,nrow=fib)
  #duracion del prestamo ofrecido por cada banco
  eta<-matrix(data=eta0,ncol=Time,nrow=fib)
  #valor neto de la firma
  WNi<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #Valor neto promedio del sector
  WNc<-vector(mode = "numeric",length = Time)
  WNc[1]<-1
  WNk<-vector(mode = "numeric",length = Time)
  WNk[1]<-1
  
  #matriz de depositos
  deposits<-matrix(nrow=fic+fik+nw+no+nr+nm,ncol=4, dimnames = list(c(),c("id","b" ,"D", "i"))) 
  #id<- depositante
  #b <- banco
  #D <- monto
  #i <- tasa
  #supongo que los contratos de deposito son por un periodo
  #si los hogares cambian de banco mueven todo lo depositado, porque el banco anterior no mantiene la tasa
  
  #Banco depositario de cada firma, es el numero del banco que le capta depositos en t
  dmarket<-matrix(data=sample(1:fib,fic+fik+nw+no+nr+nm,replace=TRUE),nrow=fic+fik+nw+no+nr+nm,ncol=Time)
  
  #probabilidad de que la firma o familia cambie de banco para depositar
  Prsdep<-matrix(data=0,nrow=(fic+fik+nw+no+nr+nm),ncol=Time)
  
  
  #tasa del impuesto a la renta empresarial
  taopi<-vector(mode = "numeric",length = Time)
  taopi[1]<-taopi0
  #porcion de ganancias distribuidas en las firmas de consumo
  roc<-vector(mode = "numeric",length = Time)
  roc[1:Time]<-roc0
  #porcion de ganancias distribuidas en las firmas de capital
  rok<-vector(mode = "numeric",length = Time)
  rok[1:Time]<-rok0
  #porcion de ganancias distribuidas en los bancos
  rob<-vector(mode = "numeric",length = Time)
  rob[1:Time]<-rob0
  #dividendos
  Divx<-matrix(data=0,ncol=Time,nrow=(fic+fik+fib))
  #dividendos esperados
  Dive<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #demanda de prestamos
  Ld<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #monto de credito rechazado
  Lrej<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  Lr<-vector(mode = "numeric",length = Time)
  Lrc<-vector(mode = "numeric",length = Time)
  Lrk<-vector(mode = "numeric",length = Time)
  #bancos VER CANTIDADES INCICIALES
  #Saldo de creditos vigentes
  loan<-vector(mode = "numeric",length = Time)
  #Saldo de creditos vigentes a las firmas de consumo
  loanc<-vector(mode = "numeric",length = Time)
  #Saldo de creditos vigentes a las firmas de capital
  loank<-vector(mode = "numeric",length = Time)
  #saldo de depositos
  depo<-vector(mode = "numeric",length = Time)
  #saldo de depositos de las firmas de consumo
  depoc<-vector(mode = "numeric",length = Time)
  #saldo de depositos de las firmas de capital
  depok<-vector(mode = "numeric",length = Time)
  #saldo de depositos de los operarios
  depow<-vector(mode = "numeric",length = Time)
  #saldo de depositos de los oficinistas
  depoo<-vector(mode = "numeric",length = Time)
  #saldo de depositos de los investigadores
  depor<-vector(mode = "numeric",length = Time)
  #saldo de depositos de los gerentes
  depom<-vector(mode = "numeric",length = Time)
  #ratio de capital del banco
  CR<-matrix(data=0,ncol=Time,nrow=fib)
  #valor neto del banco
  NW<-matrix(data=0,ncol=Time,nrow=fib)
  #prestamos concedidos
  Ltot<-matrix(data=1,ncol=Time,nrow=fib)
  #ratio de capital objetivo del banco
  CRT<-matrix(data=CRT0,ncol=Time,nrow=fib)
  #tasa de interes promedio del mercado de prestamos
  itechol<-vector(mode = "numeric",length = Time)
  itechol[1]<-ilb0
  #servicio de la deuda (primer tramo de pago del prestamo solicitado)
  dsLd<-vector(mode = "numeric",length = (fic+fik))
  #probabilidad de default
  Prdx<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #porcion del prestamo esperada a recuperar por el banco en caso de default de la firma
  deltax<-matrix(data=0,ncol=Time,nrow=(fic+fik))
  #beneficio esperado de prestarle a la firma
  bene<-vector(mode = "numeric",length = (fic+fik))
  #reservas
  R<-matrix(data=0,ncol=Time,nrow = fib)
  
  
  #mercado de bonos y depositos
  
  #tasa de interes del banco central sobre los avances
  itechocb<-vector(mode = "numeric",length = Time)
  itechocb[]<-itechocb0
  #objetivo del ratio de liquidez del banco
  LRT<-matrix(data=LRT0,ncol=Time,nrow=fib)
  #ratio de liquidez del banco
  LR<-matrix(data=0,ncol=Time,nrow=fib)
  #tasa de interes promedio de los depositos
  itechod<-vector(mode = "numeric",length = Time)
  itechod[1]<-idb0
  #monto del deposito
  saldep<-matrix(data=0,nrow = fic+fik+nw+no+nr+nm,ncol = Time)
  #intereses a cobrar
  intdep<-matrix(data=0,nrow = fic+fik+nw+no+nr+nm,ncol = Time)
  #intereses a pagar
  intdepap<-matrix(data=0,nrow = fib,ncol = Time)
  
  #comportamiento de las familias
  
  #salario de reserva
  wD<-matrix(data=wn0,ncol=Time,nrow=(nw+no+nr+nm))
  #salario
  w<-matrix(data=0,nrow = (nw+no+nr+nm),ncol = Time)
  #salario acordado (puede diferir del percibido en caso de default de la firma)
  wa<-matrix(data=0,nrow = (nw+no+nr+nm),ncol = Time)
  #estar desempleado (1=estar desempleado, 0=estar empleado)
  uh<-matrix(data=0,ncol=Time,nrow=(nw+no+nr+nm))#DUDA arranca en 0 o en 1, eso determina pleno empleo o desempleo 
  
  #ingreso imponible del hogar (sin dole)
  ytax<-matrix(ncol=Time,nrow=(nw+no+nr+nm))
  #ingreso bruto del hogar (antes de impuestos)
  yh<-matrix(ncol=Time,nrow=(nw+no+nr+nm))
  #ingreso del hogar (antes de impuestos)
  yat<-matrix(ncol=Time,nrow=(nw+no+nr+nm))
  #ingreso bruto del hogar (antes de impuestos)
  yait<-matrix(ncol=Time,nrow=(nw+no+nr+nm))
  #ingreso para calcular gini
  ygini<-matrix(ncol=Time,nrow=(nw+no+nr+nm))
  
  
  #Matriz para calcular Gini
  Gini<-matrix(nrow = nw+no+nr+nm,ncol = 4)
  #indice de Gini
  ig<-vector(mode = "numeric",length = Time)
  #indice de Gini calculado con paquete
  ig2<-vector(mode = "numeric",length = Time)
  #indice de Gini despues de impuestos
  igat<-vector(mode = "numeric",length = Time)
  #indice de Gini despues del impuesto al ingreso
  igait<-vector(mode = "numeric",length = Time)
  #empresa para la que trabaja w, 0 esta desempleado, 999 gobierno <OJO CON ESTO QUE SE ME PUEDE COMPLICAR CON MaS EMPRESAS>
  trabajow<-matrix(data=0,ncol=Time,nrow=nw)
  
  #empresa para la que trabaja o, 0 esta desempleado, 999 gobierno <OJO CON ESTO QUE SE ME PUEDE COMPLICAR CON MaS EMPRESAS>
  trabajoo<-matrix(data=0,ncol=Time,nrow=no)
  
  #empresa para la que trabaja r, 0 esta desempleado, 999 gobierno <OJO CON ESTO QUE SE ME PUEDE COMPLICAR CON MaS EMPRESAS>
  trabajor<-matrix(data=0,ncol=Time,nrow=nr)
  
  #empresa para la que trabaja m, 0 esta desempleado, 999 gobierno <OJO CON ESTO QUE SE ME PUEDE COMPLICAR CON MaS EMPRESAS>
  trabajom<-matrix(data=0,ncol=Time,nrow=nm)
  
  #total de riqueza de los manager (en proporcion se repartiran las acciones de los bancos)
  acciones_totales<-vector(mode = "numeric",length = Time)
  acciones_totales[1]<-1
  #total de acciones en cada empresa
  acciones<-matrix(data=0,ncol=Time,nrow=fic+fik)
  #matriz auxiliar para calcular aportes en caso de capitalizacion
  aporte<-matrix(data = 0,nrow = nm,ncol=fic+fik+fib)
  #matriz auxiliar para calcular aportes en caso de capitalizacion de las firmas de consumo al no pagar un credito
  aportec<-matrix(data = 0,nrow = nm,ncol=fic)
  #control de los aportes del periodo
  aportectotal<-taoi<-vector(mode = "numeric",length = Time)
  #Monto de la capitalizacion realizada
  capitalizacion<-matrix(data=0,nrow=fic+fik+fib, ncol= Time)
  #Monto de la capitalizacion necasaria
  acapitalizar<-matrix(data=0,nrow=fic+fik+fib, ncol= Time)
  #expectativas de precio del bien de consumo
  peh<-matrix(data=1,ncol=Time,nrow=(nw+no+nr+nm))
  #consumo deseado del hogar
  cdh<-matrix(data=1,ncol=Time,nrow=(nw+no+nr+nm))
  #ingreso neto del hogar
  NIh<-matrix(data=0,ncol=Time,nrow=(nw+no+nr+nm))
  #vector auxiliar para ir gastando ingreso
  disponible<-vector(mode = "numeric",length = (fic+fik+fib+nw+no+nr+nm+2))#los ultimos son Gobierno y Banco Central
  
  #matriz auxiliar para contabilizar la deuda de fic con fik 
  apagar<-matrix(data = 0,nrow = fic,ncol = fik)
  #matriz auxiliar para contabilizar el precio de k en la deuda de fic con fik 
  apagarp<-matrix(data = 0,nrow = fic,ncol = fik)
  #matriz auxiliar para contabilizar la variedad (productividad) en la deuda de fic con fik 
  apagarv<-matrix(data = 0,nrow = fic,ncol = fik)
  #matriz auxiliar para contabilizar la antig?edad en la deuda de fic con fik (puede haber capital de la misma productividad y distinta antig?edad, pasa en el 1er perido) 
  apagara<-matrix(data = 0,nrow = fic,ncol = fik)
  #ahorro
  Sh<-matrix(data=0,ncol=Time,nrow=(nw+no+nr+nm))
  #Dividendos recibidos
  Divh<-matrix(data=0,ncol=Time,nrow = (nw+no+nr+nm))
  #consumo del hogar
  ch<-matrix(data=0,ncol=Time,nrow=(nw+no+nr+nm))
  #subsidio por desempleo
  d<-matrix(data=0,ncol=Time,nrow=(nw+no+nr+nm))
  
  #oferta, demanda, cantidad y precio del bien de consumo en t
  constrans<-cbind(h=c(1:(nw+no+nr+nm)),fic=sample(1:fic,nw+no+nr+nm,replace=TRUE),c=0,p=1)#al cambiar del periodo tengo que cambiar constrans1 por constrans
  
  #oferta, demanda, cantidad y precio del bien de consumo en t-1
  constrans1<-cbind(h=c(1:(nw+no+nr+nm)),fic=sample(1:fic,nw+no+nr+nm,replace=TRUE),c=0,p=1)#al cambiar del periodo tengo que cambiar constrans1 por constrans
  
  #interacciones en todo los periodos
  consmarket<-matrix(data=sample(1:fic,nw+no+nr+nm,replace=TRUE),nrow=(nw+no+nr+nm),ncol=Time)
  
  #precio del antiguo proveedor en t
  pold<-matrix(data=0,nrow=(nw+no+nr+nm),ncol=Time)
  #precio del candidato a nuevo proveedor en t
  pnew<-matrix(data=0,nrow=(nw+no+nr+nm),ncol=Time)
  
  #probabilidad de que la familia cambie de proveedor de consumo
  Prsc<-matrix(data=0,nrow=(nw+no+nr+nm),ncol=Time)
  
  
  #Default=1, 0 o/c
  default<-matrix(data=0, nrow = (fic+fik+fib), ncol=Time)
  
  
  
  #comportamiento del gobierno y el banco central
  
  #bonos adquiridos por los bancos y BC
  bonos<-matrix(data=0,nrow = fib+1,ncol=Time)#el 1 es por el BC
  #tasa impositiva al ingreso
  taoi<-vector(mode = "numeric",length = Time)
  taoi[1]<-taoi0
  
  #Disenio progresivo
  #umbral para el cambio de tasa
  umbral<-vector(mode = "numeric",length = Time)
  #Tasa menor al umbrla
  taoi1<-vector(mode = "numeric",length = Time)
  #Tasa menor al umbrla
  taoi2<-vector(mode = "numeric",length = Time)
  taoi2[]<-taoi
  #tasa impositiva a la riqueza
  taow<-vector(mode = "numeric",length = Time)
  taow[1]<-taow0
  #riqueza del hogar
  wh<-matrix(data=1,ncol=Time,nrow=(nw+no+nr+nm))
  #cantidad de desempleados
  U<-u0*(nw+no+nr+nm)
  #factor de revision de las tasas impositivas
  rev<-vector(mode = "numeric",length = Time)
  rev[1]<-1
  #cantidad de bonos emitidos por encima de los emitidos en el periodo anterior
  deltab<-vector(mode = "numeric",length = Time)
  deltab[1]<-0
  #precio del bono EN DESUSO
  ptechob<-vector(mode = "numeric",length = Time)
  ptechob[]<-ptechob0
  #tasa de interes del bono
  itechob<-vector(mode = "numeric",length = Time)
  itechob[]<-itechob0
  #resultado fiscal
  dfg<-vector(mode = "numeric",length = Time)
  dfg[1]<-0
  #deuda publica
  debtg<-0
  #recaudacion impositiva
  Tt<-0
  #ganancias del banco central
  piCB<-vector(mode = "numeric",length = Time)
  piCB[1]<-0
  #avances de capital del banco central
  CAcb<-matrix(data=0,nrow=fib,ncol = Time)
  #impuesto a la renta
  proftax<-matrix(data=0,nrow = fic+fik+fib,ncol = Time)
  #impuesto al ingreso personal
  inctax<-matrix(data=0,nrow = nw+no+nr+nm,ncol = Time)
  #impuesto a la riqueza
  wtax<-matrix(data=0,nrow = nw+no+nr+nm,ncol = Time)
  #producto real
  gdp<-vector(mode = "numeric",length = Time)
  #producto nominal
  ngdp<-vector(mode = "numeric",length = Time)
  #masa salarial funcionarios publicos
  sueldosg<-vector(mode = "numeric",length = Time)
  sueldosg[1]<-0
  
  cajaG<-vector(mode = "numeric",length = Time)
  cajaBC<-vector(mode = "numeric",length = Time)
  cajah<-vector(mode = "numeric",length = Time)
  cajac<-vector(mode = "numeric",length = Time)
  cajak<-vector(mode = "numeric",length = Time)
  cajab<-vector(mode = "numeric",length = Time)
  
  #Apalancamiento de los bancos
  levb<-vector(mode = "numeric",length = Time)
  #Apalancamiento de las firmas de consumo (deuda/valor neto)
  levcnw<-vector(mode = "numeric",length = Time)
  #Apalancamiento de las firmas de consumo (activo/deuda)
  levcad<-vector(mode = "numeric",length = Time)
  #Apalancamiento de las firmas de capital (deuda/valor neto)
  levknw<-vector(mode = "numeric",length = Time)
  #Apalancamiento de las firmas de capital (activo/deuda)
  levkad<-vector(mode = "numeric",length = Time)
  #pasivo de las firmas de consumo
  pasc<-vector(mode = "numeric",length = Time)
  #activo de las firmas de consumo
  actc<-vector(mode = "numeric",length = Time)
  #patrimonio de las firmas de consumo
  patc<-vector(mode = "numeric",length = Time)
  #pasivo de las firmas de capital
  pask<-vector(mode = "numeric",length = Time)
  #activo de las firmas de capital
  actk<-vector(mode = "numeric",length = Time)
  #patrimonio de las firmas de capital
  patk<-vector(mode = "numeric",length = Time)
  #pasivo de las firmas
  pasivoi<-matrix(ncol=Time,nrow=fic+fik)
  #activo de las firmas de capital
  activoi<-matrix(ncol=Time,nrow=fic+fik)
  #patrimonio de las firmas de capital
  patrimonioi<-matrix(ncol=Time,nrow=fic+fik)
  #Para controlar el tiempo
  hora<-vector(mode = "numeric",length = Time)
  
  #matrices de flujos
  fh<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fcc<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fkc<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fck<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fkk<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fcb<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fkb<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fg<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fccb<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  fkcb<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  ctrlflujos<-matrix(data=0,ncol = Time,nrow = 19,dimnames = list(c("consumo","salarios","dole","cg en invent","inversion","amort capital","impuestos","int dep","int bonos","int loans","int avances","beneficios","cb beneficios","delta cash","delta dep","delta avances","delta res","delta bonos","delta loans"),c()))
  ctrlflujoag<-matrix(data=0,ncol = 10,nrow = Time,dimnames = list(c(),c("hogares","ficCA","ficKA","fikCA","fikKA","fibCA","fibKA","gob","cbCA","cbKA")))
  
  #matriz de flujos para un periodo
  flujost<-matrix(nrow = 19,ncol=10)
  
  #matrices de stocks
  sh<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  sc<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  sk<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  sb<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  sg<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  scb<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  ctrlstocks<-matrix(data=0,ncol = Time,nrow = 8,dimnames = list(c("cash","depositos","creditos","bienes consumo","bienes capital","bonos","reservas","avances"),c()))
  NWag<-matrix(data=0,ncol = 6,nrow = Time,dimnames = list(c(),c("hogares","firmas consumo","firmas capital","bancos","gobierno","banco central")))
  
  #matriz de stocks para un periodo
  stockst<-matrix(nrow = 8,ncol=6)
  
  
  
  #Valores agregados en t=0----
  
  #Los periodos anteriores son t={0,-1,-2...}
  
  #1 firmas de capital----
  
  #oficinistas
  Nok1<-floor(Nk1*shareko/fik)*fik #70
  #investigadores
  Nrk1<-floor(Nk1*sharekr/fik)*fik #70
  #gerentes
  Nmk1<-floor(Nk1*sharekm/fik)*fik #50
  #operarios
  Nwk1<-floor((Nk1-Nok1-Nrk1-Nmk1)/fik)*fik #310
  #depositos
  Dk1<-sigma*wn0*Nk1 #2500
  #produccion inicial de las firmas de capital
  yk1<-k0/kapa0 #2000
  #productividad del trabajo total
  muNtotal0<-yk1/Nk1 #4 #EL TOTAL ES EL MISMO, PERO CAMBIAN LAS PROPORCIONES
  #productividad del trabajo de operariios
  muw0<-yk1/(Nk1*sharekw) 
  mun<-matrix(data=muw0,ncol = Time,nrow = fic+fik)
  #costo unitario
  uck1<-wn0/muNtotal0 
  #precio
  pk1<-(1+muk0)*uck1 #1.34375
  #inventario
  invk1<-nu*yk1 #200
  #beneficio
  #pik1<-1/(1-ilb0*(1-taopi0)*(1-rok0)/gss)*(pk1*yk1+idb0*Dk1/(1+gss)+invk1*uck1*gss/(1+gss)-wn0*Nk1-ilb0/(1+gss)*(invk1*uck1+Dk1)) #190.7332
  pik1<-156.40120123042325/0.82 #190.7332
  #Recaudacion del impuesto a la renta
  Tk1<-pik1*taopi0 #34.33197
  #Divendos
  Divk1<-rok0*pik1*(1-taopi0) #140.17611
  #prestamos
  Lk1<-(invk1*gss/(1+gss)*uck1+Dk1*gss/(gss+1)-(pik1-pik1*taopi0-pik1*rok0*(1-taopi0)))*(1+gss)/gss #649.0105
  #sumatoria de uno a la duracion de los creditos del inverso de uno mas la tasa de crecimiento del cuasi estado estacionario a la jota
  resultado1<-vector()
  for (j in 1:eta0) {
    resultado1<-c(resultado1,1/((1+gss)^j))
  }
  #amortizacion de creditos
  amLk1<-sum(resultado1)/eta0 #k principal rep/Lk0 = 59.9328/64.7642 = 0.925401
  #el saldo del prestamo
  resultado2<-vector()
  for (j in 0:(eta0-1)) {
    resultado2<-c(resultado2,((eta0-j)/eta0)*1/((1+gss)^j))
  }
  disLk1<-sum(resultado2) #10.02113
  #Monto del prestamo solicitado en cada anio
  Lk0<-Lk1/disLk1 #64.7642
  #other cash flows FALTA DEFINIR Lk0
  OCFk1<-pik1*(1-taopi0)-invk1*uck1*gss/(1+gss)-amLk1*Lk0 #94.60733
  #Valor neto
  NWk1<-Dk1-Lk1+uck1*invk1 #2100.989
  
  #2 firmas de consumo---- 

  #oficinistas
  Noc1<-floor(Nc1*shareco/fic)*fic
  #investigadores
  Nrc1<-floor(Nc1*sharecr/fic)*fic
  #gerentes
  Nmc1<-floor(Nc1*sharecm/fic)*fic
  #operarios
  Nwc1<-floor((Nc1-Noc1-Nrc1-Nmc1)/fic)*fic
  #producto
  yc1<-k0*utecho0*mu0
  #ratio capital trabajo
  lk<-yc1/(Nwc1*mu0)
  #costo unitario variable 
  uvcc1<-wn0*Nc1/yc1 
  #sumatoria de uno a kappa del inverso de uno mas la tasa de crecimiento del cuasi estado estacionario a la jota
  resultado0<-vector()
  for (j in 1:kapa0) {
    resultado0<-c(resultado0,1/((1+gss)^j))
  }
  #amortizacion del capital
  amK1<-1/(kapa0^2)*sum(resultado0) 
  #capital amortizado
  Kamor1<-pk1*k0/(kapa0^2)*sum(resultado0) 
  pc1<-(1+muc0)*uvcc1 
  #depositos
  Dc1<-wn0*Nc1 
  #inventarios
  invc1<-nu*yc1 
  #beneficios
  pic1<-1/(1+(ilb0/gss)*(-1+taopi0+roc0*(1-taopi0)))*(pc1*yc1+idb0*Dc1/(1+gss)+invc1*gss/(1+gss)*uvcc1-wn0*Nc1-ilb0/gss*(yk1*pk1+invc1*gss/(1+gss)*uvcc1+Dc1*gss/(1+gss)-pk1*k0/(kapa0^2)*sum(resultado0))-pk1*k0/(kapa0^2)*sum(resultado0)) #1346.603
  #Recaudacion del impuesto a la renta
  Tc1<-pic1*taopi0
  #Divendos
  Divc1<-roc0*pic1*(1-taopi0)
  #prestamos ES VIABLE EL DE ABAJO
  Lc1<-(yk1*pk1+invc1*gss/(1+gss)*uvcc1+Dc1*gss/(gss+1)-(pic1-pic1*taopi0-pic1*roc0*(1-taopi0))-pk1*k0/(kapa0^2)*sum(resultado0))*(1+gss)/gss #25848.51
  #porcentaje del valor nominal de los bienes de capital una vez amortizados
  resultado3<-vector()
  for (j in 1:kapa0){
    resultado3<-c(resultado3,j/(kapa0^2)/((1+gss)^(kapa0-j)))
  }
  #capital nominal
  K1<-pk1*k0*sum(resultado3) 
  NWc1<-Dc1-Lc1+uvcc1*invc1+K1 
  amLc1<-amLk1 
  disLc1<-disLk1 
  Lc0<-Lc1/disLc1 
  #Other cash flows
  OCFc1<-pic1*(1-taopi0)+amK1*k0*pk1-invc1*uvcc1*gss/(1+gss)-amLc1*((yk1*pk1+invc1*gss/(1+gss)*uvcc1+Dc1*gss/(gss+1)-(pic1-pic1*taopi0-pic1*roc0*(1-taopi0))-pk1*k0/(kapa0^2)*sum(resultado0))*(1+gss)/gss)/disLc1 
  
  
  
  #3 Resto de los agentes----
  
  
  #poblacion
  POP<-nw+no+nr+nm
  #trabajadores totales
  Nx1<-Nc1+Nk1+Ngt
  #operarios del gobierno
  Nwg1<-Ngt*sharecw
  #oficinistas del gobierno
  Nog1<-Ngt*shareco
  #gerentes del gobierno
  Nmg1<-Ngt*sharecm
  #cosumo de los hogares
  ch1<-yc1 
  #Consumo nominal de los hogares
  Ch1<-yc1*pc1 
  #Ratio de capital de los bancos (Lc+Lk+R+Bb)/NWb
  wb0<-0.08 
  #masa salarial
  ms1<-wn0*Nx1 
  #monto de intereses popr prestamos
  iLf1<-ilb0*(Lc1+Lk1) 
  #monto de intereses por depositos de las firmas
  iDf1<-idb0*(Dc1+Dk1) 
  #dividendos de las firmas de capital y consumo
  Divf1<-Divc1+Divk1 
  
  ### ### ### ### ### ## ### ### ## ### ### ## ### ##
  #No logre despejar las variables del 3er sistema que me paso Caiani, les voy a asignar los valores de una
  ### ## # ### ### ### ### ### ## ## ### ## ### ## ### #
  
  #monto del subsidio por desempleo
  dole<-640
  NIh1<-300.387592239068-Divf1*gss/(1+gss)+pc1*yc1 
  #Dividendos de los bancos
  Divb1<-(NIh1-
            ((1-taoi20)*(wn0*Nx1+Divf1/(1+gss))+dole+((1-taoi20)*idb0/(1+gss)-taow0)*((1+gss)/gss*(NIh1-pc1*yc1+Divf1*gss/(1+gss)))))/
    ((1-taoi20)/(1+gss)+((1-taoi20)*idb0/(1+gss)-taow0))#114.5343 a mi en papel me dio 113.789
  #riqueza neta de los hogares
  Dh1<-(1+gss)/gss*(NIh1-pc1*yc1+(Divf1+Divb1)*gss/(1+gss)) #40466.6
  NWh1<-Dh1
  #Tributos pagados por los hogares
  Th1<-(wn0*Nx1+idb0*Dh1/(1+gss)+(Divf1+Divb1)/(1+gss))*taoi20+Dh1*taow0 #3602.546
  #beneficios de los bancos
  pib1<-Divb1/(rob0*(1-taopi0))
  #tributos pagados por los bancos
  Tb1<-pib1*taopi0 
  #bonos en poder de los bancos
  Bb1<-(1+gss)/itechob0*((pib1-ilb0*(Lc1+Lk1)/(1+gss))+idb0*(Dh1+Dc1+Dk1)/(1+gss)) #25513.23
  #total de bonos emitidos
  B1<-(1+gss)/gss*(wn0*Ngt+dole-Th1-Tb1-Tc1-Tk1+Bb1*itechob0/(1+gss)) #34097.62
  #bonos en poder del banco central
  Bcb1<-B1-Bb1 
  #valor neto de los bancos
  NWb1<-Lc1+Lk1+Bb1+Bcb1-Dh1-Dc1-Dk1 
  #Reservas de los bancos
  Rb1<-Bcb1 
  #beneficios del banco central
  picb1<-itechob0*Bcb1/(1+gss) 
  
  #Ingreso de los hogares
  GI0<-ms1+Divc1+Divk1+Divb1 
  #30% se lo apropian los operarios
  SIw<-0.3
  SPw<-nw/POP#(Nwc1+Nwk1+Nwg1)/Nx1#
  ww0<-SIw/SPw*wn0+SIw*(Divc1+Divk1+Divb1)/nw#(Nwc1+Nwk1+Nwg1)#
  #40% se lo apropian los oficinistas e investigadores
  SIor<-0.4
  SPor<-(no+nr)/POP
  wo0<-SIor/SPor*wn0+SIor*(Divc1+Divk1+Divb1)/(no+nr)
  wr0<-wo0 
  #30% se lo apropian los manager
  SIm<-0.3
  SPm<-nm/POP#(Nmc1+Nmk1+Nmg1)/Nx1
  wm0<-SIm/SPm*wn0+(SIm-1)*(Divc1+Divk1+Divb1)/nm#(Nmc1+Nmk1+Nmg1)
  
  
  
  #Riqueza de los hogares
  #Mantengo la misma distribucion inicial que en los ingresos
  #30% se lo apropian los operarios
  SWw<-0.3
  dw0<-SWw*Dh1/nw
  #40% se lo apropian los oficinistas e investigadores
  SWor<-0.4
  do0<-SWor*Dh1/(no+nr)
  dr0<-do0
  #30% se lo apropian los manager
  SWm<-0.3
  dm0<-SWm*Dh1/nm 
  
  
  #4 Stocks individuales----
  
  #Depositos ES IGUAL A LA RIQUEZA EN EL CASO DE LOS INDIVIDUOS
  for (i in 1:fic) {
    deposits[i,]<-c(i,sample(1:fib,1),Dc1/fic,idb0)
  }
  
  for (i in (fic+1):(fic+fik)) {
    deposits[i,]<-c(i,sample(1:fib,1),Dk1/fik,idb0)
  }
  
  for (h in (fic+fik+1):(fic+fik+nw)) {
    deposits[h,]<-c(h,sample(1:fib,1),dw0,idb0)
  }
  for (h in (fic+fik+nw+1):(fic+fik+nw+no)) {
    deposits[h,]<-c(h,sample(1:fib,1),do0,idb0)
  }
  for (h in (fic+fik+nw+no+1):(fic+fik+nw+no+nr)) {
    deposits[h,]<-c(h,sample(1:fib,1),dr0,idb0)
  }
  for (h in (fic+fik+nw+no+nr+1):(fic+fik+nw+no+nr+nm)) {
    deposits[h,]<-c(h,sample(1:fib,1),dm0,idb0)
  }
  
  for (b in 1:fib) {
    intdepap[b,1]<-sum(deposits[which(deposits[,2]==b),3]*deposits[which(deposits[,2]==b),4])
  }
  
  for (h in 1:POP) {
    wh[h,1]<-sum(deposits[which(deposits[,1]==fic+fik+h),3])
  }
  
  
  #riqueza de los managers
  acciones_totales[1]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),1])
  
  
  #Inventario
  for (i in 1:fic){
    inv[i,1]<-invc1/fic
  }
  for (i in (fic+1):(fic+fik)){
    inv[i,1]<-invk1/fik
  }
  
  #Bonos
  for (b in 1:fib) {

    #Reservas  
    R[b,1]<-Rb1/fib;
    disponible[fic+fik+b]<-R[b,1]
  }
  disponible[fic+fik+fib+nw+no+nr+nm+2]<--(sum(R[,1]))

  
  #Prestamos
  Loaninicial<-rbind(matrix(data=Lc1/fic,ncol = 1,nrow = fic),matrix(data=Lk1/fik,ncol=1,nrow=fik))
  for (i in 1:fic) {
    resultado4<-vector()
    for (j in 0:(eta0-1)){
      resultado4<-c(resultado4,(eta0-j)/(eta0*((1+gss)^j)))
    }
    L0<-Loaninicial[i]/sum(resultado4)
    loans<-rbind(loans,c(i,sample(1:fib,1),L0,ilb0,eta0,1))
    for (j in 1:(eta0-1)) {
      loans<-rbind(loans,c(i,sample(1:fib,1),L0/((1+gss)^j),ilb0,eta0,1-j))
    }
  }
  for (i in (fic+1):(fic+fik)) {
    resultado4<-vector()
    for (j in 0:(eta0-1)){
      resultado4<-c(resultado4,(eta0-j)/(eta0*((1+gss)^j)))
    }
    L0<-Loaninicial[i]/sum(resultado4)
    loans<-rbind(loans,c(i,sample(1:fib,1),L0,ilb0,eta0,1))
    for (j in 1:(eta0-1)) {
      loans<-rbind(loans,c(i,sample(1:fib,1),L0/((1+gss)^j),ilb0/((1+gss)^(j)),eta0,1-j))
    }
  }
  
  loans<-loans[-which(loans[,1]==0),]
  
  #PARA COMPROBAR sum(loans[,3]*(eta0-(1-loans[,6]))/eta0)
  
  
  #Capital
  
  
  capvarnominalinicial<-matrix(data=0,ncol = 1,nrow=kapa0)
  for (j in 1:kapa0) {
    capvarnominalinicial[j]<-pk1*k0*(kapa0-(j-1))/(((1+gss)^(j-1))*(kapa0^2)) #Estoy amortizando hasta el periodo anterior, no tengo en cuenta la amortizacion de este periodo
    
  }
  for (i in 1:fic) {
    capital[which(capital[,1]==i),]<-c(i,1,(capvarnominalinicial[1]/fic)*(kapa0)/(pk1*kapa0),1,1,pk1,mu0,kapa0)#(capvarnominalinicial[1]/fic)*(kapa0)*((1+gss)^(kapa0-1))/(pk1*1),1,1,pk1/(1+gss),mu0,kapa0)#,mu0,kapa0)#
    for (j in 2:kapa0) {
      capital<-rbind(capital,c(i,j,(capvarnominalinicial[j]/fic)*(kapa0)*((1+gss)^(j-1))/(pk1*(kapa0-(j-1))),1,j,pk1/((1+gss)^(j-1)),mu0,kapa0))#(capvarnominalinicial[j]/fic)*(kapa0)*((1+gss)^(kapa0-j))/(pk1*j),1,j,pk1/((1+gss)^j),mu0,kapa0))#,mu0,kapa0))#
    }
  }
  for (i in 1:fic) {
    K[i,1]<-sum(capital[which(capital[,1]==i),3])
  }
  
  #Considero que k0 es solo el capital de las firmas de consumo, no estoy contando el stock de las firmas de capital
  
  for (i in (fic+1):(fic+fik)) {
    capital[which(capital[,1]==i),]<-c(i,1,(invk1/fik),1,1,pk1,mu0,kapa0)
  }
  
  #Retornos
  for (i in 1:fic) {
    r[i,1]<-OCFc1/fic/sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]*(1-capital[which(capital[,1]==i & capital[,5]!=0),5]/capital[which(capital[,1]==i & capital[,5]!=0),8]))
  }
  
  #parametros de la funcion 2----
  taoi20<-tasainc
  rtecho<-robjetivo
  utecho<-uobjetivo
  nu<-inventariodeseado
  tita<-renuncias
  stigmac<-aversionc
  stigmak<-aversionk  
  sigmaFN1<-ajusteprecios
  lambda<-expectadap
  tu<-paciencia
  
  
  if (it>1000 & it<=6000){
    if (it<=2000){
      taoi20<-0.1279
    }else{
      if (it<=3000){
        taoi20<-0.1678
      }else{
        if (it<=4000){
          taoi20<-0.1991
        }else{
          if (it<=5000){
            taoi20<-0.2447
          }else{
            if (it<=6000){
              taoi20<-0.3174
            }
          }
        }
      }
    }
  }
    
if (it>10000){
      if (it<=11000){
        rtecho<-0.0335
    }else{
      if (it<=12000){
        rtecho<-0.0545
      }else{
        if (it<=13000){
          utecho<-0.75
        }else{
          if (it<=14000){
            utecho<-0.85
          }else{
            if (it<=15000){
              nu<-0.05
            }else{
              if (it<=16000){
                nu<-0.15
              }else{
                if (it<=17000){
                  stigmac<-2.9
                }else{
                  if (it<=18000){
                    stigmac<-4.9
                  }else{
                    if (it<=19000){
                      stigmak<-20.5
                    }else{
                      if (it<=20000){
                        stigmak<-22.5
                      }else{
                        if (it<=21000){
                          lambda<-0.2
                        }else{
                          if (it<=22000){
                            lambda<-0.3
                          }else{
                            if (it<=23000){
                              tu<-1
                            }else{
                              if (it<=24000){
                                tu<-3
                              }else{
                                if (it<=25000){
                                  iota<-0.25
                                }else{
                                  if (it<=26000){
                                    iota<-0.75
                                  }
                                }
                              }
                            }
                          }
                        } 
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
  
  

  
  
  
    
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    ### ### ### ### ### ## ### ### ## ### ### ## ### ##
    
    
  #5 Production planning----
  
  for (i in 1:fic) {  
    se[i,1]<-yc1/fic
    yd[i,1]<-max(0,min(se[i,1]*(1+nu)-invc1/fic,sum(capital[which(capital[,1]==i),3]*capital[which(capital[,1]==i),7])))
  }
  
  for (i in (fic+1):(fic+fik)) {  
    se[i,1]<-yk1/fik
    yd[i,1]<-se[i,1]*(1+nu)-invk1/fik
    capital[which(capital[,1]==i),3]<-capital[which(capital[,1]==i),3]+yd[i,1]
  }
  
  #6 Firm's labor demand---- 
  
  for (i in 1:(fic+fik)) {
    if (x[i,2]=="k") {
      Ndw[i,1]<-ceiling(yd[i,1]/mun[i,1])
    }else{
      if (capital[which(capital[,1]==i & capital[,2]==1),3]==0){capital[which(capital[,1]==i & capital[,2]==1),4]<-0#ver si conviene ponerle utilizacion 0, 1 u otro valor distinto
      }else{ 
        if (capital[which(capital[,1]==i & capital[,2]==1),3]*capital[which(capital[,1]==i & capital[,2]==1),7]<=yd[i,1]) {
          capital[which(capital[,1]==i & capital[,2]==1),4]<-1
        }else{
          capital[which(capital[,1]==i & capital[,2]==1),4]<-yd[i,1]/(capital[which(capital[,1]==i & capital[,2]==1),3]*capital[which(capital[,1]==i & capital[,2]==1),7])
        }    
      }
      
      for (j in 2:max(capital[,2])){
        resultado1<-vector()
        for (q in 1:(j-1)){
          resultado1<-c(resultado1,capital[which(capital[,1]==i & capital[,2]==q),4]*capital[which(capital[,1]==i & capital[,2]==q),3]*capital[which(capital[,1]==i & capital[,2]==q),7])
        }
        if (sum(resultado1)>=yd[i,1]){capital[which(capital[,1]==i & capital[,2]==j),4]<-0}
        else{
          if((sum(resultado1)+capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),7])<=yd[i,1]){capital[which(capital[,1]==i & capital[,2]==j),4]<-1
          }else{capital[which(capital[,1]==i & capital[,2]==j),4]<-(yd[i,1]-sum(resultado1))/capital[which(capital[,1]==i & capital[,2]==j-1),3]*capital[which(capital[,1]==i & capital[,2]==j-1),7]}
        }
        
        resultado2<-vector()
        for (q in 1:j){
          resultado2<-c(resultado2,capital[which(capital[,1]==i & capital[,2]==q),4]*capital[which(capital[,1]==i & capital[,2]==q),3]/lk)
        }
      }
      Ndw[i,1]<-ceiling(sum(resultado2))
      #determino la productividad segun la produccion deseada. Si no la concreta, en 13 Production se corrige atendiendo lo observado
      resultado21<-vector()
      for (j in 1:max(capital[which(capital[,1]==i),2])){
        resultado21<-c(resultado21,capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),4])
      }
      muc[i,1]<-yd[i,1]/sum(resultado21)
    }
    if (x[i,2]=="k") {
      Ndo[i,1]<-floor(Ndw[i,1]*shareko/sharekw)
      Ndr[i,1]<-floor(Ndw[i,1]*sharekr/sharekw)
      Ndm[i,1]<-floor(Ndw[i,1]*sharekm/sharekw)
    }else{ if (x[i,2]=="c"){
      Ndo[i,1]<-floor(Ndw[i,1]*shareco/sharecw)
      Ndr[i,1]<-floor(Ndw[i,1]*sharecr/sharecw)
      Ndm[i,1]<-floor(Ndw[i,1]*sharecm/sharecw)
    }
    }
    Nd[i,1]<-Ndw[i,1]+Ndo[i,1]+Ndr[i,1]+Ndm[i,1]
  }
  
  #Gobierno
  Nd[fic+fik+1,1]<-Ngt
  Ndw[fic+fik+1,1]<-floor(Nd[fic+fik+1,1]*sharecw)
  Ndo[fic+fik+1,1]<-floor(Nd[fic+fik+1,1]*shareco)
  Ndr[fic+fik+1,1]<-floor(Nd[fic+fik+1,1]*sharecr)
  Ndm[fic+fik+1,1]<-floor(Nd[fic+fik+1,1]*sharecm)
  
  #7 Price, interests and wages----
  
  #PRECIOS
  
  for (i in 1:(fic+fik)) {
    
    we[i,1]<-wn0*(1+abs(rnorm(1,muFN1,sigmaFN1)))#estoy diciendo que lo ajuste con la misma regla que ajusta wD > gss
    
    #el mark-up no puede ser negativo
    
    if (x[i,2]=="k"){#uTILIZACIoN DESEADA CORRE SOLO PARA LAS FIRMAS DE CONSUMO
      mu[i,1]<-muk0*(1+abs(rnorm(1,muFN1,sigmaFN1)))
      px[i,1]<-(1+muk0)*we[i,1]*Nd[i,1]/max(yd[i,1],1)
      capital[which(capital[,1]==i),6]<-px[i,1]
    }else{
      mu[i,1]<-muc0*(1+abs(rnorm(1,muFN1,sigmaFN1)))
      px[i,1]<-(1+muc0)*we[i,1]*Nd[i,1]/max(yd[i,1],1)
    }
  }
  
  #TASAS DE INTEReS  
  #Tasa activa
  
  for (b in 1:fib) {
    
    Ltot[b,1]<-sum(loans[which(loans[,2]==b),3]*(1-(1-loans[which(loans[,2]==b),6])/loans[which(loans[,2]==b),5]))
    NW[b,1]<-Ltot[b,1]+Bb1/fib+Rb1/fib-sum(deposits[which(deposits[,2]==b),3])
    
    #CR no puede ser menor a 0,06
    CR[b,1]<-NW[b,1]/Ltot[b,1]
    CRT[b,1]<-max(CR[b,1],0.06)
    
    if (CR[b,1]<CRT[b,1]) {ilb[b,1]<-ilb0*(1+abs(rnorm(1,mean = muFN2,sigmaFN2)))
    }else{ilb[b,1]<-ilb0*(1-abs(rnorm(1,muFN2,sigmaFN2)))}
    
    
    
    #Tasa pasiva
    
    #La tasa de interes de depositos se ajusta dependiendo de la relacion entre la liquidez 
    #del banco y su objetivo, que sera el promedio de liquidez del sector en t-1
    
    #LR no puede ser menor a 0,08 (la exigencia del BC)
    LRT[b,1]<-max(Rb1/(Dk1+Dc1+Dh1),0.08)
    
    LR[b,1]<-Rb1/fib/sum(deposits[which(deposits[,2]==b),3])
    if (LR[b,1]>=LRT[b,1]) {idb[b,1]<-idb0*(1-abs(rnorm(1,muFN2,sigmaFN2)))
    }else{idb[b,1]<-idb0*(1+abs(rnorm(1,muFN2,sigmaFN2)))}
    
    
    
    #hallo la tasa de interes sobre depositos promedio
    resultado5<-vector()
    for (j in 1:fib) {
      resultado5<-c(resultado5,idb[j,1]*sum(deposits[which(deposits[,2]==j),3]))
    }
    itechod[1]<-sum(resultado5)/sum(deposits[,3])
    
  }
  
  
  
  #SALARIOS DE RESERVA
  
  for (h in 1:(nw+no+nr+nm)){
    if (x[which(x[,1]==fic+fik+fib+h),2]=="w"){
      wD[h,1]<-ww0*sample(c((1-abs(rnorm(1,muFN1,sigmaFN1))),(1+abs(rnorm(1,muFN1,sigmaFN1)))),1,prob = c(82/2400*82/2400*6,1-82/2400*82/2400*6))
    }else{
      if (x[which(x[,1]==fic+fik+fib+h),2]=="m"){
        wD[h,1]<-wm0*sample(c((1-abs(rnorm(1,muFN1,sigmaFN1))),(1+abs(rnorm(1,muFN1,sigmaFN1)))),1,prob = c(82/400*82/400*6,1-82/400*82/400*6))
      }else{
        if (x[which(x[,1]==fic+fik+fib+h),2]=="r"){
          wD[h,1]<-wr0*sample(c((1-abs(rnorm(1,muFN1,sigmaFN1))),(1+abs(rnorm(1,muFN1,sigmaFN1)))),1,prob = c(11/81*11/81*6,1-11/81*11/81*6))
        }else{
          wD[h,1]<-wo0*sample(c((1-abs(rnorm(1,muFN1,sigmaFN1))),(1+abs(rnorm(1,muFN1,sigmaFN1)))),1,prob = c(145/1119*145/1119*6,1-145/1119*145/1119*6))
        }
      }
    }
  }
  
  #8- Investment in capital accumulation----
  
  for (i in 1:fic) {
    resultado7<-vector()  
    for (j in 1:max(capital[,2])){
      resultado7<-c(resultado7,capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),4])
    }
    resultado8<-vector()
    for (j in 1:max(capital[,2])){
      resultado8<-c(resultado8,capital[which(capital[,1]==i & capital[,2]==j),3])
    }
    ud[i,1]<-(sum(resultado7)/sum(resultado8))
    
    gd[i,1]<-gama1*(r[i,1]-rtecho)/rtecho + gama2*(ud[i,1]-utecho)/utecho
  }
  
  #9- Capital goods market (1)----
  #Aca se generan las ordenes. En el paso 11 se entregan
  
  
  #cada firma de consumo compra el equivalente a la suma del capital obsoleto y gd.
  #se lo compra con probabilidad Prs al nuevo proveedor y (1-Prs) al viejo
  #Voy a suponer que los oferentes que la firma ve al llegar al mercado se seleccionan entre aquellos que todavia tienen stock  
  
  var_of<-vector(mode = "numeric", length = fik)
  
  for (i in sample(c(1:fic),fic,replace = FALSE)) {
    
    provold<-sample(1:fik,1)
    
    varold=1
    
    
    #costo unitario laboral asociado a varold
    pkold[i,1]<-(we[i,1]/(capital[which(capital[,1]==fic+provold & capital[,2]==varold),7]*lk*sharecw)*capital[which(capital[,1]==fic+provold & capital[,2]==varold),8]+capital[which(capital[,1]==fic+provold & capital[,2]==varold),6])
    #voy a establecer los costos unitarios de cada vendedor (j) y los guardo en costosunit
    costosunit<-c(1:fik)
    for (j in 1:fik) {
      #determino la variedad de cada vendedor
      varnew<-1
      var_of[j]<-varnew
      costosunit[j]<-(we[i,1]/(capital[which(capital[,1]==fic+j & capital[,2]==varnew),7]*lk*sharecw)*capital[which(capital[,1]==fic+j & capital[,2]==varnew),8]+capital[which(capital[,1]==fic+j & capital[,2]==varnew),6])
    }
    
    
    
    #determino la oferta visible para i
    oferentes<-if (length(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)])==1){
      c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)]
    }else{
      sample(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)], min(chik,length(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)])), replace = FALSE)
    }
    
    if(length(oferentes)>0){
      oferentes<-cbind(oferentes,costosunit[oferentes],var_of[oferentes])
      
      #costo unitario laboral asociado al mejor varnew
      pknew[i,1]<-min(oferentes[,2])
      
      #proveedor candidato a sustituir al anterior
      provnew<-oferentes[which.min(oferentes[,2]),1]
      
      #Eligo vendedor
      if (capital[which(capital[,1]==(fic+provold)),3]>0){
        if (capital[which(capital[,1]==(fic+provnew)),3]>0){
          if (pknew[i,1]<pkold[i,1]){Prsk[i,1]<-1-exp(epsilonk*(pknew[i,1]-pkold[i,1])/pknew[i,1])}else{Prsk[i,1]<-0}
          #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA FIRMA DE CONSUMO ATENDIENDO ESTA PROBABILIDAD
          kmarket[i,1]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsk[i,1],Prsk[i,1]))
          if (kmarket[i,1] == provold) {varcom<-varold}else{varcom<-oferentes[which(oferentes[,1]==provnew),3]}
        }else{#si provnew agoto la oferta
          kmarket[i,1]<-provold
          varcom<-varold
        } 
      }else{if (capital[which(capital[,1]==(fic+provnew)),3]>0){
        kmarket[i,1]<-provnew
        varcom<-oferentes[which(oferentes[,1]==provnew),3]}
      }
    }else{#si oferentes es vacio
      if(capital[which(capital[,1]==fic+provold),3]>0){
        kmarket[i,1]<-provold
        varcom<-varold
      }}    
    
    #Determino la cantidad demandada    
    for (j in 1:max(capital[which(capital[,1]==i),2])){
      if(capital[which(capital[,1]==i & capital[,2]==j),8]==capital[which(capital[,1]==i & capital[,2]==j),5]){
        kobsoleto[i,1]<-kobsoleto[i,1]+capital[which(capital[,1]==i & capital[,2]==j),3]
      }
    }
    #inversion deseada
    idx[i,1]<-max(0,kobsoleto[i,1]+(gd[i,1])*K[i,1])
    #inversion deseada nominal
    Idx[i,1]<-idx[i,1]*capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
    
    
    #ME FIJO SI HAY STOCK Y SINO HAY QUE BUSCAR OTRO PROVEEDOR
    falta<-idx[i,1]
    transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3])
    if (transado > 0.0001){
      #Agrego la cantidad de capital demandada a fic. Lo incorporo con utilizacion deseada 0 xq se utilizan en el proximo periodo, no en este    
      #Determino la variedad y lo incorporo con ella
      if(#si fic tiene capital con la misma productividad
        length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]))!=0){
        num_var<-min(capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
        capital[which(capital[,1]==i & capital[,2] >=num_var),2]<-capital[which(capital[,1]==i & capital[,2] >=num_var),2]+1
        capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
      }else{#fic no tiene capital con la misma productividad
        if(#si fic solo tiene capital mas productivo
          length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7])==0)){
          num_var<-max(capital[which(capital[,1]==i),2])+1  
          capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
        }else{#fic no tiene capital de igual productividad (hay que introducir una variedad nueva) y tiene capital de menor productividad (no sera la mayor variedad)
          num_var<-min(capital[which(capital[,1]==i & capital[,7] < capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
          capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
          capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
        }
      }
      falta<-falta-transado
      #ingreso la venta de fik
      s[fic+kmarket[i,1],1]<-s[fic+kmarket[i,1],1]+transado
      #Quito el stock de la fik
      capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]-transado
      #Registro la deuda
      apagar[i,kmarket[i,1]]<- transado * capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
      apagarp[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
      apagarv[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]
      apagara[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),5]
      
      #ahora fic elige entre los que quedan para completar su inversion deseada
      if(length(oferentes)>0){
        while (falta > 0.0001 & sum(capital[which(capital[,1] %in% (fic+oferentes[,1])),3],capital[which(capital[,1] %in% (fic+provold)),3]) >0){ 
          
          if (dim(oferentes)[1]>1){
            oferentes<-matrix(data=oferentes[-which.min(oferentes[,2]),],ncol=3)
          }
          
          #costo unitario laboral asociado al mejor varnew
          pknew[i,1]<-min(oferentes[,2])
          
          #proveedor candidato a sustituir al anterior
          provnew<-oferentes[which.min(oferentes[,2]),1]
          varnew<-oferentes[which.min(oferentes[,2]),3]
          
          if (capital[which(capital[,1]==(fic+provold)),3]>0){
            if (capital[which(capital[,1]==(fic+provnew)),3]>0){
              if (pknew[i,1]<pkold[i,1]){Prsk[i,1]<-1-exp(epsilonk*(pknew[i,1]-pkold[i,1])/pknew[i,1])}else{Prsk[i,1]<-0}
              #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA FIRMA DE CONSUMO ATENDIENDO ESTA PROBABILIDAD
              kmarket[i,1]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsk[i,1],Prsk[i,1]))
              if (kmarket[i,1] == provold) {varcom<-varold}else{varcom<-varnew}
            }else{
              kmarket[i,1] <- provold
              varcom<-varold
            }
          }else{if (capital[which(capital[,1]==(fic+provnew)),3]>0){
            kmarket[i,1]<-provnew
            varcom<-varnew}
          }
          
          #determino la cantidad transada
          transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3])
          
          #Agrego la cantidad de capital transada a fic ordenada por productividad. Lo incorporo con utilizacion deseada 0 xq se utilizan en el proximo periodo, no en este
          if(#si fic tiene capital con la misma productividad
            length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]))!=0){
            if(#si tiene capital con la misma productividad y antig?edad
              length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7] & capital[,5] == 0))!=0){
              num_var<-capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7] & capital[,5] == 0),2]
              capital[which(capital[,1]==i & capital[,2] == num_var),3]<-capital[which(capital[,1]==i & capital[,2] == num_var),3]+transado
            }else{#si tiene capital con la misma productividad, pero con distinta antig?edad. Incorporo una nueva variedad creciente con la antig?edad
              num_var<-min(capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
              capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }
          }else{#fic no tiene capital con la misma productividad
            if(#si fic solo tiene capital mas productivo
              length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7])==0)){
              num_var<-max(capital[which(capital[,1]==i),2])+1  
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }else{#fic no tiene capital de igual productividad (hay que introducir una variedad nueva) y tiene capital de menor productividad (no sera la mayor variedad)
              num_var<-min(capital[which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
              capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }
          }
          falta<-falta-transado
          #ingreso la venta de fik
          s[fic+kmarket[i,1],1]<-s[fic+kmarket[i,1],1]+transado
          #Quito el stock de la fik
          capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]-transado
          #Registro la deuda
          apagar[i,kmarket[i,1]]<- apagar[i,kmarket[i,1]] + transado * capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
          apagarp[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
          apagarv[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]
          apagara[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),5]    
        }
      }else{if(capital[which(capital[,1] %in% (fic+provold)),3] > 0.0001){
        while (falta > 0.0001 & capital[which(capital[,1] %in% (fic+provold)),3] > 0.0001){ 
          kmarket[i,1] <- provold
          varcom<-varold
          #determino la cantidad transada
          transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3])
          
          #Agrego la cantidad de capital transada a fic ordenada por productividad. Lo incorporo con utilizacion deseada 0 xq se utilizan en el proximo periodo, no en este
          if(#si fic tiene capital con la misma productividad
            length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]))!=0){
            if(#si tiene capital con la misma productividad y antig?edad
              length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7] & capital[,5] == 0))!=0){
              num_var<-capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7] & capital[,5] == 0),2]
              capital[which(capital[,1]==i & capital[,2] == num_var),3]<-capital[which(capital[,1]==i & capital[,2] == num_var),3]+transado
            }else{#si tiene capital con la misma productividad, pero con distinta antig?edad. Incorporo una nueva variedad creciente con la antig?edad
              num_var<-min(capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
              capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >=num_var),2]+1
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }
          }else{#fic no tiene capital con la misma productividad
            if(#si fic solo tiene capital mas productivo
              length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7])==0)){
              num_var<-max(capital[which(capital[,1]==i),2])+1  
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }else{#fic no tiene capital de igual productividad (hay que introducir una variedad nueva) y tiene capital de menor productividad (no sera la mayor variedad)
              num_var<-min(capital[which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]),2])
              capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
              capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7],kapa0))
            }
          }
          falta<-falta-transado
          #ingreso la venta de fik
          s[fic+kmarket[i,1],1]<-s[fic+kmarket[i,1],1]+transado
          #Quito el stock de la fik
          capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),3]-transado
          #Registro la deuda
          apagar[i,kmarket[i,1]]<- apagar[i,kmarket[i,1]] + transado * capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
          apagarp[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),6]
          apagarv[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),7]
          apagara[i,kmarket[i,1]]<-capital[which(capital[,1]==fic+kmarket[i,1] & capital[,2]==varcom),5]    
        }
        
      }
      }
    }
  }
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  
  #10 Credit demand----
  
  #Considera usar depositos antes de pedir prestado
  
  for (i in 1:fic) {
    #Dividendos esperados
    Dive[i,1]<-(Divc1/fic)*(1+gss)
    #Operating cash flows esperados
    OCFe[i,1]<-OCFc1/fic
    
  }
  for (i in (fic+1):(fic+fik)) {
    #Dividendos esperados
    Dive[i,1]<-(Divk1/fik)*(1+gss)
    #Operating cash flows esperados
    OCFe[i,1]<-OCFk1/fik
    
  }
  
  for (i in 1:(fic+fik)) {
    
    #demanda de creditos (necesidad de financiamiento menos fondos propios)
    necfin<-max(Idx[i,1]+Dive[i,1]+sigma*we[i,1]*Nd[i,1]-OCFe[i,1],0)
    
    #Primero lo intenta cubrir con fondos propios
    if(deposits[which(deposits[,1]==i),3]<=0){Ld[i,1]<-necfin}else{
      Ld[i,1]<-max(0,necfin-deposits[which(deposits[,1]==i),3])
      disponible[i]<-disponible[i]+min(necfin,deposits[which(deposits[,1]==i),3])
      disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]-min(necfin,deposits[which(deposits[,1]==i),3])
      deposits[which(deposits[,1]==i),3]<-max(0,deposits[which(deposits[,1]==i),3]-necfin)
    }
    
    #matching
    
    if(Ld[i,1]>0){  
      
      
      
      #credold<-proveedor en t-1
      credold<-loans[which(loans[,1]==i & loans[,6]==1),2]
      #icredold<-tasa ofrecida por credold en t
      icredold<-ilb[credold,1]
      
      #determino la oferta visible para i
      oferentes<-sample(c(1:fib)[-which(c(1:fib)==credold)],chicred,replace=FALSE)
      oferentes<-cbind(oferentes,ilb[oferentes,1])
      
      #icrednew<-tasa ofrecida por crednew en t
      icrednew<-min(oferentes[,2])
      #crednew<-banco candidato a sustituir al prestamista anterior
      crednew<-oferentes[which.min(oferentes[,2]),1]
      
      if (icrednew<icredold){Prscred[i,1]<-1-exp(epsiloncred*(icrednew-icredold)/icrednew)}else{Prscred[i,1]<-0}
      #AHORA TENGO QUE ASIGNAR EL PRESTAMISTA DE CADA FIRMA ATENDIENDO ESTA PROBABILIDAD
      loanmarket[i,1]<- sample(c(credold,crednew), 1, replace = FALSE, prob = c(1-Prscred[i,1],Prscred[i,1]))
    }
  }
  
  
  #11 Credit supply----
  #el banco le presta a todas las firmas cuyo retorno esperado del prestamo sea >=0
  
  
  #dsLd lo uso como vector auxiliar que se reescribe para cada banco en cada periodo
  
  for (b in 1:fib) {
    for (i in 1:(fic+fik)) {
      
      if (loanmarket[i,1]==b){
        
        if (Ld[i,1] > 0.0001){
          
          deudax[i,1]<-sum(loans[which(loans[,1]==i),3]*(1-((1-loans[which(loans[,1]==i),6])/loans[which(loans[,1]==i),5]))*(1+loans[which(loans[,1]==i),4]))
          
          #para calcular los beneficios esperados necesito el stock real de capital de las firmas (kreal)
          #kreal <- stock de capital ponderado por la proporcion de vida util restante
          if (x[i,2]=="c"){
            resultado9<-vector() 
            for (j in 1:max(capital[,2])){
              resultado9<-c(resultado9,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
            }
            kreal[i,1]<-sum(resultado9)
            #de todo lo que debe, se va a poder recuperar esta proporcion
            if(deudax[i,1]==0){deltax[i,1]<-1}else{
              deltax[i,1]<-max(1,kreal[i,1]*(1-iota)/deudax[i,1])
            }
          }else{deltax[i,1]<-0}
          
          Ldi<-Ld[i,1]    
          
          bene[i]<-(-7)
          
          while (Ldi > 0.0001 & bene[i] < 0.0001) { 
            
            dsLd[i]<-(ilb[b,1]+1/eta[b,1])*Ldi
            
            #Prdx -> probabilidad de default
            # OCF VOY A USAR EL ESPERADO XQ TODAViA NO SE REALIZo EL OCF. 
            if (x[i,2]=="c") {stigma<-stigmac}else{stigma<-stigmak}
            
            Prdx[i,1]<-1/(1+exp((OCFe[i,1]-stigma*dsLd[i])/dsLd[i]))
            
            
            
            
            resultado11<-vector()
            for (q in 1:(eta[b,1]-1)){
              resultado11<-c(resultado11,((1-Prdx[i,1])^q)*Prdx[i,1]*ilb[b,1]*(q-sum(0:(q-1))/eta[b,1]))
              intpas<-sum(resultado11)
            }
            
            resultado12<-vector()
            for (j in 0:(eta[b,1]-1)){
              resultado12<-c(resultado12,((1-Prdx[i,1])^j)*Prdx[i,1]*(1-j/eta[b,1])*(1-deltax[i,1]))
              amopas<-sum(resultado12)
            }
            
            bene[i]<-Ldi*(((1-Prdx[i,1])^eta[b,1])*ilb[b,1]*(eta[b,1]-sum(0:(eta[b,1]-1))/eta[b,1])+intpas-amopas)
            
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
            
            
            
            if (bene[i] < 0.0001) {Ldi <- Ldi-0.1*Ld[i,1]}
            
          }
          
          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
          
          #hasta aca el banco calculo el beneficio esperado de prestarle a cada firma bene[i]
          #la informacion imperfecta aparecio en la demanda
          #entiendo que aca el banco recibe todas las solicitudes
          if (bene[i]<0.0001){loanmarket[i,1]<-loans[which(loans[,1]==i & loans[,6]==1),2]
          Lrej[i,1]<-Ld[i,1]
          }else{
            loans<-rbind(loans,c(i,b,Ldi,ilb[b,1],eta[b,1],1+1))
            disponible[fic+fik+b]<- disponible[fic+fik+b] - Ldi
            disponible[i]<-disponible[i] + Ldi
            Lrej[i,1]<-Ld[i,1]-Ldi
          }
        }else{loanmarket[i,1]<-loans[which(loans[,1]==i & loans[,6]==1),2]
        Lrej[i,1]<-0}
        ventanilla[i,b]<-1
      }
    }
  }
  
  

  
  
  
  Lr[1]<-sum(Lrej[,1])/sum(Ld[,1])
  Lrc[1]<-sum(Lrej[which(x[,2]=="c"),1])/sum(Ld[which(x[,2]=="c"),1])
  Lrk[1]<-sum(Lrej[which(x[,2]=="k"),1])/sum(Ld[which(x[,2]=="k"),1])
  
  #loanmarket indica el ultimo banco que le presto a la firma incluido t
  loan[1]<-sum(loans[,3]*(1-((1-loans[,6])/loans[,5])))
  loanc[1]<-sum(loans[which(loans[,1]<=100),3]*(1-((1-loans[which(loans[,1]<=100),6])/loans[which(loans[,1]<=100),5])))
  loank[1]<-sum(loans[which(loans[,1]<=110 & loans[,1]>100),3]*(1-((1-loans[which(loans[,1]<=110 & loans[,1]>100),6])/loans[which(loans[,1]<=110 & loans[,1]>100),5])))

  #12 Labor markets REVISAR----
  
  
  #la firma va contratar o despedir segun su demanda de trabajo y cantidad de empleados
  #luego va a ordenar a los desocupados segun su salario de reserva y contrata al minimo
  #wD[i,t] <-salarios de reserva
  
  #extiendo los vinculos del periodo anterior
  for (h in 1:nw) {
    trabajow[h,1]<-sample(c(0:(fic+fik),999),1,prob = (c(rep(c(u0,Nwc1/nw/fic,Nwk1/nw/fik,Ngt*sharecw/nw),c(1,fic,fik,1)))))
    if(trabajow[h,1] != 0){w[h,1]<-ww0}
  }
  for (h in 1:no) {
    trabajoo[h,1]<-sample(c(0:(fic+fik),999),1,prob = (c(rep(c(u0,Noc1/no/fic,Nok1/no/fik,Ngt*shareco/no),c(1,fic,fik,1)))))
    if(trabajoo[h,1] != 0){w[(nw+h),1]<-wo0}
  }
  for (h in 1:nr) {
    trabajor[h,1]<-sample(c(0:(fic+fik),999),1,prob = (c(rep(c(u0,Nrc1/nr/fic,Nrk1/nr/fik,Ngt*sharecr/nr),c(1,fic,fik,1)))))
    if(trabajor[h,1] != 0){w[(nw+no+h),1]<-wr0}
  }
  for (h in 1:nm) {
    trabajom[h,1]<-sample(c(0:(fic+fik),999),1,prob = (c(rep(c(u0,Nmc1/nm/fic,Nmk1/nm/fik,Ngt*sharecm/nm),c(1,fic,fik,1)))))
    if(trabajom[h,1] != 0){w[(nw+no+nr+h),1]<-wm0}
  }
  
  for (i in 1:(fic+fik)) {
    Nw[i,1]<-length(which(trabajow[,1]==i))
    No[i,1]<-length(which(trabajoo[,1]==i))
    Nr[i,1]<-length(which(trabajor[,1]==i))
    Nm[i,1]<-length(which(trabajom[,1]==i))
    Nx[i,1]<-Nw[i,1]+No[i,1]+Nr[i,1]+Nm[i,1]
  }
  
  Nw[fic+fik+1,1]<-length(which(trabajow[,1]==999))
  No[fic+fik+1,1]<-length(which(trabajoo[,1]==999))
  Nr[fic+fik+1,1]<-length(which(trabajor[,1]==999))
  Nm[fic+fik+1,1]<-length(which(trabajom[,1]==999))
  Nx[fic+fik+1,1]<-Nw[fic+fik+1,1]+No[fic+fik+1,1]+Nr[fic+fik+1,1]+Nm[fic+fik+1,1]
  

  
  
  #echo una proporcion tita de trabajadores de cada firma
  
  for (i in 1:(fic+fik)){
    
    
    empleadoswi<-which(trabajow[,1]==i)
    empleadosoi<-which(trabajoo[,1]==i)
    empleadosri<-which(trabajor[,1]==i)
    empleadosmi<-which(trabajom[,1]==i)
    
    empleados<-c(rep("w",length(empleadoswi)),rep("o",length(empleadosoi)),rep("r",length(empleadosri)),rep("m",length(empleadosmi)))
    desempleados<-if (length(empleados)==1){
      empleados
    }else{
      sample(empleados,tita*length(empleados),replace = FALSE)
    }
    
    
    
    if (length(empleadoswi)>0){
      desempleadoswi<-if (length(empleadoswi)==1){
        empleadoswi
      }else{
        sample(empleadoswi,length(which(desempleados[]=="w")),replace=FALSE)
      }
      
      for (a in desempleadoswi){
        trabajow[a,1]<-0
      }
    }
    
    if (length(empleadosoi)>0){
      desempleadosoi<-if (length(empleadosoi)==1){
        empleadosoi
      }else{
        sample(empleadosoi,length(which(desempleados[]=="o")),replace=FALSE)
      }
      
      for (a in desempleadosoi){
        trabajoo[a,1]<-0
      }
    }
    
    if (length(empleadosri)>0){
      desempleadosri<-if (length(empleadosri)==1){
        empleadosri
      }else{
        sample(empleadosri,length(which(desempleados[]=="r")),replace=FALSE)
      }
      
      for (a in desempleadosri){
        trabajor[a,1]<-0
      }
    }
    
    if (length(empleadosmi)>0){
      desempleadosmi<-if (length(empleadosmi)==1){
        empleadosmi
      }else{
        sample(empleadosmi,length(which(desempleados[]=="m")),replace=FALSE)
      }
      
      for (a in desempleadosmi){
        trabajom[a,1]<-0
      }
    }
  }
  
  
  #echo funcionarios publicos
  
  empleadoswi<-which(trabajow[,1]==999)
  empleadosoi<-which(trabajoo[,1]==999)
  empleadosri<-which(trabajor[,1]==999)
  empleadosmi<-which(trabajom[,1]==999)
  
  empleados<-c(rep("w",length(empleadoswi)),rep("o",length(empleadosoi)),rep("r",length(empleadosri)),rep("m",length(empleadosmi)))
  desempleados<-if (length(empleados)==1){
    empleados
  }else{
    sample(empleados,tita*length(empleados),replace = FALSE)
  }
  
  
  
  
  
  if (length(empleadoswi)>0){
    desempleadoswi<-if (length(empleadoswi)==1){
      empleadoswi
    }else{
      sample(empleadoswi,length(which(desempleados[]=="w")),replace=FALSE)
    }
    
    for (a in desempleadoswi){
      trabajow[a,1]<-0
    }
  }
  
  if (length(empleadosoi)>0){
    desempleadosoi<-if (length(empleadosoi)==1){
      empleadosoi
    }else{
      sample(empleadosoi,length(which(desempleados[]=="o")),replace=FALSE)
    }
    
    for (a in desempleadosoi){
      trabajoo[a,1]<-0
    }
  }
  
  if (length(empleadosri)>0){
    desempleadosri<-if (length(empleadosri)==1){
      empleadosri
    }else{
      sample(empleadosri,length(which(desempleados[]=="r")),replace=FALSE)
    }
    
    for (a in desempleadosri){
      trabajor[a,1]<-0
    }
  }
  
  if (length(empleadosmi)>0){
    desempleadosmi<-if (length(empleadosmi)==1){
      empleadosmi
    }else{
      sample(empleadosmi,length(which(desempleados[]=="m")),replace=FALSE)
    }
    
    for (a in desempleadosmi){
      trabajom[a,1]<-0
    }
  }
  
  
  #Actualizo plantillas
  for (i in 1:(fic+fik)){
    #FIRMAS
    Nw[i,1]<-length(which(trabajow[,1]==i))
    No[i,1]<-length(which(trabajoo[,1]==i))
    Nr[i,1]<-length(which(trabajor[,1]==i))
    Nm[i,1]<-length(which(trabajom[,1]==i))
    Nx[i,1]<-Nw[i,1]+No[i,1]+Nr[i,1]+Nm[i,1]
  }
  #Gobierno    
  Nw[fic+fik+1,1]<-length(which(trabajow[,1]==999))
  No[fic+fik+1,1]<-length(which(trabajoo[,1]==999))
  Nr[fic+fik+1,1]<-length(which(trabajor[,1]==999))
  Nm[fic+fik+1,1]<-length(which(trabajom[,1]==999))
  Nx[fic+fik+1,1]<-Nw[fic+fik+1,1]+No[fic+fik+1,1]+Nr[fic+fik+1,1]+Nm[fic+fik+1,1]
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  
  #La firma contrata o despide trabajadores segun su produccion planificada
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  
  #MERCADO DE OPERARIOS
  
  for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
    
    if (i == 999){
      #Gobierno
      
      empleadoswi<-which(trabajow[,1]==999)
      if (length(empleadoswi)==Ndw[fic+fik+1,1]){"ok"}else{
        #contrata trabajadores
        if(length(empleadoswi) < Ndw[fic+fik+1,1] & length(which(trabajow[,1]==0)) > 0){
          candidatoswi<-if (length(which(trabajow[,1]==0))==1){
            which(trabajow[,1]==0)
          }else{
            sample(which(trabajow[,1]==0),min(length(which(trabajow[,1]==0)),chiw*(Ndw[fic+fik+1,1]-Nw[fic+fik+1,1])),replace = FALSE)
          }
          
          wdcandidatoswi<-cbind(candidatoswi,wD[candidatoswi,1])
          while(length(empleadoswi) < Ndw[fic+fik+1,1] & dim(wdcandidatoswi)[1]>0){
            contratado<-wdcandidatoswi[which.min(wdcandidatoswi[,2]),1]
            trabajow[contratado,1]<-999
            w[contratado,1]<-wD[contratado,1]
            wdcandidatoswi<-matrix(wdcandidatoswi[-which(wdcandidatoswi[,1]==contratado),],ncol=2)
            empleadoswi<-which(trabajow[,1]==999)
          }
          
          
        }else{
          #despide trabajadores
          empleadoswi<-which(trabajow[,1]==999)
          if(min(length(empleadoswi),length(empleadoswi)-Ndw[fic+fik+1,1])>0){
            desempleadoswi<-if(length(empleadoswi)==length(empleadoswi)-Ndw[fic+fik+1,1]){
              empleadoswi
            }else{
              sample(empleadoswi,length(empleadoswi)-Ndw[fic+fik+1,1],replace=FALSE)
            }
            
            
            for (a in desempleadoswi){
              trabajow[a,1]<-0
            }   
          }
        }
        empleadoswi<-which(trabajow[,1]==999)
      }
      Nw[fic+fik+1,1]<-length(which(trabajow[,1]==999))
    }else{
      
      #FIRMAS
      
      empleadoswi<-which(trabajow[,1]==i)
      if (length(empleadoswi)==Ndw[i,1]){"ok"}else{
        #contrata trabajadores
        if(length(empleadoswi)<Ndw[i,1] & length(which(trabajow[,1]==0))>0){
          candidatoswi<-if (length(which(trabajow[,1]==0))==1){
            which(trabajow[,1]==0)
          }else{
            sample(which(trabajow[,1]==0),min(chiw*(Ndw[i,1]-Nw[i,1]),length(which(trabajow[,1]==0))),replace = FALSE)
          }
          
          wdcandidatoswi<-cbind(candidatoswi,wD[candidatoswi,1])
          while(length(empleadoswi)<Ndw[i,1] & dim(wdcandidatoswi)[1]>0){
            contratado<-wdcandidatoswi[which.min(wdcandidatoswi[,2]),1]
            trabajow[contratado,1]<-i
            w[contratado,1]<-wD[contratado,1]
            wdcandidatoswi<-matrix(wdcandidatoswi[-which(wdcandidatoswi[,1]==contratado),],ncol=2)
            empleadoswi<-which(trabajow[,1]==i)
          }
          
        }else{
          #despide trabajadores
          empleadoswi<-which(trabajow[,1]==i)
          if(min(length(empleadoswi),length(empleadoswi)-Ndw[i,1])>0){
            desempleadoswi<-if(length(empleadoswi)==min(length(empleadoswi),length(empleadoswi)-Ndw[i,1])){
              empleadoswi
            }else{
              sample(empleadoswi,min(length(empleadoswi),length(empleadoswi)-Ndw[i,1]),replace=FALSE)
            }
            
            
            for (a in desempleadoswi){
              trabajow[a,1]<-0
            }
          }
        }
        empleadoswi<-which(trabajow[,1]==i)
      }
      
      Nw[i,1]<-length(which(trabajow[,1]==i))
    }
  }    
  

  #MERCADO DE OFICINISTAS
  
  
  for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
    
    if (i == 999){
      #Gobierno
      ratiowo<-1/2#shareco/sharecw
      empleadosoi<-which(trabajoo[,1]==999)
      if (length(empleadosoi)==ceiling(Nw[fic+fik+1,1]*ratiowo)){"ok"}else{
        #contrata trabajadores
        if(length(empleadosoi)<ceiling(Nw[fic+fik+1,1]*ratiowo) & length(which(trabajoo[,1]==0))>0){
          candidatosoi<-if (length(which(trabajoo[,1]==0))==1){
            which(trabajoo[,1]==0)
          }else{
            sample(which(trabajoo[,1]==0),min(chio*(Ndo[fic+fik+1,1]-No[fic+fik+1,1]),length(which(trabajoo[,1]==0))),replace = FALSE)
          }
          
          wdcandidatosoi<-cbind(candidatosoi,wD[nw+candidatosoi,1])
          while(length(empleadosoi)<ceiling(Nw[fic+fik+1,1]*ratiowo) & dim(wdcandidatosoi)[1]>0){
            contratado<-wdcandidatosoi[which.min(wdcandidatosoi[,2]),1]
            trabajoo[contratado,1]<-999
            w[nw+contratado,1]<-wD[nw+contratado,1]
            wdcandidatosoi<-matrix(wdcandidatosoi[-which(wdcandidatosoi[,1]==contratado),],ncol=2)
            empleadosoi<-which(trabajoo[,1]==999)
          }
          
          
        }else{
          #despide trabajadores
          empleadosoi<-which(trabajoo[,1]==999)
          if(min(length(empleadosoi),length(empleadosoi)-ceiling(Nw[fic+fik+1,1]*ratiowo))>0){
            desempleadosoi<-if(length(empleadosoi)==length(empleadosoi)-ceiling(Nw[fic+fik+1,1]*ratiowo)){
              empleadosoi
            }else{
              sample(empleadosoi,length(empleadosoi)-ceiling(Nw[fic+fik+1,1]*ratiowo),replace=FALSE)
            }
            
            
            for (a in desempleadosoi){
              trabajoo[a,1]<-0
            }   
          }
        }
        empleadosoi<-which(trabajoo[,1]==999)
      }
      No[fic+fik+1,1]<-length(which(trabajoo[,1]==999))
    }else{
      
      #FIRMAS
      
      if (x[i,2]=="k") {
        ratiowo<-1/4#shareko/sharekw
      }else{ #if (x[i,2]=="c"){
        ratiowo<-1/2#shareco/sharecw
      }
      empleadosoi<-which(trabajoo[,1]==i)
      if (length(empleadosoi)==floor(Nw[i,1]*ratiowo)){"ok"}else{
        #contrata trabajadores
        if(length(empleadosoi)<floor(Nw[i,1]*ratiowo) & length(which(trabajoo[,1]==0))>0){
          candidatosoi<-if (length(which(trabajoo[,1]==0))==1){
            which(trabajoo[,1]==0)
          }else{
            sample(which(trabajoo[,1]==0),min(chio*(Ndo[i,1]-No[i,1]),length(which(trabajoo[,1]==0))),replace = FALSE)
          }
          
          wdcandidatosoi<-cbind(candidatosoi,wD[nw+candidatosoi,1])
          while(length(empleadosoi)<floor(Nw[i,1]*ratiowo) & dim(wdcandidatosoi)[1]>0){
            contratado<-wdcandidatosoi[which.min(wdcandidatosoi[,2]),1]
            trabajoo[contratado,1]<-i
            w[nw+contratado,1]<-wD[nw+contratado,1]
            wdcandidatosoi<-matrix(wdcandidatosoi[-which(wdcandidatosoi[,1]==contratado),],ncol=2)
            empleadosoi<-which(trabajoo[,1]==i)
          }
          
          
        }else{
          #despide trabajadores
          empleadosoi<-which(trabajoo[,1]==i)
          if(min(length(empleadosoi)-floor(Nw[i,1]*ratiowo),length(empleadosoi))>0){
            desempleadosoi<-if(length(empleadosoi)==length(empleadosoi)-floor(Nw[i,1]*ratiowo)){
              empleadosoi
            }else{
              sample(empleadosoi,length(empleadosoi)-floor(Nw[i,1]*ratiowo),replace=FALSE)
            }
            
            
            
            for (a in desempleadosoi){
              trabajoo[a,1]<-0
            } 
          }
        }
        empleadosoi<-which(trabajoo[,1]==i)
      }
      No[i,1]<-length(which(trabajoo[,1]==i))
    }
  }
  

  #MERCADO DE INVESTIGADORES
  
  
  for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
    
    if (i == 999){
      #Gobierno
      #No contrata investigadores
      Nr[fic+fik+1,1]<-length(which(trabajor[,1]==999))
    }else{if (x[i,2]=="c"){    
      #Firmas de consumo
      #No contratan investigadores
      Nr[i,1]<-length(which(trabajor[,1]==i))    
    }else{
      #FIRMAS DE CAPITAL    
      if (x[i,2]=="k") {
        ratiowo<-1/4#shareko/sharekw
        ratiowr<-1/4#sharekr/sharekw
        ratiowm<-1/6#sharekm/sharekw
      }
      empleadosri<-which(trabajor[,1]==i)
      if (length(empleadosri)==floor(Nw[i,1]*ratiowr)){
        "ok"
      }else{
        #contrata trabajadores
        if(length(empleadosri)<floor(Nw[i,1]*ratiowr) & length(which(trabajor[,1]==0))){
          candidatosri<-if (length(which(trabajor[,1]==0))==1){
            which(trabajor[,1]==0)
          }else{
            sample(which(trabajor[,1]==0),min(chir*(Ndr[i,1]-Nr[i,1]),length(which(trabajor[,1]==0))),replace = FALSE)
          }
          
          wdcandidatosri<-cbind(candidatosri,wD[nw+no+candidatosri,1])
          while(length(empleadosri)<floor(Nw[i,1]*ratiowr) & dim(wdcandidatosri)[1]>0){
            contratado<-wdcandidatosri[which.min(wdcandidatosri[,2]),1]
            trabajor[contratado,1]<-i
            w[nw+no+contratado,1]<-wD[nw+no+contratado,1]
            wdcandidatosri<-matrix(wdcandidatosri[-which(wdcandidatosri[,1]==contratado),],ncol=2)
            empleadosri<-which(trabajor[,1]==i)
          }
          
          
        }else{
          #despide trabajadores
          empleadosri<-which(trabajor[,1]==i)
          if(min(length(empleadosri),length(empleadosri)-floor(Nw[i,1]*ratiowr)) > 0){
            desempleadosri<-if(length(empleadosri)==length(empleadosri)-floor(Nw[i,1]*ratiowr)){
              empleadosri
            }else{
              sample(empleadosri,min(length(empleadosri),length(empleadosri)-floor(Nw[i,1]*ratiowr)),replace = FALSE)#min(ceiling(Nw[i,1]*ratiowr),ceiling(No[i,1]*ratiowr/ratiowo))),replace=FALSE)
            }
            
            
            for (a in desempleadosri){
              trabajor[a,1]<-0
            } 
          }
        }
        empleadosri<-which(trabajor[,1]==i)
      }
      Nr[i,1]<-length(which(trabajor[,1]==i))
    }
    }   
  }
  #MERCADO DE MANAGERS
  
  for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
    
    if (i == 999){
      #Gobierno
      ratiowo<-1/2#shareco/sharecw
      ratiowr<-0#sharecr/sharecw
      ratiowm<-1/6#sharecm/sharecw
      
      empleadosmi<-which(trabajom[,1]==999)
      if (length(empleadosmi)==ceiling(Nw[fic+fik+1,1]*ratiowm)){"ok"}else{
        #contrata trabajadores
        if(length(empleadosmi)<ceiling(Nw[fic+fik+1,1]*ratiowm) & length(which(trabajom[,1]==0))>0){
          candidatosmi<-if (length(which(trabajom[,1]==0))==1){
            which(trabajom[,1]==0)
          }else{
            sample(which(trabajom[,1]==0),min(chim*(Ndm[fic+fik+1,1]-Nm[fic+fik+1,1]),length(which(trabajom[,1]==0))),replace = FALSE)
          }
          
          wdcandidatosmi<-cbind(candidatosmi,wD[nw+no+nr+candidatosmi,1])
          while(length(empleadosmi)<ceiling(Nw[fic+fik+1,1]*ratiowm) & dim(wdcandidatosmi)[1]>0){
            contratado<-wdcandidatosmi[which.min(wdcandidatosmi[,2]),1]
            trabajom[contratado,1]<-999
            w[nw+no+nr+contratado,1]<-wD[nw+no+nr+contratado,1]
            wdcandidatosmi<-matrix(wdcandidatosmi[-which(wdcandidatosmi[,1]==contratado),],ncol=2)
            empleadosmi<-which(trabajom[,1]==999)
          }
          
          
        }else{
          #despide trabajadores
          empleadosmi<-which(trabajom[,1]==999)
          if(min(length(empleadosmi),length(empleadosmi)-ceiling(Nw[fic+fik+1,1]*ratiowm))>0){
            desempleadosmi<-if(length(empleadosmi)==(length(empleadosmi)-ceiling(Nw[fic+fik+1,1]*ratiowm))){
              empleadosmi
            }else{
              sample(empleadosmi,(length(empleadosmi)-ceiling(Nw[fic+fik+1,1]*ratiowm)),replace=FALSE)
            }
            
            
            for (a in desempleadosmi){
              trabajom[a,1]<-0
            }   
          }
        }
        empleadosmi<-which(trabajom[,1]==fic+fik+1)
      }
      Nm[fic+fik+1,1]<-length(which(trabajom[,1]==999))
    }else{
      
      #FIRMAS
      
      if (x[i,2]=="k") {
        ratiowo<-1/4#shareko/sharekw
        ratiowr<-1/4#sharekr/sharekw
        ratiowm<-1/6#sharekm/sharekw
      }else{ 
        ratiowo<-1/2#shareco/sharecw
        ratiowr<-0#sharecr/sharecw
        ratiowm<-1/6#sharecm/sharecw
      }
      
      empleadosmi<-which(trabajom[,1]==i)
      if (length(empleadosmi)==floor(Nw[i,1]*ratiowm)){
        "ok"
      }else{
        #contrata trabajadores
        if(length(empleadosmi)<floor(Nw[i,1]*ratiowm) & length(which(trabajom[,1]==0))>0){
          candidatosmi<-if (length(which(trabajom[,1]==0))==1){
            which(trabajom[,1]==0)
          }else{
            sample(which(trabajom[,1]==0),min(chim*(Ndm[i,1]-Nm[i,1]),length(which(trabajom[,1]==0))),replace = FALSE)
          }
          
          wdcandidatosmi<-cbind(candidatosmi,wD[nw+no+nr+candidatosmi,1])
          while(length(empleadosmi)<floor(Nw[i,1]*ratiowm) & dim(wdcandidatosmi)[1]>0){
            contratado<-wdcandidatosmi[which.min(wdcandidatosmi[,2]),1]
            trabajom[contratado,1]<-i
            w[nw+no+nr+contratado,1]<-wD[nw+no+nr+contratado,1]
            wdcandidatosmi<-matrix(wdcandidatosmi[-which(wdcandidatosmi[,1]==contratado),],ncol=2)
            empleadosmi<-which(trabajom[,1]==i)
          }
          
          
        }else{
          #despide trabajadores
          empleadosmi<-which(trabajom[,1]==i)
          if(min(length(empleadosmi),length(empleadosmi)-floor(Nw[i,1]*ratiowm))>0){
            desempleadosmi<-if(length(empleadosmi)==length(empleadosmi)-floor(Nw[i,1]*ratiowm)){
              empleadosmi
            }else{
              sample(empleadosmi,length(empleadosmi)-floor(Nw[i,1]*ratiowm),replace=FALSE)
            }
            
            
            for (a in desempleadosmi){
              trabajom[a,1]<-0
            }
          }
        }
        empleadosmi<-which(trabajom[,1]==i)
      }
      
      Nm[i,1]<-length(which(trabajom[,1]==i))
    }
  }
  

  for (i in 1:(fic+fik)){
    #FIRMAS
    Nw[i,1]<-length(which(trabajow[,1]==i))
    No[i,1]<-length(which(trabajoo[,1]==i))
    Nr[i,1]<-length(which(trabajor[,1]==i))
    Nm[i,1]<-length(which(trabajom[,1]==i))
    Nx[i,1]<-Nw[i,1]+No[i,1]+Nr[i,1]+Nm[i,1]
  }
  #Gobierno    
  Nw[fic+fik+1,1]<-length(which(trabajow[,1]==999))
  No[fic+fik+1,1]<-length(which(trabajoo[,1]==999))
  Nr[fic+fik+1,1]<-length(which(trabajor[,1]==999))
  Nm[fic+fik+1,1]<-length(which(trabajom[,1]==999))
  Nx[fic+fik+1,1]<-Nw[fic+fik+1,1]+No[fic+fik+1,1]+Nr[fic+fik+1,1]+Nm[fic+fik+1,1]
  
  
  #actualizo bases de operarios
  for (a in 1:nw) {
    if (trabajow[a,1]==0){
      w[a,1]<-0
      uh[a,1]<-1
    }else{
      uh[a,1]<-0
    }
  }
  
  #actualizo bases de oficinistas
  for (a in 1:no) {
    if (trabajoo[a,1]==0){
      w[nw+a,1]<-0
      uh[nw+a,1]<-1
    }else{
      uh[nw+a,1]<-0
    }
  }
  
  #actualizo bases de investigadores
  for (a in 1:nr) {
    if (trabajor[a,1]==0){
      w[nw+no+a,1]<-0
      uh[nw+no+a,1]<-1
    }else{
      uh[nw+no+a,1]<-0
    }
  }
  
  #actualizo bases de managers
  for (a in 1:nm) {
    if (trabajom[a,1]==0){
      w[nw+no+nr+a,1]<-0
      uh[nw+no+nr+a,1]<-1
    }else{
      uh[nw+no+nr+a,1]<-0
    }
    wh[nw+no+nr+a,1]<-disponible[fic+fik+fib+nw+no+nr+a]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+a),3]
  }
  
  #Capital accionario
  for (i in 1:(fic+fik)) {
    acciones[i,1]<-sum(wh[nw+no+nr+which(trabajom[,1]==i),1])
  }    
  
  
  

  
  
  #13 Production----
  
  #LA PRODUCCION VA A ESTAR DETERMINADA POR LA CANTIDAD DE OPERARIOS
  
  for (i in (fic+1):(fic+fik)) {
    y[i,1]<-Nw[i,1]*mun[i,1]
    if(s[i,1] > 0 & y[i,1] < s[i,1]-inv[i,1]){#se desarman ventas
      adevolver<-(s[i,1]-inv[i,1]-y[i,1])/s[i,1]
      acreedorc<-which(apagar[,i-fic]>0)
      vecank[i-fic,1]<-vecank[i-fic,1]+s[i,1]-inv[i,1]-y[i,1]
      
      for (c in acreedorc) {
        capital[which(capital[,1]==c & capital[,5]==0 & capital[,6]==apagarp[c,i-fic] & capital[,7]==apagarv[c,i-fic]),3]<-capital[which(capital[,1]==c & capital[,5]==0 & capital[,6]==apagarp[c,i-fic] & capital[,7]==apagarv[c,i-fic]),3]-adevolver*apagar[c,i-fic]/apagarp[c,i-fic]
        cocank[c,1]<-cocank[c,1]+adevolver*apagar[c,i-fic]/apagarp[c,i-fic]
        apagar[c,i-fic]<-(1-adevolver)*apagar[c,i-fic]
      }
      s[i,1]<-(1-adevolver)*s[i,1]
    }else{if (y[i,1]<yd[i,1]){
      capital[which(capital[,1]==i),3]<-capital[which(capital[,1]==i),3]-(yd[i,1]-y[i,1])
    }
    }
    inv[i,1]<-inv[i,1]+y[i,1]
  }
  
  
  for (i in 1:fic){
    if(Nw[i,1]>=Ndw[i,1]){y[i,1]<-yd[i,1]
    resultado22<-vector()  
    for (j in capital[which(capital[,1]==i & capital[,5]>0),2]) {
      resultado22<-c(resultado22,capital[which(capital[,1]==i & capital[,2]==j),4]*capital[which(capital[,1]==i & capital[,2]==j),3])
    }
    ux[i,1]<-sum(resultado22)/sum(capital[which(capital[,1]==i & capital[,5]>0),3])    
    
    }else{
      capital2<-capital[which(capital[,1]==i),] #creo una matriz auxiliar para introducir la utilizacion efectiva
      ktotal<-lk*Nw[i,1] #maximo capital a utilizar
      if (capital2[which(capital2[,1]==i & capital2[,2]==1),3]*capital2[which(capital2[,1]==i & capital2[,2]==1),4]>=ktotal) {
        capital2[which(capital2[,1]==i & capital2[,2]==1),4]<-ktotal/capital2[which(capital2[,1]==i & capital2[,2]==1),3]
        capital2[which(capital2[,1]==i & capital2[,2]!=1),4]<-0
      }else{
        capital2[which(capital2[,1]==i & capital2[,2]==1),4]<-1
        for (j in 2:max(capital2[,2])){
          resultado14<-vector()
          for (q in 1:(j-1)){
            resultado14<-c(resultado14,capital2[which(capital2[,1]==i & capital2[,2]==q),4]*capital2[which(capital2[,1]==i & capital2[,2]==q),3])
          }
          if (sum(resultado14)+capital2[which(capital2[,1]==i & capital2[,2]==j),3]*capital2[which(capital2[,1]==i & capital2[,2]==j),4]>=ktotal){
            capital2[which(capital2[,1]==i & capital2[,2]==j),4]<-max((ktotal-sum(resultado14))/capital2[which(capital2[,1]==i & capital2[,2]==j),3],0)
            capital2[which(capital2[,1]==i & capital2[,2]>j),4]<-0
          }else{
            capital2[which(capital2[,1]==i & capital2[,2]==j),4]<-1
            "sigue"}
        }
      }#Si la mejor variedad no alcanza la restriccion de capital (se pueden usar otras)    
      resultado15<-vector()
      for (j in 1:max(capital2[,2])){
        resultado15<-c(resultado15,capital2[which(capital2[,1]==i & capital2[,2]==j),3]*capital2[which(capital2[,1]==i & capital2[,2]==j),4]*capital2[which(capital2[,1]==i & capital2[,2]==j),7])
      }  
      y[i,1]<-sum(resultado15)      
      
      resultado16<-vector()
      for (j in 1:max(capital2[,2])){
        resultado16<-c(resultado16,capital2[which(capital2[,1]==i & capital2[,2]==j),3]*capital2[which(capital2[,1]==i & capital2[,2]==j),4])
      }
      if(y[i,1]>0){
        muc[i,1]<-y[i,1]/sum(resultado16)
      }
      resultado22<-vector()  
      for (j in capital2[which(capital2[,1]==i & capital2[,5]>0),2]) {
        resultado22<-c(resultado22,capital2[which(capital2[,1]==i & capital2[,2]==j),4]*capital2[which(capital2[,1]==i & capital2[,2]==j),3])
      }
      ux[i,1]<-sum(resultado22)/sum(capital2[which(capital2[,1]==i & capital2[,5]>0),3])    
      
    }
    inv[i,1]<-inv[i,1]+y[i,1]  
  }
  
  ufic[1]<-sum(capital[,3]*capital[,4])/sum(capital[,3])
  mufic[1]<-sum(muc[which(x[,2]=="c"),1]*y[which(x[,2]=="c"),1])/sum(y[which(x[,2]=="c"),1])
  #14 R&D Activity----
  
  for (i in (fic+1):(fic+fik)){
    
    #innovacion
    
    Prinn[i,1]<-1-exp(-xiinn[i,1]*Nr[i,1])
    
    if (sample(c(1,0),1,replace = TRUE,prob = c(Prinn[i,1],1-Prinn[i,1]))==1){
      #Proceso de innovacion exitoso se crea una nueva variedad de capital 
      #Si quiero que el stock de variedades anteriores no se transforme desmarco aca y 
      #marco las siguientes
      
      #Creo la nueva variedad
      capital<-rbind(capital,c(i,0,0,0,0,0,capital[which(capital[,1]==i & capital[,2]==1),7]*(1+abs(rnorm(1,mean = muFN3,sigmaFN3))),kapa0))
      #Como todo el stock de la firma de capital se transforma al innovar o imitar, solo tiene una variedad. Si corro esto la matriz capital queda vacia
      
      Nuevavariedad[i-fic,1]<-1 #matriz que indica con un 1 el periodo t en que la firma i logro una innovacion
      
      
    }else{"innovacion sin exito"}
    
    #OJO! se supone que el inventario de las firmas de capital se actualiza a la nueva variedad
    
    
    #imitacion
    
    Primi[i,1]<-(1-exp(-xiimi[i,1]*Nr[i,1]))
    
    if (sample(c(1,0),1,replace = TRUE,prob = c(Primi[i,1],1-Primi[i,1]))==1){
      aimitar<-sample(c((fic+1):(fic+fik))[-i],sample(c(1:(fik-1)),1),replace = FALSE)
      varaimi<-vector()
      for (j in aimitar) {
        varaimi<-c(varaimi,capital[which(capital[,1]==j & capital[,2]==1),7])
      }
      varimi<-max(varaimi)
      varactual<-max(capital[which(capital[,1]==i),7])
      if (varactual>=varimi){"no imita, ya produce una variedad equivalente o superior"}else{
        
        #Creo la nueva variedad imitada  
        if(length(which(capital[,1]==i & capital[,2]==0))>0){
          capital[which(capital[,1]==i & capital[,2]==0),]<-c(i,0,0,0,0,0,varimi,kapa0)
        }else{
          capital<-rbind(capital,c(i,0,0,0,0,0,varimi,kapa0))
        }
        Imitacion[i-fic,1]<-1 #matriz que indica con un 1 el periodo t en que la firma i logro una imitacion
        
      }
    }else{"R&D imitacion no dio resultado"}
    
  }
  
  
  for (i in (fic+1):(fic+fik)) {
    
    #Transformo todo el stock a la nueva variedad
    if(length(which(capital[,1]==i & capital[,2]==0))>0){
      capital[which(capital[,1]==i & capital[,2]==1),]<-c(i,1,sum(capital[which(capital[,1]==i),3]),0,0,0,capital[which(capital[,1]==i & capital[,2]==0),7],kapa0)
      capital<-capital[-which(capital[,1]==i & capital[,2]==0),]
    }
  }

  #15 Capital goods market (2)----
  #El intercambio ya esta hecho en la matriz capital. Ahora introduzco los pagos
  
  for (i in sample(1:fic,fic,replace = FALSE)){
    if(sum(apagar[i,])>0){
      acreedork<-which(apagar[i,]>0)
      if(disponible[i]+deposits[which(deposits[,1]==i),3]>=sum(apagar[i,])){
        if(disponible[i]>=sum(apagar[i,])){
          disponible[i]<-disponible[i] - sum(apagar[i,])
        }else{
          deposits[which(deposits[,1]==i),3]<-deposits[which(deposits[,1]==i),3] + disponible[i] - sum(apagar[i,])
          disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]] + disponible[i] - sum(apagar[i,])
          disponible[i]<-0
        }
        disponible[fic+acreedork]<-disponible[fic+acreedork]+apagar[i,acreedork]
        inversion[i,1]<-sum(apagar[i,which(apagar[i,]>0)])
        apagar[i,]<-0
        apagarp[i,]<-0
        apagarv[i,]<-0
        apagara[i,]<-0
      }else{
        #No compra todo lo que pidio. Cancela una parte
        acancelar<-(sum(apagar[i,])-disponible[i]-deposits[which(deposits[,1]==i),3])/sum(apagar[i,])
        
        #caso de default 
        #se desarma la venta de fik cancelada
        for (k in acreedork) {
          cocanc[i,1]<-cocanc[i,1]+(apagar[i,k]/apagarp[i,k])*acancelar
          vecanc[k,1]<-vecanc[k,1]+(apagar[i,k]/apagarp[i,k])*acancelar
          s[fic+k,1]<-s[fic+k,1]-(apagar[i,k]/apagarp[i,k])*acancelar
          #Recompongo el stock de la fik
          capital[which(capital[,1]==fic+k),3]<-capital[which(capital[,1]==fic+k),3]+(apagar[i,k]/apagarp[i,k])*acancelar
          #Quito el pedido de capital de fic
          capital[which(capital[,1]==i & capital[,5]==0 & capital[,7]==apagarv[i,k]),3]<-capital[which(capital[,1]==i & capital[,5]==0 & capital[,7]==apagarv[i,k]),3]-(apagar[i,k]/apagarp[i,k])*acancelar
        }
        #Pago los pedidos que mantuve
        if(disponible[i]>=sum(apagar[i,])*(1-acancelar)){
          disponible[i]<-disponible[i] - sum(apagar[i,])*(1-acancelar)
        }else{
          deposits[which(deposits[,1]==i),3]<-deposits[which(deposits[,1]==i),3] + disponible[i] - sum(apagar[i,])*(1-acancelar)
          disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]] + disponible[i] - sum(apagar[i,])*(1-acancelar)
          disponible[i]<-0
        }
        disponible[fic+acreedork]<-disponible[fic+acreedork]+apagar[i,acreedork]*(1-acancelar)
        inversion[i,1]<-sum(apagar[i,which(apagar[i,]>0)])*(1-acancelar)
        apagar[i,]<-0
        apagarp[i,]<-0
        apagarv[i,]<-0
        apagara[i,]<-0
      }
    }
  }

  
  
  #16 Consumption goods market----
  
  
  
  
  ofcons<-inv[c(1:fic),1]
  
  #esto es solo porque es el primer periodo
  constrans1<-cbind(h=c(1:(nw+no+nr+nm)),fic=sample(1:fic,nw+no+nr+nm,replace=TRUE),c=0,p=pc1)#al cambiar del periodo tengo que cambiar constrans1 por constrans
  constrans<-constrans1
  NIh[1:nw,1]<-sample(c(ww0,omega*ww0),1,prob = c(1-u0,u0))+dw0*idb0
  NIh[(nw+1):(nw+no),1]<-sample(c(wo0,omega*ww0),1,prob = c(1-u0,u0))+do0*idb0
  NIh[(nw+no+1):(nw+no+nr),1]<-sample(c(wr0,omega*ww0),1,prob = c(1-u0,u0))+do0*idb0
  NIh[(nw+no+nr+1):(nw+no+nr+nm),1]<-sample(c(wm0,omega*ww0),1,prob = c(1-u0,u0))+dm0*idb0+(Divc1+Divk1+Divb1)/nm
  Sh<-NIh
  #
  #NIh en este periodo es lo pagado en el periodo anterior (probabilidad u0 de haber estado desempleado)
  #Sh arranca igual a NIh antes de consumir
  
  
  
  #consumo deseado
  for (h in sample(1:(nw+no+nr+nm),(nw+no+nr+nm),replace = FALSE)) {
    peh[h,1]<-pc1  + lambda * (mean(constrans1[which(constrans1[,1]==h),4]) - pc1)
    
    if (x[fic+fik+fib+h,2]=="w"){
      alfa<-alfaw
      cdh0<-alfa*ww0}else{
        if (x[fic+fik+fib+h,2]=="m"){
          alfa<-alfam
          cdh0<-alfa*wm0}else{
            alfa<-alfaor
            cdh0<-alfa*wo0
          }
      }
    
    
    cdh[h,1]<-max(0,alfa * NIh[h,1] / peh[h,1],beta * cdh0)
    #supongo que su consumo pasado equivale a lo que habrian consumido si hubieran estado trabajando
    
    #Matching
    
    #provold<-proveedor en t-1
    provold<-sample(1:fic,1,replace=TRUE)
    
    #precio de provold
    pold[h,1]<-px[provold,1]
    #determino la oferta visible para h
    oferentes<-if (length(c(1:fic)[-which(ofcons[]==0)])==1){
      c(1:fic)[-which(ofcons[]==0)]
    }else{
      sample(c(1:fic)[-which(ofcons[]==0)], min(chic,length(c(1:fic)[-which(ofcons[]==0)])), replace = FALSE)
    }
    
    oferentes<-cbind(oferentes,px[oferentes,1],ofcons[oferentes])
    
    falta<-cdh[h,1]
    
    #ahora h elige proveedor para completar su inversion deseada
    while (falta > 0 & (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3])>0 & sum(ofcons[c(oferentes[,1],provold)]) >0){ 
      
      #proveedor candidato a sustituir al anterior
      provnew<-oferentes[which.min(oferentes[which(ofcons[oferentes[,1]]> 0),2]),1]
      #precio del mejor oferente
      if (length(provnew)>0){pnew[h,1]<-px[provnew,1]}
      
      if (pnew[h,1]<pold[h,1] & pnew[h,1] > 0){Prsc[h,1]<-1-exp(epsilonc*(pnew[h,1]-pold[h,1])/pnew[h,1])}else{Prsc[h,1]<-0}
      #Le pongo el >0 porque si oferentes no tiene stock voy acumulando resultados "integrer(0)" y en ese caso va con provold

      #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA HOGAR ATENDIENDO ESTA PROBABILIDAD
      if (ofcons[provold]>0){
        if (length(provnew)>0){
          consmarket[h,1]<-sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsc[h,1],Prsc[h,1]))
        }else{consmarket[h,1]<-provold}
      }else{consmarket[h,1]<-provnew}
      #Concreto el matching
      transado<-min(falta,(disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3])/px[consmarket[h,1],1],ofcons[consmarket[h,1]])
      #h consume
      ch[h,1]<-ch[h,1]+transado
      #gasta
      Sh[h,1]<-Sh[h,1]-transado*px[consmarket[h,1],1]
      if (disponible[fic+fik+fib+h] > transado*px[consmarket[h,1],1]){
        disponible[fic+fik+fib+h]<- disponible[fic+fik+fib+h] - transado*px[consmarket[h,1],1]
      }else{
        deposits[which(deposits[,1]==fic+fik+h),3]<- deposits[which(deposits[,1]==fic+fik+h),3]  - transado*px[consmarket[h,1],1] + disponible[fic+fik+fib+h]
        disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]  - transado*px[consmarket[h,1],1] + disponible[fic+fik+fib+h]
        disponible[fic+fik+fib+h]<-0
      }
      #la firma vende
      s[consmarket[h,1],1]<-s[consmarket[h,1],1] + transado
      disponible[consmarket[h,1]]<-disponible[consmarket[h,1]] + transado*px[consmarket[h,1],1]
      #se registra la interaccion
      constrans<-rbind(constrans,c(h,consmarket[h,1],transado,px[consmarket[h,1],1]))
      #Quito el stock de la fic
      ofcons[consmarket[h,1]]<-ofcons[consmarket[h,1]]-transado
      oferentes[,3]<-ofcons[oferentes[,1]]
      inv[consmarket[h,1],1]<-inv[consmarket[h,1],1]-transado
      #satisfazgo
      falta<-falta-transado
      #Saco al oferente de la lista porque si no, no termina el while, se quedo sin oferta
      if (dim(oferentes)[1]>1){
        oferentes<-matrix(data=oferentes[-which.min(oferentes[,2]),],ncol=3)
      }    
      
    }
    #si no son necesarias, saco las filas que cree al principio, cuando hice la matriz
    if(length(which(constrans[,1]==h & constrans[,3]>0))>0 & length(which(constrans[,1]==h & constrans[,3]==0))>0){constrans<-as.matrix(constrans[-which(constrans[,1]==h & constrans[,3]==0),])
    }  
  }

  
  #17 Interest, bonds and loans repayment----

  
  #las firmas pagan intereses sobre los prestamos y amortizan una proporcion cte del principal
  #si tiene fondos la firma paga y el banco cobra    
  for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
    cuota<-sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1/loans[which(loans[,1]==i & loans[,6] <= 1),5]+loans[which(loans[,1]==i & loans[,6] <= 1),4]*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5])))
    if (disponible[i]+deposits[which(deposits[,1]==i),3]>=cuota){
      for (b in 1:fib){
        if(length(which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b))>0){
          disponible[fic+fik+b]<-disponible[fic+fik+b]+sum(loans[which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b),3]*(1/loans[which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b),5]+loans[which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b),4]*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b),6])/loans[which(loans[,1]==i & loans[,6] <= 1 & loans[,2]==b),5])))
        }
      }
      if (disponible[i]>=cuota){
        disponible[i]<-disponible[i]-cuota
      }else{
        deposits[which(deposits[,1]==i),3]<-deposits[which(deposits[,1]==i),3]+disponible[i]-cuota
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-cuota
        disponible[i]<-0}
    }else{
      #caso de default
      default[i,1]<-1
      #Si le dieron un prestamo este anio no lo considero en el default
      #El capital que compro este anio, si considero que lo puede vender
      if (x[i,2]=="c"){
        resultado17<-vector() 
        for (j in 1:max(capital[which(capital[,1]==i),2])){
          resultado17<-c(resultado17,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
        }
        kreal[i,1]<-sum(resultado17)
        #de todo lo que debe, se va a poder recuperar esta proporcion
        if(deudax[i,1]==0){deltax[i,1]<-1}else{
          deltax[i,1]<-min(max(kreal[i,1]*(1-iota)/deudax[i,1],0),1)
        }
        #actualizo acciones
        acciones[i,1]<-sum(wh[nw+no+nr+which(trabajom[,1]==i),1])
        #los manager compran el capital
        for (h in 1:nm) {
          if(trabajom[h,1]==i & wh[nw+no+nr+h,1]>0){
            parte<-acciones[i,1]/wh[nw+no+nr+h,1]
            if (disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]>=deltax[i,1]*deudax[i,1]*parte){
              aportec[h,i]<-deltax[i,1]*deudax[i,1]*parte
              #el banco cobra
              for (b in 1:fib){
                if(length(which(loans[,1]==i & loans[,2]==b))>0){
                  
                  disponible[fic+fik+b]<-disponible[fic+fik+b]+sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))*deltax[i,1]*parte
                  recupero[b,1]<-recupero[b,1]+sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))*deltax[i,1]*parte
                  perdida[b,1]<-perdida[b,1]+sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))*(1-deltax[i,1]*parte)
                }
              }
              if (disponible[fic+fik+fib+nw+no+nr+h]>=deltax[i,1]*deudax[i,1]*parte){
                disponible[fic+fik+fib+nw+no+nr+h]<-disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,1]*deudax[i,1]*parte
              }else{
                deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]<-deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]+disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,1]*deudax[i,1]*parte
                disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]+disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,1]*deudax[i,1]*parte
                disponible[fic+fik+fib+nw+no+nr+h]<-0
              }
            }else{
              #el manager paga con todo su capital
              puede_pagar<-sum(disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
              aportec[h,i]<-puede_pagar
              prop_a_cobrar<-puede_pagar/(deltax[i,1]*deudax[i,1]*parte)
              for (b in 1:fib){
                if(length(which(loans[,1]==i & loans[,2]==b))>0){
                  
                  disponible[fic+fik+b]<-disponible[fic+fik+b]+deltax[i,1]*parte*prop_a_cobrar*sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
                  recupero[b,t]<-recupero[b,t]+deltax[i,1]*parte*prop_a_cobrar*sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
                  perdida[b,t]<-perdida[b,t]+(1-deltax[i,1]*parte*prop_a_cobrar)*sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*(1+loans[which(loans[,1]==i & loans[,6] <= 1),4])*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
                }
              }
              disponible[fic+fik+fib+nw+no+nr+h]<-0
              disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
              deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]<-0
            }
          }
        }
        for (b in 1:fib) {
          if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1))>0){
            perdida[b,1]<-perdida[b,1]+round(sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1),3]),digits = 4)
            loandefhoy[i,1]<-loandefhoy[i,1]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1),3])
          }
        }
      }else{
        for (b in 1:fib) {
          if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]<=1))>0){
            perdida[b,1]<-perdida[b,1]+round(sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=1),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=1),4])*(1-(1-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=1),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=1),5])),digits = 4)
          }
        }
        
        for (b in 1:fib) {
          if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1))>0){
            perdida[b,1]<-perdida[b,1]+round(sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1),3]),digits = 4)
            loandefhoy[i,1]<-loandefhoy[i,1]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==1+1),3])
          }
        }
      }
      #se cancela el credito contra lo recibido y el resto perdidas   
      loans<-as.matrix(loans[-which(loans[,1]==i),])        
      
    }
  }
  
  
  
  #el gobierno paga los bonos e intereses
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-(1+itechob0)*B1
  for (b in 1:fib){
    disponible[fic+fik+b]<-disponible[fic+fik+b]+Bb1/fib*(1+itechob0)
  }  
  disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]+Bcb1*(1+itechob0)
  
  
  
  
  #los bancos pagan intereses sobre depositos y avances del banco central con intereses
  for (b in 1:fib){
    saldepinicial<-matrix(ncol = 2,nrow = fic+fik+POP)
    saldepinicial[,1]<-1:(fic+fik+POP)
    saldepinicial[,2]<-c(rep(Dc1/fic,fic),rep(Dk1/fik,fik),rep(dw0,nw),rep(do0,no+nr),rep(dm0,nm))
    saldepinicial[deposits[which(deposits[,2]!=b),1],2]<-0
    disponible[fic+fik+b]<-disponible[fic+fik+b]-sum(saldepinicial[,2])*idb0
    int1[b]<-sum(saldepinicial[,2])*idb0
  }  
  #disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]#+CAcb[b,t-1]*(1+itechocb[t-1]) por ser el primer periodo supongo que no pidieron cash advances
  for (i in 1:fic){
    disponible[i]<-disponible[i]+Dc1/fic*idb0
  }
  for (i in (fic+1):(fic+fik)) {
    disponible[i]<-disponible[i]+Dk1/fik*idb0
  }
  for (h in (fic+fik+1):(fic+fik+nw)){
    disponible[h+fib]<-disponible[h+fib]+dw0*idb0
  }
  for (h in (fic+fik+nw+1):(fic+fik+nw+no+nr)){
    disponible[h+fib]<-disponible[h+fib]+do0*idb0
  }
  for (h in (fic+fik+nw+no+nr+1):(fic+fik+POP)){
    disponible[h+fib]<-disponible[h+fib]+dm0*idb0
  }
  
  aportectotal[1]<-sum(aportec)
  
  #18 Wage and dole----
  
  #Actualizo los salarios acordados
  wa[,1]<-w[,1]
  
  #Si defaulteo, tengo que revisar w[i,t]  
  for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
    sueldos<-sum(w[which(trabajow[,1]==i),1],w[nw+which(trabajoo[,1]==i),1],w[nw+no+which(trabajor[,1]==i),1],w[nw+no+nr+which(trabajom[,1]==i),1])
    if(Nx[i,1]>0){wn[i,1]<-sueldos/Nx[i,1]}else{wn[i,1]<-0}
    if(disponible[i]+deposits[which(deposits[,1]==i),3]<sueldos){
      #default      
      default[i,1]<-1
      prop_sueldo_a_cobrar<-max(min(1,(disponible[i]+deposits[which(deposits[,1]==i),3])/sueldos),0)
      wnpago[i,1]<-wn[i,1]*prop_sueldo_a_cobrar
      for (h in 1:nw){
        if (trabajow[h,1]==i){
          w[h,1]<-w[h,1]*prop_sueldo_a_cobrar
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+1):(nw+no)){
        if (trabajoo[h-nw,1]==i){
          w[h,1]<-w[h,1]*prop_sueldo_a_cobrar
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+no+1):(nw+no+nr)){
        if (trabajor[h-nw-no,1]==i){
          w[h,1]<-w[h,1]*prop_sueldo_a_cobrar
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+no+nr+1):(nw+no+nr+nm)){
        if (trabajoo[h-nw-no-nr,1]==i){
          w[h,1]<-w[h,1]*prop_sueldo_a_cobrar
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]-max(0,deposits[which(deposits[,1]==i),3])
      deposits[which(deposits[,1]==i),3]<-0
      disponible[i]<-0
      
      
      
    }else{
      wnpago[i,1]<-wn[i,1]
      for (h in 1:nw){
        if (trabajow[h,1]==i){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+1):(nw+no)){
        if (trabajoo[h-nw,1]==i){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+no+1):(nw+no+nr)){
        if (trabajor[h-nw-no,1]==i){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      for (h in (nw+no+nr+1):(nw+no+nr+nm)){
        if (trabajom[h-nw-no-nr,1]==i){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
        }
      }
      if(disponible[i]>=sueldos){
        disponible[i]<-disponible[i]-sueldos
      }else{
        deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-sueldos,digits=4)
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-sueldos
        disponible[i]<-0
      }
    }
  }
  #sueldos funcionarios publicos
  
  sueldosg[1]<-sum(w[which(trabajow[,1]==999),1],w[nw+which(trabajoo[,1]==999),1],w[nw+no+which(trabajor[,1]==999),1],w[nw+no+nr+which(trabajom[,1]==999),1])
  wn[fic+fik+1,1]<-sueldosg[1]/Nx[fic+fik+1,1]
  wnpago[fic+fik+1,1]<-wn[fic+fik+1,1]
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-sueldosg[1]
  for (h in 1:nw){
    if (trabajow[h,1]==999){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
    }
  }
  for (h in (nw+1):(nw+no)){
    if (trabajoo[h-nw,1]==999){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
    }
  }
  for (h in (nw+no+1):(nw+no+nr)){
    if (trabajor[h-nw-no,1]==999){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
    }
  }
  for (h in (nw+no+nr+1):(nw+no+nr+nm)){
    if (trabajom[h-nw-no-nr,1]==999){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,1]  
    }
  }
  
  

  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-sum(uh[1:nw,1])*omega*mean(wa[which(uh[(1:nw),1]==0),1])-sum(uh[(nw+1):(nw+no+nr),1])*omega*mean(wa[nw+which(uh[(nw+1):(nw+no+nr),1]==0),1])-sum(uh[(nw+no+nr+1):(nw+no+nr+nm),1])*omega*mean(wa[nw+no+nr+which(uh[(nw+nr+no+1):(nw+nr+no+nm),1]==0),1])
  
  for (h in 1:nw){
    if(uh[h,1]==1){
      d[h,1]<-omega*mean(wa[which(uh[(1:nw),1]==0),1])
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,1]
    }
  }
  
  for (h in (nw+1):(nw+no+nr)){
    if(uh[h,1]==1){
      d[h,1]<-omega*mean(wa[nw+which(uh[(nw+1):(nw+no+nr),1]==0),1])
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,1]
    }
  }
  
  
  for (h in (nw+no+nr+1):(nw+no+nr+nm)){
    if(uh[h,1]==1){
      d[h,1]<-omega*mean(wa[nw+no+nr+which(uh[(nw+no+nr+1):(nw+no+nr+nm),1]==0),1])
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,1]
    }
  }
  
  #19 Dividends----
  
  #Actualizo riqueza  
  for (h in 1:nm) {
    wh[nw+no+nr+h,1]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
  } 
  
  
  #antes de repartir dividendos, se establecen las tasas impositivas.
  
  gdp[1]<-sum(y[,1]*px[,1])
  ngdp[1]<-sum(y[,1]*px[,1])
  
  #En el primer periodo no hay revision de tasas, vengo en el cuasi SS
  #tasas
  taopi[1]<-taopi0
  umbral[1]<-umbral0
  taoi1[1]<-taoi10
  taoi2[1]<-taoi20
  taow[1]<-taow0
  
  #inflacion
  pi[1]<-gss
  #inflacion minorista
  pic[1]<-gss
  #inflacion mayorista
  pik[1]<-gss
  
  ipc[1]<-1
  ipg[1]<-1
  igdp[1]<-1
  ipeh[1]<-1
  
  #mark-up
  mupc[1]<-sum(mu[which(x[,2]=="c"),1]*y[which(x[,2]=="c"),1])/sum(y[which(x[,2]=="c"),1])
  mupk[1]<-sum(mu[which(x[,2]=="k"),1]*y[which(x[,2]=="k"),1])/sum(y[which(x[,2]=="k"),1])
  mup[1]<-sum(mu[,1]*y[,1])/sum(y[,1])
  #mark-up sobre ventas
  mupsc[1]<-sum(mu[which(x[,2]=="c"),1]*s[which(x[,2]=="c"),1])/sum(s[which(x[,2]=="c"),1])
  mupsk[1]<-sum(mu[which(x[,2]=="k"),1]*s[which(x[,2]=="k"),1])/sum(s[which(x[,2]=="k"),1])
  mups[1]<-sum(mu[,1]*s[,1])/sum(s[,1])
  
  
  
  #Resultado de las firmas
  
  for (i in 1:fic) {
    uc[i,1]<-wn[i,1]/(muc[i,1]*lk*sharecw)
    pix[i,1]<-s[i,1]*px[i,1]+Dc1/fic*idb0+inv[i,1]*uc[i,1]-invc1/fic*uvcc1-wn[i,1]*Nx[i,1]-sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*loans[which(loans[,1]==i & loans[,6] <= 1),4]*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))-sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]/capital[which(capital[,1]==i & capital[,5]!=0),8])
  }
  for (i in (fic+1):(fic+fik)) {
    inv[i,1]<-sum(capital[which(capital[,1]==i),3])
    uc[i,1]<-wn[i,1]/(mun[i,1]*sharekw)
    pix[i,1]<-s[i,1]*px[i,1]+Dk1/fik*idb0+inv[i,1]*uc[i,1]-invk1/fik*uck1-wn[i,1]*Nx[i,1]-sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]*loans[which(loans[,1]==i & loans[,6] <= 1),4]*(1-(1-loans[which(loans[,1]==i & loans[,6] <= 1),6])/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
  }
  for (b in 1:fib) {
    pix[fic+fik+b,1]<-sum(loans[which(loans[,2]==b & loans[,6] <= 1),3]*loans[which(loans[,2]==b & loans[,6] <= 1),4]*(1-(1-loans[which(loans[,2]==b & loans[,6] <= 1),6])/loans[which(loans[,2]==b & loans[,6] <= 1),5]))+Bb1/fib*itechob0-intdepap[b,1]-perdida[b,1]#-CAcb[b,t-1]*(1+itechocb[t-1]) estoy suponiendo que no pidio avances de efectivo en el periodo 0
  }
  
  
  
  #Se establecen las condiciones de entrada para el caso en que una firma se funda y surja otra
  
  #Valor neto promedio del sector de consumo  
  for (i in 1:fic) {
    resultado19<-vector()
    for (j in 1:max(capital[which(capital[,1]==i),2])){
      resultado19<-c(resultado19,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
    }
    kreal[i,1]<-sum(resultado19)
    
    WNi[i,1]<-sum(inv[i,1]*uc[i,1],kreal[i,1],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
  }
  WNc[1]<-sum(WNi[1:fic,1])/fic
  
  for (i in (fic+1):(fic+fik)) {
    WNi[i,1]<-sum(inv[i,1]*uc[i,1],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
  }
  WNk[1]<-sum(WNi[(fic+1):(fic+fik),1])/fik
  
  
  
  
  #Se establecen los dividendos a repartir_firmas de consumo
  for (i in sample(1:fic,fic,replace = FALSE)){
    Divx[i,1]<-max(0,roc[1]*pix[i,1]*(1-taopi[1]))
    acciones[i,1]<-sum(wh[nw+no+nr+which(trabajom[,1]==i),1])
    if (default[i,1]==1){Divx[i,1]<-0
    #Si no cumple la condicion de entrada, dividendos negativos (hay que recapitalizar)  
    if(WNi[i,1]<0.25*WNc[1]){
      Divx[i,1]<-WNi[i,1]-0.25*WNc[1]
      for (h in 1:nm) {
        if (trabajom[h,1]==i){
          aporte[h,i]<-min(-Divx[i,1]*wh[(nw+no+nr+h),1]/acciones[i,1],disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
        }else{
          aporte[h,i]<-0
        }
      }    
      capitalizacion[i,1]<-min(-Divx[i,1],sum(aporte[,i]))
      disponible[i]<-disponible[i]+capitalizacion[i,1]
    }
    }
    #Si no tiene managers, no reparte
    if (length(which(trabajom[,1]==i))==0){Divx[i,1]=0}
    #Si hay para repartir se reparten        
    if (Divx[i,1]>0){if (disponible[i]>Divx[i,1]){
      disponible[i]<-disponible[i]-Divx[i,1]
    }else{if (disponible[i]+deposits[which(deposits[,1]==i),3]>Divx[i,1]){
      deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-Divx[i,1],digits = 4)
      disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-Divx[i,1]
      disponible[i]<-0
    }else{
      Divx[i,1]<-0 #Podria repartir todo lo que tiene la firma, prefiero este criterio conservador
    }
    }
    }
  }
  
  #Se establecen los dividendos a repartir_firmas de capital  
  for (i in sample((fic+1):(fic+fik),fik,replace = FALSE)) {
    Divx[i,1]<-max(0,rok[1]*pix[i,1]*(1-taopi[1]))
    acciones[i,1]<-sum(wh[nw+no+nr+which(trabajom[,1]==i),1])
    if (default[i,1]==1){Divx[i,1]<-0
    #Si no cumple la condicion de entrada, dividendos negativos (hay que recapitalizar)  
    if(WNi[i,1]<0.25*WNk[1]){
      Divx[i,1]<-WNi[i,1]-0.25*WNk[1]
      for (h in 1:nm) {
        if (trabajom[h,1]==i){
          aporte[h,i]<-min(-Divx[i,1]*wh[(nw+no+nr+h),1]/acciones[i,1],disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
        }else{
          aporte[h,i]<-0
        }
      }    
      capitalizacion[i,1]<-min(-Divx[i,1],sum(aporte[,i]))
      disponible[i]<-disponible[i]+capitalizacion[i,1]
    }
    }
    #Si no tiene managers, no reparte
    if (length(which(trabajom[,1]==i))==0){Divx[i,1]=0}
    #Si hay para repartir se reparten        
    if (Divx[i,1]>0){if (disponible[i]>Divx[i,1]){
      disponible[i]<-disponible[i]-Divx[i,1]
    }else{if (disponible[i]+deposits[which(deposits[,1]==i),3]>Divx[i,1]){
      deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-Divx[i,1],digits = 4)
      disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-Divx[i,1]
      disponible[i]<-0
    }else{
      Divx[i,1]<-0 #Podria repartir todo lo que tiene la firma, prefiero este criterio conservador
    }
    }
    }
  }
  
  
  #Se reparten los dividendos de las firmas o se recapitaliza, segun corresponda  
  for (h in (nw+no+nr+1):(nw+no+nr+nm)){
    Divh[h,1]<-if(trabajom[h-(nw+no+nr),1]!=0 & trabajom[h-(nw+no+nr),1]!=999){
      if(Divx[trabajom[h-(nw+no+nr),1],1]>0.0001){
        if(acciones[trabajom[h-(nw+no+nr),1],1]>0){
          wh[h,1]*Divx[trabajom[h-(nw+no+nr),1],1]/acciones[trabajom[h-(nw+no+nr),1],1]
        }else{
          Divx[trabajom[h-(nw+no+nr),1],1]/length(which(trabajom[,1]==trabajom[h-(nw+no+nr),1]))}
      }else{
        -aporte[h-(nw+no+nr),trabajom[h-(nw+no+nr),1]]}
    }else{
      0}  
    #Actualizo riqueza  
    wh[h,1]<-disponible[fic+fik+fib+h]+max(0,deposits[which(deposits[,1]==fic+fik+h),3])+Divh[h,1]
  }
  
  
  
  #Se establecen los dividendos a repartir_bancos  
  for (b in sample(1:fib,fib,replace=FALSE)) {
    Divx[fic+fik+b,1]<-max(0,rob[1]*pix[fic+fik+b,1]*(1-taopi[1]))
    if (default[fic+fik+b,1]==1){Divx[fic+fik+b,1]<-0}
    #si hay para repartir se reparte
    if (Divx[fic+fik+b,1]>0.0001){
      disponible[fic+fik+b]<-disponible[fic+fik+b]-Divx[fic+fik+b,1]
    }
    Ltot[b,1]<-sum(loans[which(loans[,2]==b),3]*(1-(1-loans[which(loans[,2]==b),6])/loans[which(loans[,2]==b),5]))
    NW[b,1]<-Ltot[b,1]+Bb1/fib+disponible[fic+fik+b]-sum(deposits[which(deposits[,2]==b),3])
    CR[b,1]<-NW[b,1]/Ltot[b,1]
  }
  CR_promedio<-max(0.06,sum(CR[,1]/fib))
  for (b in 1:fib) {
    if (CR[b,1]<0){
      default[fic+fik+b,1]<-1
      acapitalizar[fic+fik+b,1]<-(0.06-CR[b,1])*Ltot[b,1] #Le voy a exigir el capital minimo legal
      acciones_totales[1]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),1])    
      for (h in 1:nm) {
        wh[nw+no+nr+h,1]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])+Divh[nw+no+nr+h,1]-sum(aporte[h,(fic+fik+1):(fic+fik+b)])
        if(wh[nw+no+nr+h,1]>0.0001){
          aporte[h,fic+fik+b]<-acapitalizar[fic+fik+b,1]*wh[(nw+no+nr+h),1]/acciones_totales[1]
        }
      }
      capitalizacion[fic+fik+b,1]<-sum(aporte[,fic+fik+b])
      disponible[fic+fik+b]<-disponible[fic+fik+b]+capitalizacion[fic+fik+b,1]
    }
  }
  
  
  acciones_totales[1]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),1])
  
  #Se reparten los dividendos o se recapitaliza, segun corresponda  
  for (h in (nw+no+nr+1):(nw+no+nr+nm)){
    #bancos
    for (b in 1:fib) {
      Divh[h,1]<-Divh[h,1]+wh[h,1]*Divx[(fic+fik+b),1]/acciones_totales[1]-aporte[h-(nw+no+nr),fic+fik+b]
    }
    #contabilizo ingresos  
    deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+Divh[h,1]+disponible[fic+fik+fib+h],digits = 4)
    disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+Divh[h,1]+disponible[fic+fik+fib+h]
    disponible[fic+fik+fib+h]<-0
  }
  #Reparto los dividendos en proporcion a la riqueza de los managers
  #Considero que los bancos son propiedad de todos los managers 
  #y las firmas solo de los que trabajan en ella
  
  
  
  #20 Taxes----
  
  #Renta empresarial
  
  for (i in (fic+fik+1):(fic+fik+fib)){
    proftax[i,1]<-max(taopi[1]*pix[i,1],0)
    if(default[i,1]==1){proftax[i,1]<-0}
    disponible[i]<-disponible[i]-proftax[i,1]
  }
  for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
    proftax[i,1]<-max(taopi[1]*pix[i,1],0)
    if(default[i,1]==1){proftax[i,1]<-0}
    if (disponible[i]+deposits[which(deposits[,1]==i),3]>proftax[i,1]){
      if (disponible[i]>proftax[i,1]){
        disponible[i]<-disponible[i]-proftax[i,1]
      }else{
        deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-proftax[i,1],digits = 4)
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-proftax[i,1]
        disponible[i]<-0
      }
    }else{
      default[i,1]<-1
      
      proftax[i,1]<-disponible[i]+deposits[which(deposits[,1]==i),3]
      disponible[i]<-0
      disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]-max(0,deposits[which(deposits[,1]==i),3])
      deposits[which(deposits[,1]==i),3]<-0
    }
  }
  
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(proftax[,1])
  
  for (i in 1:fic) {
    OCF[i,1]<-pix[i,1]-proftax[i,1]+sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]/capital[which(capital[,1]==i & capital[,5]!=0),8])-(+inv[i,1]*uc[i,1]-invc1*uvcc1/fic)-(sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
    r[i,1]<-OCF[i,1]/sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]*(1-capital[which(capital[,1]==i & capital[,5]!=0),5]/capital[which(capital[,1]==i & capital[,5]!=0),8]))
  }
  for (i in (fic+1):(fic+fik)) {
    OCF[i,1]<-pix[i,1]-proftax[i,1]-(+inv[i,1]*uc[i,1]-invk1/fik*uck1)-(sum(loans[which(loans[,1]==i & loans[,6] <= 1),3]/loans[which(loans[,1]==i & loans[,6] <= 1),5]))
  } 
  
  
  vk[1]<-sum(OCF[1:100,1])/sum(capital[which(capital[,1]<=100 & capital[,5]!=0),3]*capital[which(capital[,1]<=100 & capital[,5]!=0),6]*(1-capital[which(capital[,1]<=100 & capital[,5]!=0),5]/capital[which(capital[,1]<=100 & capital[,5]!=0),8]))
  
  
  #Impuesto al ingreso personal 
  
  for (h in 1:(nw+no+nr+nm)){
    ytax[h,1]<-sum(w[h,1],if(h<=nw){dw0}else{if(h>nw+no+nr){dm0}else{do0}}*idb0,if(x[h,2]=="m"){(Divc1+Divk1+Divb1)/nm})
    inctax[h,1]<-taoi1[1]*min(umbral[1],max(0,ytax[h,1]))+taoi2[1]*max(0,ytax[h,1]-umbral[1])
    #pongo los dividendos del periodo anterior porque los de este 
    #ejercicio aun no se distribuyeron, 
    #idem para intereses de depositos
    
    #calculo el ingreso antes de impuestos
    yh[h,1]<-ytax[h,1]+d[h,1]
    ygini[h,1]<-if(yh[h,1]<0){0}else{yh[h,1]}
    if (disponible[fic+fik+fib+h]>=inctax[h,1]){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]-inctax[h,1]
    }else{
      if (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]>=inctax[h,1]){
        deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+disponible[fic+fik+fib+h]-inctax[h,1],digits = 4)
        disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+disponible[fic+fik+fib+h]-inctax[h,1]
        disponible[fic+fik+fib+h]<-0
      }else{
        #Voy a modificar inctax[i,t] a la baja, considerando lo que el Estado puede cobrar efectivamente
        inctax[h,1]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
        disponible[fic+fik+fib+h]<-0
        disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+h),3])
        deposits[which(deposits[,1]==fic+fik+h),3]<-0
      }
    } 
    yait[h,1]<-if(ygini[h,1]-inctax[h,1]<0){0}else{ygini[h,1]-inctax[h,1]}
    
  }
  #Revisar     
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(inctax[,1])
  
  #Impuesto a la riqueza
  
  #actualizo riqueza
  
  for (h in 1:(nw+no+nr+nm)){
    wh[h,1]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
    wtax[h,1]<-taow[1]*wh[h,1]
    if (disponible[fic+fik+fib+h]>=wtax[h,1]){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]-wtax[h,1]
    }else{
      if (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]>=wtax[h,1]){
        deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+disponible[fic+fik+fib+h]-wtax[h,1],digits=4)
        disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+disponible[fic+fik+fib+h]-wtax[h,1]
        disponible[fic+fik+fib+h]<-0
      }else{
        #Voy a modificar wtax[i,t] a la baja, considerando lo que el Estado puede cobrar efectivamente
        wtax[h,1]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
        disponible[fic+fik+fib+h]<-0
        disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+h),3])
        deposits[which(deposits[,1]==fic+fik+h),3]<-0
      }
    } 
    yat[h,1]<-if(yait[h,1]-wtax[h,1]<0){0}else{yait[h,1]-wtax[h,1]}
  }  
  #Revisar
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(wtax[,1])
  
  #Gini
  Gini[,1]<-sort(yh[,1],decreasing = FALSE)
  Gini[,3]<-c(1:POP)*100/POP
  for (h in 1:POP) {
    Gini[h,2]<-sum(Gini[1:h,1])*100/sum(Gini[,1])
  }
  Gini[,4]<-Gini[,3]-Gini[,2]
  ig[1]<-sum(Gini[1:(POP-1),4])/sum(Gini[1:(POP-1),3])
  ig2[1]<-Gini(yh[,1])
  igat[1]<-Gini(yat[,1])
  igait[1]<-Gini(yait[,1])
  
  
  #resultado fiscal VER DoNDE VA. ACa NO CUENTO LOS BONOS EMITIDOS EN ESTE PERiODO. Ya los cuento como ingreso y los del periodo anterior como gasto (es decir rollover). Si genero superavit, el rollover es menor al 100%  
  piCB[1]<-Bcb1*itechob0#+sum(CAcb[,t-1])*itechocb[t-1] en el SS no hay avances de capital
  dfg[1]<-sum(proftax[,1],inctax[,1],wtax[,1])+piCB[1]-sueldosg[1]-sum(d[,1])-B1*itechob0
  deltab[1]<--dfg[1]
  if (deltab[1]<0){deltab[1]<-max(deltab[1],-B1)}
  
  #actualizo balances
  for (i in 1:fic) {
    WNi[i,1]<-sum(inv[i,1]*uc[i,1],kreal[i,1],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
    activoi[i,1]<-sum(inv[i,1]*uc[i,1],kreal[i,1],disponible[i],deposits[which(deposits[,1]==i),3])
    pasivoi[i,1]<-sum(if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
  }
  for (i in (fic+1):(fic+fik)) {
    WNi[i,1]<-sum(inv[i,1]*uc[i,1],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
    activoi[i,1]<-sum(inv[i,1]*uc[i,1],disponible[i],deposits[which(deposits[,1]==i),3])
    pasivoi[i,1]<-sum(if(length(which(loans[,1]==i & loans[,6]!=1+1))>0){sum(loans[which(loans[,1]==i & loans[,6]!=1+1),3]*(1-((1-loans[which(loans[,1]==i & loans[,6]!=1+1),6])/loans[which(loans[,1]==i & loans[,6]!=1+1),5])))},if(length(which(loans[,1]==i & loans[,6]==1+1))>0){sum(loans[which(loans[,1]==i & loans[,6]==1+1),3])})
  }
  
  levb[1]<-sum(Ltot[,1])/sum(NW[,1])
  pasc[1]<-sum(pasivoi[which(x[,2]=="c"),1])
  actc[1]<-sum(activoi[which(x[,2]=="c"),1])
  patc[1]<-sum(WNi[which(x[,2]=="c"),1])
  pask[1]<-sum(pasivoi[which(x[,2]=="k"),1])
  actk[1]<-sum(activoi[which(x[,2]=="k"),1])
  patk[1]<-sum(WNi[which(x[,2]=="k"),1])
  levcnw[1]<-pasc[1]/patc[1]
  levcad[1]<-actc[1]/patc[1]
  levknw[1]<-pask[1]/patk[1]
  levkad[1]<-actk[1]/patk[1]
  
  
  #21 Deposit market interaction TENGO CREACIoN SECUNDARIA DE DINERO----
  #CUANDO DEPOSITO AUMENTO DEPoSITOS Y AUMENTO DISPONIBLE sum(disponible[c(1:(fic+fik),(fic+fik+fib+1):(fic+fik+fib+POP))]) 30062.86 o 29465.73 o 29528.16
  #M1 hasta aca sum(disponible,deposits[,3]) 69634.36
  #M1 luego de este paso sum(disponible,deposits[,3]) + sum(disponible[c(1:(fic+fik),(fic+fik+fib+1):(fic+fik+fib+POP))]) 99697.22
  
  for (i in 1:(fic+fik)) {
    
    #Matching
    
    #provold<-proveedor en t-1
    provold<-deposits[which(deposits[,1]==i),2]
    
    #tasa de provold
    idepold<-idb[provold,1]
    #determino la oferta visible para i
    oferentes<-sample(c(1:fib), chidep, replace = FALSE)
    oferentes<-cbind(oferentes,idb[oferentes,1])
    
    #ahora i elige el banco donde depositar
    #banco candidato a sustituir al anterior
    provnew<-oferentes[which.max(oferentes[,2]),1]
    #tasa del mejor oferente
    idepnew<-idb[provnew,1]
    
    if (idepnew>idepold){Prsdep[i,1]<-1-exp(epsilondep*(idepold-idepnew)/idepold)}else{Prsdep[i,1]<-0}
    
    #AHORA TENGO QUE ASIGNAR EL BANCO ATENDIENDO ESTA PROBABILIDAD
    dmarket[i,1]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsdep[i,1],Prsdep[i,1]))
    
    #Concreto el matching
    #retiro el deposito del periodo anterior 
    disponible[fic+fik+provold]<-disponible[fic+fik+provold]-deposits[which(deposits[,1]==i),3]
    #deposito
    deposits[which(deposits[,1]==i),]<-c(i,dmarket[i,1],deposits[which(deposits[,1]==i),3]+disponible[i],idb[dmarket[i,1],1])
    disponible[fic+fik+dmarket[i,1]]<-disponible[fic+fik+dmarket[i,1]]+deposits[which(deposits[,1]==i),3]
    #satisfazgo
    disponible[i]<-0
    #queda determinado el interes de los depositos en este periodo
    intdep[i,1]<-deposits[which(deposits[,1]==i),3]*deposits[which(deposits[,1]==i),4]
    saldep[i,1]<-deposits[which(deposits[,1]==i),3]
    
  }
  
  
  
  for (h in (fic+fik+fib+1):(fic+fik+fib+nw+no+nr+nm)){
    #Matching
    
    #provold<-proveedor en t-1
    provold<-sample(1:fib,1)
    
    #tasa de provold
    idepold<-idb[provold,1]
    #determino la oferta visible para h
    oferentes<-sample(c(1:fib), chidep, replace = FALSE)
    oferentes<-cbind(oferentes,idb[oferentes,1])
    
    #ahora h elige el banco donde depositar
    #banco candidato a sustituir al anterior
    provnew<-oferentes[which.max(oferentes[,2]),1]
    #tasa del mejor oferente
    idepnew<-idb[provnew,1]
    
    if (idepnew>idepold){Prsdep[h-fib,1]<-1-exp(epsilondep*(idepold-idepnew)/idepold)}else{Prsdep[h-fib,1]<-0}
    
    #AHORA TENGO QUE ASIGNAR EL BANCO ATENDIENDO ESTA PROBABILIDAD
    dmarket[h-fib,1]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsdep[h-fib,1],Prsdep[h-fib,1]))
    
    #Concreto el matching
    #retiro el deposito del periodo anterior
    disponible[fic+fik+provold]<-disponible[fic+fik+provold]-deposits[which(deposits[,1]==h-fib),3]
    #deposito
    deposits[which(deposits[,1]==h-fib),]<-c(h-fib,dmarket[h-fib,1],deposits[which(deposits[,1]==h-fib),3]+disponible[h],idb[dmarket[h-fib,1],1])
    disponible[fic+fik+dmarket[h-fib,1]]<-disponible[fic+fik+dmarket[h-fib,1]]+deposits[which(deposits[,1]==h-fib),3]
    #satisfazgo
    disponible[h]<-0
    #queda determinado el interes de los depositos en este periodo
    intdep[h-fib,1]<-deposits[which(deposits[,1]==h-fib),3]*deposits[which(deposits[,1]==h-fib),4]   
    saldep[h-fib,1]<-deposits[which(deposits[,1]==h-fib),3]   
    
  }
  
  #determino los intereses que le correspondera pagar a cada banco
  for (b in 1:fib) {
    intdepap[b,1]<-sum(intdep[deposits[which(deposits[,2]==b),1],1])
  }
  
  #22 Bond purchases----
  
  #tengo que actualizar las reservas del banco VER disponible
  #Yo supongo que el ratio de liquidez es sobre los depositos totales
  
  for (b in sample(1:fib,fib,replace = FALSE)) {
    LR[b,1]=disponible[fic+fik+b]/sum(deposits[which(deposits[,2]==b),3])
    falta=disponible[fic+fik+b]-LRT[b,1]*sum(deposits[which(deposits[,2]==b),3])
    if (falta>0 & deltab[1]+B1>sum(bonos[,1])){
      bonos[b,1]<-min(falta,deltab[1]+B1-sum(bonos[,1]))
      disponible[fic+fik+b]<-disponible[fic+fik+b]-bonos[b,1]
      disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+bonos[b,1]
      falta=disponible[fic+fik+b]-LRT[b,1]*sum(deposits[which(deposits[,2]==b),3])
    }
  }
  
  #BC compra el remanente de bonos
  if (deltab[1]+B1>sum(bonos[,1])) {
    bonos[fib+1,1]<-deltab[1]+B1-sum(bonos[,1])
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-bonos[fib+1,1]
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+bonos[fib+1,1]
  }
  
  
  #23 Cash advances----
  
  for (b in 1:fib) {
    if (disponible[fic+fik+b]/sum(deposits[which(deposits[,2]==b),3])<0.08) {
      CAcb[b,1]<-0.08*sum(deposits[which(deposits[,2]==b),3])-disponible[fic+fik+b]
      disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-CAcb[b,1]
      disponible[fic+fik+b]<-disponible[fic+fik+b]+CAcb[b,1]
    }
    R[b,1]<-disponible[fic+fik+b]
    disponible[fic+fik+b]<-disponible[fic+fik+b]-R[b,1]
  }
  disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]+sum(R[,1])
  
  depo[1]<-sum(deposits[,3])
  depoc[1]<-sum(deposits[which(deposits[,1]<=100),3])
  depok[1]<-sum(deposits[which(deposits[,1]<=110 & deposits[,1]>100),3])
  depow[1]<-sum(deposits[which(deposits[,1]<=110+nw & deposits[,1]>110),3])
  depoo[1]<-sum(deposits[which(deposits[,1]<=110+nw+no & deposits[,1]>110+nw),3])
  depor[1]<-sum(deposits[which(deposits[,1]<=110+nw+no+nr & deposits[,1]>110+nw+no),3])
  depom[1]<-sum(deposits[which(deposits[,1]<=110+POP & deposits[,1]>110+nw+no+nr),3])
  
  #BC transfiere ganancias
  disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-piCB[1]
  disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+piCB[1]
  
  cajah[1]<-sum(disponible[(fic+fik+fib+1):(fic+fik+fib+nw+no+nr+nm)])
  cajac[1]<-sum(disponible[1:fic])
  cajak[1]<-sum(disponible[(fic+1):(fic+fik)])
  cajab[1]<-sum(disponible[(fic+fik+1):(fic+fik+fib)])
  cajaBC[1]<-round(disponible[fic+fik+fib+nw+no+nr+nm+2],digits = 4)
  cajaG[1]<-round(disponible[fic+fik+fib+nw+no+nr+nm+1],digits = 4)
  
  #Flujos----    
  
  fh[1,1]<--sum(constrans[,3]*constrans[,4])
  fh[2,1]<-sum(w[,1])
  fh[3,1]<-sum(d[,1])
  fh[7,1]<--sum(inctax[,1]+wtax[,1])
  fh[8,1]<-Dh1*idb0
  fh[12,1]<-sum(Divh[,1])+sum(aporte)+sum(aportec)
  fh[14,1]<--(cajah[1]-0+sum(aporte)+sum(aportec))
  fh[15,1]<--(sum(saldep[(fic+fik+1):(fic+fik+POP),1])-Dh1)
  
  fcc[1,1]<-sum(s[1:fic,1]*px[1:fic,1])
  fcc[2,1]<--sum(wnpago[1:fic,1]*Nx[1:fic,1])
  fcc[4,1]<-(sum(inv[1:fic,1]*uc[1:fic,1])-invc1*uvcc1)
  fcc[6,1]<--sum(capital[which(capital[,1]<=fic & capital[,5]!=0),3]*capital[which(capital[,1]<=fic & capital[,5]!=0),6]/capital[which(capital[,1]<=fic & capital[,5]!=0),8])
  fcc[7,1]<--sum(proftax[1:fic,1])
  fcc[8,1]<-Dc1*idb0
  fcc[10,1]<--sum(loans[which(loans[,1]<=fic & loans[,6]<=1),3]*loans[which(loans[,1]<=fic & loans[,6]<=1),4]*(1-(1-loans[which(loans[,1]<=fic & loans[,6] <= 1),6])/loans[which(loans[,1]<=fic & loans[,6] <=1),5]))
  fcc[12,1]<--sum(pix[1:fic,1]-proftax[1:fic,1])
  
  fkc[4,1]<--(sum(inv[1:fic,1]*uc[1:fic,1])-uvcc1*invc1)
  fkc[5,1]<--sum(inversion[,1])
  fkc[6,1]<-sum(capital[which(capital[,1]<=fic & capital[,5]!=0),3]*capital[which(capital[,1]<=fic & capital[,5]!=0),6]/capital[which(capital[,1]<=fic & capital[,5]!=0),8])
  fkc[12,1]<-sum(pix[1:fic,1]-proftax[1:fic,1]-Divx[1:fic,1])
  fkc[14,1]<-sum(capitalizacion[1:fic,1])
  fkc[15,1]<--(sum(saldep[1:fic,1])-Dc1)
  fkc[19,1]<-sum(loans[which(loans[,1]<=fic & loans[,6]==1+1),3])+sum(loandefhoy[1:fic,1])-sum(loans[which(loans[,1]<=fic & loans[,6]<=1),3]*(1/loans[which(loans[,1]<=fic & loans[,6]<=1),5]))
  
  
  
  fck[2,1]<--sum(wnpago[(fic+1):(fic+fik),1]*Nx[(fic+1):(fic+fik),1])
  fck[4,1]<-(sum(inv[(fic+1):(fic+fik),1]*uc[(fic+1):(fic+fik),1])-invk1*uck1)
  fck[5,1]<-sum(s[(fic+1):(fic+fik),1]*px[(fic+1):(fic+fik),1])
  fck[7,1]<--sum(proftax[(fic+1):(fic+fik),1])
  fck[8,1]<-Dk1*idb0
  fck[10,1]<--sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=1),3]*loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=1),4]*(1-(1-loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=1),6])/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=1),5]))
  fck[12,1]<--sum(pix[(fic+1):(fic+fik),1]-proftax[(fic+1):(fic+fik),1])
  
  fkk[4,1]<--(sum(inv[(fic+1):(fic+fik),1]*uc[(fic+1):(fic+fik),1])-invk1*uck1)
  fkk[12,1]<-sum(pix[(fic+1):(fic+fik),1]-proftax[(fic+1):(fic+fik),1])-sum(Divx[(fic+1):(fic+fik),1])
  fkk[14,1]<-sum(capitalizacion[(fic+1):(fic+fik),1])
  fkk[15,1]<--(sum(saldep[(fic+1):(fic+fik),1])-Dk1)
  fkk[19,1]<-sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]==1+1),3])+sum(loandefhoy[(fic+1):(fic+fik),1])-sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=1),3]*(1/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=1),5]))
  
  fcb[7,1]<--sum(proftax[(fic+fik+1):(fic+fik+fib),1])
  fcb[8,1]<--sum(Dh1+Dc1+Dk1)*idb0
  fcb[9,1]<-Bb1*itechob0
  fcb[10,1]<-sum(loans[which(loans[,6]<=1),3]*loans[which(loans[,6]<=1),4]*(1-(1-loans[which(loans[,6] <=1),6])/loans[which(loans[,6] <=1),5]))
  fcb[11,1]<--0
  fcb[12,1]<--sum(pix[(fic+fik+1):(fic+fik+fib),1]-proftax[(fic+fik+1):(fic+fik+fib),1])
  fcb[14,1]<--sum(perdida[,1])
  
  fkb[12,1]<-sum(pix[(fic+fik+1):(fic+fik+fib),1]-proftax[(fic+fik+1):(fic+fik+fib),1])-sum(Divx[(fic+fik+1):(fic+fik+fib),1])
  fkb[14,1]<--(cajab[1]-0)+sum(capitalizacion[(fic+fik+1):(fic+fik+fib),1])+sum(aportec)+sum(perdida[,1])
  fkb[15,1]<-(sum(saldep[,1])-(Dc1+Dk1+Dh1))
  fkb[16,1]<-sum(CAcb[,1])
  fkb[17,1]<--(sum(R[,1])-Rb1)
  fkb[18,1]<--(sum(bonos[1:fib,1])-Bb1)
  fkb[19,1]<--(sum(loans[which(loans[,6]==1+1),3])+sum(loandefhoy[,1])-sum(loans[which(loans[,6]<=1),3]*(1/loans[which(loans[,6]<=1),5])))
  
  fg[2,1]<--wnpago[fic+fik+1]*Nx[fic+fik+1]
  fg[3,1]<--sum(d[,1])
  fg[7,1]<-sum(inctax[,1],proftax[,1],wtax[,1])
  fg[9,1]<--sum(B1*itechob0)
  fg[13,1]<-piCB[1]
  fg[18,1]<-(sum(bonos[,1])-B1)
  
  
  fccb[9,1]<-sum(Bcb1*itechob0)
  fccb[11,1]<-0  
  fccb[13,1]<--piCB[1]
  
  fkcb[14,1]<--(cajaBC[1]-0)
  fkcb[16,1]<--sum(CAcb[,1])
  fkcb[17,1]<-(sum(R[,1])-Rb1)
  fkcb[18,1]<--(bonos[fib+1,1]-Bcb1)
  
  
  ctrlflujoag[1,1]<-sum(fh[,1])
  ctrlflujoag[1,2]<-sum(fcc[,1])
  ctrlflujoag[1,3]<-sum(fkc[,1])
  ctrlflujoag[1,4]<-sum(fck[,1])
  ctrlflujoag[1,5]<-sum(fkk[,1])
  ctrlflujoag[1,6]<-sum(fcb[,1])
  ctrlflujoag[1,7]<-sum(fkb[,1])
  ctrlflujoag[1,8]<-sum(fg[,1])
  ctrlflujoag[1,9]<-sum(fccb[,1])
  ctrlflujoag[1,10]<-sum(fkcb[,1])
  
  ctrlflujos[1,1]<-sum(fh[1,1],fcc[1,1],fkc[1,1],fck[1,1],fkk[1,1],fcb[1,1],fkb[1,1],fg[1,1],fccb[1,1],fkcb[1,1])
  ctrlflujos[2,1]<-sum(fh[2,1],fcc[2,1],fkc[2,1],fck[2,1],fkk[2,1],fcb[2,1],fkb[2,1],fg[2,1],fccb[2,1],fkcb[2,1])
  ctrlflujos[3,1]<-sum(fh[3,1],fcc[3,1],fkc[3,1],fck[3,1],fkk[3,1],fcb[3,1],fkb[3,1],fg[3,1],fccb[3,1],fkcb[3,1])
  ctrlflujos[4,1]<-sum(fh[4,1],fcc[4,1],fkc[4,1],fck[4,1],fkk[4,1],fcb[4,1],fkb[4,1],fg[4,1],fccb[4,1],fkcb[4,1])
  ctrlflujos[5,1]<-sum(fh[5,1],fcc[5,1],fkc[5,1],fck[5,1],fkk[5,1],fcb[5,1],fkb[5,1],fg[5,1],fccb[5,1],fkcb[5,1])
  ctrlflujos[6,1]<-sum(fh[6,1],fcc[6,1],fkc[6,1],fck[6,1],fkk[6,1],fcb[6,1],fkb[6,1],fg[6,1],fccb[6,1],fkcb[6,1])
  ctrlflujos[7,1]<-sum(fh[7,1],fcc[7,1],fkc[7,1],fck[7,1],fkk[7,1],fcb[7,1],fkb[7,1],fg[7,1],fccb[7,1],fkcb[7,1])
  ctrlflujos[8,1]<-sum(fh[8,1],fcc[8,1],fkc[8,1],fck[8,1],fkk[8,1],fcb[8,1],fkb[8,1],fg[8,1],fccb[8,1],fkcb[8,1])
  ctrlflujos[9,1]<-sum(fh[9,1],fcc[9,1],fkc[9,1],fck[9,1],fkk[9,1],fcb[9,1],fkb[9,1],fg[9,1],fccb[9,1],fkcb[9,1])
  ctrlflujos[10,1]<-sum(fh[10,1],fcc[10,1],fkc[10,1],fck[10,1],fkk[10,1],fcb[10,1],fkb[10,1],fg[10,1],fccb[10,1],fkcb[10,1])
  ctrlflujos[11,1]<-sum(fh[11,1],fcc[11,1],fkc[11,1],fck[11,1],fkk[11,1],fcb[11,1],fkb[11,1],fg[11,1],fccb[11,1],fkcb[11,1])
  ctrlflujos[12,1]<-sum(fh[12,1],fcc[12,1],fkc[12,1],fck[12,1],fkk[12,1],fcb[12,1],fkb[12,1],fg[12,1],fccb[12,1],fkcb[12,1])
  ctrlflujos[13,1]<-sum(fh[13,1],fcc[13,1],fkc[13,1],fck[13,1],fkk[13,1],fcb[13,1],fkb[13,1],fg[13,1],fccb[13,1],fkcb[13,1])
  ctrlflujos[14,1]<-sum(fh[14,1],fcc[14,1],fkc[14,1],fck[14,1],fkk[14,1],fcb[14,1],fkb[14,1],fg[14,1],fccb[14,1],fkcb[14,1])
  ctrlflujos[15,1]<-sum(fh[15,1],fcc[15,1],fkc[15,1],fck[15,1],fkk[15,1],fcb[15,1],fkb[15,1],fg[15,1],fccb[15,1],fkcb[15,1])
  ctrlflujos[16,1]<-sum(fh[16,1],fcc[16,1],fkc[16,1],fck[16,1],fkk[16,1],fcb[16,1],fkb[16,1],fg[16,1],fccb[16,1],fkcb[16,1])
  ctrlflujos[17,1]<-sum(fh[17,1],fcc[17,1],fkc[17,1],fck[17,1],fkk[17,1],fcb[17,1],fkb[17,1],fg[17,1],fccb[17,1],fkcb[17,1])
  ctrlflujos[18,1]<-sum(fh[18,1],fcc[18,1],fkc[18,1],fck[18,1],fkk[18,1],fcb[18,1],fkb[18,1],fg[18,1],fccb[18,1],fkcb[18,1])
  ctrlflujos[19,1]<-sum(fh[19,1],fcc[19,1],fkc[19,1],fck[19,1],fkk[19,1],fcb[19,1],fkb[19,1],fg[19,1],fccb[19,1],fkcb[19,1])
  ctrlflujos<-round(ctrlflujos)
  ctrlflujoag<-round(ctrlflujoag)
  
  #Stocks----
  
  sh[1,1]<-cajah[1]+sum(aporte)+sum(aportec)
  sh[2,1]<-sum(deposits[which(deposits[,1]>fic+fik),3])
  
  sc[1,1]<--sum(capitalizacion[1:fic,1])
  sc[2,1]<-sum(deposits[which(deposits[,1]<=fic),3])
  sc[3,1]<--(sum(loans[which(loans[,1]<=fic & loans[,6]==1+1),3])+sum(loans[which(loans[,1]<=fic & loans[,6]<=1),3]*(1-((1+1)-loans[which(loans[,1]<=fic & loans[,6] <=1),6])/loans[which(loans[,1]<=fic & loans[,6] <=1),5])))
  sc[4,1]<-sum(inv[1:fic,1]*uc[1:fic,1])
  sc[5,1]<-sum(capital[which(capital[,1]<=fic),6]*capital[which(capital[,1]<=fic),3]*(1-capital[which(capital[,1]<=fic),5]/capital[which(capital[,1]<=fic),8]))
  
  sk[1,1]<--sum(capitalizacion[(fic+1):(fic+fik),1])
  sk[2,1]<-sum(deposits[which(deposits[,1]>fic & deposits[,1]<=fic+fik),3])
  sk[3,1]<--(sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]==1+1),3])+sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=1),3]*(1-((1+1)-loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=1),6])/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=1),5])))
  sk[5,1]<-sum(inv[(fic+1):(fic+fik),1]*uc[(fic+1):(fic+fik),1])
  
  sb[1,1]<-cajab[1]-sum(capitalizacion[(fic+fik+1):(fic+fik+fib),1])-sum(aportec)
  sb[2,1]<--(sum(deposits[,3]))
  sb[3,1]<-sum(loans[which(loans[,6]==1+1),3])+sum(loans[which(loans[,6]<=1),3]*(1-((1+1)-loans[which(loans[,6] <=1),6])/loans[which(loans[,6] <=1),5]))
  sb[6,1]<-sum(bonos[1:fib,1])
  sb[7,1]<-sum(R[,1])
  sb[8,1]<--(sum(CAcb[,1]))
  
  sg[1,1]<-cajaG[1]
  sg[6,1]<--(sum(bonos[,1]))
  
  scb[1,1]<--cajaBC[1]
  scb[6,1]<-bonos[fib+1,1]
  scb[7,1]<--(sum(R[,1]))
  scb[8,1]<-sum(CAcb[,1])
  
  ctrlstocks[1,1]<-sum(sh[1,1],sc[1,1],sk[1,1],sb[1,1],sg[1,1],scb[1,1])
  ctrlstocks[2,1]<-sum(sh[2,1],sc[2,1],sk[2,1],sb[2,1],sg[2,1],scb[2,1])
  ctrlstocks[3,1]<-sum(sh[3,1],sc[3,1],sk[3,1],sb[3,1],sg[3,1],scb[3,1])
  ctrlstocks[4,1]<-sum(sh[4,1],sc[4,1],sk[4,1],sb[4,1],sg[4,1],scb[4,1])
  ctrlstocks[5,1]<-sum(sh[5,1],sc[5,1],sk[5,1],sb[5,1],sg[5,1],scb[5,1])
  ctrlstocks[6,1]<-sum(sh[6,1],sc[6,1],sk[6,1],sb[6,1],sg[6,1],scb[6,1])
  ctrlstocks[7,1]<-sum(sh[7,1],sc[7,1],sk[7,1],sb[7,1],sg[7,1],scb[7,1])
  ctrlstocks[8,1]<-sum(sh[8,1],sc[8,1],sk[8,1],sb[8,1],sg[8,1],scb[8,1])
  ctrlstocks<-round(ctrlstocks)
  
  NWag[1,1]<-sum(sh[,1])
  NWag[1,2]<-sum(sc[,1])
  NWag[1,3]<-sum(sk[,1])
  NWag[1,4]<-sum(sb[,1])
  NWag[1,5]<-sum(sg[,1])
  NWag[1,6]<-sum(scb[,1])
  NWag<-round(NWag)
    
      #Siguientes periodos----
  hora[1]<-Sys.time()
  for (t in 2:Time) {
    acciones<-round(acciones,digits = 4)  
    bonos<-round(bonos,digits = 4)
    CAcb<-round(CAcb,digits = 4)
    capital<-round(capital,digits = 4)
    cdh<-round(cdh,digits = 4)
    ch<-round(ch,digits = 4)
    CR<-round(CR,digits = 4)
    CRT<-round(CRT,digits = 4)  
    d<-round(d,digits = 4)
    deltax<-round(deltax,digits = 4)
    deposits<-round(deposits,digits = 4)
    deudax<-round(deudax,digits = 4)
    Dive<-round(Dive,digits = 4)
    Divh<-round(Divh,digits = 4)
    Divx<-round(Divx,digits = 4)  
    gd<-round(gd,digits = 4)
    idx<-round(idx,digits = 4)
    Idx<-round(Idx,digits = 4)
    inctax<-round(inctax,digits = 4)
    intdep<-round(intdep,digits = 4)
    intdepap<-round(intdepap,digits = 4)
    inv<-round(inv,digits = 4)  
    K<-round(K,digits = 4)
    kamor<-round(kamor,digits = 4)
    kapa<-round(kapa,digits = 4)
    kreal<-round(kreal,digits = 4)
    Ld<-round(Ld,digits = 4)
    
    LR<-round(LR,digits = 4)  
    LRT<-round(LRT,digits = 4)
    Ltot<-round(Ltot,digits = 4)
    OCF<-round(OCF,digits = 4)
    OCFe<-round(OCFe,digits = 4)
    peh<-round(peh,digits = 4)
    pix<-round(pix,digits = 4)
    pknew<-round(pknew,digits = 4)  
    pkold<-round(pkold,digits = 4)
    pnew<-round(pnew,digits = 4)
    pold<-round(pold,digits = 4)
    proftax<-round(proftax,digits = 4)
    px<-round(px,digits = 4)
    s<-round(s,digits = 4)
    se<-round(se,digits = 4)  
    Sh<-round(Sh,digits = 4)
    ud<-round(ud,digits = 4)
    ux<-round(ux,digits = 4)
    w<-round(w,digits = 4)
    wD<-round(wD,digits = 4)  
    we<-round(we,digits = 4)
    wh<-round(wh,digits = 4)
    wn<-round(wn,digits = 4)
    WNi<-round(WNi,digits = 4)
    wtax<-round(wtax,digits = 4)
    y<-round(y,digits = 4)
    yd<-round(yd,digits = 4)
    
    
    ilb<-round(ilb,digits = 8)
    idb<-round(idb,digits = 8)
    loans<-round(loans,digits = 8)
    mu<-round(mu,digits = 8)
    muc<-round(muc,digits = 8)
    mun<-round(mun,digits = 8)
    Prdx<-round(Prdx,digits = 8)
    Primi<-round(Primi,digits = 8)
    Prinn<-round(Prinn,digits = 8)
    Prsc<-round(Prsc,digits = 8)
    Prscred<-round(Prscred,digits = 8)
    Prsdep<-round(Prsdep,digits = 8)
    Prsk<-round(Prsk,digits = 8)
    r<-round(r,digits = 8)
    uc<-round(uc,digits = 8)
    
    
    #REVISAR
    Nd<-ceiling(Nd)
    Ndm<-ceiling(Ndm)
    Ndo<-ceiling(Ndo)
    Ndr<-ceiling(Ndr)
    Ndw<-ceiling(Ndw)
    Nm<-ceiling(Nm)
    No<-ceiling(No)
    Nr<-ceiling(Nr)
    Nw<-ceiling(Nw)
    Nx<-ceiling(Nx)
    NW<-ceiling(NW)
    
    
    
    #Actualizo elementos  
    if(length(which(loans[,5]<=t-loans[,6]))>0){
      loans<-as.matrix(loans[-which(loans[,5]<=t-loans[,6]),])}
    constrans1<-round(constrans,digits = 4)
    constrans<-round(cbind(h=constrans[,1],fic=constrans[,2],c=0,p=constrans[,4]),digits = 4)
    aporte<-matrix(data = 0,nrow = nm,ncol=fic+fik+fib)
    aportec<-matrix(data = 0,nrow = nm,ncol=fic)
    dsLd<-vector(mode = "numeric",length = (fic+fik))
    bene<-vector(mode = "numeric",length = (fic+fik))
    apagar<-matrix(data = 0,nrow = fic,ncol = fik)
    apagarp<-matrix(data = 0,nrow = fic,ncol = fik)
    apagarv<-matrix(data = 0,nrow = fic,ncol = fik)
    apagara<-matrix(data = 0,nrow = fic,ncol = fik)
    ventanilla<-matrix(data=0,nrow = fic+fik,ncol = fib)
    disponible<-vector(mode = "numeric",length = (fic+fik+fib+nw+no+nr+nm+2))
    for (b in 1:fib) {
      disponible[fic+fik+b]<-round(R[b,t-1],digits = 4)
    }
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-cajaBC[t-1]-sum(R[,t-1])
    
    #edad del capital
    capital[,5]<-capital[,5]+1
    #elimino el capital obsoleto del periodo anterior
    for (i in 1:fic) {
      if(length(capital[which(capital[,1]==i & capital[,5]<=capital[,8]),])>0 & length(which(capital[,1]==i & capital[,5]>capital[,8]))>0){
        capital<-capital[-(which(capital[,1]==i & capital[,5]>capital[,8])),]  
      }else{
        capital[which(capital[,1]==i & capital[,5]>capital[,8]),3]<-0
      }
    }
    #actualizo el stock de capital y las variedades
    for (i in 1:fic) {
      K[i,t]<-sum(capital[which(capital[,1]==i),3])
      if(length(capital[which(capital[,1]==i),2])>0){
        varahora<-sort(capital[which(capital[,1]==i),2])
        vardebeser<-1:length(capital[which(capital[,1]==i),2])    
        for (j in 1:length(capital[which(capital[,1]==i),2])) {
          capital[which(capital[,1]==i & capital[,2]==varahora[j]),2]<-vardebeser[j]
        }
      }else{capital[which(capital[,1]==i),2]<-1}
    }  
    
    for(h in 1:(nw+no+nr+nm)){
      #ingreso disponible
      NIh[h,t]<-max(0,sum(w[h,t-1]+Divh[h,t-1]+d[h,t-1]-inctax[h,t-1]-wtax[h,t-1],if(t==1){intdep[fic+fik+h,t-1]}else{intdep[fic+fik+h,t-2]}))
      #riqueza
      wh[h,t]<-deposits[fic+fik+h,3]
      #riqueza de los managers
      acciones_totales[t]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),t])
      #ahorro maximo
      Sh[h,t]<-NIh[h,t]
    }
    
    
    
    for (i in 1:(fic+fik)) {  
      se[i,t]<-max(0,se[i,t-1]+lambda*(s[i,t-1]-se[i,t-1]))
      yd[i,t]<-max(0,min(se[i,t]*(1+nu)-inv[i,(t-1)],if(x[i,2]=="c"){sum(capital[which(capital[,1]==i),3]*capital[which(capital[,1]==i),7])}))
      if(x[i,2]=="k"){
        capital[which(capital[,1]==i),3]<-capital[which(capital[,1]==i),3]+yd[i,t]
      }
    }
    
    
    for (i in 1:(fic+fik)) {
      if (x[i,2]=="k") {
        Ndw[i,t]<-ceiling(yd[i,t]/mun[i,t])
      }else{
        if (capital[which(capital[,1]==i & capital[,2]==1),3]==0){capital[which(capital[,1]==i & capital[,2]==1),4]<-0#ver si conviene ponerle utilizacion 0, 1 u otro valor distinto
        }else{ 
          if (capital[which(capital[,1]==i & capital[,2]==1),3]*capital[which(capital[,1]==i & capital[,2]==1),7]<=yd[i,t]) {
            capital[which(capital[,1]==i & capital[,2]==1),4]<-1
          }else{
            capital[which(capital[,1]==i & capital[,2]==1),4]<-yd[i,t]/(capital[which(capital[,1]==i & capital[,2]==1),3]*capital[which(capital[,1]==i & capital[,2]==1),7])
          }    
        }
        
        if(max(capital[which(capital[,1]==i),2])>1){  
          for (j in 2:max(capital[which(capital[,1]==i),2])){
            resultado1<-vector()
            for (q in 1:(j-1)){
              resultado1<-c(resultado1,capital[which(capital[,1]==i & capital[,2]==q),4]*capital[which(capital[,1]==i & capital[,2]==q),3]*capital[which(capital[,1]==i & capital[,2]==q),7])
            }
            if (sum(resultado1)>=yd[i,t]){capital[which(capital[,1]==i & capital[,2]==j),4]<-0
            }else{
              if((sum(resultado1)+capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),7])<=yd[i,t]){capital[which(capital[,1]==i & capital[,2]==j),4]<-1
              }else{capital[which(capital[,1]==i & capital[,2]==j),4]<-(yd[i,t]-sum(resultado1))/capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),7]}
            }
            
            resultado2<-vector()
            for (q in 1:j){
              resultado2<-c(resultado2,capital[which(capital[,1]==i & capital[,2]==q),4]*capital[which(capital[,1]==i & capital[,2]==q),3]/lk)
            }  
          }
        }else{resultado2<-capital[which(capital[,1]==i & capital[,2]==1),4]*capital[which(capital[,1]==i & capital[,2]==1),3]/lk}
        Ndw[i,t]<-ceiling(sum(resultado2))
        #determino la productividad segun la produccion deseada. Si no la concreta, en 13 Production se corrige atendiendo lo observado
        resultado21<-vector()
        for (j in 1:max(capital[which(capital[,1]==i),2])){
          resultado21<-c(resultado21,capital[which(capital[,1]==i & capital[,2]==j),3]*capital[which(capital[,1]==i & capital[,2]==j),4])
        }
        if(yd[i,t]>0 & sum(capital[which(capital[,1]==i),3]*capital[which(capital[,1]==i),4])>0){muc[i,t]<-yd[i,t]/sum(capital[which(capital[,1]==i),3]*capital[which(capital[,1]==i),4])}
      }
      
      if (x[i,2]=="k") {
        Ndo[i,t]<-floor(Ndw[i,t]*shareko/sharekw)
        Ndr[i,t]<-floor(Ndw[i,t]*sharekr/sharekw)
        Ndm[i,t]<-floor(Ndw[i,t]*sharekm/sharekw)
      }else{ if (x[i,2]=="c"){
        Ndo[i,t]<-floor(Ndw[i,t]*shareco/sharecw)
        Ndr[i,t]<-floor(Ndw[i,t]*sharecr/sharecw)
        Ndm[i,t]<-floor(Ndw[i,t]*sharecm/sharecw)
      }
      }
      Nd[i,t]<-Ndw[i,t]+Ndo[i,t]+Ndr[i,t]+Ndm[i,t]
    }
    
    #Gobierno
    Nd[fic+fik+1,t]<-Ngt
    Ndw[fic+fik+1,t]<-ceiling(Nd[fic+fik+1,t]*sharecw)
    Ndo[fic+fik+1,t]<-ceiling(Nd[fic+fik+1,t]*shareco)
    Ndr[fic+fik+1,t]<-ceiling(Nd[fic+fik+1,t]*sharecr)
    Ndm[fic+fik+1,t]<-ceiling(Nd[fic+fik+1,t]*sharecm)
    
    #3 Price, interests and wages----
    #PRECIOS
    
    for (i in 1:(fic+fik)) {
      
      we[i,t]<-if(wn[i,t-1]<0.01){
        we[i,t-1]*(1+pic[t-1])#eSTO LO INVENTe YO
      }else{
        we[i,t-1]+lambda*(wn[i,t-1]-we[i,t-1])
      }
      #seguro para tener un salario esperado positivo LO INVENTe YO    
      if(we[i,t]<0.01){we[i,t]<-we[i,t-1]}
      
      #el mark-up no puede ser negativo
      
      if (x[i,2]=="k"){#uTILIZACIoN DESEADA CORRE SOLO PARA LAS FIRMAS DE CONSUMO
        if (if(s[i,t-1]>0){inv[i,t-1]/s[i,t-1]}else{nu+1}<=nu){
          mu[i,t]<-max(mu[i,t-1]*(1+abs(rnorm(1,muFN1,sigmaFN1))),0)
        }else{
          mu[i,t]<-max(mu[i,t-1]*(1-abs(rnorm(1,muFN1,sigmaFN1))),0)}
      }else{
        if (if(s[i,t-1]>0){inv[i,t-1]/s[i,t-1]}else{nu+1}<=nu & ux[i,t-1]>=utecho) {
          mu[i,t]<-max(mu[i,t-1]*(1+abs(rnorm(1,muFN1,sigmaFN1))),0)
        }else{
          mu[i,t]<-max(mu[i,t-1]*(1-abs(rnorm(1,muFN1,sigmaFN1))),0)}
      }
      
      px[i,t]<-(1+mu[i,t])*we[i,t]*Nd[i,t]/max(yd[i,t],1)
      if(px[i,t]==0){px[i,t]<-median(px[1:fic,t-1])}#eSTO LO INVENTe YO
    }
    
    for (i in (fic+1):(fic+fik)) {
      capital[which(capital[,1]==i),6]<-px[i,t]
    }
    
    #TASAS DE INTEReS  
    
    #Para la tasa activa hallo el promedio de la tasa activa para el sector
    itechol[t-1]<-sum(ilb[,t-1]*Ltot[,t-1])/sum(Ltot[,t-1])
    
    
    #Para la tasa pasiva, hallo el promedio de liquidez del sector
    #LR esta actualizado antes de comprar bonos, o sea que representa si hubo exceso de liquidez o no
    resultado4<-vector()
    for (b in 1:fib) {
      resultado4<-c(resultado4,LR[b,t-1]*if(sum(deposits[which(deposits[,2]==b),3])>0.0001){
        sum(deposits[which(deposits[,2]==b),3])
      }else{
        0})
    }
    LRpromedio<-sum(resultado4)/sum(deposits[,3])
    
    #hallo la tasa de interes pasiva del mercado
    resultado5<-vector()
    for (b in 1:fib) {
      resultado5<-c(resultado5,idb[b,t-1]*sum(deposits[which(deposits[,2]==b),3]))
    }
    itechod[t-1]<-sum(resultado5)/sum(deposits[,3])
    
    #Tasa activa
    for (b in 1:fib) {
      
      Ltot[b,t]<-sum(loans[which(loans[,2]==b),3]*(1-(t-loans[which(loans[,2]==b),6])/loans[which(loans[,2]==b),5]))
      NW[b,t]<-Ltot[b,t]+bonos[b,t-1]+R[b,t-1]-sum(deposits[which(deposits[,2]==b),3])-CAcb[b,t-1]
      
      #CR no puede ser menor a 0,06
      CR[b,t]<-if(Ltot[b,t]>0){NW[b,t]/Ltot[b,t]}else{1}
      CRT[b,t]<-max(sum(NW[,t-1])/sum(Ltot[,t-1]),0.06)
      
      
      
      if (CR[b,t]<CRT[b,t]) {ilb[b,t]<-itechol[t-1]*(1+abs(rnorm(1,muFN2,sigmaFN2)))
      }else{ilb[b,t]<-itechol[t-1]*(1-abs(rnorm(1,muFN2,sigmaFN2)))}
      
      #Tasa pasiva
      
      #La tasa de interes de depositos se ajusta dependiendo de la 
      #relacion entre la liquidez del banco y su objetivo, 
      #que sera el promedio de liquidez del sector en t-1
      
      
      #determino el objetivo de liquidez de cada institucion. 
      #Seran iguales.
      #LR no puede ser menor a 0,08 (la exigencia del BC)
      LRT[b,t]<-max(LRpromedio,0.08)
      
      
      
      if (LR[b,t-1]>=LRT[b,t]) {idb[b,t]<-itechod[t-1]*(1-abs(rnorm(1,muFN2,sigmaFN2)))
      }else{
        idb[b,t]<-itechod[t-1]*(1+abs(rnorm(1,muFN2,sigmaFN2)))}
    }
    
    
    
    #SALARIOS DE RESERVA
    for (h in 1:(nw+no+nr+nm)){
      resultado6<-vector()
      if (t == 2){resultado6<-c(uh[h,1])}else{
        if (t==3){resultado6<-c(uh[h,1],uh[h,2])}else{
          if (t==4){resultado6<-c(uh[h,1],uh[h,2],uh[h,3])}else{
            for (j in 1:4){
              
              resultado6<-c(resultado6,uh[h,(t-j)])  
            }  
          }
        }
        
      }
      if (sum(resultado6)>if(x[fic+fik+fib+h,2]=="m"){tum}else{tu}){
        wD[h,t]<-wD[h,(t-1)]*(1-abs(rnorm(1,muFN1,sigmaFN1)))
      }else{wD[h,t]<-wD[h,(t-1)]*(1+abs(rnorm(1,muFN1,sigmaFN1)))} 
      #seguro para que el salario deseado sea positivo eSTO LO INVENTe YO
      if(wD[h,t]<0.01){wD[h,t]<-wD[h,t-1]}
    }
    
    #4- Investment in capital accumulation----
    
    for (i in 1:fic) {
      ud[i,t]<-if(sum(capital[which(capital[,1]==i),3])>0.0001){
        (sum(capital[which(capital[,1]==i),3]*capital[which(capital[,1]==i),4])/sum(capital[which(capital[,1]==i),3]))
      }else{
        1}
      gd[i,t]<-gama1*(r[i,t-1]-rtecho)/rtecho + gama2*(ud[i,t]-utecho)/utecho
    }
    
    
    #5- Capital goods market (1)----
    #Aca se generan las ordenes. En el paso 11 se entregan
    #cada firma de consumo compra el equivalente a la suma del capital obsoleto y gd.
    #se lo compra con probabilidad Prs al nuevo proveedor y (1-Prs) al viejo
    
    var_of<-vector(mode = "numeric", length = fik)
    
    for (i in sample(c(1:fic),fic,replace = FALSE)) {
      
      provold<-kmarket[i,t-1]
      
      varold=1
      
      #costo unitario laboral asociado a varold
      pkold[i,t]<-((we[i,t]/(capital[which(capital[,1]==fic+provold & capital[,2]==varold),7]*lk*sharecw))*capital[which(capital[,1]==fic+provold & capital[,2]==varold),8]+capital[which(capital[,1]==fic+provold & capital[,2]==varold),6])
      #voy a establecer los costos unitarios de cada vendedor (j) y los guardo en costosunit
      costosunit<-c(1:fik)
      for (j in 1:fik) {
        #determino la variedad de cada vendedor
        varnew<-1
        var_of[j]<-varnew
        costosunit[j]<-(we[i,t]/(capital[which(capital[,1]==fic+j & capital[,2]==varnew),7]*lk*sharecw)*capital[which(capital[,1]==fic+j & capital[,2]==varnew),8]+capital[which(capital[,1]==fic+j & capital[,2]==varnew),6])
      }
      
      #determino la oferta visible para i
      oferentes<-if(length(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)])==1){
        c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)]
      }else{
        sample(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)], min(chik,length(c(1:fik)[-c(which(c(1:fik)==provold),capital[which(capital[,1]>=101 & capital[,1]<=110 & capital[,3]==0),1]-100)])), replace = FALSE)
      }
      
      
      if(length(oferentes)>0){
        oferentes<-cbind(oferentes,costosunit[oferentes],var_of[oferentes])
        
        #costo unitario laboral asociado al mejor varnew
        pknew[i,t]<-min(oferentes[,2])
        
        #proveedor candidato a sustituir al anterior
        provnew<-as.numeric(oferentes[which.min(oferentes[,2]),1])
        #Eligo vendedor
        if (capital[which(capital[,1]==(fic+provold)),3]>0){
          if (capital[which(capital[,1]==(fic+provnew)),3]>0){     
            if (pknew[i,t]<pkold[i,t]){Prsk[i,t]<-1-exp(epsilonk*(pknew[i,t]-pkold[i,t])/pknew[i,t])}else{Prsk[i,t]<-0}
            #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA FIRMA DE CONSUMO ATENDIENDO ESTA PROBABILIDAD
            kmarket[i,t]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsk[i,t],Prsk[i,t]))
            if (kmarket[i,t] == provold) {varcom<-varold}else{varcom<-as.numeric(oferentes[which(oferentes[,1]==provnew),3])}
          }else{#si provnew agoto la oferta
            kmarket[i,t]<-provold
            varcom<-varold
          } 
        }else{if (capital[which(capital[,1]==(fic+provnew)),3]>0){
          kmarket[i,t]<-provnew
          varcom<-as.numeric(oferentes[which(oferentes[,1]==provnew),3])}else{kmarket[i,t]<-kmarket[i,t-1]}
        }
      }else{#si oferentes es vacio
        if(capital[which(capital[,1]==fic+provold),3]>0){
          kmarket[i,t]<-provold
          varcom<-varold
        }else{kmarket[i,t]<-kmarket[i,t-1]}
      }    
      
      
      #Determino la cantidad demandada    
      for (j in 1:max(capital[which(capital[,1]==i),2])){
        if(capital[which(capital[,1]==i & capital[,2]==j),8]==capital[which(capital[,1]==i & capital[,2]==j),5]){
          kobsoleto[i,t]<-kobsoleto[i,t]+capital[which(capital[,1]==i & capital[,2]==j),3]
        }
      }
      #inversion deseada INTRODUZCO RESTRICCIoN PARA QUE DECIDA REPONER CAPITAL EN CASO QUE NO TENGA O SOLO TENGA CAPITAL QUE ENTRA EN OBSOLESCENCIA
      idx[i,t]<-if(K[i,t]<0.0001){(capvarnominalinicial[1]/fic)*(kapa0)/(pk1*kapa0)}else{max(if(kobsoleto[i,t]==K[i,t]){kobsoleto[i,t]}else{0},kobsoleto[i,t]+gd[i,t]*K[i,t])}
      #inversion deseada nominal
      Idx[i,t]<-idx[i,t]*capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
      
      
      
      #ME FIJO SI HAY STOCK Y SINO HAY QUE BUSCAR OTRO PROVEEDOR
      falta<-idx[i,t]
      transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3])
      if (transado > 0){
        #Agrego la cantidad de capital demandada a fic. Lo incorporo con antig?edad 0 y utilizacion deseada 0 xq se utilizan en el proximo periodo, no en este    
        #Determino la variedad y lo incorporo con ella
        if(#si fic tiene capital con la misma o menor productividad
          length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]))!=0){
          num_var<-min(capital[which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]),2])
          capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
          capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
          
        }else{#si fic solo tiene capital mas productivo -> length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7])==0)
          num_var<-max(capital[which(capital[,1]==i),2])+1
          capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
        }
        
        
        falta<-falta-transado
        #ingreso la venta de fik
        s[fic+kmarket[i,t],t]<-s[fic+kmarket[i,t],t]+transado
        #Quito el stock de la fik
        capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]-transado
        #Registro la deuda
        apagar[i,kmarket[i,t]]<- transado * capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
        apagarp[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
        apagarv[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]
        apagara[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),5]
        
        
        
        
        #ahora fic elige entre los que quedan para completar su inversion deseada
        if(length(oferentes)>0){
          while (falta > 0.0001 & sum(capital[which(capital[,1] %in% (fic+oferentes[,1])),3],capital[which(capital[,1] %in% (fic+provold)),3]) >0.0001){
            
            if (dim(oferentes)[1]>1){
              oferentes<-matrix(data=oferentes[-which.min(oferentes[,2]),],ncol=3)
            }
            
            #costo unitario laboral asociado al mejor varnew
            pknew[i,t]<-min(oferentes[,2])
            
            #proveedor candidato a sustituir al anterior
            provnew<-as.numeric(oferentes[which.min(oferentes[,2]),1])
            varnew<-oferentes[which.min(oferentes[,2]),3]
            
            if (capital[which(capital[,1]==(fic+provold)),3]>0){
              if (capital[which(capital[,1]==(fic+provnew)),3]>0){
                if (pknew[i,t]<pkold[i,t]){Prsk[i,t]<-1-exp(epsilonk*(pknew[i,t]-pkold[i,t])/pknew[i,t])}else{Prsk[i,t]<-0}
                #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA FIRMA DE CONSUMO ATENDIENDO ESTA PROBABILIDAD
                kmarket[i,t]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsk[i,t],Prsk[i,t]))
                if (kmarket[i,t] == provold) {varcom<-varold}else{varcom<-varnew}
              }else{
                kmarket[i,t] <- provold
                varcom<-varold
              }
            }else{if (capital[which(capital[,1]==(fic+provnew)),3]>0){
              kmarket[i,t]<-provnew
              varcom<-varnew}
            }
            
            #determino la cantidad transada
            transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3])
            
            if(#si tiene capital con la misma productividad y antig?edad
              length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7] & capital[,5] == 0))!=0){
              num_var<-as.numeric(capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7] & capital[,5] == 0),2])
              capital[which(capital[,1]==i & capital[,2] == num_var),3]<-capital[which(capital[,1]==i & capital[,2] == num_var),3]+transado
            }else{#fic no tiene capital con la misma productividad y antig?edad
              if(#si fic tiene capital con la misma o menor productividad
                length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]))!=0){
                num_var<-min(capital[which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]),2])
                capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
                capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
                
              }else{#si fic solo tiene capital mas productivo length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7])==0)
                num_var<-max(capital[which(capital[,1]==i),2])+1  
                capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
              }
              
            }
            falta<-falta-transado
            #ingreso la venta de fik
            s[fic+kmarket[i,t],t]<-s[fic+kmarket[i,t],t]+transado
            #Quito el stock de la fik
            capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]-transado
            #Registro la deuda
            apagar[i,kmarket[i,t]]<- apagar[i,kmarket[i,t]] + transado * capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
            apagarp[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
            apagarv[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]
            apagara[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),5]
          }
          
          
          
        }else{if(capital[which(capital[,1] %in% (fic+provold)),3] > 0){
          
          
          while (falta > 0.0001 & capital[which(capital[,1] %in% (fic+provold)),3] > 0.0001){ 
            kmarket[i,t] <- provold
            varcom<-varold
            #determino la cantidad transada
            transado<-min(falta,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3])
            
            if(#si tiene capital con la misma productividad y antig?edad
              length(which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7] & capital[,5] == 0))!=0){
              num_var<-as.numeric(capital[which(capital[,1]==i & capital[,7] == capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7] & capital[,5] == 0),2])
              capital[which(capital[,1]==i & capital[,2] == num_var),3]<-capital[which(capital[,1]==i & capital[,2] == num_var),3]+transado
            }else{#fic no tiene capital con la misma productividad y antig?edad
              if(#si fic tiene capital con la misma o menor productividad
                length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]))!=0){
                num_var<-min(capital[which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]),2])
                capital[which(capital[,1]==i & capital[,2] >= num_var),2]<-capital[which(capital[,1]==i & capital[,2] >= num_var),2]+1
                capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
                
              }else{#si fic solo tiene capital mas productivo length(which(capital[,1]==i & capital[,7] <= capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7])==0)
                num_var<-as.numeric(max(capital[which(capital[,1]==i),2])+1)  
                capital<-rbind(capital,c(i,num_var,transado,0,0,capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6],capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7],kapa0))
              }
              
            }
            falta<-falta-transado
            #ingreso la venta de fik
            s[fic+kmarket[i,t],t]<-s[fic+kmarket[i,t],t]+transado
            #Quito el stock de la fik
            capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),3]-transado
            #Registro la deuda
            apagar[i,kmarket[i,t]]<- apagar[i,kmarket[i,t]] + transado * capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
            apagarp[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),6]
            apagarv[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),7]
            apagara[i,kmarket[i,t]]<-capital[which(capital[,1]==fic+kmarket[i,t] & capital[,2]==varcom),5]    
          }
          
          
          
        }  
        }
      }
      
      
    }
    
    for (i in 1:(fic+fik)) {
      
      #Dividendos esperados
      Dive[i,t]<-Dive[i,t-1]+lambda*(Divx[i,t-1]-Dive[i,t-1])
      
      #Operating cash flows esperados
      OCFe[i,t]<-OCFe[i,t-1]+lambda*(OCF[i,t-1]-OCFe[i,t-1])
      #OCFe[i,t]<-sum(se[i,t]*px[i,t]+intdep[i,t-1]-we[i,t]*Nd[i,t],if(length(which(loans[,1]==i))>0){-sum(loans[which(loans[,1]==i),3]*(1/loans[which(loans[,1]==i),5]+loans[which(loans[,1]==i),4]*(1-(t-loans[which(loans[,1]==i),6])/loans[which(loans[,1]==i),5])))})*(1-taopi[t-1])
      
      #demanda de creditos (necesidad de financiamiento menos fondos propios)
      necfin<-max(Idx[i,t]+Dive[i,t]+sigma*we[i,t]*Nd[i,t]-OCFe[i,t],0)
      
      #Primero lo intenta cubrir con fondos propios
      if(deposits[which(deposits[,1]==i),3]<=0){Ld[i,t]<-necfin}else{
        Ld[i,t]<-max(0,necfin-deposits[which(deposits[,1]==i),3])
        disponible[i]<-disponible[i]+min(necfin,deposits[which(deposits[,1]==i),3])
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]-min(necfin,deposits[which(deposits[,1]==i),3])
        deposits[which(deposits[,1]==i),3]<-max(0,deposits[which(deposits[,1]==i),3]-necfin)
      }
      
      #matching
      
      if(Ld[i,t]>0){  
        
        
        #credold<-proveedor en t-1
        credold<-loanmarket[i,t-1]
        #icredold<-tasa ofrecida por credold en t
        icredold<-ilb[credold,t]
        
        #determino la oferta visible para i
        oferentes<-sample(c(1:fib)[-which(c(1:fib)==credold)],chicred,replace=FALSE)
        oferentes<-cbind(oferentes,ilb[oferentes,t])
        
        #icrednew<-tasa ofrecida por crednew en t
        icrednew<-min(oferentes[,2])
        #crednew<-banco candidato a sustituir al prestamista anterior
        crednew<-as.numeric(oferentes[which.min(oferentes[,2]),1])
        
        if (icrednew<icredold){Prscred[i,t]<-1-exp(epsiloncred*(icrednew-icredold)/icrednew)}else{Prscred[i,t]<-0}
        #AHORA TENGO QUE ASIGNAR EL PRESTAMISTA DE CADA FIRMA ATENDIENDO ESTA PROBABILIDAD
        loanmarket[i,t]<- sample(c(credold,crednew), 1, replace = FALSE, prob = c(1-Prscred[i,t],Prscred[i,t]))
      }
    }
    
    
    
    
    
    
    #7 Credit supply----
    #el banco le presta a todas las firmas cuyo retorno esperado del prestamo sea >=0
    #tambien prestaria un monto para el cual el retorno esperado no sea negativo ??
    #(entiendo un monto menor al solicitado) REVISAR
    
    #dsLd lo uso como vector auxiliar que se reescribe para cada banco en cada periodo
    
    for (i in 1:(fic+fik)) {
      #considero amortizacion e intereses
      deudax[i,t]<-sum(loans[which(loans[,1]==i),3]*(1-((t-loans[which(loans[,1]==i),6])/loans[which(loans[,1]==i),5]))*(1+loans[which(loans[,1]==i),4]))
    }
    
    
    for (b in 1:fib) {
      for (i in 1:(fic+fik)) {
        
        if (loanmarket[i,t]==b){
          
          if (Ld[i,t] > 0.0001){
            
            
            #para calcular los beneficios esperados necesito el stock real de capital de las firmas (kreal)
            #kreal <- stock de capital ponderado por la proporcion de vida util restante
            if (x[i,2]=="c"){
              resultado9<-vector() 
              for (j in 1:max(capital[which(capital[,1]==i),2])){
                resultado9<-c(resultado9,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
              }
              kreal[i,t]<-sum(resultado9)
              #de todo lo que debe, se va a poder recuperar esta proporcion
              if(deudax[i,t]==0){deltax[i,t]<-1}else{
                deltax[i,t]<-max(1,kreal[i,t]*(1-iota)/deudax[i,t])
              }
            }else{deltax[i,t]<-0}
            
            Ldi<-Ld[i,t]
            
            bene[i]<-(-8)
            
            
            while (Ldi > 0.0001 & bene[i] < 0.0001) {
              
              dsLd[i]<-(ilb[b,t]+1/eta[b,t])*Ldi
              
              #Prdx -> probabilidad de default
              # OCF VOY A USAR EL ESPERADO XQ TODAViA NO SE REALIZo EL OCF. 
              if (x[i,2]=="c") {stigma<-stigmac}else{stigma<-stigmak}
              
              Prdx[i,t]<-1/(1+exp((OCFe[i,t]-stigma*dsLd[i])/dsLd[i]))
              
              #AHORA TENGO QUE CALCULAR EL BENEFICIO ESPERADO PARA EL BANCO
              
              
              
              resultado11<-vector()
              for (q in 1:(eta[b,t]-1)){
                resultado24<-vector()
                for (j in 0:(q-1)) {
                  resultado24<-c(resultado24,1-j/eta[b,t])
                }
                resultado11<-c(resultado11,((1-Prdx[i,t])^q)*sum(resultado24))
              }
              intpas<-sum(resultado11)*Prdx[i,t]*ilb[b,t]
              
              resultado12<-vector()
              for (j in 0:(eta[b,t]-1)){
                resultado12<-c(resultado12,((1-Prdx[i,t])^j)*Prdx[i,t]*(1-j/eta[b,t])*(1-deltax[i,t]))
                amopas<-sum(resultado12)
              }
              
              resultado23<-vector()
              for (j in 0:(eta[b,t]-1)) {
                resultado23<-c(resultado23,1-j/eta[b,t])
              }
              saldospas<-sum(resultado23)
              
              bene[i]<-Ldi*(((1-Prdx[i,t])^eta[b,t])*ilb[b,t]*saldospas+intpas-amopas)
              
              ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
              ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
              
              
              if (bene[i] < 0.0001) {Ldi <- Ldi-Ld[i,t]*0.1}
              
            }
            
            ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
            
            
            #hasta aca el banco calculo el beneficio esperado de prestarle a cada firma bene[i]
            #la informacion imperfecta aparecio en la demanda
            #entiendo que aca el banco recibe todas las solicitudes
            if (bene[i]<0.0001){loanmarket[i,t]<-loanmarket[i,t-1]
            Lrej[i,t]<-Ld[i,t]
            }else{
              loans<-rbind(loans,c(i,b,Ldi,ilb[b,t],eta[b,t],t+1))
              disponible[fic+fik+b]<- disponible[fic+fik+b] - Ldi
              disponible[i]<-disponible[i] + Ldi
              Lrej[i,t]<-Ld[i,t]-Ldi
            }
            
          }else{loanmarket[i,t]<-loanmarket[i,t-1]
          Lrej[i,t]<-0}  
          ventanilla[i,b]<-1
        }
      }
    }
    
    
    Lr[t]<-sum(Lrej[,t])/sum(Ld[,t])
    Lrc[t]<-sum(Lrej[which(x[,2]=="c"),t])/sum(Ld[which(x[,2]=="c"),t])
    Lrk[t]<-sum(Lrej[which(x[,2]=="k"),t])/sum(Ld[which(x[,2]=="k"),t])
    #loanmarket indica el ultimo banco que le presto a la firma incluido t
    loan[t]<-sum(loans[,3]*(1-((t-loans[,6])/loans[,5])))
    loanc[t]<-sum(loans[which(loans[,1]<=100),3]*(1-((t-loans[which(loans[,1]<=100),6])/loans[which(loans[,1]<=100),5])))
    loank[t]<-sum(loans[which(loans[,1]<=110 & loans[,1]>100),3]*(1-((t-loans[which(loans[,1]<=110 & loans[,1]>100),6])/loans[which(loans[,1]<=110 & loans[,1]>100),5])))
    
    
    #8 Labor markets----
    
    #la firma va contratar o despedir segun su demanda de trabajo y cantidad de empleados
    #luego va a ordenar a los desocupados segun su salario de reserva y contrata al minimo
    #wD[i,t] <-salarios de reserva
    
    #extiendo los vinculos del periodo anterior
    trabajow[,t]<-trabajow[,t-1]
    trabajoo[,t]<-trabajoo[,t-1]
    trabajor[,t]<-trabajor[,t-1]
    trabajom[,t]<-trabajom[,t-1]
    
    w[,t]<-wa[,t-1]
    
    Nw[,t]<-Nw[,t-1]
    No[,t]<-No[,t-1]
    Nr[,t]<-Nr[,t-1]
    Nm[,t]<-Nm[,t-1]
    Nx[,t]<-Nx[,t-1]
    
    #echo una proporcion tita de trabajadores de cada firma
    
    for (i in 1:(fic+fik)){
      
      
      empleadoswi<-which(trabajow[,t]==i)
      empleadosoi<-which(trabajoo[,t]==i)
      empleadosri<-which(trabajor[,t]==i)
      empleadosmi<-which(trabajom[,t]==i)
      
      empleados<-c(rep("w",length(empleadoswi)),rep("o",length(empleadosoi)),rep("r",length(empleadosri)),rep("m",length(empleadosmi)))
      desempleados<-if(length(empleados)==1){
        empleados
      }else{
        sample(empleados,tita*length(empleados),replace = FALSE)
      }
      
      
      
      if (length(empleadoswi)>0){
        desempleadoswi<-if (length(empleadoswi)==1){
          empleadoswi
        }else{
          sample(empleadoswi,length(which(desempleados[]=="w")),replace=FALSE)
        }
        
        trabajow[desempleadoswi,t] <- 0
      }
      
      if (length(empleadosoi)>0){
        desempleadosoi<-if(length(empleadosoi)==1){
          empleadosoi
        }else{
          sample(empleadosoi,length(which(desempleados[]=="o")),replace=FALSE)
        }
        
        trabajoo[desempleadosoi,t]<-0
      }
      
      if (length(empleadosri)>0){
        desempleadosri<-if(length(empleadosri)==1){
          empleadosri
        }else{
          sample(empleadosri,length(which(desempleados[]=="r")),replace=FALSE)
        }
        
        trabajor[desempleadosri,t] <- 0
      }
      
      if (length(empleadosmi)>0){
        desempleadosmi<-if (length(empleadosmi)==1){
          empleadosmi
        }else{
          sample(empleadosmi,length(which(desempleados[]=="m")),replace=FALSE)
        }
        
        trabajom[desempleadosmi,t]<-0
      }
    }
    
    
    #echo funcionarios publicos
    
    empleadoswi<-which(trabajow[,t]==999)
    empleadosoi<-which(trabajoo[,t]==999)
    empleadosri<-which(trabajor[,t]==999)
    empleadosmi<-which(trabajom[,t]==999)
    
    empleados<-c(rep("w",length(empleadoswi)),rep("o",length(empleadosoi)),rep("r",length(empleadosri)),rep("m",length(empleadosmi)))
    desempleados<-if(length(empleados)==1){
      empleados
    }else{
      sample(empleados,tita*length(empleados),replace = FALSE)
    }
    
    
    
    
    
    if (length(empleadoswi)>0){
      desempleadoswi<-if (length(empleadoswi)==1){
        empleadoswi
      }else{
        sample(empleadoswi,length(which(desempleados[]=="w")),replace=FALSE)
      }
      
      trabajow[desempleadoswi,t] <- 0
    }
    
    if (length(empleadosoi)>0){
      desempleadosoi<-if(length(empleadosoi)==1){
        empleadosoi
      }else{
        sample(empleadosoi,length(which(desempleados[]=="o")),replace=FALSE)
      }
      
      trabajoo[desempleadosoi,t] <- 0
    }
    
    if (length(empleadosri)>0){
      desempleadosri<-if (length(empleadosri)==1){
        empleadosri
      }else{
        sample(empleadosri,length(which(desempleados[]=="r")),replace=FALSE)
      }
      
      trabajor[desempleadosri,t] <- 0
    }
    
    if (length(empleadosmi)>0){
      desempleadosmi<-if (length(empleadosmi)==1){
        empleadosmi
      }else{
        sample(empleadosmi,length(which(desempleados[]=="m")),replace=FALSE)
      }
      
      trabajom[desempleadosmi,t] <- 0
    }
    
    #Actualizo plantillas
    #FIRMAS
    for (i in 1:(fic+fik)){
      Nw[i,t]<-length(which(trabajow[,t]==i))
      No[i,t]<-length(which(trabajoo[,t]==i))
      Nr[i,t]<-length(which(trabajor[,t]==i))
      Nm[i,t]<-length(which(trabajom[,t]==i))
      Nx[i,t]<-Nw[i,t]+No[i,t]+Nr[i,t]+Nm[i,t]
    }
    #Gobierno    
    Nw[fic+fik+1,t]<-length(which(trabajow[,t]==999))
    No[fic+fik+1,t]<-length(which(trabajoo[,t]==999))
    Nr[fic+fik+1,t]<-length(which(trabajor[,t]==999))
    Nm[fic+fik+1,t]<-length(which(trabajom[,t]==999))
    Nx[fic+fik+1,t]<-Nw[fic+fik+1,t]+No[fic+fik+1,t]+Nr[fic+fik+1,t]+Nm[fic+fik+1,t]
    
    
    
    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
    #La firma contrata o despide trabajadores segun su produccion planificada
    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
    #MERCADO DE OPERARIOS
    
    for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
      
      if (i == 999){
        #Gobierno
        
        empleadoswi<-which(trabajow[,t]==999)
        if (length(empleadoswi)==Ndw[fic+fik+1,t]){"ok"}else{
          #contrata trabajadores
          if(length(empleadoswi) < Ndw[fic+fik+1,t] & length(which(trabajow[,t]==0)) > 0){
            candidatoswi<-if (length(which(trabajow[,t]==0))==1){
              which(trabajow[,t]==0)
            }else{
              sample(which(trabajow[,t]==0),min(length(which(trabajow[,t]==0)),chiw*(Ndw[fic+fik+1,t]-Nw[fic+fik+1,t])),replace = FALSE)
            }
            
            wdcandidatoswi<-cbind(candidatoswi,wD[candidatoswi,t])
            #print(8.1)
            while(length(empleadoswi) < Ndw[fic+fik+1,t] & dim(wdcandidatoswi)[1]>0){
              contratado<-wdcandidatoswi[which.min(wdcandidatoswi[,2]),1]
              trabajow[contratado,t]<-999
              w[contratado,t]<-wD[contratado,t]
              wdcandidatoswi<-matrix(wdcandidatoswi[-which(wdcandidatoswi[,1]==contratado),],ncol=2)
              empleadoswi<-which(trabajow[,t]==999)
            }
            
            
          }else{
            #despide trabajadores
            empleadoswi<-which(trabajow[,t]==999)
            if(min(length(empleadoswi),length(empleadoswi)-Ndw[fic+fik+1,t])>0){
              adespedir<-cbind(empleadoswi,w[empleadoswi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadoswi<-if(length(empleadoswi)==length(empleadoswi)-Ndw[fic+fik+1,t]){
                empleadoswi
              }else{
                adespedir[,1][1:(length(empleadoswi)-Ndw[fic+fik+1,t])]
                
              }
              
              
              trabajow[desempleadoswi,t] <- 0
            }
          }
          empleadoswi<-which(trabajow[,t]==999)
        }
        Nw[fic+fik+1,t]<-length(which(trabajow[,t]==999))
      }else{
        
        #FIRMAS
        
        empleadoswi<-which(trabajow[,t]==i)
        if (length(empleadoswi)==Ndw[i,t]){"ok"}else{
          #contrata trabajadores
          if(length(empleadoswi)<Ndw[i,t] & length(which(trabajow[,t]==0))>0){
            candidatoswi<-if (length(which(trabajow[,t]==0))==1){
              which(trabajow[,t]==0)
            }else{
              sample(which(trabajow[,t]==0),min(chiw*(Ndw[i,t]-Nw[i,t]),length(which(trabajow[,t]==0))),replace = FALSE)
            }
            
            wdcandidatoswi<-cbind(candidatoswi,wD[candidatoswi,t])
            #print(8.2)
            while(length(empleadoswi)<Ndw[i,t] & dim(wdcandidatoswi)[1]>0){
              contratado<-wdcandidatoswi[which.min(wdcandidatoswi[,2]),1]
              trabajow[contratado,t]<-i
              w[contratado,t]<-wD[contratado,t]
              wdcandidatoswi<-matrix(wdcandidatoswi[-which(wdcandidatoswi[,1]==contratado),],ncol=2)
              empleadoswi<-which(trabajow[,t]==i)
            }
            
          }else{
            #despide trabajadores
            empleadoswi<-which(trabajow[,t]==i)
            if(min(length(empleadoswi),length(empleadoswi)-Ndw[i,t])>0){
              adespedir<-cbind(empleadoswi,w[empleadoswi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadoswi<-if(length(empleadoswi)==length(empleadoswi)-Ndw[i,t]){
                empleadoswi
              }else{
                adespedir[,1][1:(length(empleadoswi)-Ndw[i,t])]
                
              }
              
              
              trabajow[desempleadoswi,t] <- 0
            }
          }
          empleadoswi<-which(trabajow[,t]==i)
        }
        
        Nw[i,t]<-length(which(trabajow[,t]==i))
      }
    }    
    
    
    #MERCADO DE OFICINISTAS
    
    
    for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
      
      if (i == 999){
        #Gobierno
        ratiowo<-shareco/sharecw
        empleadosoi<-which(trabajoo[,t]==999)
        if (length(empleadosoi)==ceiling(Nw[fic+fik+1,t]*ratiowo)){"ok"}else{
          #contrata trabajadores
          if(length(empleadosoi)<ceiling(Nw[fic+fik+1,t]*ratiowo) & length(which(trabajoo[,t]==0))>0){
            candidatosoi<-if (length(which(trabajoo[,t]==0))==1){
              which(trabajoo[,t]==0)
            }else{
              sample(which(trabajoo[,t]==0),min(chio*(Ndo[fic+fik+1,t]-No[fic+fik+1,t]),length(which(trabajoo[,t]==0))),replace = FALSE)
            }
            
            wdcandidatosoi<-cbind(candidatosoi,wD[nw+candidatosoi,t])
            #print(8.6)
            while(length(empleadosoi)<ceiling(Nw[fic+fik+1,t]*ratiowo) & dim(wdcandidatosoi)[1]>0){
              contratado<-wdcandidatosoi[which.min(wdcandidatosoi[,2]),1]
              trabajoo[contratado,t]<-999
              w[nw+contratado,t]<-wD[nw+contratado,t]
              wdcandidatosoi<-matrix(wdcandidatosoi[-which(wdcandidatosoi[,1]==contratado),],ncol=2)
              empleadosoi<-which(trabajoo[,t]==999)
            }
            
            
          }else{
            #despide trabajadores
            empleadosoi<-which(trabajoo[,t]==999)
            if(min(length(empleadosoi),length(empleadosoi)-ceiling(Nw[fic+fik+1,t]*ratiowo))>0){
              adespedir<-cbind(empleadosoi,w[nw+empleadosoi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadosoi<-if(length(empleadosoi)==length(empleadosoi)-ceiling(Nw[fic+fik+1,t]*ratiowo)){
                empleadosoi
              }else{
                adespedir[,1][1:(length(empleadosoi)-ceiling(Nw[fic+fik+1,t]*ratiowo))]
                
              }
              
              
              trabajoo[desempleadosoi,t] <- 0
            }
          }
          empleadosoi<-which(trabajoo[,t]==999)
        }
        No[fic+fik+1,t]<-length(which(trabajoo[,t]==999))
      }else{
        
        #FIRMAS
        
        if (x[i,2]=="k") {
          ratiowo<-shareko/sharekw
        }else{ 
          ratiowo<-shareco/sharecw
        }
        empleadosoi<-which(trabajoo[,t]==i)
        if (length(empleadosoi)==floor(Nw[i,t]*ratiowo)){"ok"}else{
          #contrata trabajadores
          if(length(empleadosoi)<floor(Nw[i,t]*ratiowo) & length(which(trabajoo[,t]==0))>0){
            candidatosoi<-if (length(which(trabajoo[,t]==0))==1){
              which(trabajoo[,t]==0)
            }else{
              sample(which(trabajoo[,t]==0),min(chio*(Ndo[i,t]-No[i,t]),length(which(trabajoo[,t]==0))),replace = FALSE)
            }
            
            wdcandidatosoi<-cbind(candidatosoi,wD[nw+candidatosoi,t])
            #print(8.7)
            while(length(empleadosoi)<floor(Nw[i,t]*ratiowo) & dim(wdcandidatosoi)[1]>0){
              contratado<-wdcandidatosoi[which.min(wdcandidatosoi[,2]),1]
              trabajoo[contratado,t]<-i
              w[nw+contratado,t]<-wD[nw+contratado,t]
              wdcandidatosoi<-matrix(wdcandidatosoi[-which(wdcandidatosoi[,1]==contratado),],ncol=2)
              empleadosoi<-which(trabajoo[,t]==i)
            }
            
            
          }else{
            #despide trabajadores
            empleadosoi<-which(trabajoo[,t]==i)
            if(min(length(empleadosoi)-floor(Nw[i,t]*ratiowo),length(empleadosoi))>0){
              adespedir<-cbind(empleadosoi,w[nw+empleadosoi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadosoi<-if(length(empleadosoi)==length(empleadosoi)-floor(Nw[i,t]*ratiowo)){
                empleadosoi
              }else{
                adespedir[,1][1:(length(empleadosoi)-floor(Nw[i,t]*ratiowo))]
               
              }
              
              trabajoo[desempleadosoi,t]<-0
            }
          }
          empleadosoi<-which(trabajoo[,t]==i)
        }
        No[i,t]<-length(which(trabajoo[,t]==i))
      }
    }
    
    
    #MERCADO DE INVESTIGADORES
    
    
    for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
      
      if (i == 999){
        #Gobierno
        #No contrata investigadores
        Nr[fic+fik+1,t]<-length(which(trabajor[,t]==999))
      }else{if (x[i,2]=="c"){    
        #Firmas de consumo
        #No contratan investigadores
        Nr[i,t]<-length(which(trabajor[,t]==i))    
      }else{
        #FIRMAS DE CAPITAL    
        if (x[i,2]=="k") {
          ratiowo<-shareko/sharekw
          ratiowr<-sharekr/sharekw
          ratiowm<-sharekm/sharekw
        }
        empleadosri<-which(trabajor[,t]==i)
        if (length(empleadosri)==floor(Nw[i,t]*ratiowr)){
          "ok"
        }else{
          #contrata trabajadores
          if(length(empleadosri)<floor(Nw[i,t]*ratiowr) & length(which(trabajor[,t]==0))>0){
            candidatosri<-if (length(which(trabajor[,t]==0))==1){
              which(trabajor[,t]==0)
            }else{
              sample(which(trabajor[,t]==0),min(chir*(Ndr[i,t]-Nr[i,t]),length(which(trabajor[,t]==0))),replace = FALSE)
            }
            
            wdcandidatosri<-cbind(candidatosri,wD[nw+no+candidatosri,t])
            #print(8.12)
            while(length(empleadosri)<floor(Nw[i,t]*ratiowr) & dim(wdcandidatosri)[1]>0){
              contratado<-wdcandidatosri[which.min(wdcandidatosri[,2]),1]
              trabajor[contratado,t]<-i
              w[nw+no+contratado,t]<-wD[nw+no+contratado,t]
              wdcandidatosri<-matrix(wdcandidatosri[-which(wdcandidatosri[,1]==contratado),],ncol=2)
              empleadosri<-which(trabajor[,t]==i)
            }
            
            
          }else{
            #despide trabajadores
            empleadosri<-which(trabajor[,t]==i)
            if(min(length(empleadosri),length(empleadosri)-floor(Nw[i,t]*ratiowr))>0){
              adespedir<-cbind(empleadosri,w[nw+no+empleadosri,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadosri<-if(length(empleadosri)==min(length(empleadosri),length(empleadosri)-floor(Nw[i,t]*ratiowr))){#min(ceiling(Nw[i,t]*ratiowr),ceiling(No[i,t]*ratiowr/ratiowo)))){
                empleadosri
              }else{
                adespedir[,1][1:(length(empleadosri)-floor(Nw[i,t]*ratiowr))]
              }
              
              trabajor[desempleadosri,t]<-0
            }
          }
          empleadosri<-which(trabajor[,t]==i)
        }
        Nr[i,t]<-length(which(trabajor[,t]==i))
      }
      }   
    }

    #MERCADO DE MANAGERS
    
    for (i in sample(c(1:(fic+fik),999),(fic+fik+1),replace = FALSE)){
      
      if (i == 999){
        #Gobierno
        ratiowo<-shareco/sharecw
        ratiowr<-sharecr/sharecw
        ratiowm<-sharecm/sharecw
        
        empleadosmi<-which(trabajom[,t]==999)
        if (length(empleadosmi)==ceiling(Nw[fic+fik+1,t]*ratiowm)){"ok"}else{
          #contrata trabajadores
          if(length(empleadosmi)<ceiling(Nw[fic+fik+1,t]*ratiowm) & length(which(trabajom[,t]==0))>0){
            candidatosmi<-if (length(which(trabajom[,t]==0))==1){
              which(trabajom[,t]==0)
            }else{
              sample(which(trabajom[,t]==0),min(chim*(Ndm[fic+fik+1,t]-Nm[fic+fik+1,t]),length(which(trabajom[,t]==0))),replace = FALSE)
            }
            
            wdcandidatosmi<-cbind(candidatosmi,wD[nw+no+nr+candidatosmi,t])
            #print(8.15)
            while(length(empleadosmi)<ceiling(Nw[fic+fik+1,t]*ratiowm) & dim(wdcandidatosmi)[1]>0){
              contratado<-wdcandidatosmi[which.min(wdcandidatosmi[,2]),1]
              trabajom[contratado,t]<-999
              w[nw+no+nr+contratado,t]<-wD[nw+no+nr+contratado,t]
              wdcandidatosmi<-matrix(wdcandidatosmi[-which(wdcandidatosmi[,1]==contratado),],ncol=2)
              empleadosmi<-which(trabajom[,t]==999)
            }
            
            
          }else{
            #despide trabajadores
            empleadosmi<-which(trabajom[,t]==999)
            if(min(length(empleadosmi),length(empleadosmi)-ceiling(Nw[fic+fik+1,t]*ratiowm))>0){
              adespedir<-cbind(empleadosmi,w[nw+no+nr+empleadosmi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadosmi<-if(length(empleadosmi)==(length(empleadosmi)-ceiling(Nw[fic+fik+1,t]*ratiowm))){
                empleadosmi
              }else{
                adespedir[,1][1:(length(empleadosmi)-ceiling(Nw[fic+fik+1,t]*ratiowm))]
                
              }
              
              
              trabajom[desempleadosmi,t]<-0
            }
          }
          empleadosmi<-which(trabajom[,t]==fic+fik+1)
        }
        Nm[fic+fik+1,t]<-length(which(trabajom[,t]==999))
      }else{
        
        #FIRMAS
        
        if (x[i,2]=="k") {
          ratiowo<-shareko/sharekw
          ratiowr<-sharekr/sharekw
          ratiowm<-sharekm/sharekw
        }else{ 
          ratiowo<-shareco/sharecw
          ratiowr<-sharecr/sharecw
          ratiowm<-sharecm/sharecw
        }
        
        empleadosmi<-which(trabajom[,t]==i)
        if (length(empleadosmi)==floor(Nw[i,t]*ratiowm)){#min(ceiling(Nw[i,t]*ratiowm),ceiling(No[i,t]*ratiowm/ratiowo),if(x[i,2]=="k"){ceiling(Nr[i,t]*ratiowm/ratiowr)})){
          "ok"
        }else{
          #contrata trabajadores
          if(length(empleadosmi)<floor(Nw[i,t]*ratiowm) & length(which(trabajom[,t]==0))>0){
            candidatosmi<-if (length(which(trabajom[,t]==0))==1){
              which(trabajom[,t]==0)
            }else{
              sample(which(trabajom[,t]==0),min(chim*(Ndm[i,t]-Nm[i,t]),length(which(trabajom[,t]==0))),replace = FALSE)
            }
            
            wdcandidatosmi<-cbind(candidatosmi,wD[nw+no+nr+candidatosmi,t])
            #print(8.16)
            while(length(empleadosmi)<floor(Nw[i,t]*ratiowm) & dim(wdcandidatosmi)[1]>0){
              contratado<-wdcandidatosmi[which.min(wdcandidatosmi[,2]),1]
              trabajom[contratado,t]<-i
              w[nw+no+nr+contratado,t]<-wD[nw+no+nr+contratado,t]
              wdcandidatosmi<-matrix(wdcandidatosmi[-which(wdcandidatosmi[,1]==contratado),],ncol=2)
              empleadosmi<-which(trabajom[,t]==i)
            }
            
            
          }else{
            #despide trabajadores
            empleadosmi<-which(trabajom[,t]==i)
            if(min(length(empleadosmi),length(empleadosmi)-floor(Nw[i,t]*ratiowm))>0){
              adespedir<-cbind(empleadosmi,w[nw+no+nr+empleadosmi,t])
              adespedir<-adespedir[order(adespedir[,2],decreasing = TRUE),]
              desempleadosmi<-if(length(empleadosmi)==length(empleadosmi)-floor(Nw[i,t]*ratiowm)){
                empleadosmi
              }else{
                adespedir[,1][1:(length(empleadosmi)-floor(Nw[i,t]*ratiowm))]
              }
              
              trabajom[desempleadosmi,t]<-0
            }
          }
          empleadosmi<-which(trabajom[,t]==i)
        }
        Nm[i,t]<-length(which(trabajom[,t]==i))
      }
    }
    
    
    
    #FIRMAS
    for (i in 1:(fic+fik)){
      Nw[i,t]<-length(which(trabajow[,t]==i))
      No[i,t]<-length(which(trabajoo[,t]==i))
      Nr[i,t]<-length(which(trabajor[,t]==i))
      Nm[i,t]<-length(which(trabajom[,t]==i))
      Nx[i,t]<-Nw[i,t]+No[i,t]+Nr[i,t]+Nm[i,t]
    }
    #Gobierno    
    Nw[fic+fik+1,t]<-length(which(trabajow[,t]==999))
    No[fic+fik+1,t]<-length(which(trabajoo[,t]==999))
    Nr[fic+fik+1,t]<-length(which(trabajor[,t]==999))
    Nm[fic+fik+1,t]<-length(which(trabajom[,t]==999))
    Nx[fic+fik+1,t]<-Nw[fic+fik+1,t]+No[fic+fik+1,t]+Nr[fic+fik+1,t]+Nm[fic+fik+1,t]
    
    
    #actualizo bases de operarios
    for (a in 1:nw) {
      if (trabajow[a,t]==0){
        w[a,t]<-0
        uh[a,t]<-1
      }else{
        uh[a,t]<-0
      }
    }
    
    #actualizo bases de oficinistas
    for (a in 1:no) {
      if (trabajoo[a,t]==0){
        w[nw+a,t]<-0
        uh[nw+a,t]<-1
      }else{
        uh[nw+a,t]<-0
      }
    }
    
    #actualizo bases de investigadores
    for (a in 1:nr) {
      if (trabajor[a,t]==0){
        w[nw+no+a,t]<-0
        uh[nw+no+a,t]<-1
      }else{
        uh[nw+no+a,t]<-0
      }
    }
    
    #actualizo bases de managers
    for (a in 1:nm) {
      if (trabajom[a,t]==0){
        w[nw+no+nr+a,t]<-0
        uh[nw+no+nr+a,t]<-1
      }else{
        uh[nw+no+nr+a,t]<-0
      }
      wh[nw+no+nr+a,t]<-disponible[fic+fik+fib+nw+no+nr+a]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+a),3]
    }
    
    #Capital accionario
    for (i in 1:(fic+fik)) {
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
    }    
    
    
    
    #9 Production----
    
    for (i in (fic+1):(fic+fik)) {
      y[i,t]<-Nw[i,t]*mun[i,t]
      if(s[i,t] > 0 & y[i,t] < s[i,t]-inv[i,t-1]){#se desarman ventas
        adevolver<-(s[i,t]-inv[i,t-1]-y[i,t])/s[i,t]
        acreedorc<-which(apagar[,i-fic]>0)
        vecank[i-fic,t]<-vecank[i-fic,t]+s[i,t]-inv[i,t-1]-y[i,t]
        
        for (c in acreedorc) {
          capital[which(capital[,1]==c & capital[,5]==0 & capital[,6]==apagarp[c,i-fic] & capital[,7]==apagarv[c,i-fic]),3]<-capital[which(capital[,1]==c & capital[,5]==0 & capital[,6]==apagarp[c,i-fic] & capital[,7]==apagarv[c,i-fic]),3]-adevolver*apagar[c,i-fic]/apagarp[c,i-fic]
          cocank[c,t]<-cocank[c,t]+adevolver*apagar[c,i-fic]/apagarp[c,i-fic]
          apagar[c,i-fic]<-(1-adevolver)*apagar[c,i-fic]
        }
        s[i,t]<-(1-adevolver)*s[i,t]
      }else{if (y[i,t]<yd[i,t]){
        capital[which(capital[,1]==i),3]<-capital[which(capital[,1]==i),3]-(yd[i,t]-y[i,t])
      }
      }
      inv[i,t]<-inv[i,t-1]+y[i,t]-s[i,t]
    }
    
    for (i in 1:fic){
      if(Nw[i,t]>=Ndw[i,t] & Ndw[i,t]>0){
        y[i,t]<-sum(capital[which(capital[,1]==i & capital[,5]>0),4]*capital[which(capital[,1]==i & capital[,5]>0),3]*capital[which(capital[,1]==i & capital[,5]>0),7])
        ux[i,t]<-sum(capital[which(capital[,1]==i & capital[,5]>0),4]*capital[which(capital[,1]==i & capital[,5]>0),3])/sum(capital[which(capital[,1]==i & capital[,5]>0),3])    
        muc[i,t]<-y[i,t]/sum(capital[which(capital[,1]==i & capital[,5]>0),3]*capital[which(capital[,1]==i & capital[,5]>0),4])
      }else{
        ktotal<-lk*Nw[i,t] #maximo capital a utilizar
        if(ktotal==0){
          y[i,t]<-0
          inv[i,t]<-inv[i,t-1]
          muc[i,t]<-0
          ux[i,t]<-0}else{
            if (capital[which(capital[,1]==i & capital[,2]==1),3]>=ktotal & capital[which(capital[,1]==i & capital[,2]==1),5] > 0) {
              capital[which(capital[,1]==i & capital[,2]==1),4]<-ktotal/capital[which(capital[,1]==i & capital[,2]==1),3]
              capital[which(capital[,1]==i & capital[,2]!=1),4]<-0
            }else{
              if (capital[which(capital[,1]==i & capital[,2]==1),5]==0){
                capital[which(capital[,1]==i & capital[,2]==1),4]<-0
              }else{ 
                capital[which(capital[,1]==i & capital[,2]==1),4]<-1
                if(max(capital[which(capital[,1]==i),2])>1){
                  for (j in 2:max(capital[which(capital[,1]==i),2])){
                    resultado14<-vector()
                    for (q in 1:(j-1)){
                      resultado14<-c(resultado14,if (capital[which(capital[,1]==i & capital[,2]==q),5]==0){
                        0
                      }else{
                        if (capital[which(capital[,1]==i & capital[,2]==q),3]==0){
                          0
                        }else{
                          capital[which(capital[,1]==i & capital[,2]==q),4]*capital[which(capital[,1]==i & capital[,2]==q),3]
                        }
                      })
                      
                    }
                    if (sum(resultado14)+capital[which(capital[,1]==i & capital[,2]==j),3]>=ktotal & capital[which(capital[,1]==i & capital[,2]==j),5] > 0 & capital[which(capital[,1]==i & capital[,2]==j),3]>0){
                      capital[which(capital[,1]==i & capital[,2]==j),4]<-max((ktotal-sum(resultado14))/capital[which(capital[,1]==i & capital[,2]==j),3],0)
                      capital[which(capital[,1]==i & capital[,2]>j),4]<-0
                    }else{ 
                      capital[which(capital[,1]==i & capital[,2]==j),4]<-1
                      if (capital[which(capital[,1]==i & capital[,2]==j),5] == 0){
                        capital[which(capital[,1]==i & capital[,2]==j),4]<-0
                      }
                      
                      "sigue"}
                  }
                }
              }
            }#Si la mejor variedad no alcanza la restriccion de capital (se pueden usar otras)    
            y[i,t]<-sum(capital[which(capital[,1]==i & capital[,5]>0),3]*capital[which(capital[,1]==i & capital[,5]>0),4]*capital[which(capital[,1]==i & capital[,5]>0),7])      
            inv[i,t]<-inv[i,t-1]+y[i,t]
            if(y[i,t]>0 & sum(capital[which(capital[,1]==i & capital[,5]>0),3]*capital[which(capital[,1]==i & capital[,5]>0),4]) > 0){
              muc[i,t]<-y[i,t]/sum(capital[which(capital[,1]==i & capital[,5]>0),3]*capital[which(capital[,1]==i & capital[,5]>0),4])
            }else{muc[i,t]<-0}
            ux[i,t]<-sum(capital[which(capital[,1]==i & capital[,5]>0),4]*capital[which(capital[,1]==i & capital[,5]>0),3])/sum(capital[which(capital[,1]==i & capital[,5]>0),3])    
            
            
          }
      }
      inv[i,t]<-inv[i,t-1]+y[i,t]  
    }
    
    
    
    
    ufic[t]<-sum(capital[which(capital[,5]>0),3]*capital[which(capital[,5]>0),4])/sum(capital[which(capital[,5]>0),3])
    mufic[t]<-sum(muc[which(x[,2]=="c"),t]*y[which(x[,2]=="c"),t])/sum(y[which(x[,2]=="c"),t])
    #10 R&D Activity----
    
    for (i in (fic+1):(fic+fik)){
      
      #innovacion
      
      Prinn[i,t]<-1-exp(-xiinn[i,t]*Nr[i,t])
      
      if (sample(c(1,0),1,replace = TRUE,prob = c(Prinn[i,t],1-Prinn[i,t]))==1){
        #Proceso de innovacion exitoso se crea una nueva variedad de capital 
        
        #Creo la nueva variedad
        capital<-rbind(capital,c(i,0,0,0,0,0,capital[which(capital[,1]==i & capital[,2]==1),7]*(1+abs(rnorm(1,mean = muFN3,sigmaFN3))),kapa0))
        
        Nuevavariedad[i-fic,t]<-1 #matriz que indica con un 1 el periodo t en que la firma i logro una innovacion
        
        
      }else{"innovacion sin exito"}
      
      #OJO! se supone que el inventario de las firmas de capital se actualiza a la nueva variedad
      
      
      #imitacion
      
      Primi[i,t]<-(1-exp(-xiimi[i,t]*Nr[i,t]))
      
      if (sample(c(1,0),1,replace = TRUE,prob = c(Primi[i,t],1-Primi[i,t]))==1){
        aimitar<-sample(c((fic+1):(fic+fik))[-i],sample(c(1:(fik-1)),1),replace = FALSE)
        varaimi<-vector()
        for (j in aimitar) {
          varaimi<-c(varaimi,capital[which(capital[,1]==j & capital[,2]==1),7])
        }
        varimi<-max(varaimi)
        varactual<-max(capital[which(capital[,1]==i),7])
        if (varactual>=varimi){"no imita, ya produce una variedad equivalente o superior"}else{
          
          #Creo la nueva variedad imitada  
          if(length(which(capital[,1]==i & capital[,2]==0))>0){
            capital[which(capital[,1]==i & capital[,2]==0),]<-c(i,0,0,0,0,0,varimi,kapa0)
          }else{
            capital<-rbind(capital,c(i,0,0,0,0,0,varimi,kapa0))
          }
          Imitacion[i-fic,t]<-1 #matriz que indica con un 1 el periodo t en que la firma i logro una imitacion
          
        }
      }else{"R&D imitacion no dio resultado"}
      
    }
    
    for (i in (fic+1):(fic+fik)) {
      
      #Transformo todo el stock a la nueva variedad
      if(length(which(capital[,1]==i & capital[,2]==0))>0){
        capital[which(capital[,1]==i & capital[,2]==1),]<-c(i,1,sum(capital[which(capital[,1]==i),3]),0,0,px[i,t],capital[which(capital[,1]==i & capital[,2]==0),7],kapa0)
        capital<-capital[-which(capital[,1]==i & capital[,2]==0),]
      }
    }
    
    #11 Capital goods market (2)----
    #El intercambio ya esta hecho en la matriz capital. Ahora introduzco los pagos
    
    for (i in sample(1:fic,fic,replace = FALSE)){
      if(sum(apagar[i,])>0){
        acreedork<-which(apagar[i,]>0)
        if(disponible[i]+deposits[which(deposits[,1]==i),3]>=sum(apagar[i,])){
          if(disponible[i]>=sum(apagar[i,])){
            disponible[i]<-round(disponible[i] - sum(apagar[i,]),digits = 4)
          }else{
            deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3] + disponible[i] - sum(apagar[i,]),digits = 4)
            disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]] + disponible[i] - sum(apagar[i,])
            disponible[i]<-0
          }
          disponible[fic+acreedork]<-disponible[fic+acreedork]+apagar[i,acreedork]
          inversion[i,t]<-sum(apagar[i,which(apagar[i,]>0)])
          apagar[i,]<-0
          apagarp[i,]<-0
          apagarv[i,]<-0  
          apagara[i,]<-0
        }else{
          #No compra todo lo que pidio. Cancela una parte
          acancelar<-(sum(apagar[i,])-disponible[i]-deposits[which(deposits[,1]==i),3])/sum(apagar[i,])
          
          #NO CONSIDERO QUE DEFAULTEA. CANCELA EL PEDIDO
          #se desarma la venta de fik cancelada
          for (k in acreedork) {
            cocanc[i,t]<-cocanc[i,t]+(apagar[i,k]/apagarp[i,k])*acancelar
            vecanc[k,t]<-vecanc[k,t]+(apagar[i,k]/apagarp[i,k])*acancelar
            s[fic+k,t]<-s[fic+k,t]-round((apagar[i,k]/apagarp[i,k])*acancelar,digits = 4)
            #Recompongo el stock de la fik
            capital[which(capital[,1]==fic+k),3]<-round(capital[which(capital[,1]==fic+k),3]+(apagar[i,k]/apagarp[i,k])*acancelar,digits = 4)
            inv[fic+k,t]<-round(inv[fic+k,t]+(apagar[i,k]/apagarp[i,k])*acancelar,digits = 4)
            #Quito el pedido de capital de fic
            capital[which(capital[,1]==i & capital[,5]==0 & capital[,7]==apagarv[i,k]),3]<-round(capital[which(capital[,1]==i & capital[,5]==0 & capital[,7]==apagarv[i,k]),3]-(apagar[i,k]/apagarp[i,k])*acancelar,digits = 4)
          }
          #Pago los pedidos que mantuve
          if(disponible[i]>=sum(apagar[i,])*(1-acancelar)){
            disponible[i]<-round(disponible[i] - sum(apagar[i,])*(1-acancelar),digits = 4)
          }else{
            deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3] + disponible[i] - sum(apagar[i,])*(1-acancelar),digits = 4)
            disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]] + disponible[i] - sum(apagar[i,])*(1-acancelar)
            disponible[i]<-0
          }
          disponible[fic+acreedork]<-round(disponible[fic+acreedork]+apagar[i,acreedork]*(1-acancelar),digits = 4)
          inversion[i,t]<-sum(apagar[i,which(apagar[i,]>0)])*(1-acancelar)
          apagar[i,]<-0
          apagarp[i,]<-0
          apagarv[i,]<-0  
          apagara[i,]<-0
        }
      }
    }
    
    
    
    #12 Consumption goods market----
    
    ofcons<-inv[c(1:fic),t]
    
    #NIh en este periodo es lo pagado en el periodo anterior
    #Sh arranca igual a NIh antes de consumir
    
    #consumo deseado
    for (h in sample(1:(nw+no+nr+nm),(nw+no+nr+nm),replace = FALSE)) {
      peh[h,t]<-peh[h,t-1]  + lambda * (mean(constrans1[which(constrans1[,1]==h),4]) - peh[h,t-1])
      
      if (x[fic+fik+fib+h,2]=="w"){alfa<-alfaw}else{
        if (x[fic+fik+fib+h,2]=="m"){alfa<-alfam}else{
          alfa<-alfaor
        }
      }
      
      
      cdh[h,t]<-max(alfa * NIh[h,t] / peh[h,t],beta * if (ch[h,t-1]>0){ch[h,t-1]}else{cdh[h,t-1]})
      
      #Matching
      
      #provold<-proveedor en t-1
      provold<-consmarket[h,t-1]
      
      #precio de provold
      pold[h,t]<-px[provold,t]
      #determino la oferta visible para h
      oferentes<-sample(if(length(which(ofcons[]<=0))>0){
        if (length(c(1:fic)[-which(ofcons[]<=0)])==1){
          c(c(1:fic)[-which(ofcons[]<=0)],c(1:fic)[-which(ofcons[]<=0)])
        }else{
          c(1:fic)[-which(ofcons[]<=0)]
        }
      }else{c(1:fic)}, min(chic,if(length(which(ofcons[]<=0))>0){length(c(1:fic)[-which(ofcons[]<=0)])}else{length(c(1:fic))}), replace = FALSE)
      oferentes<-cbind(oferentes,px[oferentes,t],ofcons[oferentes])
      
      falta<-cdh[h,t]
      
      #ahora h elige proveedor para completar su inversion deseada
      #    print(12.1)
      while (falta > 0.0001 & (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3])>0.0001 & sum(ofcons[c(oferentes[,1],provold)]) >0.0001){ 
        #proveedor candidato a sustituir al anterior
        provnew<-as.numeric(oferentes[which.min(oferentes[which(ofcons[oferentes[,1]]> 0),2]),1])
        #precio del mejor oferente
        if (length(provnew)>0){pnew[h,t]<-px[provnew,t]}
        
        if (pnew[h,t]<pold[h,t] & pnew[h,t] > 0){Prsc[h,t]<-1-exp(epsilonc*(pnew[h,t]-pold[h,t])/pnew[h,t])}else{Prsc[h,t]<-0}
        #Le pongo el >0 porque si oferentes no tiene stock voy acumulando resultados "integrer(0)" y en ese caso va con provold
        
        #AHORA TENGO QUE ASIGNAR EL PROVEEDOR DE CADA HOGAR ATENDIENDO ESTA PROBABILIDAD
        if (ofcons[provold]>0){
          if (length(provnew)>0){if(ofcons[provnew]>0){
            consmarket[h,t]<-sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsc[h,t],Prsc[h,t]))
          }else{consmarket[h,t]<-provold}
          }else{consmarket[h,t]<-provold}
        }else{consmarket[h,t]<-provnew}
        
        #Concreto el matching
        transado<-min(falta,(disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3])/px[consmarket[h,t],t],ofcons[consmarket[h,t]])
        #h consume
        ch[h,t]<-ch[h,t]+transado
        #gasta
        Sh[h,t]<-Sh[h,t]-transado*px[consmarket[h,t],t]
        if (disponible[fic+fik+fib+h] - transado*px[consmarket[h,t],t] > 0.0001){
          disponible[fic+fik+fib+h]<- disponible[fic+fik+fib+h] - transado*px[consmarket[h,t],t]
        }else{if(deposits[which(deposits[,1]==fic+fik+h),3] + disponible[fic+fik+fib+h]  - transado * px[consmarket[h,t],t] > 0.0001){
          deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]  - transado * px[consmarket[h,t],t] + disponible[fic+fik+fib+h],digits = 4)
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]  - transado * px[consmarket[h,t],t] + disponible[fic+fik+fib+h]
          disponible[fic+fik+fib+h]<-0
        }else{
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+h),3])
          deposits[which(deposits[,1]==fic+fik+h),3]<- 0
          disponible[fic+fik+fib+h]<-0
        }
        }
        #la firma vende
        s[consmarket[h,t],t]<-s[consmarket[h,t],t] + transado
        disponible[consmarket[h,t]]<-disponible[consmarket[h,t]] + transado*px[consmarket[h,t],t]
        #se registra la interaccion
        constrans<-rbind(constrans,c(h,consmarket[h,t],transado,px[consmarket[h,t],t]))
        if (constrans[h,1]<0){stop("se quemo todo")}
        #Quito el stock de la fic
        ofcons[consmarket[h,t]]<-ofcons[consmarket[h,t]]-transado
        inv[consmarket[h,t],t]<-inv[consmarket[h,t],t]-transado
        #satisfazgo
        falta<-falta-transado
        #Saco al oferente de la lista porque si no, no termina el while, se quedo sin oferta
        if (dim(oferentes)[1]>1){
          oferentes<-matrix(data=oferentes[-which.min(oferentes[,2]),],ncol=3)
        }   
      }
      
      
      #si no son necesarias, saco las filas que cree al principio, cuando hice la matriz
      if(length(which(constrans[,1]==h & constrans[,3]>0))>0 & length(which(constrans[,1]==h & constrans[,3]==0))>0){constrans<-as.matrix(constrans[-which(constrans[,1]==h & constrans[,3]==0),])
      if (constrans[h,1]<0){stop("se quemo todo eh!")}
      }
    }  
    
    #13 Interest, bonds and loans repayment----
    
    #Actualizo riqueza de los manager
    for (h in 1:nm) {
      wh[nw+no+nr+h,t]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
    }
    
    
    #las firmas pagan intereses sobre los prestamos y amortizan una proporcion cte del principal
    #si tiene fondos la firma paga y el banco cobra    
    for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
      #Actualizo acciones
      acciones[i,t]<-sum(wh[which(trabajom[,t]==i)+nw+no+nr,t])
      if(length(which(loans[,1]==i & loans[,6]<=t))>0){
        cuota<-sum(loans[which(loans[,1]==i & loans[,6]<=t),3]*(1/loans[which(loans[,1]==i & loans[,6]<=t),5]+loans[which(loans[,1]==i & loans[,6]<=t),4]*(1-(t-loans[which(loans[,1]==i & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,6]<=t),5])))
        if (disponible[i]+deposits[which(deposits[,1]==i),3]>=cuota){
          for (b in 1:fib){
            if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]<=t))>0){
              disponible[fic+fik+b]<-disponible[fic+fik+b]+round(sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5]+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4]*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5]))),digits = 4)
            }
    
          }
          if (disponible[i]>=cuota){
            disponible[i]<-disponible[i]-cuota
          }else{
            deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-cuota,digits = 4)
            disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-cuota
            disponible[i]<-0}
        }else{
          #caso de default
          default[i,t]<-1
          #Si le dieron un prestamo este anio lo considero todo perdida
          #El capital que compro este anio, si considero que lo puede vender
          if (x[i,2]=="c"){
            resultado17<-vector() 
            for (j in 1:max(capital[which(capital[,1]==i),2])){
              resultado17<-c(resultado17,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
            }
            kreal[i,t]<-sum(resultado17)
            #de todo lo que debe, se va a poder recuperar esta proporcion
            if(deudax[i,t]==0){deltax[i,t]<-1}else{
              deltax[i,t]<-min(1,kreal[i,t]*(1-iota)/deudax[i,t])
            }
            if(deltax[i,t]>0.0001){
              #los manager compran el capital
              for (h in which(trabajom[,t]==i)){
                if(wh[nw+no+nr+h,t]>0.0001 & acciones[i,t]>0.0001){
                  parte<-round(wh[nw+no+nr+h,t]/acciones[i,t],digits = 4)
                  if (disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]>=deltax[i,t]*deudax[i,t]*parte){
                    aportec[h,i]<-deltax[i,t]*deudax[i,t]*parte
                    #el banco cobra
                    for (b in 1:fib){
                      if(length(which(loans[,1]==i & loans[,2]==b))>0){
                        disponible[fic+fik+b]<-disponible[fic+fik+b]+round(parte*deltax[i,t]*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                        recupero[b,t]<-recupero[b,t]+round(parte*deltax[i,t]*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                        perdida[b,t]<-perdida[b,t]+round((1-parte*deltax[i,t])*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                      }
                      
                    }
                    if (disponible[fic+fik+fib+nw+no+nr+h]>=deltax[i,t]*deudax[i,t]*parte){
                      disponible[fic+fik+fib+nw+no+nr+h]<-disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,t]*deudax[i,t]*parte
                    }else{
                      deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]<-round(deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]+disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,t]*deudax[i,t]*parte,digits = 4)
                      disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]+disponible[fic+fik+fib+nw+no+nr+h]-deltax[i,t]*deudax[i,t]*parte
                      disponible[fic+fik+fib+nw+no+nr+h]<-0
                    }
                  }else{
                    #el manager paga con todo su capital
                    puede_pagar<-sum(disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
                    aportec[h,i]<-puede_pagar
                    prop_a_cobrar<-as.numeric(puede_pagar/(deltax[i,t]*deudax[i,t]*parte))
                    for (b in 1:fib){
                      if(length(which(loans[,1]==i & loans[,2]==b))>0){
                        disponible[fic+fik+b]<-disponible[fic+fik+b]+round(prop_a_cobrar*parte*deltax[i,t]*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                        recupero[b,t]<-recupero[b,t]+round(prop_a_cobrar*parte*deltax[i,t]*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                        perdida[b,t]<-perdida[b,t]+round((1-prop_a_cobrar*parte*deltax[i,t])*sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
                      }
                      
                    }
                    disponible[fic+fik+fib+nw+no+nr+h]<-0
                    disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]<-round(disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]),digits = 4)
                    deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]<-0
                  }
                }
              }
            }
            for (b in 1:fib) {
              if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1))>0){
                perdida[b,t]<-perdida[b,t]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1),3])
                loandefhoy[i,t]<-loandefhoy[i,t]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1),3])
              }
            }
          }else{
            for (b in 1:fib) {
              if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]<=t))>0){
                perdida[b,t]<-perdida[b,t]+round(sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),3]*(1+loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),4])*(1-(t-loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),6])/loans[which(loans[,1]==i & loans[,2]==b & loans[,6]<=t),5])),digits = 4)
              }
            }
            
            for (b in 1:fib) {
              if(length(which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1))>0){
                perdida[b,t]<-perdida[b,t]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1),3])
                loandefhoy[i,t]<-loandefhoy[i,t]+sum(loans[which(loans[,1]==i & loans[,2]==b & loans[,6]==t+1),3])
              }
            }
          }
          #se cancela el credito contra lo recibido y el resto perdidas   
          if(length(which(loans[,1]==i))>0){
            loans<-as.matrix(loans[-which(loans[,1]==i),])
          }
        }
      }
    }
    
    aportectotal[t]<-sum(aportec)    
    
    
    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
    
    #el gobierno paga los bonos e intereses
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-(1+itechob[t-1])*sum(bonos[,(t-1)])
    for (b in 1:fib){
      disponible[fic+fik+b]<-round(disponible[fic+fik+b]+bonos[b,t-1]*(1+itechob[t-1]),digits = 4)
      #BUSCANDO EL ERROR          
      #    for (A4 in c(1:(fic+fik+fib+POP))[-c(111:120)]) {
      #      if(disponible[A4]<0){stop("disponible negativo o infinito en A4")}
      #    }  
    }  
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]+bonos[fib+1,t-1]*(1+itechob[t-1])
    
    
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###   
    
    #los bancos pagan intereses sobre depositos y avances del banco central con intereses
    for (b in 1:fib){
      disponible[fic+fik+b]<-disponible[fic+fik+b]-round(intdepap[b,t-1]+CAcb[b,t-1]*(1+itechocb[t-1]),digits = 4)
    }
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-round(disponible[fic+fik+fib+nw+no+nr+nm+2]+sum(CAcb[,t-1])*(1+itechocb[t-1]),digits = 4)
    
    for (i in 1:(fic+fik)){
      disponible[i]<-round(disponible[i]+intdep[i,t-1],digits = 4)
    }
    for (h in (fic+fik+1):(fic+fik+nw+no+nr+nm)){
      disponible[h+fib]<-round(disponible[h+fib]+intdep[h,t-1],digits = 4)
    }
    
    
    #14 Wage and dole----
    
    
    #Actualizo riqueza  
    for (h in 1:nm) {
      wh[nw+no+nr+h,t]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
    }
    
    #Actualizo salarios acordados
    wa[,t]<-w[,t]  
    
    #SI LA FIRMA DEFAULTEA SE PAGA LO QUE HAYA EN CAJA
    #SOLO SE RECAPITALIZA EN CASO QUE NO CUMPLA LA CONDICIoN DE ENTRADA
    #Si defaulteo, tengo que revisar w[i,t]  
    for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
      #actualizo acciones
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
      sueldos<-sum(w[which(trabajow[,t]==i),t],w[nw+which(trabajoo[,t]==i),t],w[nw+no+which(trabajor[,t]==i),t],w[nw+no+nr+which(trabajom[,t]==i),t])
      if(Nx[i,t]>0){wn[i,t]<-sueldos/Nx[i,t]}else{wn[i,t]<-0}
      if(disponible[i]+deposits[which(deposits[,1]==i),3]<sueldos){
        #default      
        default[i,t]<-1
        
        prop_sueldo_a_cobrar<-max(min(1,(disponible[i]+deposits[which(deposits[,1]==i),3])/sueldos),0)
        wnpago[i,t]<-wn[i,t]*prop_sueldo_a_cobrar
        for (h in which(trabajow[,t]==i)){
          w[h,t]<-round(w[h,t]*prop_sueldo_a_cobrar,digits = 4)
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        for (h in c(which(trabajoo[,t]==i)+nw)){
          w[h,t]<-round(w[h,t]*prop_sueldo_a_cobrar,digits = 4)
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]
        }
        for (h in c(which(trabajor[,t]==i)+nw+no)){
          w[h,t]<-round(w[h,t]*prop_sueldo_a_cobrar,digits = 4)
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        for (h in c(which(trabajom[,t]==i)+nw+no+nr)){
          w[h,t]<-round(w[h,t]*prop_sueldo_a_cobrar,digits = 4)
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]-max(0,deposits[which(deposits[,1]==i),3])
        deposits[which(deposits[,1]==i),3]<-0
        disponible[i]<-0
        
        
      }else{
        wnpago[i,t]<-wn[i,t]
        for (h in which(trabajow[,t]==i)){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        for (h in c(which(trabajoo[,t]==i)+nw)){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        for (h in c(which(trabajor[,t]==i)+nw+no)){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        for (h in c(which(trabajom[,t]==i)+nw+no+nr)){
          disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
        }
        if(disponible[i]>=sueldos){
          disponible[i]<-disponible[i]-sueldos
        }else{
          deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-sueldos,digits = 4)
          disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-sueldos
          disponible[i]<-0
        }
      }
    }
    #sueldos funcionarios publicos
    
    sueldosg[t]<-sum(w[which(trabajow[,t]==999),t],w[nw+which(trabajoo[,t]==999),t],w[nw+no+which(trabajor[,t]==999),t],w[nw+no+nr+which(trabajom[,t]==999),t])
    wn[fic+fik+1,t]<-sueldosg[t]/Nx[fic+fik+1,t]
    wnpago[fic+fik+1,t]<-wn[fic+fik+1,t]
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-sueldosg[t]
    for (h in which(trabajow[,t]==999)){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
    }
    for (h in c(which(trabajoo[,t]==999)+nw)){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
    }
    for (h in c(which(trabajor[,t]==999)+nw+no)){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
    }
    for (h in c(which(trabajom[,t]==999)+nw+no+nr)){
      disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+w[h,t]  
    }
    
    
    
    
    ##dole
    
    
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]-sum(uh[1:nw,t])*omega*mean(wa[which(uh[(1:nw),t]==0),t])-sum(uh[(nw+1):(nw+no+nr),t])*omega*mean(wa[nw+which(uh[(nw+1):(nw+no+nr),t]==0),t])-sum(uh[(nw+no+nr+1):(nw+no+nr+nm),t])*omega*mean(wa[nw+no+nr+which(uh[(nw+nr+no+1):(nw+nr+no+nm),t]==0),t])
    
    for (h in 1:nw){
      if(uh[h,t]==1){
        d[h,t]<-omega*mean(wa[which(uh[(1:nw),t]==0),t])
        disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,t]
      }
    }
    
    for (h in (nw+1):(nw+no+nr)){
      if(uh[h,t]==1){
        d[h,t]<-omega*mean(wa[nw+which(uh[(nw+1):(nw+no+nr),t]==0),t])
        disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,t]
      }
    }
    
    
    for (h in (nw+no+nr+1):(nw+no+nr+nm)){
      if(uh[h,t]==1){
        d[h,t]<-omega*mean(wa[nw+no+nr+which(uh[(nw+no+nr+1):(nw+no+nr+nm),t]==0),t])
        disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]+d[h,t]
      }
    }    
    
    
    
    
    #Capital accionario
    for (i in 1:(fic+fik)) {
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
    }    
    
    
    #15 Dividends----
    
    #Actualizo riqueza  
    for (h in 1:nm) {
      wh[nw+no+nr+h,t]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
    }  
    
    #antes de repartir dividendos, se establecen las tasas impositivas.
    
    #indice de precios  
    ipc[t]<-(sum(px[which(x[,2]=="c"),t]*s[which(x[,2]=="c"),t])/sum(s[which(x[,2]=="c"),t]))/(sum(px[which(x[,2]=="c"),1]*s[which(x[,2]=="c"),1])/sum(s[which(x[,2]=="c"),1]))
    ipg[t]<-(sum(px[,t]*s[,t])/sum(s[,t]))/(sum(px[,1]*s[,1])/sum(s[,1]))  
    ipeh[t]<-(sum(peh[,t]*NIh[,t])/sum(NIh[,t]))/(sum(peh[,1]*NIh[,1])/sum(NIh[,1]))
    
    gdp[t]<-sum(y[,t]*px[,1])
    ngdp[t]<-sum(y[,t]*px[,t])
    igdp[t]<-sum(y[,t]*px[,t])/sum(y[,t]*px[,1])
    
    
    #Atento que en t=1 no hay t-2  
    #revision de tasas
    if(t==2){rev[t]=1}else{
      if (sum(bonos[,t-1])/ngdp[t-1]-sum(bonos[,t-2])/ngdp[t-2]>0){
        rev[t]<-rev[t-1]+v
      }else{
        if(dfg[t-1]/ngdp[t-1]<(-def1)){
          rev[t]<-rev[t-1]+v
        }else{
          if (dfg[t-1]/ngdp[t-1]>-def0 & sum(bonos[,t-1])/ngdp[t-1]-sum(bonos[,t-2])/ngdp[t-2]<=0){
            rev[t]<-rev[t-1]-v
          }else{
            rev[t]<-rev[t-1]
          }
        }
      }
    }
    #tasas
    taopi[t]<-taopi0*rev[t]  
    taoi1[t]<-taoi10*rev[t]
    taoi2[t]<-taoi20*rev[t]
    taow[t]<-taow0*rev[t]
    #inflacion
    pi[t]<-(sum(px[,t]*s[,t])/sum(s[,t]))/(sum(px[,t-1]*s[,t-1])/sum(s[,t-1]))-1
    #inflacion minorista
    pic[t]<-(sum(px[which(x[,2]=="c"),t]*s[which(x[,2]=="c"),t])/sum(s[which(x[,2]=="c"),t]))/(sum(px[which(x[,2]=="c"),t-1]*s[which(x[,2]=="c"),t-1])/sum(s[which(x[,2]=="c"),t-1]))-1
    
    
    #umbral del impuesto al ingreso
    umbral[t]<-umbral[t-1]*(1+pic[t])
    
    #inflacion mayorista  
    pik[t]<-(sum(px[which(x[,2]=="k"),t]*s[which(x[,2]=="k"),t])/sum(s[which(x[,2]=="k"),t]))/(sum(px[which(x[,2]=="k"),t-1]*s[which(x[,2]=="k"),t-1])/sum(s[which(x[,2]=="k"),t-1]))-1
    
    #mark-up sobre produccion
    mupc[t]<-sum(mu[which(x[,2]=="c"),t]*y[which(x[,2]=="c"),t])/sum(y[which(x[,2]=="c"),t])
    mupk[t]<-sum(mu[which(x[,2]=="k"),t]*y[which(x[,2]=="k"),t])/sum(y[which(x[,2]=="k"),t])
    mup[t]<-sum(mu[,t]*y[,t])/sum(y[,t])
    #mark-up sobre ventas
    mupsc[t]<-sum(mu[which(x[,2]=="c"),t]*s[which(x[,2]=="c"),t])/sum(s[which(x[,2]=="c"),t])
    mupsk[t]<-sum(mu[which(x[,2]=="k"),t]*s[which(x[,2]=="k"),t])/sum(s[which(x[,2]=="k"),t])
    mups[t]<-sum(mu[,t]*s[,t])/sum(s[,t])
    #Resultado de las firmas
    
    for (i in 1:fic) {
      uc[i,t]<-if (muc[i,t]==0 | wn[i,t]==0){
        uc[i,t-1]
      }else{
        wn[i,t]/(muc[i,t]*lk*sharecw)
      }
      
      pix[i,t]<-sum(s[i,t]*px[i,t]+intdep[i,t-1]+inv[i,t]*uc[i,t]-inv[i,t-1]*uc[i,t-1]-wnpago[i,t]*Nx[i,t],if(length(which(loans[,1]==i & loans[,6] <= t))>0){-sum(loans[which(loans[,1]==i & loans[,6] <= t),3]*loans[which(loans[,1]==i & loans[,6] <= t),4]*(1-(t-loans[which(loans[,1]==i & loans[,6] <= t),6])/loans[which(loans[,1]==i & loans[,6] <= t),5]))},if(length(which(capital[,1]==i & capital[,5]!=0))>0){-sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]/capital[which(capital[,1]==i & capital[,5]!=0),8])})
    }
    for (i in (fic+1):(fic+fik)) {
      inv[i,t]<-sum(capital[which(capital[,1]==i),3])
      uc[i,t]<-if (wn[i,t] == 0){
        uc[i,t]<-uc[i,t-1]
      }else{
        wn[i,t]/(mun[i,t]*sharekw)
      }
      pix[i,t]<-sum(s[i,t]*px[i,t]+intdep[i,t-1]+inv[i,t]*uc[i,t]-inv[i,t-1]*uc[i,t-1]-wnpago[i,t]*Nx[i,t],if(length(which(loans[,1]==i & loans[,6] <= t))>0){-sum(loans[which(loans[,1]==i & loans[,6] <= t),3]*loans[which(loans[,1]==i & loans[,6] <= t),4]*(1-(t-loans[which(loans[,1]==i & loans[,6] <= t),6])/loans[which(loans[,1]==i & loans[,6] <= t),5]))})
    }
    for (b in 1:fib) {#REVISAR-NO ESTOY TENIENDO EN CUENTA LOS NUEVOS CReDITOS Y DEPoSITOS, SOLO LOS INTERESES (ver manejo del tiempo)
      pix[fic+fik+b,t]<-sum(if(length(which(loans[,2]==b & loans[,6] <= t))>0){sum(loans[which(loans[,2]==b & loans[,6] <= t),3]*loans[which(loans[,2]==b & loans[,6] <= t),4]*(1-(t-loans[which(loans[,2]==b & loans[,6] <= t),6])/loans[which(loans[,2]==b & loans[,6] <= t),5]))},bonos[b,t-1]*itechob[t-1]-intdepap[b,t-1]-CAcb[b,t-1]*itechocb[t-1]-perdida[b,t])
    }
    
    
    
    
    
    #Se establecen las condiciones de entradas para el caso en que una firma se funda y surja otra
    
    #Valor neto promedio del sector de consumo  
    for (i in 1:fic) {
      resultado19<-vector()
      for (j in 1:max(capital[which(capital[,1]==i),2])){
        resultado19<-c(resultado19,capital[which(capital[,1]==i & capital[,2]==j),6]*capital[which(capital[,1]==i & capital[,2]==j),3]*(1-capital[which(capital[,1]==i & capital[,2]==j),5]/capital[which(capital[,1]==i & capital[,2]==j),8]))
      }
      kreal[i,t]<-sum(resultado19)
      WNi[i,t]<-sum(inv[i,t]*uc[i,t],kreal[i,t],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
    }
    WNc[t]<-sum(WNi[1:fic,t])/fic
    
    #Valor neto promedio del sector de capital
    for (i in (fic+1):(fic+fik)) {
      WNi[i,t]<-sum(inv[i,t]*uc[i,t],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
    }
    WNk[t]<-sum(WNi[(fic+1):(fic+fik),t])/fik
    
    
    
    
    
    #Se establecen los dividendos a repartir_firmas de consumo
    
    for (i in sample(1:fic,fic,replace = FALSE)){
      Divx[i,t]<-max(0,roc[t]*pix[i,t]*(1-taopi[t]))
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
      if (default[i,t]==1){Divx[i,t]<-0
      #Si no cumple la condicion de entrada, dividendos negativos (hay que recapitalizar)  
      if(WNi[i,t]<0.25*WNc[t]){
        acapitalizar[i,t]<-0.25*WNc[t]-WNi[i,t]
        for (h in 1:nm) {
          if (trabajom[h,t]==i & wh[nw+no+nr+h,t]>0.0001 & acciones[i,t]>0.0001){
            aporte[h,i]<-min(acapitalizar[i,t]*wh[(nw+no+nr+h),t]/acciones[i,t],disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
          }else{
            aporte[h,i]<-0
          }
        }
        capitalizacion[i,t]<-sum(aporte[,i])
        disponible[i]<-disponible[i]+capitalizacion[i,t]
      }
      }
      #Si no tiene managers, no reparte
      if (length(which(trabajom[,t]==i))==0){Divx[i,t]=0}
      #Si hay para repartir se reparten        
      if (Divx[i,t]>0){if (disponible[i]-Divx[i,t]>0.0001){
        disponible[i]<-disponible[i]-Divx[i,t]
      }else{if (disponible[i]+deposits[which(deposits[,1]==i),3]-Divx[i,t]>0.0001){
        deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-Divx[i,t],digits = 4)
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-Divx[i,t]
        disponible[i]<-0
      }else{
        Divx[i,t]<-0 #Podria repartir todo lo que tiene la firma, prefiero este criterio conservador
      }
      }
      }
    }
    
    #Se establecen los dividendos a repartir_firmas de capital  
    for (i in sample((fic+1):(fic+fik),fik,replace = FALSE)) {
      Divx[i,t]<-max(0,rok[t]*pix[i,t]*(1-taopi[t]))
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
      if (default[i,t]==1){Divx[i,t]<-0
      #Si no cumple la condicion de entrada, dividendos negativos (hay que recapitalizar)  
      if(WNi[i,t]<0.25*WNk[t]){
        acapitalizar[i,t]<-0.25*WNk[t]-WNi[i,t]
        for (h in 1:nm) {
          if (trabajom[h,t]==i & wh[nw+no+nr+h,t]>0.0001 & acciones[i,t]>0.0001){
            aporte[h,i]<-min(acapitalizar[i,t]*wh[(nw+no+nr+h),t]/acciones[i,t],disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
          }else{
            aporte[h,i]<-0
          }
        } 
        capitalizacion[i,t]<-sum(aporte[,i])
        disponible[i]<-disponible[i]+capitalizacion[i,t]
      }
      }
      #Si no tiene managers, no reparte
      if (length(which(trabajom[,t]==i))==0){Divx[i,t]=0}
      #Si hay para repartir se reparten        
      if (Divx[i,t]>0.0001){if (disponible[i]>Divx[i,t]){
        disponible[i]<-disponible[i]-Divx[i,t]
      }else{if (disponible[i]+deposits[which(deposits[,1]==i),3]>=Divx[i,t]){
        deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-Divx[i,t],digits = 4)
        disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-Divx[i,t]
        disponible[i]<-0
      }else{
        Divx[i,t]<-0 #Podria repartir todo lo que tiene la firma, prefiero este criterio conservador
      }
      }
      }
    }
    
    
    
    
    
    #Se reparten los dividendos de las firmas o se recapitaliza, segun corresponda  
    for (h in (nw+no+nr+1):(nw+no+nr+nm)){
      Divh[h,t]<-if(trabajom[h-(nw+no+nr),t]!=0 & trabajom[h-(nw+no+nr),t]!=999){
        if(Divx[trabajom[h-(nw+no+nr),t],t]>0.0001){
          if(acciones[trabajom[h-(nw+no+nr),t],t]>0){
            wh[h,t]*Divx[trabajom[h-(nw+no+nr),t],t]/acciones[trabajom[h-(nw+no+nr),t],t]
          }else{Divx[trabajom[h-(nw+no+nr),t],t]/length(which(trabajom[,t]==trabajom[h-(nw+no+nr),t]))}
        }else{-aporte[h-(nw+no+nr),trabajom[h-(nw+no+nr),t]]}
      }else{0}  
      #Actualizo riqueza  
      wh[h,t]<-disponible[fic+fik+fib+h]+max(0,deposits[which(deposits[,1]==fic+fik+h),3])+Divh[h,t]
    }
    
    
    
    
    
    
    #Se establecen los dividendos a repartir_bancos  
    acciones_totales[t]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),t])
    for (b in sample(1:fib,fib,replace=FALSE)) {
      Divx[fic+fik+b,t]<-max(0,rob[t]*pix[fic+fik+b,t]*(1-taopi[t]))
      if (default[fic+fik+b,t]==1){Divx[fic+fik+b,t]<-0}
      #si hay para repartir se reparte
      if (Divx[fic+fik+b,t]>0.0001){
        disponible[fic+fik+b]<-disponible[fic+fik+b]-Divx[fic+fik+b,t]
      }
      Ltot[b,t]<-sum(if(length(which(loans[,2]==b))>0){loans[which(loans[,2]==b),3]*(1-(t-loans[which(loans[,2]==b),6])/loans[which(loans[,2]==b),5])})
      NW[b,t]<-Ltot[b,t]+disponible[fic+fik+b]-sum(if(length(which(deposits[,2]==b))>0){deposits[which(deposits[,2]==b),3]})
      CR[b,t]<-if(Ltot[b,t]>0){NW[b,t]/Ltot[b,t]}else{1}
    }
    CR_promedio<-max(0.06,sum(NW[,t])/sum(Ltot[,t]))
    for (b in 1:fib) {
      if (CR[b,t]<0){
        default[fic+fik+b,t]<-1
        acapitalizar[fic+fik+b,t]<-(0.06-CR[b,t])*Ltot[b,t] #Le voy a exigir el capital minimo legal
        for (h in 1:nm) {
          wh[nw+no+nr+h,t]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])+Divh[nw+no+nr+h,t]-sum(aporte[h,(fic+fik+1):(fic+fik+b)])
          acciones_totales[t]<-sum(wh[(nw+no+nr+1):(nw+no+nr+nm),t])    
          if(wh[nw+no+nr+h,t]>0.0001){
            aporte[h,fic+fik+b]<-round(min(acapitalizar[fic+fik+b,t]*wh[(nw+no+nr+h),t]/acciones_totales[t],disponible[fic+fik+fib+nw+no+nr+h]+deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3]+Divh[nw+no+nr+h,t]-sum(aporte[h,(fic+fik+1):(fic+fik+b)])),digits = 4)
          }
        }
        capitalizacion[fic+fik+b,t]<-sum(aporte[,fic+fik+b])
        disponible[fic+fik+b]<-disponible[fic+fik+b]+capitalizacion[fic+fik+b,t]
      }
    }
    
    
    #Se reparten los dividendos de bancos o se recapitaliza, segun corresponda  
    for (h in (nw+no+nr+1):(nw+no+nr+nm)){
      Divh[h,t]<-Divh[h,t]+wh[h,t]*sum(Divx[(fic+fik+1):(fic+fik+fib),t])/acciones_totales[t]-sum(aporte[h-(nw+no+nr),(fic+fik+1):(fic+fik+fib)])
      
      
      #contabilizo ingresos  
      deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+Divh[h,t]+disponible[fic+fik+fib+h],digits = 4)
      disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+Divh[h,t]+disponible[fic+fik+fib+h]
      disponible[fic+fik+fib+h]<-0
    }

    #16 Taxes----

    #Actualizo riqueza  
    for (h in 1:nm) {
      wh[nw+no+nr+h,t]<-disponible[fic+fik+fib+nw+no+nr+h]+max(0,deposits[which(deposits[,1]==fic+fik+nw+no+nr+h),3])
    }  
    
    #Renta empresarial
    
    for (b in (fic+fik+1):(fic+fik+fib)){
      proftax[b,t]<-max(round(taopi[t]*pix[b,t],digits = 4),0)
      if (default[b,t]==1){proftax[b,t]<-0}
      disponible[b]<-disponible[b]-proftax[b,t]
    }
    
    for (i in sample(1:(fic+fik),(fic+fik),replace = FALSE)){
      acciones[i,t]<-sum(wh[nw+no+nr+which(trabajom[,t]==i),t])
      proftax[i,t]<-max(round(taopi[t]*pix[i,t],digits=4),0)
      if (default[i,t]==1){proftax[i,t]<-0}
      if (disponible[i]+deposits[which(deposits[,1]==i),3]>=proftax[i,t]){
        if (disponible[i]>=proftax[i,t]){
          disponible[i]<-disponible[i]-proftax[i,t]
        }else{
          deposits[which(deposits[,1]==i),3]<-round(deposits[which(deposits[,1]==i),3]+disponible[i]-proftax[i,t],digits = 4)
          disponible[fic+fik+deposits[which(deposits[,1]==i),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==i),2]]+disponible[i]-proftax[i,t]
          disponible[i]<-0
        }
      }else{
        default[i,t]=1
        proftax[i,t]<-0
      }
    }#Aca termina for fic+fik
    
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(proftax[,t])
    
    for (i in 1:fic) {
      OCF[i,t]<-sum(pix[i,t]-proftax[i,t],if(length(which(capital[,1]==i & capital[,5]!=0))>0){+sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]/capital[which(capital[,1]==i & capital[,5]!=0),8])},-(+inv[i,t]*uc[i,t]-inv[i,t-1]*uc[i,t-1]),if(length(which(loans[,1]==i & loans[,6] <= t))>0){-(sum(loans[which(loans[,1]==i & loans[,6] <= t),3]/loans[which(loans[,1]==i & loans[,6] <= t),5]))})
      if(length(c(which(capital[,1]==i & capital[,3]!=0 & capital[,5]!=0 & capital[,5] < capital[,8])))>0 & sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]*(1-capital[which(capital[,1]==i & capital[,5]!=0),5]/capital[which(capital[,1]==i & capital[,5]!=0),8]))!=0){
        r[i,t]<-OCF[i,t]/sum(capital[which(capital[,1]==i & capital[,5]!=0),3]*capital[which(capital[,1]==i & capital[,5]!=0),6]*(1-capital[which(capital[,1]==i & capital[,5]!=0),5]/capital[which(capital[,1]==i & capital[,5]!=0),8]))
      }else{r[i,t]<-rtecho}
    }
    
    for (i in (fic+1):(fic+fik)) {
      OCF[i,t]<-sum(pix[i,t]-proftax[i,t]-(+inv[i,t]*uc[i,t]-inv[i,t-1]*uc[i,t-1]),if(length(which(loans[,1]==i & loans[,6] <= t))>0){-(sum(loans[which(loans[,1]==i & loans[,6] <= t),3]/loans[which(loans[,1]==i & loans[,6] <= t),5]))})
    } 
    
    
    vk[t]<-sum(OCF[1:100,t])/sum(capital[which(capital[,1]<=100 & capital[,5]!=0),3]*capital[which(capital[,1]<=100 & capital[,5]!=0),6]*(1-capital[which(capital[,1]<=100 & capital[,5]!=0),5]/capital[which(capital[,1]<=100 & capital[,5]!=0),8]))
    
    
    
    #Impuesto al ingreso personal 
    
    for (h in 1:(nw+no+nr+nm)){
      ytax[h,t]<-sum(w[h,t]+intdep[fic+fik+h,t-1],Divh[h,t])
      inctax[h,t]<-taoi1[t]*min(umbral[t],max(0,ytax[h,t]))+taoi2[t]*max(0,ytax[h,t]-umbral[t])
      
      yh[h,t]<-ytax[h,t]+d[h,t]
      ygini[h,t]<-if(yh[h,t]<0){0}else{yh[h,t]}
      if (disponible[fic+fik+fib+h]>=inctax[h,t]){
        disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]-inctax[h,t]
      }else{
        if (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]>=inctax[h,t]){
          deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+disponible[fic+fik+fib+h]-inctax[h,t],digits = 4)
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+disponible[fic+fik+fib+h]-inctax[h,t]
          disponible[fic+fik+fib+h]<-0
        }else{
          #Voy a modificar inctax[i,t] a la baja, considerando lo que el Estado puede cobrar efectivamente
          inctax[h,t]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
          disponible[fic+fik+fib+h]<-0
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+h),3])
          deposits[which(deposits[,1]==fic+fik+h),3]<-0
        }
      }
      yait[h,t]<-if(ygini[h,t]-inctax[h,t]<0){0}else{ygini[h,t]-inctax[h,t]}
    }
    
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(inctax[,t])
    
    
    #Impuesto a la riqueza
    
    #actualizo riqueza
    
    for (h in 1:(nw+no+nr+nm)){
      wh[h,t]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
      wtax[h,t]<-taow[t]*wh[h,t]
      if (disponible[fic+fik+fib+h]>=wtax[h,t]){
        disponible[fic+fik+fib+h]<-disponible[fic+fik+fib+h]-wtax[h,t]
      }else{
        if (disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]>=wtax[h,t]){
          deposits[which(deposits[,1]==fic+fik+h),3]<-round(deposits[which(deposits[,1]==fic+fik+h),3]+disponible[fic+fik+fib+h]-wtax[h,t],digits = 4)
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]+disponible[fic+fik+fib+h]-wtax[h,t]
          disponible[fic+fik+fib+h]<-0
        }else{#Voy a modificar wtax[i,t] a la baja, considerando lo que el Estado puede cobrar efectivamente
          wtax[h,t]<-disponible[fic+fik+fib+h]+deposits[which(deposits[,1]==fic+fik+h),3]
          disponible[fic+fik+fib+h]<-0
          disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]<-disponible[fic+fik+deposits[which(deposits[,1]==fic+fik+h),2]]-max(0,deposits[which(deposits[,1]==fic+fik+h),3])
          deposits[which(deposits[,1]==fic+fik+h),3]<-0
        }
      } 
      yat[h,t]<-if(ygini[h,t]-inctax[h,t]-wtax[h,t]<0){0}else{ygini[h,t]-inctax[h,t]-wtax[h,t]}    
    }
    
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+sum(wtax[,t])
    
    #Gini
    Gini[,1]<-sort(yh[,t],decreasing = FALSE)
    Gini[,3]<-c(1:POP)*100/POP
    for (h in 1:POP) {
      Gini[h,2]<-sum(Gini[1:h,1])*100/sum(Gini[,1])
    }
    Gini[,4]<-Gini[,3]-Gini[,2]
    ig[t]<-sum(Gini[1:(POP-1),4])/sum(Gini[1:(POP-1),3])
    ig2[t]<-Gini(yh[,t])
    igat[t]<-Gini(yat[,t])
    igait[t]<-Gini(yait[,t])
    
    #resultado fiscal  
    piCB[t]<-bonos[fib+1,t-1]*itechob[t-1]+sum(CAcb[,t-1])*itechocb[t-1]
    dfg[t]<-sum(proftax[,t],inctax[,t],wtax[,t])+piCB[t]-sueldosg[t]-sum(d[,t])-sum(bonos[,t-1])*itechob[t-1]
    deltab[t]<--dfg[t]
    if (deltab[t]<0){deltab[t]<-max(deltab[t],-sum(bonos[,t-1]))}
    
    #actualizo balances  
    for (i in 1:fic) {
      WNi[i,t]<-sum(inv[i,t]*uc[i,t],kreal[i,t],disponible[i],deposits[which(deposits[,1]==i),3],if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
      activoi[i,t]<-sum(inv[i,t]*uc[i,t],kreal[i,t],disponible[i],deposits[which(deposits[,1]==i),3])
      pasivoi[i,t]<-sum(if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
    }
    for (i in (fic+1):(fic+fik)) {
      WNi[i,t]<-sum(inv[i,t]*uc[i,t],disponible[i],deposits[i,3],if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){-sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
      activoi[i,t]<-sum(inv[i,t]*uc[i,t],disponible[i],deposits[i,3])
      pasivoi[i,t]<-sum(if(length(which(loans[,1]==i & loans[,6]!=t+1))>0){sum(loans[which(loans[,1]==i & loans[,6]!=t+1),3]*(1-((t-loans[which(loans[,1]==i & loans[,6]!=t+1),6])/loans[which(loans[,1]==i & loans[,6]!=t+1),5])))},if(length(which(loans[,1]==i & loans[,6]==t+1))>0){sum(loans[which(loans[,1]==i & loans[,6]==t+1),3])})
    }
    
    levb[t]<-sum(Ltot[,t])/sum(NW[,t])
    pasc[t]<-sum(pasivoi[which(x[,2]=="c"),t])
    actc[t]<-sum(activoi[which(x[,2]=="c"),t])
    patc[t]<-sum(WNi[which(x[,2]=="c"),t])
    pask[t]<-sum(pasivoi[which(x[,2]=="k"),t])
    actk[t]<-sum(activoi[which(x[,2]=="k"),t])
    patk[t]<-sum(WNi[which(x[,2]=="k"),t])
    levcnw[t]<-pasc[t]/patc[t]
    levcad[t]<-actc[t]/patc[t]
    levknw[t]<-pask[t]/patk[t]
    levkad[t]<-actk[t]/patk[t]
    
    
    #17 Deposit market interaction----
    
    
    for (i in 1:(fic+fik)) {
      
      #Matching
      
      #provold<-proveedor en t-1
      provold<-dmarket[i,t-1]
      
      #tasa de provold
      idepold<-idb[provold,t]
      #determino la oferta visible para i
      oferentes<-sample(c(1:fib), chidep, replace = FALSE)
      oferentes<-cbind(oferentes,idb[oferentes,t])
      
      #ahora i elige el banco donde depositar
      #banco candidato a sustituir al anterior
      provnew<-as.numeric(oferentes[which.max(oferentes[,2]),1])
      #tasa del mejor oferente
      idepnew<-idb[provnew,t]
      
      if (idepnew>idepold){Prsdep[i,t]<-1-exp(epsilondep*(idepold-idepnew)/idepold)}else{Prsdep[i,t]<-0}
      
      #AHORA TENGO QUE ASIGNAR EL BANCO ATENDIENDO ESTA PROBABILIDAD
      dmarket[i,t]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsdep[i,t],Prsdep[i,t]))
      
      #Concreto el matching
      #retiro el deposito del periodo anterior
      disponible[fic+fik+provold]<-disponible[fic+fik+provold]-deposits[which(deposits[,1]==i),3]
      #deposito
      deposits[which(deposits[,1]==i),]<-c(i,dmarket[i,t],deposits[which(deposits[,1]==i),3]+disponible[i],idb[dmarket[i,t],t])
      disponible[fic+fik+dmarket[i,t]]<-disponible[fic+fik+dmarket[i,t]]+deposits[which(deposits[,1]==i),3]
      #satisfazgo
      disponible[i]<-0
      
      #queda determinado el interes de los depositos en este periodo
      intdep[i,t]<-round(deposits[which(deposits[,1]==i),3]*deposits[which(deposits[,1]==i),4],digits = 4)
      saldep[i,t]<-deposits[which(deposits[,1]==i),3]  
    }   
    
    for (h in (fic+fik+fib+1):(fic+fik+fib+nw+no+nr+nm)){
      #Matching
      
      #provold<-proveedor en t-1
      provold<-dmarket[h-fib,t-1]
      
      #tasa de provold
      idepold<-idb[provold,t]
      #determino la oferta visible para h
      oferentes<-sample(c(1:fib), chidep, replace = FALSE)
      oferentes<-cbind(oferentes,idb[oferentes,t])
      
      #ahora h elige el banco donde depositar
      #banco candidato a sustituir al anterior
      provnew<-as.numeric(oferentes[which.max(oferentes[,2]),1])
      #tasa del mejor oferente
      idepnew<-idb[provnew,t]
      
      if (idepnew>idepold){Prsdep[h-fib,t]<-1-exp(epsilondep*(idepold-idepnew)/idepold)}else{Prsdep[h-fib,t]<-0}
      
      #AHORA TENGO QUE ASIGNAR EL BANCO ATENDIENDO ESTA PROBABILIDAD
      dmarket[h-fib,t]<- sample(c(provold,provnew), 1, replace = FALSE, prob = c(1-Prsdep[h-fib,t],Prsdep[h-fib,t]))
      
      #Concreto el matching
      #retiro el deposito del periodo anterior
      disponible[fic+fik+provold]<-disponible[fic+fik+provold]-deposits[which(deposits[,1]==h-fib),3]
      #deposito
      deposits[which(deposits[,1]==h-fib),]<-c(h-fib,dmarket[h-fib,t],deposits[which(deposits[,1]==h-fib),3]+disponible[h],idb[dmarket[h-fib,t],t])
      disponible[fic+fik+dmarket[h-fib,t]]<-disponible[fic+fik+dmarket[h-fib,t]]+deposits[which(deposits[,1]==h-fib),3]
      #satisfazgo
      disponible[h]<-0
      #queda determinado el interes de los depositos en este periodo
      intdep[h-fib,t]<-deposits[which(deposits[,1]==h-fib),3]*deposits[which(deposits[,1]==h-fib),4]   
      saldep[h-fib,t]<-deposits[which(deposits[,1]==h-fib),3]
    }
    
    #determino los intereses que le correspondera pagar a cada banco
    for (b in 1:fib) {
      intdepap[b,t]<-sum(intdep[deposits[which(deposits[,2]==b),1],t])
    }  
    
    #18 Bond purchases----
    
    for (b in sample(1:fib,fib,replace = FALSE)) {
      if (sum(deposits[which(deposits[,2]==b),3])>0.0001){
        LR[b,t]=disponible[fic+fik+b]/sum(deposits[which(deposits[,2]==b),3])
        falta=disponible[fic+fik+b]-LRT[b,t]*sum(deposits[which(deposits[,2]==b),3])
        if (falta>0.0001 & deltab[t]+sum(bonos[,t-1])>sum(bonos[,t])){
          bonos[b,t]<-min(falta,deltab[t]+sum(bonos[,t-1])-sum(bonos[,t]))
          disponible[fic+fik+b]<-disponible[fic+fik+b]-bonos[b,t]
          disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+bonos[b,t]
          falta=disponible[fic+fik+b]-LRT[b,t]*sum(deposits[which(deposits[,2]==b),3])
        }
      }else{LR[b,t]=0.01}#Le asigno este valor para que sea mas bajo que LRT[b,t-1]
    }
    
    #BC compra el remanente de bonos
    if (deltab[t]+sum(bonos[,t-1])-sum(bonos[,t])>0.0001) {
      bonos[fib+1,t]<-deltab[t]+sum(bonos[,t-1])-sum(bonos[,t])
      disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-bonos[fib+1,t]
      disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+bonos[fib+1,t]
    }
    
    #19 Cash advances----
    
    for (b in 1:fib) {
      if (sum(deposits[which(deposits[,2]==b),3])>0.0001){
        if (disponible[fic+fik+b]/sum(deposits[which(deposits[,2]==b),3])<0.08){
          CAcb[b,t]<-0.08*sum(deposits[which(deposits[,2]==b),3])-disponible[fic+fik+b]
          disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-CAcb[b,t]
          disponible[fic+fik+b]<-disponible[fic+fik+b]+CAcb[b,t]
        }
      }
      R[b,t]<-disponible[fic+fik+b]
      disponible[fic+fik+b]<-round(disponible[fic+fik+b]-R[b,t],digits = 4)
    }
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]+sum(R[,t])
    
    depo[t]<-sum(deposits[,3]) 
    depoc[t]<-sum(deposits[which(deposits[,1]<=fic),3])
    depok[t]<-sum(deposits[which(deposits[,1]<=(fic+fik) & deposits[,1]>fic),3])
    depow[t]<-sum(deposits[which(deposits[,1]<=(fic+fik)+nw & deposits[,1]>(fic+fik)),3])
    depoo[t]<-sum(deposits[which(deposits[,1]<=(fic+fik)+nw+no & deposits[,1]>(fic+fik)+nw),3])
    depor[t]<-sum(deposits[which(deposits[,1]<=(fic+fik)+nw+no+nr & deposits[,1]>(fic+fik)+nw+no),3])
    depom[t]<-sum(deposits[which(deposits[,1]<=(fic+fik)+POP & deposits[,1]>(fic+fik)+nw+no+nr),3])
    
    #BC transfiere ganancias
    disponible[fic+fik+fib+nw+no+nr+nm+2]<-disponible[fic+fik+fib+nw+no+nr+nm+2]-piCB[t]
    disponible[fic+fik+fib+nw+no+nr+nm+1]<-disponible[fic+fik+fib+nw+no+nr+nm+1]+piCB[t]
    
    cajah[t]<-sum(disponible[(fic+fik+fib+1):(fic+fik+fib+nw+no+nr+nm)])
    cajac[t]<-sum(disponible[1:fic])
    cajak[t]<-sum(disponible[(fic+1):(fic+fik)])
    cajab[t]<-sum(disponible[(fic+fik+1):(fic+fik+fib)])
    cajaBC[t]<-round(disponible[fic+fik+fib+nw+no+nr+nm+2],digits = 4)
    cajaG[t]<-round(disponible[fic+fik+fib+nw+no+nr+nm+1],digits = 4)
    
    #Flujos----    
    
    fh[1,t]<--sum(constrans[,3]*constrans[,4])
    fh[2,t]<-sum(w[,t])
    fh[3,t]<-sum(d[,t])
    fh[7,t]<--sum(inctax[,t]+wtax[,t])
    fh[8,t]<-sum(intdep[(fic+fik+1):(fic+fik+POP),t-1])
    fh[12,t]<-sum(Divh[,t])+sum(aporte)#+sum(aportec)
    fh[14,t]<--(cajah[t]-cajah[t-1]+sum(aporte)+sum(aportec))
    fh[15,t]<--(sum(saldep[(fic+fik+1):(fic+fik+POP),t]-saldep[(fic+fik+1):(fic+fik+POP),t-1]))
    
    fcc[1,t]<-sum(s[1:fic,t]*px[1:fic,t])
    fcc[2,t]<--sum(wnpago[1:fic,t]*Nx[1:fic,t])
    fcc[4,t]<-(sum(inv[1:fic,t]*uc[1:fic,t])-sum(inv[1:fic,t-1]*uc[1:fic,t-1]))
    fcc[6,t]<--(sum(capital[which(capital[,1]<=fic & capital[,5]!=0),3]*capital[which(capital[,1]<=fic & capital[,5]!=0),6]/capital[which(capital[,1]<=fic & capital[,5]!=0),8]))
    fcc[7,t]<--(sum(proftax[1:fic,t]))
    fcc[8,t]<-sum(intdep[1:fic,t-1])
    fcc[10,t]<--sum(loans[which(loans[,1]<=fic & loans[,6]<=t),3]*loans[which(loans[,1]<=fic & loans[,6]<=t),4]*(1-(t-loans[which(loans[,1]<=fic & loans[,6] <= t),6])/loans[which(loans[,1]<=fic & loans[,6] <= t),5]))
    fcc[12,t]<--(sum(pix[1:fic,t]-proftax[1:fic,t]))
    
    fkc[4,t]<--(sum(inv[1:fic,t]*uc[1:fic,t])-sum(inv[1:fic,t-1]*uc[1:fic,t-1]))
    fkc[5,t]<--sum(inversion[,t])
    fkc[6,t]<-sum(capital[which(capital[,1]<=fic & capital[,5]!=0),3]*capital[which(capital[,1]<=fic & capital[,5]!=0),6]/capital[which(capital[,1]<=fic & capital[,5]!=0),8])
    fkc[12,t]<-(sum(pix[1:fic,t]-proftax[1:fic,t]-Divx[1:fic,t]))
    fkc[14,t]<--(cajac[t]-cajac[t-1])+sum(capitalizacion[1:fic,t])
    fkc[15,t]<--(sum(saldep[1:fic,t]-saldep[1:fic,t-1]))
    fkc[19,t]<-(sum(loans[which(loans[,1]<=fic & loans[,6]==t+1),3])+sum(loandefhoy[1:fic,t])-sum(loans[which(loans[,1]<=fic & loans[,6]<=t),3]*(1/loans[which(loans[,1]<=fic & loans[,6]<=t),5])))
    
    fck[2,t]<--sum(wnpago[(fic+1):(fic+fik),t]*Nx[(fic+1):(fic+fik),t])
    fck[4,t]<-(sum(inv[(fic+1):(fic+fik),t]*uc[(fic+1):(fic+fik),t])-sum(inv[(fic+1):(fic+fik),t-1]*uc[(fic+1):(fic+fik),t-1]))
    fck[5,t]<-sum(s[(fic+1):(fic+fik),t]*px[(fic+1):(fic+fik),t])
    fck[7,t]<--(sum(proftax[(fic+1):(fic+fik),t]))
    fck[8,t]<-sum(intdep[(fic+1):(fic+fik),t-1])
    fck[10,t]<--sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=t),3]*loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=t),4]*(1-(t-loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <= t),6])/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <= t),5]))
    fck[12,t]<--(sum(pix[(fic+1):(fic+fik),t]-proftax[(fic+1):(fic+fik),t]))
    
    fkk[4,t]<--(sum(inv[(fic+1):(fic+fik),t]*uc[(fic+1):(fic+fik),t])-sum(inv[(fic+1):(fic+fik),t-1]*uc[(fic+1):(fic+fik),t-1]))
    fkk[12,t]<-sum(pix[(fic+1):(fic+fik),t]-proftax[(fic+1):(fic+fik),t]-Divx[(fic+1):(fic+fik),t])
    fkk[14,t]<--(cajak[t]-cajak[t-1])+sum(capitalizacion[(fic+1):(fic+fik),t])
    fkk[15,t]<--sum(saldep[(fic+1):(fic+fik),t]-saldep[(fic+1):(fic+fik),t-1])
    fkk[19,t]<-(sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]==t+1),3])+sum(loandefhoy[(fic+1):(fic+fik),t])-sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=t),3]*(1/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=t),5])))
    
    fcb[7,t]<--(sum(proftax[(fic+fik+1):(fic+fik+fib),t]))
    fcb[8,t]<--sum(intdepap[,t-1])
    fcb[9,t]<-sum(bonos[1:fib,t-1]*itechob[t-1])
    fcb[10,t]<-sum(loans[which(loans[,6]<=t),3]*loans[which(loans[,6]<=t),4]*(1-(t-loans[which(loans[,6] <= t),6])/loans[which(loans[,6] <= t),5]))
    fcb[11,t]<--sum(CAcb[,t-1]*itechocb[t-1])
    fcb[12,t]<--(sum(pix[(fic+fik+1):(fic+fik+fib),t]-proftax[(fic+fik+1):(fic+fik+fib),t]))
    fcb[14,t]<--sum(perdida[,t])
    
    fkb[12,t]<-sum(pix[(fic+fik+1):(fic+fik+fib),t]-proftax[(fic+fik+1):(fic+fik+fib),t]-Divx[(fic+fik+1):(fic+fik+fib),t])
    fkb[14,t]<--(cajab[t]-cajab[t-1])+sum(capitalizacion[(fic+fik+1):(fic+fik+fib),t])+sum(aportec)+sum(perdida[,t])
    fkb[15,t]<-sum(saldep[,t]-saldep[,t-1])
    fkb[16,t]<-sum(CAcb[,t]-CAcb[,t-1])
    fkb[17,t]<--sum(R[,t]-R[,t-1])
    fkb[18,t]<--sum(bonos[1:fib,t]-bonos[1:fib,t-1])
    fkb[19,t]<--(sum(loans[which(loans[,6]==t+1),3])+sum(loandefhoy[,t])-sum(loans[which(loans[,6]<=t),3]*(1/loans[which(loans[,6]<=t),5])))
    
    fg[2,t]<--wnpago[fic+fik+1,t]*Nx[fic+fik+1,t]
    fg[3,t]<--sum(d[,t])
    fg[7,t]<-sum(inctax[,t],proftax[,t],wtax[,t])
    fg[9,t]<--sum(bonos[,t-1]*itechob[t-1])
    fg[13,t]<-piCB[t]
    fg[18,t]<-sum(bonos[,t]-bonos[,t-1])
    
    fccb[9,t]<-sum(bonos[fib+1,t-1]*itechob[t-1])
    fccb[11,t]<-sum(CAcb[,t-1]*itechocb[t-1])  
    fccb[13,t]<--piCB[t]
    
    fkcb[14,t]<--(cajaBC[t]-cajaBC[t-1])
    fkcb[16,t]<--sum(CAcb[,t]-CAcb[,t-1])
    fkcb[17,t]<-(sum(R[,t]-R[,t-1]))
    fkcb[18,t]<--(bonos[fib+1,t]-bonos[fib+1,t-1])    
    
    ctrlflujoag[t,1]<-sum(fh[,t])
    ctrlflujoag[t,2]<-sum(fcc[,t])
    ctrlflujoag[t,3]<-sum(fkc[,t])
    ctrlflujoag[t,4]<-sum(fck[,t])
    ctrlflujoag[t,5]<-sum(fkk[,t])
    ctrlflujoag[t,6]<-sum(fcb[,t])
    ctrlflujoag[t,7]<-sum(fkb[,t])
    ctrlflujoag[t,8]<-sum(fg[,t])
    ctrlflujoag[t,9]<-sum(fccb[,t])
    ctrlflujoag[t,10]<-sum(fkcb[,t])
    
    ctrlflujos[1,t]<-sum(fh[1,t],fcc[1,t],fkc[1,t],fck[1,t],fkk[1,t],fcb[1,t],fkb[1,t],fg[1,t],fccb[1,t],fkcb[1,t])
    ctrlflujos[2,t]<-sum(fh[2,t],fcc[2,t],fkc[2,t],fck[2,t],fkk[2,t],fcb[2,t],fkb[2,t],fg[2,t],fccb[2,t],fkcb[2,t])
    ctrlflujos[3,t]<-sum(fh[3,t],fcc[3,t],fkc[3,t],fck[3,t],fkk[3,t],fcb[3,t],fkb[3,t],fg[3,t],fccb[3,t],fkcb[3,t])
    ctrlflujos[4,t]<-sum(fh[4,t],fcc[4,t],fkc[4,t],fck[4,t],fkk[4,t],fcb[4,t],fkb[4,t],fg[4,t],fccb[4,t],fkcb[4,t])
    ctrlflujos[5,t]<-sum(fh[5,t],fcc[5,t],fkc[5,t],fck[5,t],fkk[5,t],fcb[5,t],fkb[5,t],fg[5,t],fccb[5,t],fkcb[5,t])
    ctrlflujos[6,t]<-sum(fh[6,t],fcc[6,t],fkc[6,t],fck[6,t],fkk[6,t],fcb[6,t],fkb[6,t],fg[6,t],fccb[6,t],fkcb[6,t])
    ctrlflujos[7,t]<-sum(fh[7,t],fcc[7,t],fkc[7,t],fck[7,t],fkk[7,t],fcb[7,t],fkb[7,t],fg[7,t],fccb[7,t],fkcb[7,t])
    ctrlflujos[8,t]<-sum(fh[8,t],fcc[8,t],fkc[8,t],fck[8,t],fkk[8,t],fcb[8,t],fkb[8,t],fg[8,t],fccb[8,t],fkcb[8,t])
    ctrlflujos[9,t]<-sum(fh[9,t],fcc[9,t],fkc[9,t],fck[9,t],fkk[9,t],fcb[9,t],fkb[9,t],fg[9,t],fccb[9,t],fkcb[9,t])
    ctrlflujos[10,t]<-sum(fh[10,t],fcc[10,t],fkc[10,t],fck[10,t],fkk[10,t],fcb[10,t],fkb[10,t],fg[10,t],fccb[10,t],fkcb[10,t])
    ctrlflujos[11,t]<-sum(fh[11,t],fcc[11,t],fkc[11,t],fck[11,t],fkk[11,t],fcb[11,t],fkb[11,t],fg[11,t],fccb[11,t],fkcb[11,t])
    ctrlflujos[12,t]<-sum(fh[12,t],fcc[12,t],fkc[12,t],fck[12,t],fkk[12,t],fcb[12,t],fkb[12,t],fg[12,t],fccb[12,t],fkcb[12,t])
    ctrlflujos[13,t]<-sum(fh[13,t],fcc[13,t],fkc[13,t],fck[13,t],fkk[13,t],fcb[13,t],fkb[13,t],fg[13,t],fccb[13,t],fkcb[13,t])
    ctrlflujos[14,t]<-sum(fh[14,t],fcc[14,t],fkc[14,t],fck[14,t],fkk[14,t],fcb[14,t],fkb[14,t],fg[14,t],fccb[14,t],fkcb[14,t])
    ctrlflujos[15,t]<-sum(fh[15,t],fcc[15,t],fkc[15,t],fck[15,t],fkk[15,t],fcb[15,t],fkb[15,t],fg[15,t],fccb[15,t],fkcb[15,t])
    ctrlflujos[16,t]<-sum(fh[16,t],fcc[16,t],fkc[16,t],fck[16,t],fkk[16,t],fcb[16,t],fkb[16,t],fg[16,t],fccb[16,t],fkcb[16,t])
    ctrlflujos[17,t]<-sum(fh[17,t],fcc[17,t],fkc[17,t],fck[17,t],fkk[17,t],fcb[17,t],fkb[17,t],fg[17,t],fccb[17,t],fkcb[17,t])
    ctrlflujos[18,t]<-sum(fh[18,t],fcc[18,t],fkc[18,t],fck[18,t],fkk[18,t],fcb[18,t],fkb[18,t],fg[18,t],fccb[18,t],fkcb[18,t])
    ctrlflujos[19,t]<-sum(fh[19,t],fcc[19,t],fkc[19,t],fck[19,t],fkk[19,t],fcb[19,t],fkb[19,t],fg[19,t],fccb[19,t],fkcb[19,t])
    
    ctrlflujos<-round(ctrlflujos)
    ctrlflujoag<-round(ctrlflujoag)
    
    
    #Stocks----
    
    sh[1,t]<-sh[1,t-1]-fh[14,t]
    sh[2,t]<-sum(deposits[which(deposits[,1]>fic+fik),3])
    
    sc[1,t]<--sum(capitalizacion[1:fic,1:t])
    sc[2,t]<-sum(deposits[which(deposits[,1]<=fic),3])
    sc[3,t]<--(sum(loans[which(loans[,1]<=fic & loans[,6]==t+1),3])+sum(loans[which(loans[,1]<=fic & loans[,6]<=t),3]*(1-((t+1)-loans[which(loans[,1]<=fic & loans[,6] <=t),6])/loans[which(loans[,1]<=fic & loans[,6] <=t),5])))
    sc[4,t]<-sum(inv[1:fic,t]*uc[1:fic,t])
    sc[5,t]<-sum(capital[which(capital[,1]<=fic),6]*capital[which(capital[,1]<=fic),3]*(1-capital[which(capital[,1]<=fic),5]/capital[which(capital[,1]<=fic),8]))
    
    sk[1,t]<--sum(capitalizacion[(fic+1):(fic+fik),1:t])
    sk[2,t]<-sum(deposits[which(deposits[,1]>fic & deposits[,1]<=fic+fik),3])
    sk[3,t]<--(sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]==t+1),3])+sum(loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6]<=t),3]*(1-((t+1)-loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=t),6])/loans[which(loans[,1]>fic & loans[,1]<=fic+fik & loans[,6] <=t),5])))
    sk[5,t]<-sum(inv[(fic+1):(fic+fik),t]*uc[(fic+1):(fic+fik),t])
    
    sb[1,t]<-sb[1,t-1]-fkb[14,t]-fcb[14,t]
    sb[2,t]<--(sum(deposits[,3]))
    sb[3,t]<-sum(loans[which(loans[,6]==t+1),3])+sum(loans[which(loans[,6]<=t),3]*(1-((t+1)-loans[which(loans[,6] <=t),6])/loans[which(loans[,6] <=t),5]))
    sb[6,t]<-sum(bonos[1:fib,t])
    sb[7,t]<-sum(R[,t])
    sb[8,t]<--(sum(CAcb[,t]))
    
    sg[1,t]<-cajaG[t]
    sg[6,t]<--(sum(bonos[,t]))
    
    scb[1,t]<-cajaBC[t]
    scb[6,t]<-bonos[fib+1,t]
    scb[7,t]<--(sum(R[,t]))
    scb[8,t]<-sum(CAcb[,t])
    
    ctrlstocks[1,t]<-sum(sh[1,t],sc[1,t],sk[1,t],sb[1,t],sg[1,t],scb[1,t])
    ctrlstocks[2,t]<-sum(sh[2,t],sc[2,t],sk[2,t],sb[2,t],sg[2,t],scb[2,t])
    ctrlstocks[3,t]<-sum(sh[3,t],sc[3,t],sk[3,t],sb[3,t],sg[3,t],scb[3,t])
    ctrlstocks[4,t]<-sum(sh[4,t],sc[4,t],sk[4,t],sb[4,t],sg[4,t],scb[4,t])
    ctrlstocks[5,t]<-sum(sh[5,t],sc[5,t],sk[5,t],sb[5,t],sg[5,t],scb[5,t])
    ctrlstocks[6,t]<-sum(sh[6,t],sc[6,t],sk[6,t],sb[6,t],sg[6,t],scb[6,t])
    ctrlstocks[7,t]<-sum(sh[7,t],sc[7,t],sk[7,t],sb[7,t],sg[7,t],scb[7,t])
    ctrlstocks[8,t]<-sum(sh[8,t],sc[8,t],sk[8,t],sb[8,t],sg[8,t],scb[8,t])
    
    NWag[t,1]<-sum(sh[,t])
    NWag[t,2]<-sum(sc[,t])
    NWag[t,3]<-sum(sk[,t])
    NWag[t,4]<-sum(sb[,t])
    NWag[t,5]<-sum(sg[,t])
    NWag[t,6]<-sum(scb[,t])
    
    ctrlstocks<-round(ctrlstocks)
    
    NWag<-round(NWag)
    
    
    #Este cierra el tiempo----
    hora[t]<-Sys.time()    
  }  
    
      
    
    
#Registro de la iteracion----    
    UMBRAL[it,]<<-umbral
    TASA[it,]<<-taoi2
    Y[it,]<<-gdp
    GDP[it,]<<-ngdp
    PI[it,]<<-pi
    PIC[it,]<<-pic
    PIK[it,]<<-pik
    DF[it,]<<-dfg
    DFGDP[it,]<<-dfg/ngdp
    IG[it,]<<-ig
    IG2[it,]<<-ig2
    IGAT[it,]<<-igat
    IGAIT[it,]<<-igait
    MSG[it,]<<-sueldosg
    MSGR[it,]<<-sueldosg/igdp
    UT[it,]<<-ufic
    LRTOT[it,]<<-Lr
    LRC[it,]<<-Lrc
    LRK[it,]<<-Lrk
    LOAN[it,]<<-loan
    LOANC[it,]<<-loanc
    LOANK[it,]<<-loank
    DEPO[it,]<<-depo
    DEPOC[it,]<<-depoc
    DEPOK[it,]<<-depok
    MUFIC[it,]<<-mufic
    VK[it,]<<-vk
    MUPC[it,]<<-mupc
    MUPK[it,]<<-mupk
    MUP[it,]<<-mup
    MUPSC[it,]<<-mupsc
    MUPSK[it,]<<-mupsk
    MUPS[it,]<<-mups
    LEVB[it,]<<-levb
    LEVCNW[it,]<<-levcnw
    LEVCAD[it,]<<-levcad
    LEVKNW[it,]<<-levknw
    LEVKAD[it,]<<-levkad
    ACTC[it,]<<-actc
    PASC[it,]<<-pasc
    PATC[it,]<<-patc
    ACTK[it,]<<-actk
    PASK[it,]<<-pask
    PATK[it,]<<-patk
    HORA[it,]<<-hora
    
    for (t in 1:t) {
      PC[it,t]<<-sum(px[which(x[,2]=="c"),t]*s[which(x[,2]=="c"),t])/sum(s[which(x[,2]=="c"),t])
      PK[it,t]<<-sum(px[which(x[,2]=="k"),t]*s[which(x[,2]=="k"),t])/sum(s[which(x[,2]=="k"),t])
      YC[it,t]<<-sum(y[which(x[,2]=="c"),t])
      YK[it,t]<<-sum(y[which(x[,2]=="k"),t])
      IN[it,t]<<-sum(s[which(x[,2]=="k"),t])
      CO[it,t]<<-sum(s[which(x[,2]=="c"),t])
      TI[it,t]<<-sum(inctax[,t])
      TIR[it,t]<<-sum(inctax[,t])/igdp[t]
      TW[it,t]<<-sum(wtax[,t])
      TWR[it,t]<<-sum(wtax[,t])/igdp[t]
      TP[it,t]<<-sum(proftax[,t])
      TPR[it,t]<<-sum(proftax[,t])/igdp[t]
      W[it,t]<<-sum(wh[,t])
      WR[it,t]<<-sum(wh[,t])/igdp[t]
      UN[it,t]<<-(sum(uh[,t])/POP)
      MS[it,t]<<-sum(w[,t])
      MSR[it,t]<<-sum(w[,t])/igdp[t]
      DO[it,t]<<-sum(d[,t])
      DOR[it,t]<<-sum(d[,t])/igdp[t]
      V[it,t]<<-sum(Divx[,t])
      VR[it,t]<<-sum(Divx[,t])/igdp[t]
      Q[it,t]<<-sum(default[,t])
      INN[it,t]<<-sum(Nuevavariedad[,t])
      IMI[it,t]<<-sum(Imitacion[,t])
      DG[it,t]<<-sum(bonos[,t])
      DGR[it,t]<<-sum(bonos[,t])/igdp[t]
      DGGDP[it,t]<<-sum(bonos[,t])/ngdp[t]
      SH[it,t]<<-sum(Sh[,t])
      SHR[it,t]<<-sum(Sh[,t])/ipc[t]
      WMEAN[it,t]<<-mean(w[which(uh[,t]==0),t])
      WMEANR[it,t]<<-mean(w[which(uh[,t]==0),t])/ipc[t]
      WWMEAN[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="w" & uh[,t]==0),t])
      WWMEANR[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="w" & uh[,t]==0),t])/ipc[t]
      WOMEAN[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="o" & uh[,t]==0),t])
      WOMEANR[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="o" & uh[,t]==0),t])/ipc[t]
      WRMEAN[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="r" & uh[,t]==0),t])
      WRMEANR[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="r" & uh[,t]==0),t])/ipc[t]
      WMMEAN[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="m" & uh[,t]==0),t])
      WMMEANR[it,t]<<-mean(w[which(x[(fic+fik+fib+1):(fic+fik+fib+POP),2]=="m" & uh[,t]==0),t])/ipc[t]
    }
    YH5[it,]<<-ygini[,5]
    YH25[it,]<<-ygini[,25]
    YH50[it,]<<-ygini[,50]
    YHIT5[it,]<<-ygini[,5]-inctax[,5]
    YHIT25[it,]<<-ygini[,25]-inctax[,25]
    YHIT50[it,]<<-ygini[,50]-inctax[,50]
    YHAT5[it,]<<-ygini[,5]-inctax[,5]-wtax[,5]
    YHAT25[it,]<<-ygini[,25]-inctax[,25]-wtax[,25]
    YHAT50[it,]<<-ygini[,50]-inctax[,50]-wtax[,50]
    WH5[it,]<<-wh[,5]
    WH25[it,]<<-wh[,25]
    WH50[it,]<<-wh[,50]
    WAGE1[it,]<<-w[,1]
    WAGE2[it,]<<-w[,2]
    WAGE3[it,]<<-w[,3]
    WAGE4[it,]<<-w[,4]
    WAGE5[it,]<<-w[,5]
    WAGE6[it,]<<-w[,6]
    WAGE7[it,]<<-w[,7]
    WAGE8[it,]<<-w[,8]
    WAGE9[it,]<<-w[,9]
    WAGE10[it,]<<-w[,10]
    WAGE11[it,]<<-w[,11]
    WAGE12[it,]<<-w[,12]
    WAGE13[it,]<<-w[,13]
    WAGE14[it,]<<-w[,14]
    WAGE15[it,]<<-w[,15]
    WAGE16[it,]<<-w[,16]
    WAGE17[it,]<<-w[,17]
    WAGE18[it,]<<-w[,18]
    WAGE19[it,]<<-w[,19]
    WAGE20[it,]<<-w[,20]
    WAGE21[it,]<<-w[,21]
    WAGE22[it,]<<-w[,22]
    WAGE23[it,]<<-w[,23]
    WAGE24[it,]<<-w[,24]
    WAGE25[it,]<<-w[,25]
    WAGE26[it,]<<-w[,26]
    WAGE27[it,]<<-w[,27]
    WAGE28[it,]<<-w[,28]
    WAGE29[it,]<<-w[,29]
    WAGE30[it,]<<-w[,30]
    WAGE31[it,]<<-w[,31]
    WAGE32[it,]<<-w[,32]
    WAGE33[it,]<<-w[,33]
    WAGE34[it,]<<-w[,34]
    WAGE35[it,]<<-w[,35]
    WAGE36[it,]<<-w[,36]
    WAGE37[it,]<<-w[,37]
    WAGE38[it,]<<-w[,38]
    WAGE39[it,]<<-w[,39]
    WAGE40[it,]<<-w[,40]
    WAGE41[it,]<<-w[,41]
    WAGE42[it,]<<-w[,42]
    WAGE43[it,]<<-w[,43]
    WAGE44[it,]<<-w[,44]
    WAGE45[it,]<<-w[,45]
    WAGE46[it,]<<-w[,46]
    WAGE47[it,]<<-w[,47]
    WAGE48[it,]<<-w[,48]
    WAGE49[it,]<<-w[,49]
    WAGE50[it,]<<-w[,50]
    
    DIVH1[it,]<<-Divh[,1]
    DIVH2[it,]<<-Divh[,2]
    DIVH3[it,]<<-Divh[,3]
    DIVH4[it,]<<-Divh[,4]
    DIVH5[it,]<<-Divh[,5]
    DIVH6[it,]<<-Divh[,6]
    DIVH7[it,]<<-Divh[,7]
    DIVH8[it,]<<-Divh[,8]
    DIVH9[it,]<<-Divh[,9]
    DIVH10[it,]<<-Divh[,10]
    DIVH11[it,]<<-Divh[,11]
    DIVH12[it,]<<-Divh[,12]
    DIVH13[it,]<<-Divh[,13]
    DIVH14[it,]<<-Divh[,14]
    DIVH15[it,]<<-Divh[,15]
    DIVH16[it,]<<-Divh[,16]
    DIVH17[it,]<<-Divh[,17]
    DIVH18[it,]<<-Divh[,18]
    DIVH19[it,]<<-Divh[,19]
    DIVH20[it,]<<-Divh[,20]
    DIVH21[it,]<<-Divh[,21]
    DIVH22[it,]<<-Divh[,22]
    DIVH23[it,]<<-Divh[,23]
    DIVH24[it,]<<-Divh[,24]
    DIVH25[it,]<<-Divh[,25]
    DIVH26[it,]<<-Divh[,26]
    DIVH27[it,]<<-Divh[,27]
    DIVH28[it,]<<-Divh[,28]
    DIVH29[it,]<<-Divh[,29]
    DIVH30[it,]<<-Divh[,30]
    DIVH31[it,]<<-Divh[,31]
    DIVH32[it,]<<-Divh[,32]
    DIVH33[it,]<<-Divh[,33]
    DIVH34[it,]<<-Divh[,34]
    DIVH35[it,]<<-Divh[,35]
    DIVH36[it,]<<-Divh[,36]
    DIVH37[it,]<<-Divh[,37]
    DIVH38[it,]<<-Divh[,38]
    DIVH39[it,]<<-Divh[,39]
    DIVH40[it,]<<-Divh[,40]
    DIVH41[it,]<<-Divh[,41]
    DIVH42[it,]<<-Divh[,42]
    DIVH43[it,]<<-Divh[,43]
    DIVH44[it,]<<-Divh[,44]
    DIVH45[it,]<<-Divh[,45]
    DIVH46[it,]<<-Divh[,46]
    DIVH47[it,]<<-Divh[,47]
    DIVH48[it,]<<-Divh[,48]
    DIVH49[it,]<<-Divh[,49]
    DIVH50[it,]<<-Divh[,50]
    
    YGINI1[it,]<<-ygini[,1]
    YGINI2[it,]<<-ygini[,2]
    YGINI3[it,]<<-ygini[,3]
    YGINI4[it,]<<-ygini[,4]
    YGINI5[it,]<<-ygini[,5]
    YGINI6[it,]<<-ygini[,6]
    YGINI7[it,]<<-ygini[,7]
    YGINI8[it,]<<-ygini[,8]
    YGINI9[it,]<<-ygini[,9]
    YGINI10[it,]<<-ygini[,10]
    YGINI11[it,]<<-ygini[,11]
    YGINI12[it,]<<-ygini[,12]
    YGINI13[it,]<<-ygini[,13]
    YGINI14[it,]<<-ygini[,14]
    YGINI15[it,]<<-ygini[,15]
    YGINI16[it,]<<-ygini[,16]
    YGINI17[it,]<<-ygini[,17]
    YGINI18[it,]<<-ygini[,18]
    YGINI19[it,]<<-ygini[,19]
    YGINI20[it,]<<-ygini[,20]
    YGINI21[it,]<<-ygini[,21]
    YGINI22[it,]<<-ygini[,22]
    YGINI23[it,]<<-ygini[,23]
    YGINI24[it,]<<-ygini[,24]
    YGINI25[it,]<<-ygini[,25]
    YGINI26[it,]<<-ygini[,26]
    YGINI27[it,]<<-ygini[,27]
    YGINI28[it,]<<-ygini[,28]
    YGINI29[it,]<<-ygini[,29]
    YGINI30[it,]<<-ygini[,30]
    YGINI31[it,]<<-ygini[,31]
    YGINI32[it,]<<-ygini[,32]
    YGINI33[it,]<<-ygini[,33]
    YGINI34[it,]<<-ygini[,34]
    YGINI35[it,]<<-ygini[,35]
    YGINI36[it,]<<-ygini[,36]
    YGINI37[it,]<<-ygini[,37]
    YGINI38[it,]<<-ygini[,38]
    YGINI39[it,]<<-ygini[,39]
    YGINI40[it,]<<-ygini[,40]
    YGINI41[it,]<<-ygini[,41]
    YGINI42[it,]<<-ygini[,42]
    YGINI43[it,]<<-ygini[,43]
    YGINI44[it,]<<-ygini[,44]
    YGINI45[it,]<<-ygini[,45]
    YGINI46[it,]<<-ygini[,46]
    YGINI47[it,]<<-ygini[,47]
    YGINI48[it,]<<-ygini[,48]
    YGINI49[it,]<<-ygini[,49]
    YGINI50[it,]<<-ygini[,50]
    
    DIVX1[it,]<<-Divx[,1]
    DIVX2[it,]<<-Divx[,2]
    DIVX3[it,]<<-Divx[,3]
    DIVX4[it,]<<-Divx[,4]
    DIVX5[it,]<<-Divx[,5]
    DIVX6[it,]<<-Divx[,6]
    DIVX7[it,]<<-Divx[,7]
    DIVX8[it,]<<-Divx[,8]
    DIVX9[it,]<<-Divx[,9]
    DIVX10[it,]<<-Divx[,10]
    DIVX11[it,]<<-Divx[,11]
    DIVX12[it,]<<-Divx[,12]
    DIVX13[it,]<<-Divx[,13]
    DIVX14[it,]<<-Divx[,14]
    DIVX15[it,]<<-Divx[,15]
    DIVX16[it,]<<-Divx[,16]
    DIVX17[it,]<<-Divx[,17]
    DIVX18[it,]<<-Divx[,18]
    DIVX19[it,]<<-Divx[,19]
    DIVX20[it,]<<-Divx[,20]
    DIVX21[it,]<<-Divx[,21]
    DIVX22[it,]<<-Divx[,22]
    DIVX23[it,]<<-Divx[,23]
    DIVX24[it,]<<-Divx[,24]
    DIVX25[it,]<<-Divx[,25]
    DIVX26[it,]<<-Divx[,26]
    DIVX27[it,]<<-Divx[,27]
    DIVX28[it,]<<-Divx[,28]
    DIVX29[it,]<<-Divx[,29]
    DIVX30[it,]<<-Divx[,30]
    DIVX31[it,]<<-Divx[,31]
    DIVX32[it,]<<-Divx[,32]
    DIVX33[it,]<<-Divx[,33]
    DIVX34[it,]<<-Divx[,34]
    DIVX35[it,]<<-Divx[,35]
    DIVX36[it,]<<-Divx[,36]
    DIVX37[it,]<<-Divx[,37]
    DIVX38[it,]<<-Divx[,38]
    DIVX39[it,]<<-Divx[,39]
    DIVX40[it,]<<-Divx[,40]
    DIVX41[it,]<<-Divx[,41]
    DIVX42[it,]<<-Divx[,42]
    DIVX43[it,]<<-Divx[,43]
    DIVX44[it,]<<-Divx[,44]
    DIVX45[it,]<<-Divx[,45]
    DIVX46[it,]<<-Divx[,46]
    DIVX47[it,]<<-Divx[,47]
    DIVX48[it,]<<-Divx[,48]
    DIVX49[it,]<<-Divx[,49]
    DIVX50[it,]<<-Divx[,50]
    
    S1[it,]<<-s[,1]
    S2[it,]<<-s[,2]
    S3[it,]<<-s[,3]
    S4[it,]<<-s[,4]
    S5[it,]<<-s[,5]
    S6[it,]<<-s[,6]
    S7[it,]<<-s[,7]
    S8[it,]<<-s[,8]
    S9[it,]<<-s[,9]
    S10[it,]<<-s[,10]
    S11[it,]<<-s[,11]
    S12[it,]<<-s[,12]
    S13[it,]<<-s[,13]
    S14[it,]<<-s[,14]
    S15[it,]<<-s[,15]
    S16[it,]<<-s[,16]
    S17[it,]<<-s[,17]
    S18[it,]<<-s[,18]
    S19[it,]<<-s[,19]
    S20[it,]<<-s[,20]
    S21[it,]<<-s[,21]
    S22[it,]<<-s[,22]
    S23[it,]<<-s[,23]
    S24[it,]<<-s[,24]
    S25[it,]<<-s[,25]
    S26[it,]<<-s[,26]
    S27[it,]<<-s[,27]
    S28[it,]<<-s[,28]
    S29[it,]<<-s[,29]
    S30[it,]<<-s[,30]
    S31[it,]<<-s[,31]
    S32[it,]<<-s[,32]
    S33[it,]<<-s[,33]
    S34[it,]<<-s[,34]
    S35[it,]<<-s[,35]
    S36[it,]<<-s[,36]
    S37[it,]<<-s[,37]
    S38[it,]<<-s[,38]
    S39[it,]<<-s[,39]
    S40[it,]<<-s[,40]
    S41[it,]<<-s[,41]
    S42[it,]<<-s[,42]
    S43[it,]<<-s[,43]
    S44[it,]<<-s[,44]
    S45[it,]<<-s[,45]
    S46[it,]<<-s[,46]
    S47[it,]<<-s[,47]
    S48[it,]<<-s[,48]
    S49[it,]<<-s[,49]
    S50[it,]<<-s[,50]
    
    MUC1[it,]<<-muc[,1]
    MUC2[it,]<<-muc[,2]
    MUC3[it,]<<-muc[,3]
    MUC4[it,]<<-muc[,4]
    MUC5[it,]<<-muc[,5]
    MUC6[it,]<<-muc[,6]
    MUC7[it,]<<-muc[,7]
    MUC8[it,]<<-muc[,8]
    MUC9[it,]<<-muc[,9]
    MUC10[it,]<<-muc[,10]
    MUC11[it,]<<-muc[,11]
    MUC12[it,]<<-muc[,12]
    MUC13[it,]<<-muc[,13]
    MUC14[it,]<<-muc[,14]
    MUC15[it,]<<-muc[,15]
    MUC16[it,]<<-muc[,16]
    MUC17[it,]<<-muc[,17]
    MUC18[it,]<<-muc[,18]
    MUC19[it,]<<-muc[,19]
    MUC20[it,]<<-muc[,20]
    MUC21[it,]<<-muc[,21]
    MUC22[it,]<<-muc[,22]
    MUC23[it,]<<-muc[,23]
    MUC24[it,]<<-muc[,24]
    MUC25[it,]<<-muc[,25]
    MUC26[it,]<<-muc[,26]
    MUC27[it,]<<-muc[,27]
    MUC28[it,]<<-muc[,28]
    MUC29[it,]<<-muc[,29]
    MUC30[it,]<<-muc[,30]
    MUC31[it,]<<-muc[,31]
    MUC32[it,]<<-muc[,32]
    MUC33[it,]<<-muc[,33]
    MUC34[it,]<<-muc[,34]
    MUC35[it,]<<-muc[,35]
    MUC36[it,]<<-muc[,36]
    MUC37[it,]<<-muc[,37]
    MUC38[it,]<<-muc[,38]
    MUC39[it,]<<-muc[,39]
    MUC40[it,]<<-muc[,40]
    MUC41[it,]<<-muc[,41]
    MUC42[it,]<<-muc[,42]
    MUC43[it,]<<-muc[,43]
    MUC44[it,]<<-muc[,44]
    MUC45[it,]<<-muc[,45]
    MUC46[it,]<<-muc[,46]
    MUC47[it,]<<-muc[,47]
    MUC48[it,]<<-muc[,48]
    MUC49[it,]<<-muc[,49]
    MUC50[it,]<<-muc[,50]
    
    R1[it,]<<-r[,1]
    R2[it,]<<-r[,2]
    R3[it,]<<-r[,3]
    R4[it,]<<-r[,4]
    R5[it,]<<-r[,5]
    R6[it,]<<-r[,6]
    R7[it,]<<-r[,7]
    R8[it,]<<-r[,8]
    R9[it,]<<-r[,9]
    R10[it,]<<-r[,10]
    R11[it,]<<-r[,11]
    R12[it,]<<-r[,12]
    R13[it,]<<-r[,13]
    R14[it,]<<-r[,14]
    R15[it,]<<-r[,15]
    R16[it,]<<-r[,16]
    R17[it,]<<-r[,17]
    R18[it,]<<-r[,18]
    R19[it,]<<-r[,19]
    R20[it,]<<-r[,20]
    R21[it,]<<-r[,21]
    R22[it,]<<-r[,22]
    R23[it,]<<-r[,23]
    R24[it,]<<-r[,24]
    R25[it,]<<-r[,25]
    R26[it,]<<-r[,26]
    R27[it,]<<-r[,27]
    R28[it,]<<-r[,28]
    R29[it,]<<-r[,29]
    R30[it,]<<-r[,30]
    R31[it,]<<-r[,31]
    R32[it,]<<-r[,32]
    R33[it,]<<-r[,33]
    R34[it,]<<-r[,34]
    R35[it,]<<-r[,35]
    R36[it,]<<-r[,36]
    R37[it,]<<-r[,37]
    R38[it,]<<-r[,38]
    R39[it,]<<-r[,39]
    R40[it,]<<-r[,40]
    R41[it,]<<-r[,41]
    R42[it,]<<-r[,42]
    R43[it,]<<-r[,43]
    R44[it,]<<-r[,44]
    R45[it,]<<-r[,45]
    R46[it,]<<-r[,46]
    R47[it,]<<-r[,47]
    R48[it,]<<-r[,48]
    R49[it,]<<-r[,49]
    R50[it,]<<-r[,50]
    
    MU1[it,]<<-mu[,1]
    MU2[it,]<<-mu[,2]
    MU3[it,]<<-mu[,3]
    MU4[it,]<<-mu[,4]
    MU5[it,]<<-mu[,5]
    MU6[it,]<<-mu[,6]
    MU7[it,]<<-mu[,7]
    MU8[it,]<<-mu[,8]
    MU9[it,]<<-mu[,9]
    MU10[it,]<<-mu[,10]
    MU11[it,]<<-mu[,11]
    MU12[it,]<<-mu[,12]
    MU13[it,]<<-mu[,13]
    MU14[it,]<<-mu[,14]
    MU15[it,]<<-mu[,15]
    MU16[it,]<<-mu[,16]
    MU17[it,]<<-mu[,17]
    MU18[it,]<<-mu[,18]
    MU19[it,]<<-mu[,19]
    MU20[it,]<<-mu[,20]
    MU21[it,]<<-mu[,21]
    MU22[it,]<<-mu[,22]
    MU23[it,]<<-mu[,23]
    MU24[it,]<<-mu[,24]
    MU25[it,]<<-mu[,25]
    MU26[it,]<<-mu[,26]
    MU27[it,]<<-mu[,27]
    MU28[it,]<<-mu[,28]
    MU29[it,]<<-mu[,29]
    MU30[it,]<<-mu[,30]
    MU31[it,]<<-mu[,31]
    MU32[it,]<<-mu[,32]
    MU33[it,]<<-mu[,33]
    MU34[it,]<<-mu[,34]
    MU35[it,]<<-mu[,35]
    MU36[it,]<<-mu[,36]
    MU37[it,]<<-mu[,37]
    MU38[it,]<<-mu[,38]
    MU39[it,]<<-mu[,39]
    MU40[it,]<<-mu[,40]
    MU41[it,]<<-mu[,41]
    MU42[it,]<<-mu[,42]
    MU43[it,]<<-mu[,43]
    MU44[it,]<<-mu[,44]
    MU45[it,]<<-mu[,45]
    MU46[it,]<<-mu[,46]
    MU47[it,]<<-mu[,47]
    MU48[it,]<<-mu[,48]
    MU49[it,]<<-mu[,49]
    MU50[it,]<<-mu[,50]

    }
}


library(ineq)
ABM2020(Iteraciones=26000,Time=50,POP=4000,SIw=0.3,SIor=0.4,SIm=0.3,SWw=0.3,SWor=0.4,SWm=0.3,umbralinc=0,tasabaja=0,tasainc=0.08,tu=2,robjetivo=0.04345,uobjetivo=0.8,expectadap=0.25,paciencia=2,inventariodeseado=0.1,ajusteprecios=0.015,renuncias=0.05,exitoinn=0.015,exitoimi=0.045,aversionc=3.9,aversionk=21.5)

save.image("base_umbrales_sens.RData")
