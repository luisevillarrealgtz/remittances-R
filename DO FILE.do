**///////////////////////////////////TRABAJO FINAL ECONOMIA FINANCIERA - ARCHIVO DO FILE**

*//////////////////ANALISIS DE LAS SERIES*
*Generación de la variable tiempo*
rename time tiempo
gen time=tq(1996q1)+_n-1
format %tqCY-q time
tsset time
gen obs=_n+1

*Se tomaron las siguientes series basadas en el trabajo de Ramon Castillo titulado "Remesas: un analisis de cointegracion* 
*para el caso de Mexico"* 
*como variable dependiente se tomaron a las "Remesas captadas de Estados Unidos a México", como variables independientes* 
*se tomaron al "PIB de Mexico para medir el impacto en los ingresos del pais receptor", al "PIB de Estados Unidos, para medir* 
*el impacto de los ingresos en el pais emisor" y "el tipo de cambio real, donde se analiza la devaluación del peso y sus*
*consecuencias en el mayor o menor envio de remesas al pais*
*Dichos datos abarcan desde enero de 1996 a diciembre de 2019 con una periodicidad trimestral*

*Graficas de cada serie*
twoway (tsline remesas, lcolor(black)), title("Remesas") subtitle("Serie en niveles de enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos del Banco de BANXICO.")
twoway (tsline pibmex, lcolor(black)), title("Producto Interno Bruto (PIB) de México") subtitle("Serie en niveles de enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de INEGI.")
twoway (tsline pibeua, lcolor(black)), title("Producto Interno Bruto (PIB) de Estados Unidos") subtitle("Serie en niveles de enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de BEA.")
twoway (tsline tcreal, lcolor(black)), title("Tipo de Cambio USD-MXN real") subtitle("Serie en niveles de enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de BANXICO.")

line remesas time, saving(remesas.gph)
line pibmex time, saving(pibmex.gph)
line pibeua time, saving(pibeua.gph)
line tcreal time, saving(tcreal.gph)
gr combine remesas.gph pibmex.gph pibeua.gph tcreal.gph, title("Combinación de las variables de estudio") subtitle("Series en niveles de forma trimestral de enero de 1996 a diciembre de 2019") scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO, INEGI y del BEA.")

*Elaboración de logaritmos de cada serie*
gen lremesas=log(remesas)
gen lpibmex=log(pibmex)
gen lpibeua=log(pibeua)
gen ltcreal=log(tcreal)

*Gráfica de cada serie en logaritmos*
twoway (tsline lremesas, lcolor(black)), title("Remesas") subtitle("Serie logaritmica. De enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos del Banco de BANXICO.")
twoway (tsline lpibmex, lcolor(black)), title("Producto Interno Bruto (PIB) de México") subtitle("Serie logaritmica. De enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de INEGI.")
twoway (tsline lpibeua, lcolor(black)), title("Producto Interno Bruto (PIB) de Estados Unidos") subtitle("Serie logaritmica. De enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos del BEA.")
twoway (tsline ltcreal, lcolor(black)), title("Tipo de Cambio USD-MXN real") subtitle("Serie logaritmica. De enero de 1996 a diciembre de 2019") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de BANXICO.")

line lremesas time, saving(lremesas.gph)
line lpibmex time, saving(lpibmex.gph)
line lpibeua time, saving(lpibeua.gph)
line ltcreal time, saving(ltcreal.gph)
gr combine lremesas.gph lpibmex.gph lpibeua.gph ltcreal.gph, title("Combinación de las variables de estudio") subtitle("Series logaritmicas de forma trimestral de enero de 1996 a diciembre de 2019") scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO, INEGI y del BEA.")

*Generación de variables dicotómicas*
gen date = quarterly(dofm(time))
gen Q1 = 0
gen Q2 = 0
gen Q3 = 0
gen Q4 = 0
replace Q1 = 1 if time ==1
replace Q2 = 1 if time ==4
replace Q3 = 1 if time ==7
replace Q4 = 1 if time ==10

*///////////DESESTACIONALIZACION DE LAS SERIES*
*Ya que la serie del PIB está en datos desestacionalizados no se añadieron en las series con las variables dicotómicas*
*Remesas*
reg lremesas Q1 Q2 Q3 Q4 ,noconstant
predict estacionalidad_lremesas
gen lremesas_d = lremesas - estacionalidad_lremesas

*PIB Mexico*
reg lpibmex ,noconstant
predict estacionalidad_lpibmex
gen lpibmex_d = lpibmex - estacionalidad_lpibmex

*PIB EUA*
reg lpibeua Q1 Q2 Q3 Q4 ,noconstant
predict estacionalidad_lpibeua
gen lpibeua_d = lpibeua - estacionalidad_lpibeua

*TC Real*
reg ltcreal Q1 Q2 Q3 Q4 ,noconstant
predict estacionalidad_ltcreal
gen ltcreal_d = ltcreal - estacionalidad_ltcreal

*/////////GRAFICAS DE LAS SERIES DESESTACIONALIZADAS*
twoway (tsline ligae_d, lcolor(black)), title("Indicador Global de la Actividad Economica (IGAE)") subtitle("Valores logarítmicos desestacionalizados de forma mensual de enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO.")
twoway (tsline lhom_d, lcolor(black)), title("Defunciones por homicidios") subtitle("Serie logaritmica desestacionalizada al mes de registro. De enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de INEGI.")
twoway (tsline lrobos_d, lcolor(black)), title("Reportes de Incidencia Delictiva Nacional") subtitle("Serie logaritmica desestacionalizada sobre los robos. De enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos del SNSP.")

line ligae_d time, saving(ligae_d.gph)
line lhom_d time, saving(lhom_d.gph)
line lrobos_d time, saving(lrobos_d.gph)
gr combine ligae_d.gph lhom_d.gph lrobos_d.gph, title("Combinación de las variables de estudio") subtitle("Series logaritmicas desestacionalizadas de forma mensual de enero de 1997 a agosto de 2017") scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO, INEGI y del SNSP.")


*////////PRUEBA DE RAIZ UNITARIA*
*Ho: Raiz Unitaria
*H1: Estacionaria
*Valor Estadistico < Valor Critico para Rechazar Ho


*//////IGAE***///////////////
dfuller ligae_d, trend
dfuller ligae_d, drift

zandrews ligae_d, break(both)
zandrews ligae_d, lagmethod(BIC)

*NOTA: La serie IGAE con el test de Raiz Unitaria DFULLER agregando la tendencia se rechaza Ho, sin embargo, en su forma de cambio de nivel no se rechaza Ho*
*NOTA: La serie IGAE con el test de ZANDREWS con un cambio en el intercepto y tendencia no se rechaza Ho y en su forma minimizar rezagos no se rechaza Ho.*

*//////HOMICIDIOS***///////////////*
dfuller lhom_d, trend
dfuller lhom_d, drift

zandrews lhom_d, break(both)
zandrews lhom_d, lagmethod(BIC)

*NOTA: La serie HOMICIDIOS con el test de Raiz Unitaria DFULLER agregando la tendencia no se rechaza H0 y en su forma de cambio de nivel no se rechaza Ho*
*NOTA: La serie HOMICIDIOS con el test de ZANDREWS con un cambio en el intercepto y tendencia no se rechaza Ho y en su forma minimizar rezagos no se rechaza Ho al 1%*

*//////ROBOS***///////////////*
dfuller lrobos_d, trend
dfuller lrobos_d, drift

zandrews lrobos_d, break(both)
zandrews lrobos_d, lagmethod(BIC)

*NOTA: La serie ROBOS con el test de Raiz Unitaria DFULLER agregando la tendencia no se rechaza H0 y en su forma de cambio de nivel no se rechaza Ho al 5%*
*NOTA: La serie ROBOS con el test de ZANDREWS con un cambio en el intercepto y tendencia no se rechaza Ho y en su forma minimizar rezagos no se rechaza Ho*

*LAS SERIES SON NO ESTACIONARIAS POR NO RECHAZAR H0 EN LOS CASOS ANTERIORES*
*SE PRESENTA UN CAMBIO ESTRUCTURAL EN LAS SERIES ASI QUE SE REALIZARON LAS SIGUIENTES VARIABLES DICOTOMICAS PARA*
*SER AÑADIDAS AL MODELO VAR*
*EN EL CASO DEL IGAE SE HICIERON 2 VARIABLES DICOTOMICAS: D_IGAE_IYS en octubre del 2007 por el mejor mes en el ambito*
*de la industria y los servicios del pais y la D_IGAE_REC en febrero del 2009 por la recesión del 2008-2009 en el mundo*
*EN EL CASO DE LOS HOMICIDIOS SE HICIERON 3 VARIABLES DICOTOMICAS: D_HOM_IGCO en febrero del 2007 por ser el mes con menos*
*homicidios nacionales pero en el mismo año comenzó la guerra contra el crimer organizado, la segunda es D_HOM_AGCO en* 
*mayo del 2011 con el auge de la guerra del crimen organizado, y por ultimo D_HOM_EPN en febrero del 2015 con el repunte*
*de homicidios en la administración de Enrique Peña Nieto*
*EN EL CASO DE LOS ROBOS SE HICIERON 3 VARIABLES DICOTOMICAS: D_ROBOS_EUA en julio del 2005 ya que EUA deja de establecer* 
*su programa de seguridad para reducir la violencia y el trafico de drogas, el segundo D_ROBOS_VEH en marzo del 2011 ya*
*que se implementó un programa para reducir los robos de vehiculos en la CDMX y posteriormente a nivel nacional. Por*
*último D_ROBOS_TP en mayo del 2016 ya que se comenzaron a contabilizar de mejor manera los robos en el transporte público*
*a nivel nacional*
*DE IGUAL FORMA SE PROCEDIÓ A DIFERENCIAR LAS SERIES PARA TENER TENER EL CRECIMIENTO DE CADA VARIABLE*

*IGAE*
gen dligae = d.ligae
*HOMICIDIOS*
gen dlhom = d.lhom
*ROBOS*
gen dlrobos = d.lrobos

*/////////GRAFICAS DE LAS SERIES DIFERENCIADAS*

twoway (tsline dligae, lcolor(black)), title("Indicador Global de la Actividad Economica (IGAE)") subtitle("Valores diferenciados de forma mensual de enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO.")
twoway (tsline dlhom, lcolor(black)), title("Defunciones por homicidios") subtitle("Serie diferenciada al mes de registro. De enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos de INEGI.")
twoway (tsline dlrobos, lcolor(black)), title("Reportes de Incidencia Delictiva Nacional") subtitle("Serie diferenciada sobre los robos. De enero de 1997 a agosto de 2017") legend(on) scheme(s1color) note("Elaboración propia. Datos obtenidos del SNSP.")

line dligae time, saving(dligae.gph)
line dlhom time, saving(dlhom.gph)
line dlrobos time, saving(dlrobos.gph)
gr combine dligae.gph dlhom.gph dlrobos.gph, title("Combinación de las variables de estudio") subtitle("Series diferenciadas de forma mensual de enero de 1997 a agosto de 2017") scheme(s1color) note("Elaboración propia. Valores tomados de BANXICO, INEGI y del SNSP.")

*////////PRUEBA DE RAIZ UNITARIA*
*Ho: Raiz Unitaria*
*H1: Estacionaria*
*Valor Estadistico < Valor Critico para Rechazar Ho*


*//////IGAE***///////////////*
dfuller dligae, regress

zandrews dligae, lagmethod(BIC)

*NOTA: La serie IGAE con el test de Raiz Unitaria DFULLER en su forma de cambio de nivel rechaza Ho*
*NOTA: La serie IGAE con el test de ZANDREWS en su forma minimizar rezagos rechaza Ho.*

*//////HOMICIDIOS***///////////////*
dfuller dlhom, regress

zandrews dlhom, lagmethod(BIC)

*NOTA: La serie IGAE con el test de Raiz Unitaria DFULLER en su forma de cambio de nivel rechaza Ho*
*NOTA: La serie IGAE con el test de ZANDREWS en su forma minimizar rezagos rechaza Ho.*

*//////ROBOS***///////////////*
dfuller dlrobos, regress

zandrews dlrobos, lagmethod(BIC)

*NOTA: La serie IGAE con el test de Raiz Unitaria DFULLER en su forma de cambio de nivel rechaza Ho*
*NOTA: La serie IGAE con el test de ZANDREWS en su forma minimizar rezagos rechaza Ho.*

*GENERACION DE LAS VARIABLES DICOTOMICAS QUE SE TOMARAN COMO EXOGENAS EN EL MODELO VAR*

*PRIMER VARIABLE DICOTOMICA: D_IGAE_IYS*
gen D_IGAE_IYS = 0
replace D_IGAE_IYS = 1 if obs ==131

*SEGUNDA VARIABLE DICOTOMICA: D_IGAE_REC*
gen D_IGAE_REC = 0
replace D_IGAE_REC = 1 if obs ==147

*TERCER VARIABLE DICOTOMICA: D_IGAE_REC*
gen D_HOM_IGCO = 0
replace D_HOM_IGCO = 1 if obs ==123

*CUARTA VARIABLE DICOTOMICA: D_HOM_AGCO*

gen D_HOM_AGCO = 0
replace D_HOM_AGCO = 1 if obs ==174

*QUINTA VARIABLE DICOTOMICA: D_HOM_EPN*

gen D_HOM_EPN  = 0
replace D_HOM_EPN  = 1 if obs ==219

*SEXTA VARIABLE DICOTOMICA: D_ROBOS_EUA*

gen D_ROBOS_EUA = 0
replace D_ROBOS_EUA  = 1 if obs ==104

*SEPTIMA VARIABLE DICOTOMICA: D_ROBOS_VEH*

gen D_ROBOS_VEH = 0
replace D_ROBOS_VEH  = 1 if obs ==172

*OCTAVA VARIABLE DICOTOMICA: D_ROBOS_TP*

gen D_ROBOS_TP = 0
replace D_ROBOS_TP  = 1 if obs ==234

*YA QUE LAS VARIABLES SON INTEGRADAS DE ORDEN 1 SE PROCEDERÁ A HACER UN ANALISIS DE COINTEGRACION*

*///////////////////////Determinación de los rezagos optimos///////////////////////*

varsoc dligae dlhom dlrobos, exog(D_IGAE_IYS D_IGAE_REC D_HOM_IGCO D_HOM_AGCO D_HOM_EPN D_ROBOS_EUA D_ROBOS_VEH D_ROBOS_TP) maxlag(24)

*SEGUN LOS CRITERIOS DE INFORMACION SE DETERMINAN 2 MODELOS: CON 2 Y 12 REZAGOS*

***********************MODELO 1 con 2 Rezagos**********************

*Ho: El numero de ecuaciones de cointegracion es r* 
*Ha: El numero de ecuaciones de cointegracion es r+1*
*Se rechaza Ho Stadistico > Valor crítico*

vecrank dligae dlhom dlrobos, lags(2) max ic levela

*NOTA: La prueba de conintegracion de Johansen acepta Ho cuando rank = 2 al 1%*
*NOTA: Max statistic se acepta Ho cuando rank = 1 al 5%*
*NOTA: En los Criterios de Informacion señalan 2 ecuaciones de cointegracion*

*Modelo de Correción de Errores*

vec dligae dlhom dlrobos, r(2) lags(2)

*Multiplicador de Lagrange de Autocorrelaciones de los Residuales*

*Si Prob > 0.05 se rechaza Ho
veclmar, mlag(2)

*Prueba de normalidad de los errores*

*Ho: Distribucion Normal
*Si Prob > 0.05 se rechaza Ho
vecnorm, j

predict res1_mod1, r eq(#1)
predict res2_mod1, r eq(#2)

tsline res1_mod1
tsline res2_mod1

hist res1_mod1, norm
hist res2_mod1, norm

***********************MODELO 2 con 12 Rezagos**********************

*Ho: El numero de ecuaciones de cointegracion es r* 
*Ha: El numero de ecuaciones de cointegracion es r+1*
*Se rechaza Ho Stadistico > Valor crítico*

vecrank dligae dlhom dlrobos, lags(12) max ic levela

*NOTA: La prueba de conintegracion de Johansen acepta Ho cuando rank = 1 al 1% y 5%*
*NOTA: Max statistic se acepta Ho cuando rank = 1 al 1%*
*NOTA: En los Criterios de Informacion señala 1 ecuación de cointegracion*

*Modelo de Correción de Errores*

vec dligae dlhom dlrobos, r(1) lags(12)

*Multiplicador de Lagrange de Autocorrelaciones de los Residuales*

*Si Prob > 0.05 se rechaza Ho
veclmar, mlag(12)

*Prueba de normalidad de los errores*
*Ho: Distribucion Normal
*Si Prob > 0.05 se rechaza Ho
vecnorm, j

predict res1_mod2, r eq(#1)

tsline res1_mod2

hist res1_mod2, norm

*/////FUNCIONES DE IMPULSO RESPUESTA/////*

irf create modelo1, set(imp_response4) step(24) replace

irf graph irf, impulse(dlhom) response(dligae)
irf graph irf, impulse(dlrobos) response(dligae)

irf graph oirf, impulse(dlhom) response(dligae)
irf graph oirf, impulse(dlrobos) response(dligae)

varbasic dligae dlhom dlrobos, lags(1/2) step (24)
