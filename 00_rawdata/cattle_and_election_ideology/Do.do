
***********************************************************************************************************************
*
*  File Name:      Do
*  Author:	       Alejandro Ponce de Leon
*  Email:	       a.ponce78@gmail.com
*  Purpose:	       Do file to work with cattle violence data
*                  
***********************************************************************************************************************

clear
set more off

*first import the file
use "C:\Users\Administrator\Dropbox\UT\2017 - Summer\Thesis\1. Data\4. Municipal_data\4. municipal data.dta" 

*LABELS

label var demo_1 "Índice de ruralidad = Población rural/población total"
label var demo_10 "Número de personas que no se auto identifican como pertenecientes a un grupo minoritario. "
label var demo_11 "número de personas que se identifican como indígena"
label var demo_12 "número de personas que se identifican como negro, mulato o afro"
label var demo_13 "número de personas que se identifican como palenque"
label var demo_14 "número de personas que se identifican como raizal"
label var demo_15 "número de personas que se identifican como rom"
label var demo_16 "número de personas que se identifican como sin etnia"
label var demo_17 "sumatoria del número de personas que se identifican como afrocolombianos"
label var demo_2 "Población Rural - Estimaciones de 1993 a 2005 y proyecciones de 2005 a 2012"
label var demo_3 "Población Total - Estimaciones de 1993 a 2005 y proyecciones de 2005 a 2012"
label var demo_4 "Población Urbana - Estimaciones de 1993 a 2005 y proyecciones de 2005 a 2012"
label var demo_5 "Tasa de alfabetismo en 1985"
label var demo_6 "Tasa de alfabetismo en 1993"
label var demo_7 "Tasa de alfabetismo en 2005"
label var demo_8 "Población total por municipio "
label var demo_9 "Total de la población, estimado, censo 1993. "
label var eco_1 " Área sembrada (Ha) de maiztr"
label var eco_10 "Área sembrada (Ha) de caÑaf"
label var eco_11 "Área sembrada (Ha) de caÑam"
label var eco_12 "Área sembrada (Ha) de cebada"
label var eco_13 "Área sembrada (Ha) de coco"
label var eco_14 "Área sembrada (Ha) de fique"
label var eco_15 "Área sembrada (Ha) de flores"
label var eco_16 "Área sembrada (Ha) de floresyf"
label var eco_17 "Área sembrada (Ha) de frijol"
label var eco_18 "Área sembrada (Ha) de hortalizasv"
label var eco_19 "Área sembrada (Ha) de maizf"
label var eco_2 "Área cosechada (Ha) de banano "
label var eco_20 "Área sembrada (Ha) de maizt"
label var eco_21 "Área sembrada (Ha) de mani"
label var eco_22 "Área sembrada (Ha) de Ñame"
label var eco_23 "Área sembrada (Ha) de palmaa"
label var eco_24 "Área sembrada (Ha) de palmar"
label var eco_25 "Área sembrada (Ha) de papa"
label var eco_26 "Área sembrada (Ha) de platano"
label var eco_27 "Área sembrada (Ha) de platanoe"
label var eco_28 "Área sembrada (Ha) de sorgo"
label var eco_29 "Área sembrada (Ha) de tabacon"
label var eco_3 "Área sembrada (Ha) de ajonjoli"
label var eco_30 "Área sembrada (Ha) de tabacor"
label var eco_31 "Área sembrada (Ha) de trigo"
label var eco_32 "Área sembrada (Ha) de yuca"
label var eco_33 "Gini de propietarios (sin rep)"
label var eco_34 "Gini de terrenos"
label var eco_35 "Incidencia de la pobreza municipal"
label var eco_36 "Índice de aptitud de los suelos"
label var eco_37 "Índice de disponibilidad de agua"
label var eco_38 "Índice de erosión de los suelos"
label var eco_39 "Índice de gini municipal"
label var eco_4 "Área sembrada (Ha) de algodón"
label var eco_40 "Necesidades básicas Insatisfechas"
label var eco_41 "Necesidades básicas Insatisfechas Censo 2005 Cabecera municipal"
label var eco_42 "Necesidades básicas Insatisfechas Censo 2005 rural disperso (resto)"
label var eco_43 "Número de créditos a grandes productores"
label var eco_44 "Número de créditos a medianos productores"
label var eco_45 "Número de créditos a pequeños productores"
label var eco_46 "PIB agrícola municipal"
label var eco_47 "PIB municipal (contantes 2005)"
label var eco_48 "PIB municipal en el sector de Servicios"
label var eco_49 "PIB municipal en el sector Industrial"
label var eco_5 "Área sembrada (Ha) de arracacha"
label var eco_50 "PIB per cápita municipal (Constante 2005)"
label var eco_51 "PIB total municipal"
label var eco_52 "Tasa de Informalidad de la tierra (predios sin matrícula inmobiliaria)"
label var eco_53 "Déficit total del municipio"
label var eco_54 "Gastos totales del municipio"
label var eco_55 "Ingresos totales de la alcaldía municipal"
label var eco_56 "Área (Ha) agrícola"
label var eco_57 "Área (Ha) en pastos"
label var eco_58 "Área (Ha) en rastrojo"
label var eco_59 "Número de Productores Residentes en el área rural dispersa censada"
label var eco_6 "Área sembrada (Ha) de arroz"
label var eco_60 "Prop de Personas en NBI (%)"
label var eco_61 " Porcentaje de cobertura de vacunación de los animales censados"
label var eco_62 "Numero de bovinos incluidos en censo"
label var eco_63 "Numero de bovinos vacunados"
label var eco_64 "numero de hembras > 3 años"
label var eco_65 "numero de hembras entre 1 - 2 años"
label var eco_66 "numero de hembras entre 2 - 3 años"
label var eco_67 "numero de machos > 3 años"
label var eco_68 "numero de machos entre 2 - 3 años"
label var eco_69 "numero de machos entre 1 - 2 años"
label var eco_7 "Área sembrada (Ha) de arrozsme"
label var eco_70 "Numero de terneros < 1 año"
label var eco_71 "Número total de bufalinos incluidos en el censo"
label var eco_72 "Número total de caprinos incluidos en el censo"
label var eco_73 "Número total de equinos incluidos en el censo"
label var eco_74 "Número total de ovinos incluidos en el censo"
label var eco_75 "Número total de porcinos incluidos en el censo"
label var eco_76 "número total hembras"
label var eco_77 "número total machos"
label var eco_78 "Porcentaje de cobertura de vacunación sobre los predios censados"
label var eco_79 "Predios censados"
label var eco_8 "Área sembrada (Ha) de cacao"
label var eco_80 "predios vacunados"
label var eco_9 "Área sembrada (Ha) de caña"
label var geo_1 "Altura del municipio - MSNM (Metros Sobre el Nivel del Mar)"
label var geo_10 "Dummy 1: Región caribe"
label var geo_11 "Dummy 1: Región Orinoquía"
label var geo_12 "Dummy 1: Región pacífica"
label var geo_13 "Provincias nacionales DANE"
label var geo_2 "Área oficial municipio DANE - hm² (hectáreas)"
label var geo_3 "Área oficial municipio DANE - km² (kilómetros cuadrados)"
label var geo_4 "Código de Provincia"
label var geo_5 "Distancia lineal a Bogotá - km (Kilómetros)"
label var geo_6 "Distancia lineal a la capital del Departamento - km (Kilómetros)"
label var geo_7 "Distancia lineal al principal mercado mayorista de alimentos"
label var geo_8 "Dummy 1: Región Amazonía"
label var geo_9 "Dummy 1: Región andina"
label var id_1 "Código único que combina el código y del año "
label var id_2 "Year of the sample"
label var id_3 "Código Dane del municipio"
label var id_4 "nombre del municipio"
label var id_5 "código dane del departamento"
label var id_6 "nombre del departamento"
label var pol_1 "Dummy, 1 si EMPATE"
label var pol_10 "Partido con mayor votación"
label var pol_11 "Porcentaje de votos a favor de los partidos minoritarios"
label var pol_12 "Porcentaje de votos a favor del partido conservador"
label var pol_13 "Porcentaje de votos a favor del partido de centro"
label var pol_14 "Porcentaje de votos a favor del partido de izquierda independiente"
label var pol_15 "Porcentaje de votos a favor del partido de liberal"
label var pol_16 "Porcentaje de votos a favor del partido de tercera vía"
label var pol_17 "Porcentaje de votos a favor del partido del arribismo"
label var pol_18 "Suma total de votos efectivos, por elección"
label var pol_2 "Dummy, 1 si gana partido CENTRO"
label var pol_3 "Dummy, 1 si gana partido CONSERVADOR"
label var pol_4 "Dummy, 1 si gana partido de TERCERA VIA"
label var pol_5 "Dummy, 1 si gana partido IZQUIERDA"
label var pol_6 "Dummy, 1 si gana partido LIBERAL"
label var pol_7 "Dummy, 1 si gana partido MINORITARIO"
label var pol_8 "Dummy, 1 si gana partido URIBISMO"
label var pol_9 "Índice de competencia electoral (menor, mayor competencia)"
label var vio_10 "Dummy - si recibió refuerzos policiales de seguridad democrática"
label var vio_11 "Dummy de presencia de AUC"
label var vio_12 "Dummy de presencia de conflictos de tierras de 1901-1931"
label var vio_13 "Dummy de presencia de cultivos de coca"
label var vio_14 "Dummy de presencia de ELN"
label var vio_15 "Dummy de presencia de FARC"
label var vio_16 "Dummy de presencia de violencia de 1948 a1953"
label var vio_17 "Homicidio ocurrencia"
label var vio_18 "Hurto a comercio"
label var vio_19 "Hurto a personas"
label var vio_20 "Hurto a residencia"
label var vio_21 "Hurto automotores"
label var vio_22 "Número de eventos relacionados con minas anti-persona"
label var vio_23 "Número total de hurtos"
label var vio_24 "Secuestro ocurrencia"
label var vio_25 "Total de lotes con cultivos de coca"
label var vio_26 "bd est-gue en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_27 "bd est-par/neo en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_28 "bd par/neo-gue en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_29 "Calculo Chapman para determinar número de personas desplazadas"
label var vio_30 "Calculo Lincoln-Petersen para determinar número de personas desplazadas"
label var vio_31 "pest en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_32 "pgue en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_33 "ppar/neo en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_34 "td est-gue en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_35 "td est-par/neo en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_36 "td par/neo-gue en http://www.cerac.org.co/libro/3_Controlando_la_medicion.pd"
label var vio_37 "Numero de masacres registradas por municipio (datos 2)"
label var vio_38 "Número de personas asesinadas selectivamente, registradas por municipio. "
label var vio_39 "Número de personas secuestradas registradas por municipio. (Cesar Caballero)"
label var vio_4 "Acto terrorista ocurrencia"
label var vio_40 "Número de concejales municipales asesinados, registrados por "
label var vio_41 "Número de homicidios registrados por municipio"
label var vio_42 "Numero de masacres registradas por municipio (datos 1)"
label var vio_43 "Número de personas desplazadas registradas por municipio"
label var vio_44 "Número total de personas desplazadas, por municipio receptor"
label var vio_45 "delito de abigeato en los departamentos de Antioquia, cauca, Caquetá, Arauca, córdoba, meta, Nariño, norte de Santander, Tolima, putumayo y valle"
label var vio_46 "Índice de incidencia del conflicto categoría="
label var vio_47 "Índice de incidencia del conflicto, porcentaje "
label var vio_48 "Sum of all crimes reported in ACEO"
label var vio_49 "Sum of cases of attempted murder reported in ACEO"
label var vio_5 "Amenazas ocurrencia"
label var vio_50 "Sum of cases of cattle rustling reported in ACEO"
label var vio_51 "Sum of cases of extortion in ACEO"
label var vio_52 "sum of cases of forced disappearance reported in ACEO"
label var vio_53 "Sum of cases of forced displacement reported in ACEO"
label var vio_54 "Sum of cases of forced recruitment reported in ACEO"
label var vio_55 "Sum of cases of homicides reported in ACEO"
label var vio_56 "Sum of cases of physical injury reported in ACEO"
label var vio_57 "Sum of cases of robbery reported in ACEO"
label var vio_58 "Sum of cases of threat reported in ACEO"
label var vio_59 "Sum of kidnappings and or abductions reported in ACEO"
label var vio_6 "Cultivos de coca por municipio en hectáreas"
label var vio_60 "Sum of sexual crimes reported in ACO"
label var vio_61 "Sum of terrorist acts reported in ACEO"
label var vio_62 "Número de Homicidios"
label var vio_63 "Numero de masacres cometidas"
label var vio_64 "Número de personas expulsadas del municipio, por año. "
label var vio_65 "Número de secuestros cometidos 1996 - 2012"
label var vio_66 "Presencia de grupos armados durante el periodo 1997 a 2001."
label var vio_67 "Presencia de grupos armados durante el periodo 2002-2007"
label var vio_68 "Número de personas heridas en accidente por MAP / MUSE"
label var vio_69 "Número de personas muertas en accidente por MAP / MUSE"
label var vio_7 "Delitos contra la integridad sexual ocurrencia"
label var vio_70 " suma caballo, yegua, burro, mula hurtadas"
label var vio_71 "Hurto cabeza de buey por unidad en municipios del país (cantidad)"
label var vio_72 "Hurto cabeza de búfalo por unidad en municipios del país (cantidad)"
label var vio_73 "Hurto cabeza de burro por unidad en municipios del país (cantidad)"
label var vio_74 "Hurto cabeza de caballo por unidad en municipios del país (cantidad)"
label var vio_75 "Hurto cabeza de cabras por unidad en municipios del país (cantidad)"
label var vio_76 "Hurto cabeza de cerdo por unidad en municipios del país (cantidad)"
label var vio_77 "Hurto cabeza de chivo por unidad en municipios del país (cantidad)"
label var vio_78 "Hurto cabeza de mula por unidad en municipios del país (cantidad)"
label var vio_79 "Hurto cabeza de novillo por unidad en municipios del país (cantidad)"
label var vio_8 "Desaparición forzada ocurrencia"
label var vio_80 "Hurto cabeza de oveja por unidad en municipios del país (cantidad)"
label var vio_81 "Hurto cabeza de toro por unidad en municipios del país (cantidad)"
label var vio_82 "Hurto cabeza de vaca por unidad en municipios del país (cantidad)"
label var vio_83 "Hurto cabeza de yegua por unidad en municipios del país (cantidad)"
label var vio_84 "suma caballo y yegua hurtadas"
label var vio_85 "suma vaca, toro, novillo hurtadas"
label var vio_86 "suma vaca, toro, novillo, búfalo y buey hurtadas"
label var vio_87 "número de personas desplazadas, por municipio"
label var vio_9 "Desplazamiento ocurrencia"
label var vio_1 "Temporalidad del conflicto"
label var vio_2 "Tipología por municipios del conflicto armado"
label var vio_3 "Intensidad del conflicto armado"


*Check out ALL the data
summarize  id_1 id_2 id_3 id_4 id_5 id_6 geo_1 geo_10 geo_11 geo_12 geo_13 geo_2 geo_3 geo_4 geo_5 geo_6 geo_7 geo_8 geo_9 demo_1 demo_10 demo_11 demo_12 demo_13 demo_14 demo_15 demo_16 demo_17 demo_2 demo_3 demo_4 demo_5 demo_6 demo_7 demo_8 demo_9 eco_1 eco_10 eco_11 eco_12 eco_13 eco_14 eco_15 eco_16 eco_17 eco_18 eco_19 eco_2 eco_20 eco_21 eco_22 eco_23 eco_24 eco_25 eco_26 eco_27 eco_28 eco_29 eco_3 eco_30 eco_31 eco_32 eco_33 eco_34 eco_35 eco_36 eco_37 eco_38 eco_39 eco_4 eco_40 eco_41 eco_42 eco_43 eco_44 eco_45 eco_46 eco_47 eco_48 eco_49 eco_5 eco_50 eco_51 eco_52 eco_53 eco_54 eco_55 eco_56 eco_57 eco_58 eco_59 eco_6 eco_60 eco_61 eco_62 eco_63 eco_64 eco_65 eco_66 eco_67 eco_68 eco_69 eco_7 eco_70 eco_71 eco_72 eco_73 eco_74 eco_75 eco_76 eco_77 eco_78 eco_79 eco_8 eco_80 eco_9 pol_1 pol_10 pol_11 pol_12 pol_13 pol_14 pol_15 pol_16 pol_17 pol_18 pol_2 pol_3 pol_4 pol_5 pol_6 pol_7 pol_8 pol_9 vio_1 vio_10 vio_11 vio_12 vio_13 vio_14 vio_15 vio_16 vio_17 vio_18 vio_19 vio_2 vio_20 vio_21 vio_22 vio_23 vio_24 vio_25 vio_26 vio_27 vio_28 vio_29 vio_3 vio_30 vio_31 vio_32 vio_33 vio_34 vio_35 vio_36 vio_37 vio_38 vio_39 vio_4 vio_40 vio_41 vio_42 vio_43 vio_44 vio_45 vio_46 vio_47 vio_48 vio_49 vio_5 vio_50 vio_51 vio_52 vio_53 vio_54 vio_55 vio_56 vio_57 vio_58 vio_59 vio_6 vio_60 vio_61 vio_62 vio_63 vio_64 vio_65 vio_66 vio_67 vio_68 vio_69 vio_7 vio_70 vio_71 vio_72 vio_73 vio_74 vio_75 vio_76 vio_77 vio_78 vio_79 vio_8 vio_80 vio_81 vio_82 vio_83 vio_84 vio_85 vio_86 vio_87 vio_9

*Change some of the variables from string to numeric

describe vio_31 vio_32 vio_33 vio_34 vio_35 vio_36 vio_37 vio_26 vio_27 vio_28 vio_39 eco_52 demo_8
destring vio_31 vio_32 vio_33 vio_34 vio_35 vio_36 vio_37 vio_26 vio_27 vio_28 vio_39 eco_52 demo_8, replace
summarize vio_31 vio_32 vio_33 vio_34 vio_35 vio_36 vio_37 vio_26 vio_27 vio_28 vio_39 eco_52 demo_8

**Figure out what to do with the MISSING

*********************CERAC - PRESENCIA
replace vio_26=0 if (vio_26==. & id_2<2010)
replace vio_27=0 if (vio_27==. & id_2<2010)
replace vio_28=0 if (vio_28==. & id_2<2010)
replace vio_31=0 if (vio_31==. & id_2<2010)
replace vio_32=0 if (vio_32==. & id_2<2010)
replace vio_33=0 if (vio_33==. & id_2<2010)
replace vio_34=0 if (vio_34==. & id_2<2010)
replace vio_35=0 if (vio_35==. & id_2<2010)
replace vio_36=0 if (vio_36==. & id_2<2010)


*********************CNMR
replace vio_37=0 if (vio_37==. & id_2<2013)
replace vio_38=0 if (vio_38==. & id_2<2013)
replace vio_39=0 if (vio_39==. & id_2<2011)
replace vio_42=0 if (vio_42==. & id_2<2013)
replace vio_43=0 if (vio_43==. & id_2<2013)
replace vio_40=0 if (vio_40==. & id_2<2013)
replace vio_40=. if (vio_40==0 & (id_2==1995|id_2==1994|id_2==1993|id_2==1992|id_2==1991|id_2==1990))

replace vio_44=0 if (vio_44==. & id_2<2013)
replace vio_44=. if (vio_44==0 & (id_2==1997|id_2==1996|id_2==1995|id_2==1994|id_2==1993|id_2==1992|id_2==1991|id_2==1990))

replace vio_29=0 if (vio_29==. & id_2<2007)
replace vio_29=. if (vio_29==0 & (id_2==1995|id_2==1994|id_2==1993|id_2==1992|id_2==1991|id_2==1990))

replace vio_30=0 if (vio_30==. & id_2<2007)
replace vio_30=. if (vio_30==0 & (id_2==1995|id_2==1994|id_2==1993|id_2==1992|id_2==1991|id_2==1990))

*********************ACEO
replace vio_48=0 if (vio_48== . & id_2<2014)
replace vio_49=0 if (vio_49== . & id_2<2014)
replace vio_50=0 if (vio_50== . & id_2<2014)
replace vio_51=0 if (vio_51== . & id_2<2014)
replace vio_52=0 if (vio_52== . & id_2<2014)
replace vio_53=0 if (vio_53== . & id_2<2014)
replace vio_54=0 if (vio_54== . & id_2<2014)
replace vio_55=0 if (vio_55== . & id_2<2014)
replace vio_56=0 if (vio_56== . & id_2<2014)
replace vio_57=0 if (vio_57== . & id_2<2014)
replace vio_58=0 if (vio_58== . & id_2<2014)
replace vio_59=0 if (vio_59== . & id_2<2014)
replace vio_60=0 if (vio_60== . & id_2<2014)
replace vio_61=0 if (vio_61== . & id_2<2014)

*********************Censo Ganadero
*	eco_62	Numero de bovinos incluidos en censo	2001 - 2014
replace eco_62=0 if (eco_62==. & id_2>2001)
*	eco_64	numero de hembras > 3 años	2001 - 2014
replace eco_64=0 if (eco_64==. & id_2>2001)
replace eco_64=. if (eco_64==0 & id_2==2013)
*	eco_65	numero de hembras entre 1 - 2 años	2001 - 2014
replace eco_65=0 if (eco_65==. & id_2>2001)
replace eco_65=. if (eco_65==0 & id_2==2013)
*	eco_66	numero de hembras entre 2 - 3 años	2001 - 2014
replace eco_66=0 if (eco_66==. & id_2>2001)
replace eco_66=. if (eco_66==0 & id_2==2013)
*	eco_67	numero de machos > 3 años	2001 - 2014
replace eco_67=0 if (eco_67==. & id_2>2001)
replace eco_67=. if (eco_67==0 & id_2==2013)
*	eco_68	numero de machos entre  2 - 3 años	2001 - 2014
replace eco_68=0 if (eco_68==. & id_2>2001)
replace eco_68=. if (eco_68==0 & id_2==2013)
*	eco_69	numero de machos entre 1 - 2 años	2001 - 2014
replace eco_69=0 if (eco_69==. & id_2>2001)
replace eco_69=. if (eco_69==0 & id_2==2013)
*	eco_70	Numero de terneros < 1 año	2001 - 2014
replace eco_70=0 if (eco_70==. & id_2>2001)
replace eco_70=. if (eco_70==0 & id_2==2013)
*	eco_76	numero total hembras	2001 - 2014
replace eco_76=0 if (eco_76==. & id_2>2001)
replace eco_76=. if (eco_76==0 & id_2==2013)
*	eco_77	numero total machos	2001 - 2014
replace eco_77=0 if (eco_77==. & id_2>2001)
replace eco_77=. if (eco_77==0 & id_2==2013)
*	eco_78	Porcentaje de cobertura de vacunacion sobre los predios censados	2001 - 2014
replace eco_78=0 if (eco_78==. & id_2>2001)
replace eco_78=. if (eco_78==0 & id_2==2013)
*	eco_79	Predios censados	2001 - 2014
replace eco_79=0 if (eco_79==. & id_2>2001)
replace eco_79=. if (eco_79==0 & id_2==2013)
*	eco_80	predios vacunados	2001 - 2014
replace eco_80=0 if (eco_80==. & id_2>2001)
replace eco_80=. if (eco_80==0 & id_2==2013)

*********************FEDEMUNICIPIOS
replace vio_62=0 if (vio_62==. & id_2<2013)
replace vio_63=0 if (vio_63==. & id_2<2013)
replace vio_64=0 if (vio_64==. & id_2<2013)
replace vio_68=0 if (vio_68==. & id_2<2014)
replace vio_69=0 if (vio_69==. & id_2<2014)
replace vio_41=0 if (vio_41==. & id_2<2013)

*********************POLICIA (GANADO)
replace vio_70=0 if (vio_70==. & id_2>2002)
replace vio_71=0 if (vio_71==. & id_2>2002)
replace vio_72=0 if (vio_72==. & id_2>2002)
replace vio_73=0 if (vio_73==. & id_2>2002)
replace vio_74=0 if (vio_74==. & id_2>2002)
replace vio_75=0 if (vio_75==. & id_2>2002)
replace vio_76=0 if (vio_76==. & id_2>2002)
replace vio_77=0 if (vio_77==. & id_2>2002)
replace vio_78=0 if (vio_78==. & id_2>2002)
replace vio_79=0 if (vio_79==. & id_2>2002)
replace vio_80=0 if (vio_80==. & id_2>2002)
replace vio_81=0 if (vio_81==. & id_2>2002)
replace vio_82=0 if (vio_82==. & id_2>2002)
replace vio_83=0 if (vio_83==. & id_2>2002)
replace vio_84=0 if (vio_84==. & id_2>2002)
replace vio_85=0 if (vio_85==. & id_2>2002)
replace vio_86=0 if (vio_86==. & id_2>2002)
replace vio_45=0 if (vio_45==. & id_2>2002)

*********************CEDE
replace vio_5=0 if (vio_5==. & id_2>1992)
replace vio_8=0 if (vio_8==. & id_2>1992)
replace vio_24=0 if (vio_24==. & id_2>1992)
replace vio_9=0 if (vio_9==. & id_2>1992)
replace vio_17=0 if (vio_17==. & id_2>1992)
replace vio_23=0 if (vio_23==. & id_2>1992)
replace vio_6=0 if (vio_6==. & id_2>1992)
replace vio_25=0 if (vio_25==. & id_2>1992)
replace vio_9=0 if (vio_9==. & id_2>1992)

********************************************************************************************************
* 										EXAMINE DATA: GENERAL 										   *
********************************************************************************************************

*Look at crimes against cattle ranchers:
summarize vio_48 vio_49 vio_51 vio_52 vio_53 vio_54 vio_55 vio_56 vio_57 vio_58 vio_59

*Look at displacement
summarize vio_53 vio_29 vio_30 vio_43 vio_44 vio_87 vio_9

*Look at kidnapping
summarize vio_59 vio_24 vio_39 vio_65

*****************************
*   VIO50 comes from ACEO   *
*****************************

*look at the DP (cattle rustling):
summarize vio_50 vio_85
hist vio_85 
hist vio_50

*I could also try CENSO ganadero, to see if it works (eco_62)
ta eco_62
hist eco_62

*maybe log 
g ln_eco62=ln(eco_62)

*A small test with FARC dummy:
reg ln_eco62 vio_15 vio_32 vio_14
reg vio_85 vio_15 vio_32 vio_14
reg vio_50 vio_15 vio_32 vio_14

*****************************
*  THINGS TO DO OR CHANGE   *
*****************************
*either log or drop cases for vio_85 (drop if vio_85>1000)?????

********************************************************************************************************
* 								    1. ARMED ACTORS ACTIONS LOCATIONS 								   *
********************************************************************************************************
 
************************************
*POSSIBLE VARIABLES ON ARMED ACTORS*
************************************

***FARC***
*vio_15 FARC(Dummy)
*vio_14 *ELN(Dummy)
*vio_32 *GUERRILLAS (DUMMY) -CERAC

***AUC***
*vio_11 *AUC 
*vio_31 * estado -CERAC
*vio_33 *PARAMILITARES -CERAC

***WAR***
*vio_3 *CERAC 0 to 2
*vio_46* cerac, 1 - 5
*vio_47*DNP Indice de conflicto, porcentaje (1 - 0) (Maybe multiply)

*************
*SIMPLE TEST*
*************
summarize vio_15 vio_14 vio_32 
summarize vio_11 vio_31 vio_33 
summarize vio_3 vio_47 vio_46

*CEDE HAS MORE COMPLETE DATA:
*FARC: vio_15
*ELN: vio_14
*AUC: vio_11


**************************
*  DOES IT WORK BETTER?  *
**************************

*Test against two counts of homicides:
*vio_41 
***FARC***
reg vio_41 vio_15 vio_32 vio_14
***AUC***
reg vio_41 vio_11 vio_33 vio_31
***WAR***
reg vio_41 vio_3 vio_46 vio_47

*vio_62 
***FARC***
reg vio_62 vio_15 vio_32 vio_14
***AUC***
reg vio_62 vio_11 vio_33 vio_31
***WAR***
reg vio_62 vio_3 vio_46 vio_47

 
**************************
*  TESTING LOCATION A.A  *
**************************

*POSSIBLE VARIABLES*
*geo_5	Distancia lineal a Bogotá - km (Kilómetros)
*geo_6	Distancia lineal a la capital del Departamento - km (Kilómetros)
*demo_1 Índice de ruralidad = Población rural/población total
*demo_3	Población Total  - Estimaciones de 1993 a 2005 y proyecciones de 2005 a 2012
*demo_6	Tasa de alfabetismo en 1993
*demo_7	Tasa de alfabetismo en 2005
*eco_34	Gini de terrenos --  Rural GINI, from 2001 to 2010
*eco_35	Incidencia de la pobreza municipal
*eco_40	Necesidades básicas Insatisfechas
*eco_46	PIB agrícola municipal
*eco_55	Ingresos totales de la alcaldía municipal
*eco_60	Prop de Personas en NBI (%)
*eco_56 Área (Ha) agricola (a 2014)
*eco_62	Numero de bovinos incluidos en censo
*pol_11	Porcentaje de votos a favor de los partidos minoritarios
*pol_12	Porcentaje de votos a favor del partido conservador
*pol_13	Porcentaje de votos a favor del partido de centro
*pol_14	Porcentaje de votos a favor del partido de izquierda independiente
*pol_15	Porcentaje de votos a favor del partido de liberal
*pol_16	Porcentaje de votos a favor del partido de tercera via
*pol_17	Porcentaje de votos a favor del partido del uribismo
*pol_18	Suma total de votos efectivos, por eleccion
*pol_9	Indice de competencia electoral (menor, mayor competencia)
*	vio_6	Cultivos de coca por municipio en hectáreas	1993 - 2015
*	vio_25	Total de lotes con cultivos de coca	1993 - 2015
*	vio_41	Número de homicidios registrados por municipio	1990 - 2012
*	vio_23	Número total de hurtos	1993 - 2015
*vio_5	Amenazas ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_8	Desaparición forzada ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_9	Desplazamiento ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_24	Secuestro ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_17	Homicidio ocurrencia	CEDE – Uniandes	1993 – 2015



*1. FARC - vio_15 
*Control
reg vio_15 geo_6 demo_3, cluster(id_3)

*Agriculture
reg vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62, cluster(id_3)

*Politics
reg vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62 pol_12 pol_14 pol_15 pol_16 pol_17 pol_9, cluster(id_3)

***TEST criminal activity, general.






********************************************************************************************************
* 								    3. Municipalities are af. most A.A.								   *
********************************************************************************************************

**************************
*  Simple cattle models  *
**************************

reg ln_eco62 vio_14 vio_11 vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62 pol_12 pol_14 pol_15 pol_16 pol_17 pol_9

reg vio_85 vio_14 vio_11 vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62 pol_12 pol_14 pol_15 pol_16 pol_17 pol_9

*ACEO
reg vio_48 vio_14 vio_11 vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62 pol_12 pol_14 pol_15 pol_16 pol_17 pol_9
reg vio_50 vio_14 vio_11 vio_15 geo_6 demo_3 demo_1 eco_46 eco_56 eco_62 pol_12 pol_14 pol_15 pol_16 pol_17 pol_9


**************************
*   Things to do-chage   *
**************************

*See what happens to two or three DPs (Kidnapping, rustling, and homicide) with armed actor presence and maybe control
*Would this be a MANOVA? 
**(needs to be logit regression)?????


*** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ***
*							MANOVA						 *
*** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ***

***Figure out the effects of armed actor presence in a muni

*vio_5	Amenazas ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_8	Desaparición forzada ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_9	Desplazamiento ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_24	Secuestro ocurrencia	CEDE – Uniandes	1993 – 2015
*vio_17	Homicidio ocurrencia	CEDE – Uniandes	1993 – 2015
***eco_36 Rural GINI, from 2001 to 2010



**************************
*   Things to do-chage   *
**************************


***CREATE T-1 VARIABLE
**CREATE CONSTANT PRESENCE OF ARMED GROUPS TO TEST ARJONA'S???

*Here's some code for making a variable that will measure the years of your "treatment" (the # of *consecutive years measuring FARC presence). 

*code to create a new variable: 
*gen varname=cond(farc<=year+2000 & farc<., 1, 0)

*where, 
*varname = whatever you want to call the new variable measuring the consecutive years of FARC presence 
*farc = dummy variable 
*year = year


**SEE IF I NEED TO CROP THE SAMPLE
***I need to test the idea that if there's other groups, it is going to be a predatory attitude...
**CAN I MAKE AN AVERAGE OF CATTLE MUNICIPALITIES, AND TEST IT AS A PROXI FOR PRESENCE OF ARMED ACTORS????? NAMELY, AUC.
**WHAT ARE THE MUNIS WERE AUC CAME? T-1 OR T-2 
***Using this cross-sectional data set, I estimate negative binomial II (NB) and zero inflated negative binomial (ZINB) regressions, which are count models appropriate for the nature of the dependent variable(s) (number of people executed by the left, in t1, and number of people executed by the right, in t2). NB permits controlling for overdispersion; ZINB allows controlling both for overdispersion and the excess of zeros in the dependent variable (Long 1997).
***maybe also contrast the munis with hight variation in cattle theft, build a category, and test it against other crimes... 
***CLIENTELISM AND CONTINUITY OF ELECTORAL RESULTS... I NEED TO SEE PATTERNS OF CLIENTELIST STATE RELATIONS....
***Coca as an external, mediating, factor.... CENTENO'S argument.
**FARC, AUC, Competence per muni, agregated and per year (is there an overlap????) 
**test if DURABLE presence changes anything in the DP... You would argue that, once an armed actor is there, there is going to be more cattle (security)
***How am i accounting for market???? and economy??? i have to explain this.... somehow....
***i have to test the clasic models... ethnic, inequality, competition, etc.
** i need to see what happens to cattle production once they are there... to test common narratives both for FARC and AUC
*** what is within case analysis????
**Multinomial Probit???? 
***See other productions, maybe top ten agricultural products
***How am I fixing State capacity?
***Fixed effects… thinking about historical analysis methods, how can I fix everything else?
***Build regions by patterns in: production, geography, patterns of interaction. Maybe two kinds of region? One about violence and another one about production?
***On ranchers: see sequence of events, make new variable. Test it against armed actor, region, and wealth.
***How do I account for consumption and population increase? Or to macro-economic trends??? At least explain the sector, maybe one section.
***Primitive accoumulation… I have to add on that.
***Cows are not the only resources. I have to include what happens to others. 
***I have to talk about how FARC is not going or didn’t concentrate in areas where the inequalities are severe. What happens in their strongholds? 
***What happens in municipalities with other resources? Interaction? Cattle is the #1 produce. Coca?
***I need to show what happens to social indicators AFTER war. 


********************************************************************************************************
* 								    4. Test the theory with other comm								   *
********************************************************************************************************



** ROBUSTNESS CHECKS**




********************************************************************************************************
* 								    5. I need to figure out regions 								   *
********************************************************************************************************

*id_3	Codigo Dane del municipio
*id_5	código dane del departamento
*geo_8	Dummy 1: Región amazonía
*geo_9	Dummy 1: Región andina
*geo_10	Dummy 1: Región caribe
*geo_11	Dummy 1: Región orinoquía
*geo_12	Dummy 1: Región pacífica
*geo_4	Código de Provincia



*	geo_1	Altura del municipio - MSNM (Metros Sobre el Nivel del Mar)	1993 - 2014
*	geo_2	Área oficial municipio DANE - hm² (hectáreas)	1993 - 2014
*	geo_3	Área oficial municipio DANE - km² (kilómetros cuadrados)	1993 - 2014
*	geo_4	Código de Provincia	1993 - 2014
*	geo_5	Distancia lineal a Bogotá - km (Kilómetros)	1993 - 2014
*	geo_6	Distancia lineal a la capital del Departamento - km (Kilómetros)	1993 - 2014
*	geo_7	Distancia lineal al principal mercado mayorista de alimentos	1993 - 2014
*	geo_8	Dummy 1: Región amazonía	1993 - 2014
*	geo_9	Dummy 1: Región andina	1993 - 2014
*	geo_10	Dummy 1: Región caribe	1993 - 2014
*	geo_11	Dummy 1: Región orinoquía	1993 - 2014
*	geo_12	Dummy 1: Región pacífica	1993 - 2014
*	geo_13	Provincias nacionales DANE	1993 - 2014



********************************************************************************************************
* 								    KRIS VELASCO'S CODE - TIME SERIES 								   *
********************************************************************************************************


xtset id_3 id_2, yearly
gen time_since = 0 if vio_15==1
by id_3: replace time_since = 1 if L.time_since==0 &time_since==.
by id_3: replace time_since = 2 if L.time_since==1 &time_since==.
by id_3: replace time_since = 3 if L.time_since==2 &time_since==.
by id_3: replace time_since = 4 if L.time_since==3 &time_since==.
by id_3: replace time_since = 5 if L.time_since==4 &time_since==.
by id_3: replace time_since = 6 if L.time_since==5 &time_since==.
by id_3: replace time_since = 7 if L.time_since==6 &time_since==.
by id_3: replace time_since = 8 if L.time_since==7 &time_since==.
by id_3: replace time_since = 9 if L.time_since==8 &time_since==.
by id_3: replace time_since = 10 if L.time_since==9 &time_since==.
by id_3: replace time_since = 11 if L.time_since==10 &time_since==.
by id_3: replace time_since = 12 if L.time_since==11 &time_since==.
by id_3: replace time_since = 13 if L.time_since==12 &time_since==.
by id_3: replace time_since = 14 if L.time_since==13 &time_since==.
by id_3: replace time_since = 15 if L.time_since==14 &time_since==.
by id_3: replace time_since = 16 if L.time_since==15 &time_since==.
by id_3: replace time_since = 17 if L.time_since==16 &time_since==.
by id_3: replace time_since = 18 if L.time_since==17 &time_since==.
by id_3: replace time_since = 19 if L.time_since==18 &time_since==.
by id_3: replace time_since = 20 if L.time_since==19 &time_since==.
by id_3: replace time_since = 21 if L.time_since==20 &time_since==.
by id_3: replace time_since = 22 if L.time_since==21 &time_since==.
replace time_since=0 if time_since==.


xtreg vio_85 i.vio_15 c.time_since, vce(cluster id_3)

/*
Random-effects GLS regression                   Number of obs      =     12342
Group variable: id_3                            Number of groups   =      1122

R-sq:  within  = 0.0000                         Obs per group: min =        11
       between = 0.0143                                        avg =      11.0
       overall = 0.0008                                        max =        11

                                                Wald chi2(2)       =      7.81
corr(u_i, X)   = 0 (assumed)                    Prob > chi2        =    0.0202

                                (Std. Err. adjusted for 1122 clusters in id_3)
------------------------------------------------------------------------------
             |               Robust
      vio_85 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
    1.vio_15 |   17.91006   8.301626     2.16   0.031     1.639171    34.18095
  time_since |  -.1680443   .0847734    -1.98   0.047    -.3341972   -.0018914
       _cons |    6.97967   .6861472    10.17   0.000     5.634846    8.324494
-------------+----------------------------------------------------------------
     sigma_u |  51.064672
     sigma_e |  347.50575
         rho |  .02113679   (fraction of variance due to u_i)
------------------------------------------------------------------------------

Ok, so I figured out that you don't have to put in the interaction term (there's a long math explanation for
why). So as you can see, the effect of time_since is significant and negative, which supports your argument
that over time, the effect of the FARC entering a municipality on cattle stealing will diminish.

However, this effect is somewhat small. According to the model above, municipalities that experience FARC 
intervention will generally see about 18 cattle stolen. However, every year after this that effect goes down 
by just .168. In other words, it will take 100 years for the effect to completely go away. 

If you have any other questions, please let me know! This was a fun stata challenge. 
*/
