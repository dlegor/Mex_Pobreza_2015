##################################################################
######   EJERCICIO DE VALIDACIÓN DE CIFRAS DE POBREZA  ###########
##################################################################

######Programa para estimación de pobreza multidimensional#####      		
#Metodología Oficial del CONEVAL#

##(Versión Julio 2016 para discusión, no citar ni difundir)##

###################################################################
#Se cargan datos desde las bases de Stata en formato ".dta" y se 
#procesan en R project.
#Recomendaciones para ejecución:
#Usar R por lo menos en la versión 3.2.4 revisada el 2016-03-16

###################################################################

#Se valida que se cuenta con los paquetes necesarios para la ejecucion
# de este código.
#Para validar la versión de R que se tiene instalada se corre el siguiente 
#comando:

#version

#Se revisa que se tengan los paquetes requeridos o se instalan en caso
#contrario. También se cargan las librerías para ejecutar el código.

#Se limpia la consola
rm(list=ls())

#Función para validar los paquetes:

paquetes<-function(paq_inst){
  #Se revisa si los paquetes que se requieren en el código se encuentran instalados	
  new.packages <- paq_inst[!(paq_inst %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    message("########## SE ININCIA LA REVISIÓN DE INSTALCIÓN DE LAS LIBRERÍAS ##########")
    install.packages(new.packages,dependencies=TRUE)
    
    return (message("########## SE INSTALARON LOS PAQUETES REQUERIDOS EN EL CÓDIGO ##########"))}
  
  else return(print("########## SE TIENEN TODOSO LOS PAQUETES NECESARIOS PARA ESTE CÓDIGO ##########"))
}

#Lista de paquetes requeridos:
lista.de.paquetes<-c("tibble","dplyr","foreign","car","magrittr","stringr","XLConnect","doBy","Matrix")
install.packages("car",dependencies=TRUE)
install.packages("XLConnect",dependencies=TRUE)

#Validación de existencia de paquetes:
paquetes(lista.de.paquetes)

#Se cargan las librerías ya que se validó la instalación.
unlist(lapply(lista.de.paquetes,function(x) suppressMessages(require(x, character.only = TRUE))))

#Se preparan la ejecución del código desplazandose al directorio correspondiente.
#Colocar dentro del siguiente comando la ubicación del directorio de trabajo

#setwd("C:/Usuario/Directorio/de/trabajo/")
setwd("~/Documentos/Code_R_2015/")
############################################################
###### INCLUYE CAMBIOS DE MCS 2014 y MCS-MUESTRA 2015 ######
############################################################												                             
######### I. DIMENSIONES DE POBREZA ########################     
############################################################	

################################################     
####### CALIDAD Y ESPACIOS DE LA VIVIENDA ######
################################################

rm(list=ls())

#Se cargan las bases
vivienda <-read.dta('Bases/Datos_R/viviendas.dta') 
conc_hogar <-read.dta('Bases/Datos_R/concentradohogar.dta')

#Modificaciones a las bases
vivienda=vivienda[with(vivienda,order(folioviv)),]
hogares <- inner_join(vivienda,conc_hogar,by = c("folioviv", "ubica_geo", "ageb", "tam_loc", "est_socio", "est_dis", "upm"))
rm(vivienda,conc_hogar)

levels(hogares$mat_pisos)[levels(hogares$mat_pisos)=="&"]<-""

hogares$pisos<-as.numeric(hogares$mat_pisos)
hogares$techos<-as.numeric(hogares$mat_techos)
hogares$pared<-as.numeric(hogares$mat_pared)

hogares=hogares%>%
  group_by(folioviv)%>%
  mutate(pisos1=mean(pisos),techos1=mean(techos),pared1=mean(pared))%>%
  select(-pisos,-techos,-pared)%>%
  as.data.frame()

names(hogares)[names(hogares)=="pisos1"]<-"pisos"
names(hogares)[names(hogares)=="techos1"]<-"techos"
names(hogares)[names(hogares)=="pared1"]<-"pared"

### Material de pisos ###

attach(hogares)
hogares$cv_pisos=NA
hogares$cv_pisos[pisos==1]=1
hogares$cv_pisos[pisos==2]=2
hogares$cv_pisos[pisos==3]=3

### Material de techos ###

hogares$cv_techos=NA
hogares$cv_techos[techos==1]=1
hogares$cv_techos[techos==2]=2
hogares$cv_techos[techos==3]=3
hogares$cv_techos[techos==4]=4
hogares$cv_techos[techos==5]=5
hogares$cv_techos[techos==6]=6
hogares$cv_techos[techos==7]=8
hogares$cv_techos[techos==8]=7
hogares$cv_techos[techos==9]=9
hogares$cv_techos[techos==10]=10

### Material de muros ###

hogares$cv_muros=NA
hogares$cv_muros[pared==1]=1
hogares$cv_muros[pared==2]=2
hogares$cv_muros[pared==3]=3
hogares$cv_muros[pared==4]=4
hogares$cv_muros[pared==5]=5
hogares$cv_muros[pared==6]=6
hogares$cv_muros[pared==7]=7
hogares$cv_muros[pared==8]=8

### Índice de hacinamiento ###

hogares$tot_resid<-as.numeric(tot_resid)
hogares$num_cuarto<-as.numeric(num_cuarto)
hogares$cv_hac= tot_resid/num_cuarto
detach(hogares)

hogares=rename(hogares,num_ind=tot_resid,cuart=num_cuarto)

### Indicador de carencia del material de piso de la vivienda ###

attach(hogares)
hogares$icv_pisos=NA
hogares$icv_pisos[cv_pisos==1]=1
hogares$icv_pisos[cv_pisos>1 & is.na(mat_pisos)==FALSE]=0

### Indicador de carencia del material de techo de la vivienda ###

hogares$icv_techos=NA
hogares$icv_techos[cv_techos<=2]=1
hogares$icv_techos[cv_techos>2 & is.na(cv_techos)==FALSE]=0

### Indicador de carencia del material de muros de la vivienda ###

hogares$icv_muros=NA
hogares$icv_muros[cv_muros<=5]=1 
hogares$icv_muros[cv_muros>5 & is.na(cv_muros)==FALSE]=0

### Indicador de carencia por índice de hacinamiento de la vivienda ###

hogares$icv_hac=NA
hogares$icv_hac[cv_hac>2.5 & is.na(cv_hac)==FALSE]=1 
hogares$icv_hac[cv_hac<=2.5]=0 
detach(hogares)

### Indicador de carencia de la calidad y espacios de la vivienda ###

attach(hogares)
hogares$ic_cv=0
hogares$ic_cv[icv_pisos==1 | icv_techos==1 | icv_muros==1 | icv_hac==1]=1
hogares$ic_cv[icv_pisos==0 & cv_techos==0 & icv_muros==0 & icv_hac==0]=0
hogares$ic_cv[is.na(icv_pisos)==TRUE | is.na(icv_techos)==TRUE | is.na(icv_muros)==TRUE | is.na(icv_hac)==TRUE]=NA
detach(hogares)

attr(hogares$ic_cv,"Descripción")<-"CONEVAL Indicador de carencia calidad y espacios de la vivienda"
attributes(hogares$ic_cv)
table(hogares$ic_cv,dnn="CONEVAL Indicador de carencia calidad y espacios de la vivienda")

select(hogares,folioviv,foliohog,starts_with("cv_"),starts_with("icv_"),ic_cv)%>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/pviviendac.dta')

#################################################
### ACCESO A SERVICIOS BÁSICOS DE LA VIVIENDA ###
#################################################

#Se limpia el entorno
rm(list=ls())  

conc_hogar <-read.dta('Bases/Datos_R/concentradohogar.dta') 
conc_hogar<-conc_hogar%>%
  select(folioviv,foliohog)%>%
  arrange(folioviv)%>%
  as.data.frame()

viviendas <-read.dta('Bases/Datos_R/viviendas.dta')

hogares <- inner_join(conc_hogar,viviendas,by="folioviv")
rm(conc_hogar,viviendas)

levels(hogares$combustible)[levels(hogares$combustible)=="&"]<-""
hogares$elect<-as.numeric(hogares$disp_elect)
hogares$dis_agua<-as.numeric(hogares$disp_agua)
hogares$combus<-as.numeric(hogares$combustible)
hogares$estufa<-as.numeric(hogares$estufa_chi)

#### Acceso al agua ####

attach(hogares)
hogares$sb_agua=NA
hogares$sb_agua[dis_agua==7]=1
hogares$sb_agua[dis_agua==6]=2
hogares$sb_agua[dis_agua==5]=3
hogares$sb_agua[dis_agua==4]=4
hogares$sb_agua[dis_agua==3]=5
hogares$sb_agua[dis_agua==2]=6
hogares$sb_agua[dis_agua==1]=7

#### Servicio de drenaje ####

hogares$sb_drenaje=NA
hogares$sb_drenaje[drenaje==5]=1
hogares$sb_drenaje[drenaje==4]=2
hogares$sb_drenaje[drenaje==3]=3
hogares$sb_drenaje[drenaje==2]=4
hogares$sb_drenaje[drenaje==1]=5

#### Servicio de electricidad ####

hogares$sb_luz=NA
hogares$sb_luz[elect==5]=1
hogares$sb_luz[elect==4 | elect==3 ]=2
hogares$sb_luz[elect==2]=3
hogares$sb_luz[elect==1]=4
detach(hogares)

#### Servicio de combustible para cocinar ####

hogares$combus=recode(hogares$combus,"-1:NA")
hogares$estufa=recode(hogares$estufa,"-1:NA")
attach(hogares)
hogares$sb_combus=NA
hogares$sb_combus[combus==6]=1
hogares$sb_combus[combus==5]=2
hogares$sb_combus[combus==4]=3
hogares$sb_combus[combus==3]=4
hogares$sb_combus[combus==2 | combus==1 ]=5

### Indicador de carencia de acceso al agua en la vivienda ###

hogares$isb_agua=NA
hogares$isb_agua[sb_agua==5|sb_agua==1 | sb_agua==2 | sb_agua==3 | sb_agua==4]=1
hogares$isb_agua[sb_agua==7|sb_agua==6]=0

### Indicador de carencia de servicio de drenaje en la vivienda ###

hogares$isb_dren=NA
hogares$isb_dren[sb_drenaje<=3]=1
hogares$isb_dren[sb_drenaje>3 & is.na(sb_drenaje)==FALSE]=0

### Indicador de carencia de servicios de electricidad en la vivienda ###

hogares$isb_luz=NA
hogares$isb_luz[sb_luz==1]=1
hogares$isb_luz[sb_luz>1 & is.na(sb_luz)==FALSE]=0
detach(hogares)

### Indicador de carencia combustible ### 

attach(hogares)
hogares$isb_combus=NA
hogares$isb_combus[sb_combus>=1 & sb_combus<=4]=0
hogares$isb_combus[sb_combus==5 & estufa==1]=0
hogares$isb_combus[sb_combus==5 & estufa==2]=1
detach(hogares)

### Indicador de carencia de acceso a servicios básicos de la vivienda ###

attach(hogares)
hogares$ic_sbv=NA
hogares$ic_sbv[isb_agua==1 | isb_dren==1 | isb_luz==1 | isb_combus==1]=1
hogares$ic_sbv[isb_agua==0 & isb_dren==0 & isb_luz==0 & isb_combus==0]=0
hogares$ic_sbv[is.na(isb_agua)==TRUE | is.na(isb_dren)==TRUE | is.na(isb_luz)==TRUE | is.na(isb_combus)==TRUE]=NA
attr(hogares$ic_sbv,"Descripción")<-"CONEVAL Indicador de carencia de acceso a servicios básicos de la vivienda"
attributes(hogares$ic_sbv)
table(hogares$ic_sbv,dnn="CONEVAL Indicador de carencia de acceso a servicios básicos de la vivienda")
detach(hogares)

hogares%>%select(folioviv,foliohog,starts_with("sb_"),starts_with("isb_"),ic_sbv)%>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/pviviendas.dta')

########################
### REZAGO EDUCATIVO ###
########################

rm(list=ls())  

poblacion <-read.dta('Bases/Datos_R/poblacion.dta') 
poblacion$parentesco<-as.numeric(poblacion$parentesco)
poblacion1=poblacion%>%
  filter(parentesco<400 | parentesco >=500)%>%
  filter(parentesco<700 | parentesco >=800)%>%
  mutate(inst_1=replace(inst_1,inst_1==" ",""))%>%
  mutate(inst_2=replace(inst_2,inst_2==" ",""))%>%
  mutate(inst_3=replace(inst_3,inst_3==" ",""))%>%
  mutate(inst_4=replace(inst_4,inst_4==" ",""))%>%
  mutate(inst_5=replace(inst_5,inst_5==" ",""))%>%
  mutate(inscr_1=replace(inscr_1,inscr_1==" ",""))%>%
  mutate(inscr_2=replace(inscr_2,inscr_2==" ",""))%>%
  mutate(inscr_3=replace(inscr_3,inscr_3==" ",""))%>%
  mutate(inscr_4=replace(inscr_4,inscr_4==" ",""))%>%
  mutate(inscr_5=replace(inscr_5,inscr_5==" ",""))%>%
  mutate(inscr_6=replace(inscr_6,inscr_6==" ",""))%>%
  mutate(inscr_7=replace(inscr_7,inscr_7==" ",""))%>%
  mutate(atemed=replace(atemed,atemed==" ",""))%>%
  mutate(segpop=replace(segpop,segpop==" ",""))%>%
  mutate(segvol_1=replace(segvol_1,segpop==" ",""))%>%
  mutate(disc1=replace(disc1,disc1==" " | disc1=="&",""))%>%
  as.data.frame()
rm(poblacion)

### Año de nacimiento de cada miembro del hogar ###

poblacion1$anac_e=2015 - poblacion1$edad[is.na(poblacion1$edad)==FALSE]
poblacion1$asis_esc <- as.numeric(poblacion1$asis_esc)
poblacion1$nivelaprob <- as.numeric(poblacion1$nivelaprob)
poblacion1$gradoaprob <- as.numeric(poblacion1$gradoaprob)
poblacion1$antec_esc <- as.numeric(poblacion1$antec_esc)

### Inasistencia a la escuela ###

attach(poblacion1)
poblacion1$inas_esc=0
poblacion1$inas_esc[asis_esc==1]=0
poblacion1$inas_esc[asis_esc==2]=1

### Nivel educativo ###

poblacion1$niv_ed[nivelaprob==0 | nivelaprob==1]=0
poblacion1$niv_ed[nivelaprob==2 & gradoaprob<6]=0
poblacion1$niv_ed[nivelaprob==2 & gradoaprob==6]=1
poblacion1$niv_ed[nivelaprob==3 & gradoaprob<3]=1
poblacion1$niv_ed[nivelaprob==5 & gradoaprob<3 & antec_esc==1 ]=1
poblacion1$niv_ed[nivelaprob==6 & gradoaprob<3 & antec_esc==1 ]=1
poblacion1$niv_ed[nivelaprob==3 & gradoaprob==3]=2
poblacion1$niv_ed[nivelaprob==4]=2
poblacion1$niv_ed[nivelaprob==5 & gradoaprob>=3 & antec_esc==1]=2
poblacion1$niv_ed[nivelaprob==5 & antec_esc>=2 & is.na(antec_esc)==FALSE]=2
poblacion1$niv_ed[nivelaprob==6 & antec_esc==1 & gradoaprob>=3]=2
poblacion1$niv_ed[nivelaprob==6 & antec_esc>=2 & is.na(antec_esc)==FALSE]=2
poblacion1$niv_ed[nivelaprob>=7 & is.na(nivelaprob)==FALSE]=2
detach(poblacion1)

### Indicador de carencia por rezago educativo ###

attach(poblacion1)
poblacion1$ic_rezedu[edad>=3 & edad<=15 & inas_esc==1 & niv_ed<2 ]=1
poblacion1$ic_rezedu[anac_e>=1982 & edad>=16 & niv_ed<2 ]=1
poblacion1$ic_rezedu[anac_e<=1981 & edad>=16 & niv_ed==0] =1
poblacion1$ic_rezedu[edad<=2]=0
poblacion1$ic_rezedu[edad>=3 & edad<=15 & inas_esc==0]=0
poblacion1$ic_rezedu[edad>=3 & edad<=15 & niv_ed==2 & inas_esc==1]=0
poblacion1$ic_rezedu[anac_e>=1982 & is.na(anac_e)==FALSE & edad>=16 & niv_ed==2]=0
poblacion1$ic_rezedu[anac_e<=1981 & edad >=16 & niv_ed>=1 & (niv_ed==1 | niv_ed==2)]=0
attr(poblacion1$ic_rezedu,"Descripción") <-"CONEVAL Indicador de carencia por rezago educativo"
table(poblacion1$ic_rezedu,dnn="CONEVAL Indicador de carencia por rezago educativo")

poblacion1$hli=NA
poblacion1$hli[hablaind=="1" & edad>=3]=1
poblacion1$hli[hablaind=="2" & edad>=3]=0
#lina de revisión
poblacion1$hablaind <-as.numeric(hablaind)
poblacion1$hablaind <- recode(hablaind,"2=0")

poblacion1=poblacion1 %>%
  arrange(folioviv,foliohog)%>%
  group_by(folioviv,foliohog)%>%
  mutate(indigena=ifelse(hablaind==1 & ((parentesco>=101 & parentesco<300)| parentesco==601
                                        |parentesco==602 |parentesco==606 |parentesco==607 |parentesco==608 |parentesco==615),1,NA))%>%
  as.data.frame()
poblacion1=poblacion1 %>%group_by(folioviv,foliohog)%>%mutate(h_indigena=mean(indigena,na.rm=TRUE))
detach(poblacion1)

poblacion1$h_indigena[poblacion1$hablaind==1 & is.na(poblacion1$h_indigena)==TRUE]=1  
poblacion1$h_indigena<-recode(poblacion1$h_indigena,"NA=0")

poblacion_2=select(poblacion1,-indigena)%>%
  rename(indigena=h_indigena)%>%
  as.data.frame()

attach(poblacion_2)
poblacion_2$c_ed1=0
poblacion_2$c_ed1[ic_rezedu==0 & edad>=3 & edad<=15]=0
poblacion_2$c_ed1[ic_rezedu==1 & edad>=3 & edad<=15]=1

poblacion_2$c_ed2=0
poblacion_2$c_ed2[ic_rezedu==0 & edad>=16 & anac_e<=1981]=0
poblacion_2$c_ed2[ic_rezedu==1 & edad>=16 & anac_e<=1981]=1

poblacion_2$c_ed3=0
poblacion_2$c_ed3[ic_rezedu==0 & edad>=16 & anac_e>=1982 & is.na(anac_e)==FALSE]=0
poblacion_2$c_ed3[ic_rezedu==1 &  edad>=16 & anac_e>=1982 & is.na(anac_e)==FALSE]=1
detach(poblacion_2)

select(poblacion_2,folioviv,foliohog,numren,anac_e,inas_esc,niv_ed,ic_rezedu,hli,indigena,c_ed1,c_ed2,c_ed3)%>%
  arrange(folioviv,foliohog,numren)%>%
  write.dta('Bases/Bases_R/peducacion.dta')

########################################
###### ACCESO A SERVICIOS DE SALUD #####
########################################

rm(list=ls())

trabajos <- read.dta('Bases/Datos_R/trabajos.dta')   
trabajos=trabajos %>%
  select(folioviv,foliohog,numren,id_trabajo,subor,indep,pago,tiene_suel,pres_1,pres_8)%>%
  arrange(folioviv,foliohog,numren)%>%
  mutate(pres_1=replace(pres_1,pres_1==" ",""))%>%
  mutate(pres_8=replace(pres_8,pres_8==" ",""))%>%
  as.data.frame()

### Revisión de Duplicates ####

trabajos$id=paste0(trabajos$folioviv,trabajos$foliohog,trabajos$numren)

### Tabla con Id y dupli para Duplicados ###

df_dupli<-trabajos%>%group_by(id)%>%
  summarise(dupli=n()-1)%>%
  as.data.frame()
trabajos_1=left_join(trabajos,df_dupli,by="id")
table(trabajos_1$dupli,dnn="Duplicados presentes en los datos")

trabajos_1$basetrab<-1
rm(trabajos,df_dupli)

### Ocupación principal o secundaria ###

trabajos_1$id_trabajo <- as.numeric(trabajos_1$id_trabajo)
trabajos_1$id_trabajo <-recode(trabajos_1$id_trabajo,"1=1;2=0")
#trabajos_1$ocupa_t[trabajos_1$id_trabajo==0]=trabajos_1$id_trabajo[trabajos_1$id_trabajo==0]
trabajos_1$ocupa_t[trabajos_1$id_trabajo==0 & is.na(trabajos_1$id_trabajo)==FALSE]=trabajos_1$id_trabajo[trabajos_1$id_trabajo==0 & is.na(trabajos_1$id_trabajo)==FALSE]

trabajos=trabajos_1%>%arrange(folioviv,foliohog,numren)%>%group_by(folioviv,foliohog,numren)%>%
  mutate(ocupa_tt=max(ocupa_t,na.rm=TRUE))%>%as.data.frame()
rm(trabajos_1)

trabajos$ocupa1[trabajos$id_trabajo==1]=trabajos$id_trabajo[trabajos$id_trabajo==1]
trabajos$ocupa2[trabajos$ocupa_tt==0 & trabajos$id_trabajo==1 & is.na(trabajos$ocupa_tt)==FALSE]=
trabajos$ocupa_tt[trabajos$ocupa_tt==0 & trabajos$id_trabajo==1 & is.na(trabajos$ocupa_tt)==FALSE]

trabajos$ocupa2<-recode(trabajos$ocupa2,"0=1;NA=0")
trabajos=select(trabajos,-ocupa_t,-ocupa_tt)  

### Tipo de trabajo ###

attach(trabajos)
trabajos$subor<-as.numeric(subor)
trabajos$indep <-as.numeric(indep)
trabajos$tiene_suel <- as.numeric(tiene_suel)
trabajos$pago <- as.numeric(pago)
detach(trabajos)

### Procesamiento ###
attach(trabajos)
trabajos$tipo_trab=0
trabajos$tipo_trab[subor==1]=1
trabajos$tipo_trab[indep==1 & subor==2 & tiene_suel==1]=2
trabajos$tipo_trab[indep==1 & tiene_suel==2]=3
trabajos$tipo_trab[indep==2 & subor==2 & (pago==2 | pago==3)]=3
detach(trabajos)

trabajos_2=trabajos%>%
  mutate(tipo_trab1=ifelse(id_trabajo==1,tipo_trab,NA),
         tipo_trab2_t=ifelse(id_trabajo==0,tipo_trab,NA))%>%
  group_by(folioviv, foliohog, numren)%>%
  mutate(tipo_trab2_tt=max(tipo_trab2_t,na.rm=TRUE))%>%
  mutate(tipo_trab2=ifelse(ocupa2==1,tipo_trab2_tt,NA))%>%
  select(-tipo_trab2_t,-tipo_trab2_tt)%>%
  as.data.frame()

trabajos_2%>%filter(ocupa1==1)%>%
  select(folioviv, foliohog, numren, starts_with("tipo_trab"),starts_with("ocupa"), basetrab)%>%
  arrange(folioviv,foliohog,numren)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/pocupa.dta')

poblacion <-read.dta('Bases/Datos_R/poblacion.dta') 

poblacion$parentesco<-as.numeric(poblacion$parentesco)
poblacion_1=poblacion%>%
  filter(parentesco<400 | parentesco >=500)%>%
  filter(parentesco<700 | parentesco >=800)%>%
  arrange(folioviv,foliohog,numren)%>%
  mutate(inst_1=replace(inst_1,inst_1==" ",""))%>%
  mutate(inst_2=replace(inst_2,inst_2==" ",""))%>%
  mutate(inst_3=replace(inst_3,inst_3==" ",""))%>%
  mutate(inst_4=replace(inst_4,inst_4==" ",""))%>%
  mutate(inst_5=replace(inst_5,inst_5==" ",""))%>%
  mutate(inscr_1=replace(inscr_1,inscr_1==" ",""))%>%
  mutate(inscr_2=replace(inscr_2,inscr_2==" ",""))%>%
  mutate(inscr_3=replace(inscr_3,inscr_3==" ",""))%>%
  mutate(inscr_4=replace(inscr_4,inscr_4==" ",""))%>%
  mutate(inscr_5=replace(inscr_5,inscr_5==" ",""))%>%
  mutate(inscr_6=replace(inscr_6,inscr_6==" ",""))%>%
  mutate(inscr_7=replace(inscr_7,inscr_7==" ",""))%>%
  mutate(atemed=replace(atemed,atemed==" ",""))%>%
  mutate(segpop=replace(segpop,segpop==" ",""))%>%
  mutate(segvol_1=replace(segvol_1,segpop==" ",""))%>%
  mutate(disc1=replace(disc1,disc1==" " | disc1=="&",""))%>%
  as.data.frame()

ocupa <-read.dta('Bases/Bases_R/pocupa.dta')

trab_pobl=left_join(poblacion_1,ocupa,by=c("folioviv","foliohog","numren"))
rm(poblacion ,poblacion_1 ,ocupa,trabajos_2,trabajos)

### Ocupada/desocupada ###

attach(trab_pobl)
trab_pobl$act_ <-as.numeric(act_pnea1)
trab_pobl$act_ <-as.numeric(act_pnea2)
trab_pobl$edad <-as.numeric(edad)


trab_pobl$ocupada[basetrab==1 & edad>=16]=1
trab_pobl$ocupada <-recode(trab_pobl$ocupada,"NA=0")

trab_pobl$act_buscot[act_pnea1==1 | act_pnea2==1]=1
trab_pobl$act_rento=NA
trab_pobl$act_pensio[act_pnea1==2 | act_pnea2==2]=3
trab_pobl$act_quehac[act_pnea1==3 | act_pnea2==3]=4
trab_pobl$act_estudi[act_pnea1==4 | act_pnea2==4]=5
trab_pobl$act_discap[act_pnea1==5 | act_pnea2==5]=6
trab_pobl$act_otra[act_pnea1==6 | act_pnea2==6]=7
detach(trab_pobl)

attach(trab_pobl)
trab_pobl$desocupada=NA
trab_pobl$desocupada[edad>=16 & parentesco<400 & act_buscot==1]=1
trab_pobl$desocupada[edad>=16 & (parentesco>=500 & parentesco<700) & act_buscot==1]=1
trab_pobl$desocupada <-recode(trab_pobl$desocupada,"NA=0")

### PNA ###

trab_pobl$pna=NA
trab_pobl$pna[edad>=16 & is.na(act_buscot)==TRUE & parentesco<400 & (act_rento ==2 
                                                                     | act_pensio==3 | act_quehac==4 | act_estudi==5 | act_discap==6 |  act_otra==7)]=1
trab_pobl$pna[edad>=16 & is.na(act_buscot)==TRUE & (parentesco>=500 & parentesco<700) & 
                (act_rento ==2 | act_pensio==3 | act_quehac==4 | act_estudi==5 | act_discap==6 |  act_otra==7)]=1
trab_pobl$pna=recode(trab_pobl$pna,"NA=0")
detach(trab_pobl)

### PEA ###

attach(trab_pobl)
trab_pobl$pea=NA
trab_pobl$pea[ocupada==1 & edad>=16 ]=1
trab_pobl$pea[desocupada==1 & edad>=16]=2
trab_pobl$pea[pna==1 & edad>=16]=0

attr(trab_pobl$pea,"Descripción") <-"CONEVAL población económicamente activa"
attributes(trab_pobl$pea)
table(trab_pobl$pea,dnn="CONEVAL población económicamente activa")
detach(trab_pobl)

### Tipo de trabajo ###

attach(trab_pobl)
trab_pobl$tipo_trab1[pea==0 | pea==2 | is.na(pea)==TRUE]=NA
trab_pobl$tipo_trab1[pea==1 & is.na(pea)==FALSE & is.na(tipo_trab)==FALSE & ocupa1==1 & is.na(ocupa1)==FALSE]=trab_pobl$tipo_trab1[pea==1 & is.na(pea)==FALSE & is.na(tipo_trab)==FALSE & ocupa1==1 & is.na(ocupa1)==FALSE]

trab_pobl$tipo_trab2[pea==0 | pea==2 | is.na(pea)==TRUE]=NA
trab_pobl$tipo_trab2[pea==1 & is.na(pea)==FALSE & is.na(tipo_trab)==FALSE & ocupa2==1 & is.na(ocupa2)==FALSE]=trab_pobl$tipo_trab2[pea==1 & is.na(pea)==FALSE & is.na(tipo_trab)==FALSE & ocupa2==1 & is.na(ocupa2)==FALSE]

### Servicios médicos ###

trab_pobl$atemed <-as.numeric(atemed)
trab_pobl$inscr_1 <- as.numeric(inscr_1)
trab_pobl$inscr_2 <-as.numeric(inscr_2)
trab_pobl$inscr_3 <-as.numeric(inscr_3)
trab_pobl$inscr_4 <-as.numeric(inscr_4)
trab_pobl$inscr_5 <-as.numeric(inscr_5)
trab_pobl$inscr_6 <-as.numeric(inscr_6)
trab_pobl$inscr_7 <-as.numeric(inscr_7)
trab_pobl$inscr_8 <-as.numeric(inscr_8)

trab_pobl$inst_1 <- as.numeric(inst_1)
trab_pobl$inst_2 <- as.numeric(inst_2)
trab_pobl$inst_3 <- as.numeric(inst_3)
trab_pobl$inst_4 <- as.numeric(inst_4)
trab_pobl$inst_5 <- as.numeric(inst_5)

trab_pobl$smlab1=NA
trab_pobl$smlab1[ocupa1==1 & atemed==1 &  inscr_1==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4)]=1
trab_pobl$smlab1[ocupa1==1 & is.na(trab_pobl$smlab1)==TRUE]=0
attr(trab_pobl$smlab1,"Descripción") <- "CONEVAL acceso a servicios médicos por prestacion laboral Ocupación Principal"
attributes(trab_pobl$smlab1)

trab_pobl$smlab2=NA
trab_pobl$smlab2[ocupa2==1 & atemed==1 &  inscr_1==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4)]=1
trab_pobl$smlab2[ocupa2==1 & is.na(trab_pobl$smlab2)==TRUE]=0
attr(trab_pobl$smlab2,"Descripción") <- "CONEVAL acceso a servicios médicos por prestacion laboral Ocupación Secundaria"
attributes(trab_pobl$smlab2)

### Servicios médicos (Contratación Voluntaria) ###

trab_pobl$smcv=NA
trab_pobl$smcv[atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & inscr_6==6]=1
trab_pobl$smcv <-recode(trab_pobl$smcv,"NA=0")
attr(trab_pobl$smcv,"Descripción") <- "CONEVAL acceso a servicios médicos contratación voluntaria"
attributes(trab_pobl$smcv)
detach(trab_pobl)

attach(trab_pobl)
trab_pobl$sa_dir=NA
trab_pobl$sa_dir[tipo_trab1==1 & smlab1==1]=1
trab_pobl$sa_dir[tipo_trab1==2 & (smlab1==1 | smcv==1)]=1
trab_pobl$sa_dir[tipo_trab1==3 & (smlab1==1 | smcv==1)]=1
trab_pobl$sa_dir[tipo_trab2==1 & smlab2==1]=1
trab_pobl$sa_dir[tipo_trab2==2 & (smlab2==1 | smcv==1)]=1
trab_pobl$sa_dir[tipo_trab1==3 & (smlab2==1 | smcv==1)]=1

### Criterio 2: Acceso a través de núcleos familiares ###

### Parentesco ###

trab_pobl$par=NA
trab_pobl$par[parentesco==101 | parentesco==102]=1
trab_pobl$par[parentesco>=201 & parentesco<300]=2
trab_pobl$par[parentesco>=300 & parentesco<=400]=3
trab_pobl$par[parentesco==601]=4
trab_pobl$par[parentesco==615]=5
trab_pobl$par[is.na(trab_pobl$par)==TRUE]=6
attr(trab_pobl$par,"Descripción") <- "CONEVAL parentesco"
attributes(trab_pobl$par)
detach(trab_pobl)

### Acceso a seguridad social por jefe de hogar ### 

attach(trab_pobl)
trab_pobl$j_sa=NA
trab_pobl$j_sa[par==1 & sa_dir==1]=1
trab_pobl$j_sa[par==1 & sa_dir==1 & (inst_2==2 | inst_3==3) & inscr_6==6 & (is.na(inst_1)==TRUE  & is.na(inst_4)==TRUE & is.na(inst_5)==TRUE )
               & (is.na(inscr_1)==TRUE & is.na(inscr_2)==TRUE & is.na(inscr_3)==TRUE & is.na(inscr_4)==TRUE & is.na(inscr_5)==TRUE  & is.na(inscr_7)==TRUE & is.na(inscr_8)==TRUE)]=NA
trab_pobl$j_sa[is.na(trab_pobl$j_sa)==TRUE]=0

trab_pobl_1=trab_pobl%>%
  group_by(folioviv,foliohog)%>%
  mutate(jef_sa=max(j_sa))%>%
  as.data.frame()

attr(trab_pobl_1$jef_sa,"Descripción") <-"CONEVAL acceso a servicios de salud por jefe de hogar"
attributes(trab_pobl_1$jef_sa)
detach(trab_pobl)
rm(trab_pobl)

### Acceso a seguridad social por conyuge ###

trab_pobl_1$c_sa=NA
trab_pobl_1$c_sa[trab_pobl_1$par==2 & trab_pobl_1$sa_dir==1]=1
trab_pobl_1$c_sa[is.na(trab_pobl_1$c_sa)==TRUE]=0
trab_pobl=trab_pobl_1%>%group_by(folioviv,foliohog)%>%
  mutate(cony_sa=max(c_sa))%>%
  as.data.frame()

attr(trab_pobl$cony_sa,"Descripción") <- "CONEVAL acceso a servicios de salud por conyuge"
attributes(trab_pobl$cony_sa)

### Acceso a seguridad social por hijo ###

trab_pobl$h_sa=NA
trab_pobl$h_sa[trab_pobl$par==3 & trab_pobl$sa_dir==1]=1
trab_pobl$h_sa[is.na(trab_pobl$h_sa)==TRUE]=0
trab_pobl=trab_pobl%>%
  group_by(folioviv,foliohog)%>%
  mutate(hijo_sa=max(h_sa))%>%
  as.data.frame()

attr(trab_pobl$hijo_sa,"Descripción") <-"CONEVAL acceso a servicios de salud por hijo"
attributes(trab_pobl$hijo_sa)

### Servicios de salud por nucleos o contratación ###

trab_pobl$s_salud=NA
trab_pobl$s_salud[trab_pobl$atemed==1 & (trab_pobl$inst_1==1 | trab_pobl$inst_2==2 | trab_pobl$inst_3==3 | trab_pobl$inst_4==4)]=1
trab_pobl$s_salud[is.na(trab_pobl$s_salud)==TRUE]=0
attr(trab_pobl$s_salud,"descripción") <-"CONEVAL acceso a ss por nucleos familiares o contratación"
attributes(trab_pobl$s_salud)

### Indicador de carencia por servicios de salud ###

trab_pobl$asis_esc <-as.numeric(trab_pobl$asis_esc)
trab_pobl$segvol_2 <-as.numeric(trab_pobl$segvol_2)
trab_pobl$segpop <-as.numeric(trab_pobl$segpop)

trab_pobl$inas_esc=NA
trab_pobl$inas_esc[trab_pobl$asis_esc==2]=1
trab_pobl$inas_esc[trab_pobl$asis_esc==1]=0

trab_pobl$ic_asalud=1
trab_pobl$ic_asalud[trab_pobl$sa_dir==1]=0
trab_pobl$ic_asalud[trab_pobl$par==1 & trab_pobl$cony_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==1 & trab_pobl$pea==0 & trab_pobl$hijo_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==2 & trab_pobl$jef_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==2 & trab_pobl$pea==0 & trab_pobl$hijo_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==3 & trab_pobl$edad<16 & trab_pobl$jef_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==3 & trab_pobl$edad<16 & trab_pobl$cony_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==3 & trab_pobl$edad>=16 & trab_pobl$edad<=25 & trab_pobl$inas_esc==0 & trab_pobl$jef_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==3 & trab_pobl$edad>=16 & trab_pobl$edad<=25 & trab_pobl$inas_esc==0 & trab_pobl$cony_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==4 & trab_pobl$pea==0 & trab_pobl$jef_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$par==5 & trab_pobl$pea==0 & trab_pobl$cony_sa==1]=0
trab_pobl$ic_asalud[trab_pobl$s_salud==1]=0
trab_pobl$ic_asalud[trab_pobl$segpop==1 | (trab_pobl$segpop==2 & trab_pobl$atemed==1 & (trab_pobl$inst_1==1 | trab_pobl$inst_2==2 | trab_pobl$inst_3==3 | trab_pobl$inst_4==4 | trab_pobl$inst_5==5)) | trab_pobl$segvol_2==2]=0

#1 Población afiliada al Seguro Popular
#2 Población afiliada al IMSS
#3 Población afiliada al ISSSTE 
#4 Población afiliada al ISSSTE estatal
#5 Población afiliada a Pemex, Defensa o Marina
#6 Población con seguro privado de gastos médicos
#7 Población con acceso a servicios médicos por seguridad social indirecta2
#8 Población afiliada a otra institución médica distinta de las anteriores

####  Por institucion ###

trab_pobl$c_sal1=0
trab_pobl$c_sal1[trab_pobl$segpop==1]=1

trab_pobl$c_sal2=0 
trab_pobl$c_sal2[trab_pobl$atemed==1 & trab_pobl$inst_1==1]=1

trab_pobl$c_sal3=0
trab_pobl$c_sal3[trab_pobl$atemed==1 & trab_pobl$inst_2==2]=1

trab_pobl$c_sal4=0 
trab_pobl$c_sal4[trab_pobl$atemed==1 & trab_pobl$inst_3==3]=1

trab_pobl$c_sal5=0 
trab_pobl$c_sal5[trab_pobl$atemed==1 & trab_pobl$inst_4==4]=1

trab_pobl$c_sal6=0 
trab_pobl$c_sal6[trab_pobl$segvol_2==2]=1

trab_pobl$c_sal7=0 
trab_pobl$c_sal7[trab_pobl$segpop==2 & trab_pobl$sa_dir!=1 & trab_pobl$ic_asalud==0 & (is.na(trab_pobl$inst_1)==TRUE & is.na(trab_pobl$inst_2)==TRUE & is.na(trab_pobl$inst_3)==TRUE & is.na(trab_pobl$inst_4)==TRUE & is.na(trab_pobl$inst_5)==TRUE)]=1

trab_pobl$c_sal8=0  
trab_pobl$c_sal8[trab_pobl$atemed==1 & trab_pobl$inst_5==5]=1

trab_pobl%>%select(folioviv, foliohog, numren, sexo, ic_asalud, starts_with("sa_"),ends_with("_sa") , segpop, atemed, starts_with("inst_"),
                   starts_with("inscr_"),starts_with("segvol_"),starts_with("c_sal"))%>%
  arrange(folioviv,foliohog,numren)%>%
  as.data.frame()%>%write.dta('Bases/Bases_R/pasalud.dta')

#################################
### ACCESO A SEGURIDAD SOCIAL ### 
#################################  

rm(list=ls())
conc_hogar<-read.dta('Bases/Datos_R/concentradohogar.dta')
conc_hogar%>% select(folioviv, foliohog,tam_loc,factor)%>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta("Bases/Bases_R/concenss.dta")

poblacion<-read.dta('Bases/Datos_R/poblacion.dta')

poblacion$parentesco<-as.numeric(poblacion$parentesco)
poblacion%>%select(folioviv, foliohog, numren, parentesco, sexo, edad, starts_with("act_"), atemed, 
                   starts_with("inscr_"),starts_with("inst_"), segvol_1, segvol_2,asis_esc,starts_with("disc"))%>%
  filter(parentesco<400 | parentesco >=500)%>%
  filter(parentesco<700 | parentesco >=800)%>%
  mutate(disc1=replace(disc1,disc1==" " | disc1=="&",NA))%>%
  arrange( folioviv,foliohog, numren)%>%
  as.data.frame()%>%
  write.dta("Bases/Bases_R/pobss.dta",convert.factors = "string")

Ingresos<-read.dta('Bases/Datos_R/ingresos.dta')

Ingresos=Ingresos[(Ingresos$clave=="P032" | Ingresos$clave=="P033" | Ingresos$clave=="P044" | Ingresos$clave=="P045"),]

mes1 = 0.9963995085
mes2 = 0.9982899814
mes3 = 1.0023544980
mes4 = 0.9997593944
mes5 = 0.9947668274
mes6 = 0.9964338807
mes7 = 0.9978947007
mes8 = 1.0000000000
mes9 = 1.0037465735
mes10  = 1.0089110017
mes11  = 1.0144191522
mes12  = 1.0185524134

### Réplica de Ingresos a ser deflactados

Ingresos$ing_1d<-Ingresos$ing_1
Ingresos$ing_2d<-Ingresos$ing_2
Ingresos$ing_3d<-Ingresos$ing_3
Ingresos$ing_4d<-Ingresos$ing_4
Ingresos$ing_5d<-Ingresos$ing_5
Ingresos$ing_6d<-Ingresos$ing_6

### Todos los rubros de ingreso se deflactan según el mes de referencia
### N: hacer un tab a mes para identificar missings en mes_* 

Ingresos$mes_1<-as.numeric(Ingresos$mes_1)
Ingresos$mes_2<-as.numeric(Ingresos$mes_2)
Ingresos$mes_3<-as.numeric(Ingresos$mes_3)
Ingresos$mes_4<-as.numeric(Ingresos$mes_4)
Ingresos$mes_5<-as.numeric(Ingresos$mes_5)
Ingresos$mes_6<-as.numeric(Ingresos$mes_6)

attach(Ingresos)

Ingresos$ing_1d[(is.na(mes_1==7)!=TRUE & mes_1==7 & is.na(ing_1)!=TRUE)]=Ingresos$ing_1[(is.na(mes_1==7)!=TRUE & mes_1==7 & is.na(ing_1)!=TRUE)]/mes7
Ingresos$ing_1d[(is.na(mes_1==8)!=TRUE & mes_1==8 & is.na(ing_1)!=TRUE)]=Ingresos$ing_1[(is.na(mes_1==8)!=TRUE & mes_1==8 & is.na(ing_1)!=TRUE)]/mes8
Ingresos$ing_1d[(is.na(mes_1==9)!=TRUE & mes_1==9 & is.na(ing_1)!=TRUE)]=Ingresos$ing_1[(is.na(mes_1==9)!=TRUE & mes_1==9 & is.na(ing_1)!=TRUE)]/mes9
Ingresos$ing_1d[(is.na(mes_1==10)!=TRUE & mes_1==10 & is.na(ing_1)!=TRUE)]=Ingresos$ing_1[(is.na(mes_1==10)!=TRUE & mes_1==10 & is.na(ing_1)!=TRUE)]/mes10

Ingresos$ing_2d[(is.na(mes_2==6)!=TRUE & mes_2==6 & is.na(ing_2)!=TRUE)]=Ingresos$ing_2[(is.na(mes_2==7)!=TRUE & mes_2==6 & is.na(ing_2)!=TRUE)]/mes6
Ingresos$ing_2d[(is.na(mes_2==7)!=TRUE & mes_2==7 & is.na(ing_2)!=TRUE)]=Ingresos$ing_2[(is.na(mes_2==8)!=TRUE & mes_2==7 & is.na(ing_2)!=TRUE)]/mes7
Ingresos$ing_2d[(is.na(mes_2==8)!=TRUE & mes_2==8 & is.na(ing_2)!=TRUE)]=Ingresos$ing_2[(is.na(mes_2==9)!=TRUE & mes_2==8 & is.na(ing_2)!=TRUE)]/mes8
Ingresos$ing_2d[(is.na(mes_2==9)!=TRUE & mes_2==9 & is.na(ing_2)!=TRUE)]=Ingresos$ing_2[(is.na(mes_2==10)!=TRUE & mes_2==9 & is.na(ing_2)!=TRUE)]/mes9

Ingresos$ing_3d[(is.na(mes_3==5)!=TRUE & mes_3==5 & is.na(ing_3)!=TRUE)]=Ingresos$ing_3[(is.na(mes_3==5)!=TRUE & mes_3==5 & is.na(ing_3)!=TRUE)]/mes5
Ingresos$ing_3d[(is.na(mes_3==6)!=TRUE & mes_3==6 & is.na(ing_3)!=TRUE)]=Ingresos$ing_3[(is.na(mes_3==6)!=TRUE & mes_3==6 & is.na(ing_3)!=TRUE)]/mes6
Ingresos$ing_3d[(is.na(mes_3==7)!=TRUE & mes_3==7 & is.na(ing_3)!=TRUE)]=Ingresos$ing_3[(is.na(mes_3==7)!=TRUE & mes_3==7 & is.na(ing_3)!=TRUE)]/mes7
Ingresos$ing_3d[(is.na(mes_3==8)!=TRUE & mes_3==8 & is.na(ing_3)!=TRUE)]=Ingresos$ing_3[(is.na(mes_3==8)!=TRUE & mes_3==8 & is.na(ing_3)!=TRUE)]/mes8

Ingresos$ing_4d[(is.na(mes_4==4)!=TRUE & mes_4==4 & is.na(ing_4)!=TRUE)]=Ingresos$ing_4[(is.na(mes_4==4)!=TRUE & mes_4==4 & is.na(ing_4)!=TRUE)]/mes4
Ingresos$ing_4d[(is.na(mes_4==5)!=TRUE & mes_4==5 & is.na(ing_4)!=TRUE)]=Ingresos$ing_4[(is.na(mes_4==5)!=TRUE & mes_4==5 & is.na(ing_4)!=TRUE)]/mes5
Ingresos$ing_4d[(is.na(mes_4==6)!=TRUE & mes_4==6 & is.na(ing_4)!=TRUE)]=Ingresos$ing_4[(is.na(mes_4==6)!=TRUE & mes_4==6 & is.na(ing_4)!=TRUE)]/mes6
Ingresos$ing_4d[(is.na(mes_4==7)!=TRUE & mes_4==7 & is.na(ing_4)!=TRUE)]=Ingresos$ing_4[(is.na(mes_4==7)!=TRUE & mes_4==7 & is.na(ing_4)!=TRUE)]/mes7

Ingresos$ing_5d[(is.na(mes_5==3)!=TRUE & mes_5==3 & is.na(ing_5)!=TRUE)]=Ingresos$ing_5[(is.na(mes_5==3)!=TRUE & mes_5==3 & is.na(ing_5)!=TRUE)]/mes3
Ingresos$ing_5d[(is.na(mes_5==4)!=TRUE & mes_5==4 & is.na(ing_5)!=TRUE)]=Ingresos$ing_5[(is.na(mes_5==4)!=TRUE & mes_5==4 & is.na(ing_5)!=TRUE)]/mes4
Ingresos$ing_5d[(is.na(mes_5==5)!=TRUE & mes_5==5 & is.na(ing_5)!=TRUE)]=Ingresos$ing_5[(is.na(mes_5==5)!=TRUE & mes_5==5 & is.na(ing_5)!=TRUE)]/mes5
Ingresos$ing_5d[(is.na(mes_5==6)!=TRUE & mes_5==6 & is.na(ing_5)!=TRUE)]=Ingresos$ing_5[(is.na(mes_5==6)!=TRUE & mes_5==6 & is.na(ing_5)!=TRUE)]/mes6

Ingresos$ing_6d[(is.na(mes_6==2)!=TRUE & mes_6==2 & is.na(ing_6)!=TRUE)]=Ingresos$ing_6[(is.na(mes_6==2)!=TRUE & mes_6==2 & is.na(ing_6)!=TRUE)]/mes2
Ingresos$ing_6d[(is.na(mes_6==3)!=TRUE & mes_6==3 & is.na(ing_6)!=TRUE)]=Ingresos$ing_6[(is.na(mes_6==3)!=TRUE & mes_6==3 & is.na(ing_6)!=TRUE)]/mes3
Ingresos$ing_6d[(is.na(mes_6==4)!=TRUE & mes_6==4 & is.na(ing_6)!=TRUE)]=Ingresos$ing_6[(is.na(mes_6==4)!=TRUE & mes_6==4 & is.na(ing_6)!=TRUE)]/mes4
Ingresos$ing_6d[(is.na(mes_6==5)!=TRUE & mes_6==5 & is.na(ing_6)!=TRUE)]=Ingresos$ing_6[(is.na(mes_6==5)!=TRUE & mes_6==5 & is.na(ing_6)!=TRUE)]/mes5

### Ingresos de largo plazo ###

Ingresos$ingmon<-rowMeans(data.frame(ing_1d,ing_2d,ing_3d,ing_4d,ing_5d,ing_6d),na.rm = TRUE)
detach(Ingresos)

### 1. Por jubilaciones dentro o fuera del país ### 
attach(Ingresos)
Ingresos$ingmon_jub[(clave=="P032" | clave=="P033") & is.na(clave)!=TRUE]<-Ingresos$ingmon[(clave=="P032" | clave=="P033") & is.na(clave)!=TRUE]

### 2. Ingreso por programa de adultos mayores ###
Ingresos$ingmon_pam[(clave=="P044" | clave=="P045") & is.na(clave)!=TRUE]<-Ingresos$ingmon[(clave=="P044"| clave=="P045") & is.na(clave)!=TRUE]
detach(Ingresos)

Ingresos_1<-summaryBy(ingmon+ingmon_jub+ingmon_pam~folioviv+foliohog+numren,FUN=c(sum),data=Ingresos,na.rm=TRUE)
names(Ingresos_1)<- c("folioviv","foliohog","numren","ingmon","ingmon_jub","ingmon_pam")
Ingresos_1$ing_jub[Ingresos_1$ingmon_jub>0 & is.na(Ingresos_1$ingmon_jub)!=TRUE]=1

attr(Ingresos_1$ing_jub,"Descripción")<-c
table(Ingresos_1$ing_jub,dnn="Ingreso por jubilación real, promedio de 6 meses")
table(Ingresos_1$ingmon_pam,dnn="Ingreso por jubilación real, promedio de 6 meses")

Ingresos_1%>%arrange(folioviv,foliohog,numren)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/ingss.dta')

pob_ss<-read.dta('Bases/Bases_R/pobss.dta') 

Base<-full_join(Ingresos_1,pob_ss,by=c("folioviv", "foliohog","numren"))

Base%>%arrange(folioviv,foliohog,numren)%>%
  as.data.frame()%>%
  write.dta("Bases/Bases_R/ss.dta")

rm(list=ls())

### Base de trabajo ###

trabajos<-read.dta('Bases/Datos_R/trabajos.dta')
trabajos<-select(trabajos,folioviv,foliohog,numren,id_trabajo,subor,indep,pago,tiene_suel,pres_1,pres_8) 
trabajos<-trabajos%>%
  arrange(folioviv,foliohog,numren)%>%
  mutate(pres_1=replace(pres_1,pres_1==" ",""))%>%
  mutate(pres_8=replace(pres_8,pres_8==" ",""))%>%
  as.data.frame()

### Duplicates ###
trabajos$id=paste0(trabajos$folioviv,trabajos$foliohog,trabajos$numren)

df_dupli<-trabajos%>%group_by(id)%>%
  summarise(dupli=n()-1)%>%
  as.data.frame()
trabajos=merge(trabajos,df_dupli,by.x="id",all.x=TRUE)
table(trabajos$dupli,dnn="Duplicados\n O=Duplicados 1=No Duplicados")

trabajos$basetrab=1
base_ss<-read.dta("Bases/Bases_R/ss.dta")

trabajos<-full_join(trabajos,base_ss,by = c("folioviv", "foliohog", "numren"))

##### Criterio 1: Acceso Directo ###
### Ocupada/desocupada ###

trabajos$act_pnea1<-as.numeric(trabajos$act_pnea1)
trabajos$act_pnea2<-as.numeric(trabajos$act_pnea2)

attach(trabajos)
trabajos$act_buscot[act_pnea1==1 | act_pnea2==1]=1
trabajos$act_rento=NA
trabajos$act_pensio[act_pnea1==2 | act_pnea2==2]=3
trabajos$act_quehac[act_pnea1==3 | act_pnea2==3]=4
trabajos$act_estdi[act_pnea1==4 | act_pnea2==4]=5
trabajos$act_discap[act_pnea1==5 | act_pnea2==5]=6
trabajos$act_otra[act_pnea1==6 | act_pnea2==6]=7
detach(trabajos)

trabajos$ocupada[trabajos$basetrab==1 & trabajos$edad>=16]=1
trabajos$ocupada<-recode(trabajos$ocupada,"NA=0")

trabajos$parentesco<-as.numeric(trabajos$parentesco)
trabajos$act_buscot<-as.numeric(trabajos$act_buscot)
trabajos$act_rento<-as.numeric(trabajos$act_rento)
trabajos$act_pensio<-as.numeric(trabajos$act_pensio)
trabajos$act_quehac<-as.numeric(trabajos$act_quehac)
trabajos$act_estdi<-as.numeric(trabajos$act_estdi)
trabajos$act_discap<-as.numeric(trabajos$act_discap)
trabajos$act_otra<-as.numeric(trabajos$act_otra)

attach(trabajos)
trabajos$desocupada[edad>=16 & parentesco<400 & act_buscot==1]=1
trabajos$desocupada[edad>=16 & (parentesco>=500 & parentesco<700) & act_buscot==1]=1
trabajos$desocupada<-recode(trabajos$desocupada,"NA=0")

### PNA ###

trabajos$pna[edad>=16 & is.na(act_buscot)==TRUE & parentesco<400 & 
               (act_rento == 2 | act_pensio== 3 |act_quehac== 4 | act_estdi== 5 | act_discap== 6 |  act_otra== 7)]=1
trabajos$pna[edad>=16 & is.na(act_buscot)==TRUE & (parentesco>=500 & parentesco<700) & 
               (act_rento == 2 | act_pensio== 3 | act_quehac== 4 | act_estdi== 5 | act_discap== 6 |  act_otra== 7)]=1
trabajos$pna<-recode(trabajos$pna,"NA=0")
detach(trabajos)

### PEA ###
attach(trabajos)
trabajos$pea[ocupada==1 & edad>=16]=1
trabajos$pea[desocupada==1 & edad>=16]=2
trabajos$pea[pna==1 & edad>=16]=0
attr(trabajos$pea,"Descripción")<- "CONEVAL población económicamente activa"
table(trabajos$pea,dnn="CONEVAL población económicamente activa")
detach(trabajos)

### Tipo de trabajo ###
trabajos$subor<-as.numeric(trabajos$subor)
trabajos$indep<-as.numeric(trabajos$indep)
trabajos$tiene_suel<-as.numeric(trabajos$tiene_suel)
trabajos$pago<-as.numeric(trabajos$pago)
trabajos$atemed<-as.numeric(trabajos$atemed)
trabajos$inst_1<-as.numeric(trabajos$inst_1)
trabajos$inst_2<-as.numeric(trabajos$inst_2)
trabajos$inst_3<-as.numeric(trabajos$inst_3)
trabajos$inst_4<-as.numeric(trabajos$inst_4)
trabajos$inst_5<-as.numeric(trabajos$inst_5)
trabajos$inscr_1<-as.numeric(trabajos$inscr_1)
trabajos$inscr_2<-as.numeric(trabajos$inscr_2)
trabajos$inscr_3<-as.numeric(trabajos$inscr_3)
trabajos$inscr_4<-as.numeric(trabajos$inscr_4)
trabajos$inscr_5<-as.numeric(trabajos$inscr_5)
trabajos$pres_1<-as.numeric(trabajos$pres_1)
trabajos$pres_8<-as.numeric(trabajos$pres_8)
trabajos$segvol_1<-as.numeric(trabajos$segvol_1)

attach(trabajos)
trabajos$tipo_trab[pea==1 & subor==1]=1
trabajos$tipo_trab[pea==1 & indep==1 & tiene_suel==1]=2
trabajos$tipo_trab[pea==1 & indep==2 & subor==2 & pago==1]=2
trabajos$tipo_trab[pea==1 & indep==1 & tiene_suel==2]=3
trabajos$tipo_trab[pea==1 & indep==2 & subor==2 & (pago==2 | pago==3)]=3
attr(trabajos$tipo_trab,"Descripción")<-"CONEVAL tipo de trabajo"
table(trabajos$tipo_trab,dnn="CONEVAL tipo de trabajo")

### Servicios médicos ###
trabajos$smlab[atemed==1 &  inscr_1==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4)]=1
attr(trabajos$smlab,"Descripción")<-"CONEVAL acceso a servicios médicos por prestacion laboral"
table(trabajos$tipo_trab,dnn="CONEVAL acceso a servicios médicos por prestacion laboral")

### Incapacidad con goce de sueldo ###
trabajos$inclab[pres_1==1]=1
trabajos$inclab<-recode(trabajos$inclab,"NA=0")
attr(trabajos$smlab,"Descripción")<-"CONEVAL incapacidad con sueldo por prestacion laboral"
attributes(trabajos$smlab)
table(trabajos$SMLAB,dnn="CONEVAL incapacidad con sueldo por prestacion laboral")

### Afore ###
trabajos$aforlab[pres_8==8]=1
trabajos$aforlab<-recode(trabajos$aforlab,"NA=0")
attr(trabajos$aforlab,"Descripción")<-"CONEVAL acceso a pension o jubilación para retiro"
attributes(trabajos$aforlab)
table(trabajos$aforlab,dnn="CONEVAL acceso a pension o jubilación para retiro")

### Servicios médicos (Contratación Voluntaria) ### 
trabajos$smcv[atemed==1 & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & inscr_6==6]=1
trabajos$smcv<-recode(trabajos$smcv,"NA=0")
table(trabajos$aforlab,dnn="CONEVAL acceso a servicios médicos contratación voluntaria")

### Afore (Contratación Voluntaria) ###
trabajos$aforecv[segvol_1==1]=1
trabajos$aforecv<-recode(trabajos$aforecv,"NA=0")
table(trabajos$aforecv,dnn="CONEVAL acceso a afore por contratación voluntaria")

### Jubilados y pensionados ###
trabajos$jub[act_pensio==3]=1
trabajos$jub[ing_jub==1]=1
trabajos$jub[inscr_2==2]=1
trabajos$jub<-recode(trabajos$jub,"NA=0")
table(trabajos$aforecv,dnn="CONEVAL jubilados y pensionados")
detach(trabajos)

### Acceso Directo a Seguridad Social ###
attach(trabajos)
trabajos$ss_dir[tipo_trab==1 & inclab==1 & aforlab==1 & smlab==1]=1
trabajos$ss_dir[tipo_trab==2 & (aforlab==1 | aforecv==1) & (smlab==1 | smcv==1)]=1
trabajos$ss_dir[tipo_trab==3 & aforecv==1 & (smlab==1 | smcv==1)]=1
trabajos$ss_dir[jub==1]=1
trabajos$ss_dir<-recode(trabajos$ss_dir,"NA=0")
detach(trabajos)

### Se asigna SS si esta proeien del segundo empleo ###
trabajos$idi<-paste0(trabajos$folioviv,trabajos$foliohog,trabajos$numren)
trabajos=trabajos%>%group_by(idi)%>%mutate(ss_dir2=ifelse(dupli==1,max(ss_dir,na.rm=TRUE),NA))%>%as.data.frame()
trabajos$ss_dir3<-trabajos$ss_dir
trabajos$ss_dir3[trabajos$dupli==1 & is.na(trabajos$dupli)==FALSE]=trabajos$ss_dir2[trabajos$dupli==1 & is.na(trabajos$dupli)==FALSE]

trabajos$id_trabajo<-recode(trabajos$id_trabajo,"NA=0")
trabajos=trabajos[trabajos$id_trabajo!="2",]
trabajos=trabajos[!duplicated(trabajos$idi),]

trabajos=rename(trabajos,ss_dir4=ss_dir)
trabajos=rename(trabajos,ss_dir=ss_dir3)
attr(trabajos$ss_dir,"Descripción")<-"CONEVAL acceso directo a seguridad social"
attributes(trabajos$ss_dir)
table(trabajos$ss_dir,dnn="CONEVAL acceso directo a seguridad social")

### Criterio 2: Acceso a través de núcleos familiares ###
### Parentesco ###

trabajos$par[trabajos$parentesco >=100 & trabajos$parentesco <200]=1
trabajos$par[trabajos$parentesco >=201 & trabajos$parentesco <300]=2
trabajos$par[trabajos$parentesco >=300 & trabajos$parentesco <=400]=3
trabajos$par[trabajos$parentesco == 601]=4
trabajos$par[trabajos$parentesco == 615]=5
trabajos$par[is.na(trabajos$parentesco) == TRUE |(trabajos$parentesco >400) & (trabajos$parentesco <=600)|
               (trabajos$parentesco >601) & (trabajos$parentesco <=614)|(trabajos$parentesco >=616)]=6

attr(trabajos$par,"Descripción")<-"CONEVAL parentesco"
attributes(trabajos$par)
table(trabajos$par,dnn="CONEVAL parentesco")

### Acceso a seguridad social por jefe de hogar ### 
attach(trabajos)
trabajos$j_ss[par==1 & ss_dir==1]=1
trabajos$j_ss[is.na(trabajos$j_ss)]=0
trabajos=trabajos%>%group_by(folioviv,foliohog)%>%mutate(jef_ss=max(j_ss))%>%as.data.frame()

attr(trabajos$j_ss,"Descripción")<-"CONEVAL acceso a seguridad social por jefe de hogar"
attributes(trabajos$j_ss)
table(trabajos$j_ss,dnn="CONEVAL acceso a seguridad social por jefe de hogar")

### Acceso a seguridad social por conyuge ###

trabajos$c_ss[par==2 & ss_dir==1]=1
trabajos$c_ss[is.na(trabajos$c_ss)]=0
trabajos=trabajos%>%group_by(folioviv,foliohog)%>%mutate(cony_ss=max(c_ss))%>%as.data.frame()
attr(trabajos$j_ss,"Descripción")<-"CONEVAL acceso a seguridad social por conyuge"
attributes(trabajos$j_ss)
table(trabajos$j_ss,dnn="CONEVAL acceso a seguridad social por conyuge")

### Acceso a seguridad social por hijo ###
trabajos$h_ss[par==3 & ss_dir==1 & jub==0]=1
trabajos$h_ss[par==3 & ss_dir==1 & jub==1 & edad>25]=1
trabajos$h_ss[is.na(trabajos$h_ss)==TRUE]=0  
trabajos=trabajos%>%group_by(folioviv,foliohog)%>%mutate(hijo_ss=max(h_ss))%>%as.data.frame()
table(trabajos$h_ss,dnn="CONEVAL acceso a seguridad social por hijo")
detach(trabajos)

### Criterio 3: Acceso a través de núcleos familiares ###
### servicios de salud por nucleos o contratación ###
attach(trabajos)
trabajos$s_salud[atemed==1 & (inscr_3==3 | inscr_4==4 | inscr_6==6 | inscr_7==7) & (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4)]=1 
trabajos$s_salud[is.na(trabajos$s_salud)]=0
table(trabajos$h_ss,dnn="CONEVAL acceso a ss por nucleos familiares o contratación")

### Criterio 4: Acceso a través de programas sociales ###

trabajos=trabajos%>%arrange(folioviv,foliohog)%>%write.dta('Bases/Bases_R/ss.dta')
trabajos<-read.dta('Bases/Bases_R/ss.dta')
concess<-read.dta('Bases/Bases_R/concenss.dta')
base<-full_join(trabajos,concess,by=c("folioviv","foliohog"))
detach(trabajos)

### Programa de adultos mayores ###
attach(base)
base$pam[edad>=65 & ingmon_pam>0]=1
base$pam[ingmon_pam<=0]=0

### Indicador de carencia por acceso a seguridad social ###
base$asis_esc<-as.numeric(base$asis_esc)

base$inas_esc[base$asis_esc==2]=1 
base$inas_esc[base$asis_esc==1]=0

base$ic_ss=1
base$ic_ss[base$ss_dir==1]=0
base$ic_ss[base$par==1 & base$cony_ss==1]=0
base$ic_ss[base$par==1 & base$pea==0 & base$hijo_ss==1]=0
base$ic_ss[base$par==2 & base$jef_ss==1 ]=0
base$ic_ss[base$par==2 & base$pea==0 & hijo_ss==1]=0
base$ic_ss[base$par==3 & base$edad<16 & base$jef_ss==1]=0
base$ic_ss[base$par==3 & base$edad<16 & base$cony_ss==1]=0
base$ic_ss[base$par==3 & base$edad>=16 & base$edad<=25 & base$inas_esc==0 & base$jef_ss==1]=0
base$ic_ss[base$par==3 & base$edad>=16 & base$edad<=25 & base$inas_esc==0 & base$cony_ss==1]=0
base$ic_ss[base$par==4 & base$pea==0 & base$jef_ss==1]=0
base$ic_ss[base$par==5 & base$pea==0 & base$cony_ss==1]=0
base$ic_ss[base$s_salud==1]=0  
base$ic_ss[base$pam==1]=0
table(base$ic_ss,dnn="CONEVAL Indicador de carencia por acceso a seguridad social")

base$disc1<-as.numeric(base$disc1)
base$disc2<-as.numeric(base$disc2)
base$disc3<-as.numeric(base$disc3)
base$disc4<-as.numeric(base$disc4)
base$disc5<-as.numeric(base$disc5)
base$disc6<-as.numeric(base$disc6)
base$disc7<-as.numeric(base$disc7)

base$discap=NA
base$discap[base$disc1>=1 & base$disc1<=7]=1
base$discap[disc2>=2 & disc2<=7]=1
base$discap[disc3>=3 & disc3<=7]=1
base$discap[disc4>=4 & disc4<=7]=1
base$discap[disc5>=5 & disc5<=7]=1
base$discap[disc6>=6 & disc6<=7]=1
base$discap[disc7==7]=1
base$discap[disc1==8|is.na(disc1)==FALSE]=0
detach(base)
base%>%select(folioviv,foliohog,numren,pea,tipo_trab,smlab,inclab,aforlab,smcv,aforecv,jub,ss_dir,par,jef_ss,cony_ss,hijo_ss,s_salud,pam,ic_ss,ic_ss,discap,edad,pea,pna)%>%
  arrange(folioviv,foliohog,numren)%>%
  as.data.frame()%>%
  write.dta("Bases/Bases_R/pss.dta")

########################
##### ALIMENTACIÓN #####
########################

rm(list=ls())

pob <- read.dta('Bases/Datos_R/poblacion.dta')    
pob$parentesco<-as.numeric(pob$parentesco)

pob_1=pob%>%
  filter(parentesco<400 | parentesco >=500)%>%
  filter(parentesco<700 | parentesco >=800)%>%
  mutate(inst_1=replace(inst_1,inst_1==" ",""))%>%
  mutate(inst_2=replace(inst_2,inst_2==" ",""))%>%
  mutate(inst_3=replace(inst_3,inst_3==" ",""))%>%
  mutate(inst_4=replace(inst_4,inst_4==" ",""))%>%
  mutate(inst_5=replace(inst_5,inst_5==" ",""))%>%
  mutate(inscr_1=replace(inscr_1,inscr_1==" ",""))%>%
  mutate(inscr_2=replace(inscr_2,inscr_2==" ",""))%>%
  mutate(inscr_3=replace(inscr_3,inscr_3==" ",""))%>%
  mutate(inscr_4=replace(inscr_4,inscr_4==" ",""))%>%
  mutate(inscr_5=replace(inscr_5,inscr_5==" ",""))%>%
  mutate(inscr_6=replace(inscr_6,inscr_6==" ",""))%>%
  mutate(inscr_7=replace(inscr_7,inscr_7==" ",""))%>%
  mutate(atemed=replace(atemed,atemed==" ",""))%>%
  mutate(segpop=replace(segpop,segpop==" ",""))%>%
  mutate(segvol_1=replace(segvol_1,segpop==" ",""))%>%
  mutate(disc1=replace(disc1,disc1==" " | disc1=="&",""))%>%
  as.data.frame()

pob_1$id_men=NA
pob_1$id_men[pob_1$edad<18]=1
pob_1$id_men[pob_1$edad>=18]=0

pob_1%>%
  select(folioviv,foliohog,id_men)%>%
  group_by(folioviv,foliohog)%>%
  summarise(id_men=max(id_men)) %>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/menores_de_edad.dta')

hog<-read.dta('Bases/Datos_R/hogares.dta')
menores <-read.dta('Bases/Bases_R/menores_de_edad.dta')

hog_men=hog%>%
  arrange(folioviv,foliohog)%>%
  merge(menores,by=c("folioviv","foliohog"))%>%
  as.data.frame()

hog_men$acc_alim1<-as.numeric(hog_men$acc_alim1)
hog_men$acc_alim2<-as.numeric(hog_men$acc_alim2)
hog_men$acc_alim3<-as.numeric(hog_men$acc_alim3)
hog_men$acc_alim4<-as.numeric(hog_men$acc_alim4)
hog_men$acc_alim5<-as.numeric(hog_men$acc_alim5)
hog_men$acc_alim6<-as.numeric(hog_men$acc_alim6)
hog_men$acc_alim7<-as.numeric(hog_men$acc_alim7)
hog_men$acc_alim8<-as.numeric(hog_men$acc_alim8)
hog_men$acc_alim9<-as.numeric(hog_men$acc_alim9)
hog_men$acc_alim10<-as.numeric(hog_men$acc_alim10)
hog_men$acc_alim11<-as.numeric(hog_men$acc_alim11)
hog_men$acc_alim12<-as.numeric(hog_men$acc_alim12)
hog_men$acc_alim13<-as.numeric(hog_men$acc_alim13)
hog_men$acc_alim14<-as.numeric(hog_men$acc_alim14)
hog_men$acc_alim15<-as.numeric(hog_men$acc_alim15)
hog_men$acc_alim16<-as.numeric(hog_men$acc_alim16)
hog_men$acc_alim18<-as.numeric(hog_men$acc_alim18)


names(hog_men)[names(hog_men)=="acc_alim4"]<-"ia_1"
hog_men$ia_1[hog_men$ia_1==2|is.na(hog_men$ia_1)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim5"]<-"ia_2"
hog_men$ia_2[hog_men$ia_2==2|is.na(hog_men$ia_2)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim6"]<-"ia_3"
hog_men$ia_3[hog_men$ia_3==2|is.na(hog_men$ia_3)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim2"]<-"ia_4"
hog_men$ia_4[hog_men$ia_4==2|is.na(hog_men$ia_4)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim7"]<-"ia_5"
hog_men$ia_5[hog_men$ia_5==2|is.na(hog_men$ia_5)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim8"]<-"ia_6"
hog_men$ia_6[hog_men$ia_6==2|is.na(hog_men$ia_6)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim11"]<-"ia_7"
hog_men$ia_7[hog_men$ia_7==2|is.na(hog_men$ia_7)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim12"]<-"ia_8"
hog_men$ia_8[hog_men$ia_8==2|is.na(hog_men$ia_8)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim13"]<-"ia_9"
hog_men$ia_9[hog_men$ia_9==2|is.na(hog_men$ia_9)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim14"]<-"ia_10"
hog_men$ia_10[hog_men$ia_10==2|is.na(hog_men$ia_10)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim15"]<-"ia_11"
hog_men$ia_11[hog_men$ia_11==2|is.na(hog_men$ia_11)==TRUE]=0

names(hog_men)[names(hog_men)=="acc_alim16"]<-"ia_12"
hog_men$ia_12[hog_men$ia_12==2|is.na(hog_men$ia_12)==TRUE]=0

hog_men$tot_ia_ad[hog_men$id_men==0]=with(hog_men[hog_men$id_men==0,],ia_1+ia_2+ia_3+ia_4+ia_5+ia_6)
hog_men$tot_ia_men[hog_men$id_men==1]=with(hog_men[hog_men$id_men==1,],ia_1+ia_2+ia_3+ia_4+ia_5+ia_6+ia_7+ia_8+ia_9+ia_10+ia_11+ia_12)

attach(hog_men)
hog_men$ins_ali[tot_ia_ad==0]=0
hog_men$ins_ali[tot_ia_ad==1|tot_ia_ad==2]=1
hog_men$ins_ali[tot_ia_ad==3|tot_ia_ad==4]=2
hog_men$ins_ali[tot_ia_ad==5|tot_ia_ad==6]=3
hog_men$ins_ali[tot_ia_men==0]=0
hog_men$ins_ali[tot_ia_men==1|tot_ia_men==2|tot_ia_men==3]=1
hog_men$ins_ali[tot_ia_men==4|tot_ia_men==5|tot_ia_men==6|tot_ia_men==7]=2
hog_men$ins_ali[tot_ia_men==8|tot_ia_men==9|tot_ia_men==10|tot_ia_men==11|tot_ia_men==12]=3
detach(hog_men)

hog_men$ic_ali[hog_men$ins_ali==2|hog_men$ins_ali==3]=1
hog_men$ic_ali[hog_men$ins_ali==0|hog_men$ins_ali==1]=0
hog_men%>% arrange(folioviv,foliohog)%>%
        as.data.frame()%>%
        write.dta('Bases/Bases_R/palimentacion.dta')

###################################
##### ESCALAS DE EQUIVALENCIA #####
###################################

rm(list=ls())
pob<-read.dta('Bases/Datos_R/poblacion.dta')

pob_1=pob%>%
  mutate(inst_1=replace(inst_1,inst_1==" ",""))%>%
  mutate(inst_2=replace(inst_2,inst_2==" ",""))%>%
  mutate(inst_3=replace(inst_3,inst_3==" ",""))%>%
  mutate(inst_4=replace(inst_4,inst_4==" ",""))%>%
  mutate(inst_5=replace(inst_5,inst_5==" ",""))%>%
  mutate(inscr_1=replace(inscr_1,inscr_1==" ",""))%>%
  mutate(inscr_2=replace(inscr_2,inscr_2==" ",""))%>%
  mutate(inscr_3=replace(inscr_3,inscr_3==" ",""))%>%
  mutate(inscr_4=replace(inscr_4,inscr_4==" ",""))%>%
  mutate(inscr_5=replace(inscr_5,inscr_5==" ",""))%>%
  mutate(inscr_6=replace(inscr_6,inscr_6==" ",""))%>%
  mutate(inscr_7=replace(inscr_7,inscr_7==" ",""))%>%
  mutate(atemed=replace(atemed,atemed==" ",""))%>%
  mutate(segpop=replace(segpop,segpop==" ",""))%>%
  mutate(segvol_1=replace(segvol_1,segpop==" ",""))%>%
  mutate(disc1=replace(disc1,disc1==" " | disc1=="&",""))%>%
  as.data.frame()

con_hog <-read.dta('Bases/Datos_R/concentradohogar.dta')
pob_hog<-merge(pob_1,con_hog,by=c("folioviv","foliohog"))

pob_hog$una_persona=NA
pob_hog$una_persona[pob_hog$tot_integ==1]=1
pob_hog$una_persona[pob_hog$tot_integ>1 & is.na(pob_hog$tot_integ)==FALSE]=0

pob_hog$una_persona=NA
pob_hog$una_persona[pob_hog$tot_integ==1]=1
pob_hog$una_persona[pob_hog$tot_integ>1 & is.na(pob_hog$tot_integ)==FALSE]=0

pob_hog$parentesco <-as.numeric(pob_hog$parentesco)
attach(pob_hog)
pob_hog$escala=NA
pob_hog$escala[una_persona==1]=1
pob_hog$escala[edad>=0 & edad<=5 & una_persona==0]=0.7031
pob_hog$escala[edad>=6 & edad<=12 & una_persona==0]=0.7382
pob_hog$escala[edad>=13 & edad<=18 & una_persona==0]=0.7057
pob_hog$escala[edad>=19 & edad<=65 & una_persona==0]=0.9945
pob_hog$escala[edad>=66 & una_persona==0]=0.9945
pob_hog$escala[(parentesco>=400 & parentesco<500)|(parentesco>=700 & parentesco<800)]=0
detach(pob_hog)

pob_hog%>%
  select(folioviv,foliohog,escala)%>%
  group_by(folioviv,foliohog)%>%
  summarise(escala=sum(escala)) %>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/escala_equivalencia.dta')

#############################
##### INGRESO MONETARIO #####
#############################

rm(list=ls())

trab <-read.dta('Bases/Datos_R/trabajos.dta')

trab$pres_1=ifelse(trab$pres_1==" ","",trab$pres_1)
trab$pres_8=ifelse(trab$pres_8==" ","",trab$pres_8)

trab_2=trab%>%select(folioviv,foliohog,numren,id_trabajo,pres_2)%>%as.data.frame()

trab_2$pres_2 <-as.numeric(trab_2$pres_2)
trab_2$id_trabajo <- as.numeric(trab_2$id_trabajo)

trab_3=reshape(trab_2, idvar = c( "folioviv", "foliohog", "numren"), timevar="id_trabajo", direction = "wide")
names(trab_3)[4:5] <- c("pres_21", "pres_22")

trab_3$aguinaldo_1=NA
trab_3$aguinaldo_1[trab_3$pres_21==2]=1
trab_3$aguinaldo_1=recode(trab_3$aguinaldo_1,"NA=0")

trab_3$aguinaldo_2=NA
trab_3$aguinaldo_2[trab_3$pres_22==2]=1
trab_3$aguinaldo_2=recode(trab_3$aguinaldo_2,"NA=0")

trab_3%>%select(folioviv, foliohog, numren, aguinaldo_1,aguinaldo_2)%>%
  arrange(folioviv,foliohog,numren)%>%as.data.frame()%>%write.dta('Bases/Bases_R/aguinaldo.dta')

ingr<-read.dta('Bases/Datos_R/ingresos.dta')

### Duplicados ###
nombres<-names(ingr)
a<-unlist(lapply(ingr,function(i)length(unique(i))))
b<-unlist(lapply(ingr,function(i)sum(duplicated(i))))
M<-matrix(c(unname(a),unname(b)),ncol=2,nrow=17)
colnames(M)<-c("Observaciones","Duplicados")
rownames(M)<-nombres
### Tabla de duplicados ###
print(M) 

ingr_2<-ingr%>%arrange(folioviv,foliohog,numren)%>%as.data.frame()
aguinaldo<-read.dta('Bases/Bases_R/aguinaldo.dta')

ingr_agu<-full_join(ingr_2,aguinaldo,by=c("folioviv", "foliohog", "numren"))

ingr_agu<-arrange(ingr_agu,folioviv,foliohog,numren)
c1=(ingr_agu$aguinaldo_1!=1 & is.na(ingr_agu$clave)==FALSE & ingr_agu$clave=="P009")
ingresos=ingr_agu[!c1,]
c2=(ingresos$aguinaldo_2!=1 & is.na(ingresos$clave)==FALSE & ingresos$clave=="P016")

ingresos2=ingresos[!c2,]
rm ("a","aguinaldo","b","c1","c2","ingr","ingr_2","ingr_agu","ingresos","M","nombres",  
    "trab","trab_2","trab_3")   

ingresos=ingresos2
rm(ingresos2)

### Definición de los deflactores 2015 ###

dic14	=	0.9973017796
ene15	=	0.9963995085
feb15	=	0.9982899814
mar15	=	1.0023544980
abr15	=	0.9997593944
may15	=	0.9947668274
jun15	=	0.9964338807
jul15	=	0.9978947007
ago15	=	1.0000000000
sep15	=	1.0037465735
oct15 =	1.0089110017
nov15	=	1.0144191522
dic15	=	1.0185524134

ingresos$mes_1<-as.numeric(ingresos$mes_1)
ingresos$mes_2<-as.numeric(ingresos$mes_2)
ingresos$mes_3<-as.numeric(ingresos$mes_3)
ingresos$mes_4<-as.numeric(ingresos$mes_4)
ingresos$mes_5<-as.numeric(ingresos$mes_5)
ingresos$mes_6<-as.numeric(ingresos$mes_6)

attach(ingresos)

ingresos$ing_6[mes_6==2 & is.na(mes_6)== FALSE]=ingresos$ing_6[mes_6==2 & is.na(mes_6)== FALSE]/feb15
ingresos$ing_6[mes_6==3 & is.na(mes_6)== FALSE ]=ingresos$ing_6[mes_6==3 & is.na(mes_6)!=TRUE ]/mar15
ingresos$ing_6[mes_6==4 & is.na(mes_6)== FALSE ]=ingresos$ing_6[mes_6==4 & is.na(mes_6)!=TRUE ]/abr15
ingresos$ing_6[mes_6==5 & is.na(mes_6)== FALSE ]=ingresos$ing_6[mes_6==5 & is.na(mes_6)!=TRUE ]/may15
ingresos$ing_6[mes_6==6 & is.na(mes_6)== FALSE ]=ingresos$ing_6[mes_6==6 & is.na(mes_6)!=TRUE ]/jun15

ingresos$ing_5[mes_5==3 & is.na(mes_5)!=TRUE ]=ingresos$ing_5[mes_5==3 & is.na(mes_5)!=TRUE ]/mar15
ingresos$ing_5[mes_5==4 & is.na(mes_5)!=TRUE ]=ingresos$ing_5[mes_5==4 & is.na(mes_5)!=TRUE ]/abr15
ingresos$ing_5[mes_5==5 & is.na(mes_5)!=TRUE ]=ingresos$ing_5[mes_5==5 & is.na(mes_5)!=TRUE ]/may15
ingresos$ing_5[mes_5==6 & is.na(mes_5)!=TRUE ]=ingresos$ing_5[mes_5==6 & is.na(mes_5)!=TRUE ]/jun15
ingresos$ing_5[mes_5==7 & is.na(mes_5)!=TRUE ]=ingresos$ing_5[mes_5==7 & is.na(mes_5)!=TRUE ]/jul15

ingresos$ing_4[mes_4==4 & is.na(mes_4)!=TRUE ]=ingresos$ing_4[mes_4==4 & is.na(mes_4)!=TRUE ]/abr15
ingresos$ing_4[mes_4==5 & is.na(mes_4)!=TRUE ]=ingresos$ing_4[mes_4==5 & is.na(mes_4)!=TRUE ]/may15
ingresos$ing_4[mes_4==6 & is.na(mes_4)!=TRUE ]=ingresos$ing_4[mes_4==6 & is.na(mes_4)!=TRUE ]/jun15
ingresos$ing_4[mes_4==7 & is.na(mes_4)!=TRUE ]=ingresos$ing_4[mes_4==7 & is.na(mes_4)!=TRUE ]/jul15
ingresos$ing_4[mes_4==8 & is.na(mes_4)!=TRUE ]=ingresos$ing_4[mes_4==8 & is.na(mes_4)!=TRUE ]/ago15

ingresos$ing_3[mes_3==5 & is.na(mes_3)!=TRUE ]=ingresos$ing_3[mes_3==5 & is.na(mes_3)!=TRUE ]/may15
ingresos$ing_3[mes_3==6 & is.na(mes_3)!=TRUE ]=ingresos$ing_3[mes_3==6 & is.na(mes_3)!=TRUE ]/jun15
ingresos$ing_3[mes_3==7 & is.na(mes_3)!=TRUE ]=ingresos$ing_3[mes_3==7 & is.na(mes_3)!=TRUE ]/jul15
ingresos$ing_3[mes_3==8 & is.na(mes_3)!=TRUE ]=ingresos$ing_3[mes_3==8 & is.na(mes_3)!=TRUE ]/ago15
ingresos$ing_3[mes_3==9 & is.na(mes_3)!=TRUE ]=ingresos$ing_3[mes_3==9 & is.na(mes_3)!=TRUE ]/sep15

ingresos$ing_2[mes_2==6 & is.na(mes_2)!=TRUE ]=ingresos$ing_2[mes_2==6 & is.na(mes_2)!=TRUE ]/jun15
ingresos$ing_2[mes_2==7 & is.na(mes_2)!=TRUE ]=ingresos$ing_2[mes_2==7 & is.na(mes_2)!=TRUE ]/jul15
ingresos$ing_2[mes_2==8 & is.na(mes_2)!=TRUE ]=ingresos$ing_2[mes_2==8 & is.na(mes_2)!=TRUE ]/ago15
ingresos$ing_2[mes_2==9 & is.na(mes_2)!=TRUE ]=ingresos$ing_2[mes_2==9 & is.na(mes_2)!=TRUE ]/sep15
ingresos$ing_2[mes_2==10 & is.na(mes_2)!=TRUE ]=ingresos$ing_2[mes_2==10 & is.na(mes_2)!=TRUE ]/oct15

ingresos$ing_1[mes_1==7 & is.na(mes_1)!=TRUE ]=ingresos$ing_1[mes_1==7 & is.na(mes_1)!=TRUE ]/jul15
ingresos$ing_1[mes_1==8 & is.na(mes_1)!=TRUE ]=ingresos$ing_1[mes_1==8 & is.na(mes_1)!=TRUE ]/ago15
ingresos$ing_1[mes_1==9 & is.na(mes_1)!=TRUE ]=ingresos$ing_1[mes_1==9 & is.na(mes_1)!=TRUE ]/sep15
ingresos$ing_1[mes_1==10 & is.na(mes_1)!=TRUE ]=ingresos$ing_1[mes_1==10 & is.na(mes_1)!=TRUE ]/oct15
ingresos$ing_1[mes_1==11 & is.na(mes_1)!=TRUE ]=ingresos$ing_1[mes_1==11 & is.na(mes_1)!=TRUE ]/nov15
detach(ingresos)

### Se generan los ingresos a largo plazo ###
### Reparto de utilidades ###
ingresos$ing_1[is.na(ingresos$ing_1)== FALSE  & (ingresos$clave=="P008" | ingresos$clave=="P015")]=
ingresos$ing_1[is.na(ingresos$ing_1)== FALSE  & (ingresos$clave=="P008" | ingresos$clave=="P015")]/(12*may15)

### Aguinaldo ###
attach(ingresos)
ingresos$ing_1[is.na(ingresos$ing_1)== FALSE  & (clave=="P009" | clave=="P016")]=ingresos$ing_1[is.na(ingresos$ing_1)== FALSE & (clave=="P009" | clave=="P016")]/(12*dic14)
A1<-data.frame(ingresos$ing_1,ingresos$ing_2, ingresos$ing_3,ingresos$ing_4,ingresos$ing_5,ingresos$ing_6)
ingresos$ing_mens=rowMeans(A1, na.rm = TRUE)

ingresos$ing_2[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE]<-recode(ingresos$ing_2[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE],"0=NA")  
ingresos$ing_3[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE]<-recode(ingresos$ing_3[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE],"0=NA")  
ingresos$ing_4[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE]<-recode(ingresos$ing_4[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE ],"0=NA")  
ingresos$ing_5[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE]<-recode(ingresos$ing_5[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE],"0=NA")  
ingresos$ing_6[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE]<-recode(ingresos$ing_6[(clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016") & is.na(clave)==FALSE],"0=NA")  

#Se define la categoria de ingreso monetario
ingresos$ing_mon=NA

for(i in 1:9){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_mon[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

for(i in 11:16){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_mon[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

for(i in 18:48){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_mon[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

for(i in 67:81){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_mon[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

#Se define la categoria de ingreso laboral
ingresos$ing_lab=NA

for(i in 1:9){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_lab[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}
for(i in 11:16){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_lab[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}
for(i in 18:22){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_lab[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}
for(i in 67:81){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_lab[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

#Se define la categoria de ingreso por remuneraciones del trabajo 

ingresos$ing_rem=NA

for(i in 1:9){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_rem[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

ingresos$ing_rem[(clave=="P011" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P011" & is.na(clave)!=TRUE)]

for(i in 13:16){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_rem[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

ingresos$ing_rem[(clave=="P018" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P018" & is.na(clave)!=TRUE)]
ingresos$ing_rem[(clave=="P020" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P020" & is.na(clave)!=TRUE)]
ingresos$ing_rem[(clave=="P067" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P067" & is.na(clave)!=TRUE)]

#Se define la categoria de ingreso por negocios propios

ingresos$ing_neg=NA
for(i in 68:81){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_neg[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

#Se define otros ingresos 

ingresos$ing_otro=NA
for(i in 21:22){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_otro[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

#Se define la categoria de ingreso por renta de la propiedad

ingresos$ing_rent=NA
for(i in 23:31){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_rent[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}

ingresos$ing_rent[(clave=="P012" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P012" & is.na(clave)!=TRUE)]
ingresos$ing_rent[(clave=="P019" & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave=="P019" & is.na(clave)!=TRUE)]

#Se define la categoria de ingreso por transferencias

ingresos$ing_trans=NA
for(i in 32:48){
  string=paste0("P", str_pad(as.character(i),width=3,pad="0"))
  ingresos$ing_trans[(clave==string & is.na(clave)!=TRUE)]=ingresos$ing_mens[(clave==string & is.na(clave)!=TRUE)]}
detach(ingresos)

#Se estima el total de ingresos de cada hogar
ingresos_2=summaryBy(ing_1+ing_2+ing_3+ing_4+ing_5+ing_6+ing_tri+ing_mens+ing_mon+ing_lab+ing_rem+ing_neg+ing_otro+ing_rent+ing_trans~folioviv+foliohog,FUN=c(sum),na.rm=TRUE,data=ingresos)
names(ingresos_2)<-c("folioviv","foliohog","ing_1","ing_2","ing_3","ing_4","ing_5","ing_6","ing_tri","ing_mens","ing_mon","ing_lab","ing_rem","ing_neg","ing_otro","ing_rent","ing_trans")

ingresos_2%>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/ing_mon.dta')

################################
##### INGRESO NO MONETARIO #####
################################

rm(list=ls())

g_hogar <- read.dta('Bases/Datos_R/gastoshogar.dta') 
g_hogar <-arrange(g_hogar,folioviv,foliohog) 
g_hogar$id_base=1
g_pers <-read.dta('Bases/Datos_R/gastospersona.dta')

gastos<-full_join(g_hogar,g_pers,by = c("folioviv", "foliohog", "clave", "tipo_gasto", "gasto_nm", "gas_nm_tri"))

gastos$id_base[is.na(gastos$id_base)==TRUE]=2
gastos$frecuencia[gastos$frecuencia==" "]=""
gastos%>%as_data_frame()%>%write.dta('Bases/Bases_R/G_total.dta')

gastos<-read.dta('Bases/Bases_R/G_total.dta')
gastos$decena=as.numeric(substr(gastos$folioviv,8,8))
rm(g_hogar,g_pers)

### Apoyo mensual ###

gastos$aponm= as.numeric(gastos$gas_nm_tri)/3
gastos$aponm[is.na(gastos$aponm)==TRUE]=0

### Remuneraciones en especie ###

gastos$remesp[gastos$tipo_gasto=="G4"]=1
attr(gastos$remesp,"Nombre largo") <-"Remuneraciones en especie"
attributes(gastos$remesp)

### Regalos y transferencias ###


attach(gastos)
gastos$reg[tipo_gasto=="G5"|tipo_gasto=="G6"]=1

gastos1=gastos%>%filter(tipo_gasto!="G2" & tipo_gasto!="G3" & tipo_gasto!="G7")%>%as.data.frame()
attr(gastos1$reg,"Descripcion") <- "Regalos de otros hogares y transferencias de instituciones"
attributes(gastos1$reg)

c1=((gastos1$frecuencia==11 | gastos1$frecuencia==12|is.na(gastos1$frecuencia)==TRUE) & gastos1$tipo_gasto=="G5" & gastos1$id_base==2)

gastos2=gastos1[!c1,]  

detach(gastos)
gastos=as.data.frame(gastos2)

### Definición de los deflactores ###
#Rubro 1.1 semanal, Alimentos	
d11w07=	0.9977551691
d11w08=	1.0000000000
d11w09=	1.0058606120
d11w10=	1.0087468232
d11w11=	1.0125389839

#Rubro 1.2 semanal, Bebidas alcohólicas y tabaco
d12w07=	0.9993722536
d12w08=	1.0000000000
d12w09=	1.0033662900
d12w10=	1.0044256121
d12w11=	1.0045747018

#Rubro 2 trimestral, Vestido, calzado y accesorios 
d2t05=	0.9972905781
d2t06=	0.9978533042
d2t07=	0.9996784422
d2t08=	1.0052759293

#Rubro 3 mensual, Vivienda 
d3m07=	0.9986836290
d3m08=	1.0000000000
d3m09=	1.0010377232
d3m10=	1.0152391568
d3m11=	1.0316217307

#Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar	
d42m07=	0.9967082541
d42m08=	1.0000000000
d42m09=	1.0011171469
d42m10=	1.0040251401
d42m11=	1.0080758637

#Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar	
d42t05=	0.9946502023
d42t06=	0.9964495763
d42t07=	0.9992751337
d42t08=	1.0017140957

#Rubro 4.1 semestral, Muebles y aparatos dómesticos	
d41s02=	0.9967813039
d41s03=	0.9979614394
d41s04=	0.9998009218
d41s05=	1.0024749400

### Rubro 5.1 trimestral, Salud ###	
d51t05=	0.9951593081
d51t06=	0.9976925199
d51t07=	1.0006549007
d51t08=	1.0036562969

#Rubro 6.1.1 semanal, Transporte público urbano	
d611w07=	0.9946556014
d611w08=	1.0000000000
d611w09=	1.0050088391
d611w10=	1.0078815557
d611w11=	1.0090601061

#Rubro 6 mensual, Transporte	
d6m07=	0.9998887821
d6m08=	1.0000000000
d6m09=	1.0000397207
d6m10=	0.9989672622
d6m11=	0.9979742451

#Rubro 6 semestral, Transporte	
d6s02=	0.9908483544
d6s03=	0.9945662104
d6s04=	0.9962424233
d6s05=	0.9981940329

#Rubro 7 mensual, Educación y esparcimiento	
d7m07=	0.9995785200
d7m08=	1.0000000000
d7m09=	1.0108896669
d7m10=	1.0119476677
d7m11=	1.0125497819

#Rubro 2.3 mensual, Accesorios y cuidados del vestido	
d23m07=	0.9953611954
d23m08=	1.0000000000
d23m09=	1.0024027079
d23m10=	1.0096108315
d23m11=	1.0102334310

#Rubro 2.3 trimestral,  Accesorios y cuidados del vestido	
d23t05=	0.9963082481
d23t06=	0.9979655904
d23t07=	0.9992546344
d23t08=	1.0040045131

#INPC semestral	
dINPCs02=	0.9982498804
dINPCs03=	0.9985348835
dINPCs04=	0.9987668961
dINPCs05=	1.0002921640

#Recodificacion de decenas

attach(gastos) 
gastos$decena1=NA
gastos$decena1[decena==2]=1 
gastos$decena1[decena==3]=2 
gastos$decena1[decena==4]=3 
gastos$decena1[decena==5]=4
gastos$decena1[decena==6]=5 
gastos$decena1[decena==7]=6 
gastos$decena1[decena==8]=7 
gastos$decena1[decena==9]=8 
gastos$decena1[decena==0]=9 

gastos_1=gastos%>%select(-decena)%>%as.data.frame()
names(gastos_1)[names(gastos_1)=="decena1"]<-"decena"
detach(gastos)

gastos=gastos_1
rm(gastos_1)

### Rubro 1 (semanal): Alimentos consumidos dentro (sin tomar en cuenta bebidas alcohólicas) y fuera del hogar ###

attach(gastos)

for(i in 1:222){
  string=paste0("A", str_pad(as.character(i),width=3,pad="0"))
  gastos$ali_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 242:247){
  string=paste0("A", str_pad(as.character(i),width=3,pad="0"))
  gastos$ali_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$ali_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==0 & is.na(decena)!=TRUE)]/d11w08
gastos$ali_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==1 & is.na(decena)!=TRUE)]/d11w08
gastos$ali_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==2 & is.na(decena)!=TRUE)]/d11w08
gastos$ali_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==3 & is.na(decena)!=TRUE)]/d11w09
gastos$ali_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==4 & is.na(decena)!=TRUE)]/d11w09
gastos$ali_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==5 & is.na(decena)!=TRUE)]/d11w09
gastos$ali_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==6 & is.na(decena)!=TRUE)]/d11w10
gastos$ali_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==7 & is.na(decena)!=TRUE)]/d11w10
gastos$ali_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==8 & is.na(decena)!=TRUE)]/d11w10
gastos$ali_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$ali_m[(decena==9 & is.na(decena)!=TRUE)]/d11w11

### Rubro 2 (semanal): Bebidas alcohólicas y tabaco ###

for(i in 223:241){
  string=paste0("A", str_pad(as.character(i),width=3,pad="0"))
  gastos$alta_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$alta_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==0 & is.na(decena)!=TRUE)]/d12w08
gastos$alta_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==1 & is.na(decena)!=TRUE)]/d12w08
gastos$alta_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==2 & is.na(decena)!=TRUE)]/d12w08
gastos$alta_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==3 & is.na(decena)!=TRUE)]/d12w09
gastos$alta_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==4 & is.na(decena)!=TRUE)]/d12w09
gastos$alta_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==5 & is.na(decena)!=TRUE)]/d12w09
gastos$alta_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==6 & is.na(decena)!=TRUE)]/d12w10
gastos$alta_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==7 & is.na(decena)!=TRUE)]/d12w10
gastos$alta_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==8 & is.na(decena)!=TRUE)]/d12w10
gastos$alta_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$alta_m[(decena==9 & is.na(decena)!=TRUE)]/d12w11

### Rubro 3 (trimestral): Vestido y Calzado ###

for(i in 1:122){
  string=paste0("H", str_pad(as.character(i),width=3,pad="0"))
  gastos$veca_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$veca_m[(clave=="H136" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=='H136' & is.na(clave)!=TRUE)]

gastos$veca_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==0 & is.na(decena)!=TRUE)]/d2t05
gastos$veca_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==1 & is.na(decena)!=TRUE)]/d2t05
gastos$veca_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==2 & is.na(decena)!=TRUE)]/d2t06
gastos$veca_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==3 & is.na(decena)!=TRUE)]/d2t06
gastos$veca_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==4 & is.na(decena)!=TRUE)]/d2t06
gastos$veca_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==5 & is.na(decena)!=TRUE)]/d2t07
gastos$veca_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==6 & is.na(decena)!=TRUE)]/d2t07
gastos$veca_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==7 & is.na(decena)!=TRUE)]/d2t07
gastos$veca_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==8 & is.na(decena)!=TRUE)]/d2t08
gastos$veca_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$veca_m[(decena==9 & is.na(decena)!=TRUE)]/d2t08

### Rubro 4 (mensual): Vivienda, servicio de conservación, energía eléctrica y combustible ###

for(i in 1:16){
  string=paste0("G", str_pad(as.character(i),width=3,pad="0"))
  gastos$viv_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 1:4){
  string=paste0("R", str_pad(as.character(i),width=3,pad="0"))
  gastos$viv_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$viv_m[(clave=="R013" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="R013" & is.na(clave)!=TRUE)]

gastos$viv_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==0 & is.na(decena)!=TRUE)]/d3m07
gastos$viv_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==1 & is.na(decena)!=TRUE)]/d3m07
gastos$viv_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==2 & is.na(decena)!=TRUE)]/d3m08
gastos$viv_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==3 & is.na(decena)!=TRUE)]/d3m08
gastos$viv_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==4 & is.na(decena)!=TRUE)]/d3m08
gastos$viv_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==5 & is.na(decena)!=TRUE)]/d3m09
gastos$viv_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==6 & is.na(decena)!=TRUE)]/d3m09
gastos$viv_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==7 & is.na(decena)!=TRUE)]/d3m09
gastos$viv_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==8 & is.na(decena)!=TRUE)]/d3m10
gastos$viv_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$viv_m[(decena==9 & is.na(decena)!=TRUE)]/d3m10

### Rubro 5 (mensual) Artículos de limpieza: Artículos y servicios de limpieza ###

for(i in 1:24){
  string=paste0("C", str_pad(as.character(i),width=3,pad="0"))
  gastos$lim_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$lim_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==0 & is.na(decena)!=TRUE)]/d42m07
gastos$lim_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==1 & is.na(decena)!=TRUE)]/d42m07
gastos$lim_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==2 & is.na(decena)!=TRUE)]/d42m08
gastos$lim_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==3 & is.na(decena)!=TRUE)]/d42m08
gastos$lim_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==4 & is.na(decena)!=TRUE)]/d42m08
gastos$lim_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==5 & is.na(decena)!=TRUE)]/d42m09
gastos$lim_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==6 & is.na(decena)!=TRUE)]/d42m09
gastos$lim_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==7 & is.na(decena)!=TRUE)]/d42m09
gastos$lim_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==8 & is.na(decena)!=TRUE)]/d42m10
gastos$lim_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$lim_m[(decena==9 & is.na(decena)!=TRUE)]/d42m10

### Rubro 6 (trimestral): Artículos de limpieza: Cristaleria, utesnsilios domésticos y blancos ###

for(i in 1:26){
  string=paste0("I", str_pad(as.character(i),width=3,pad="0"))
  gastos$cris_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$cris_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==0 & is.na(decena)!=TRUE)]/d42t05
gastos$cris_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==1 & is.na(decena)!=TRUE)]/d42t05
gastos$cris_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==2 & is.na(decena)!=TRUE)]/d42t06
gastos$cris_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==3 & is.na(decena)!=TRUE)]/d42t06
gastos$cris_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==4 & is.na(decena)!=TRUE)]/d42t06
gastos$cris_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==5 & is.na(decena)!=TRUE)]/d42t07
gastos$cris_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==6 & is.na(decena)!=TRUE)]/d42t07
gastos$cris_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==7 & is.na(decena)!=TRUE)]/d42t07
gastos$cris_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==8 & is.na(decena)!=TRUE)]/d42t08
gastos$cris_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$cris_m[(decena==9 & is.na(decena)!=TRUE)]/d42t08

### Rubro 7 (semestral): Artículos de limpieza: enseres domésticos y muebles ###

for(i in 1:37){
  string=paste0("K", str_pad(as.character(i),width=3,pad="0"))
  gastos$ens_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$ens_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==0 & is.na(decena)!=TRUE)]/d41s02
gastos$ens_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==1 & is.na(decena)!=TRUE)]/d41s02
gastos$ens_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==2 & is.na(decena)!=TRUE)]/d41s03
gastos$ens_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==3 & is.na(decena)!=TRUE)]/d41s03
gastos$ens_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==4 & is.na(decena)!=TRUE)]/d41s03
gastos$ens_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==5 & is.na(decena)!=TRUE)]/d41s04
gastos$ens_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==6 & is.na(decena)!=TRUE)]/d41s04
gastos$ens_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==7 & is.na(decena)!=TRUE)]/d41s04
gastos$ens_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==8 & is.na(decena)!=TRUE)]/d41s05
gastos$ens_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$ens_m[(decena==9 & is.na(decena)!=TRUE)]/d41s05

### Rubro 8 (trimestral): Salud ###

for(i in 1:72){
  string=paste0("J", str_pad(as.character(i),width=3,pad="0"))
  gastos$sal_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$sal_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==0 & is.na(decena)!=TRUE)]/d51t05
gastos$sal_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==1 & is.na(decena)!=TRUE)]/d51t05
gastos$sal_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==2 & is.na(decena)!=TRUE)]/d51t06
gastos$sal_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==3 & is.na(decena)!=TRUE)]/d51t06
gastos$sal_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==4 & is.na(decena)!=TRUE)]/d51t06
gastos$sal_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==5 & is.na(decena)!=TRUE)]/d51t07
gastos$sal_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==6 & is.na(decena)!=TRUE)]/d51t07
gastos$sal_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==7 & is.na(decena)!=TRUE)]/d51t07
gastos$sal_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==8 & is.na(decena)!=TRUE)]/d51t08
gastos$sal_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$sal_m[(decena==9 & is.na(decena)!=TRUE)]/d51t08

### Rubro 9 (semanal): Transporte Público ###

for(i in 1:7){
  string=paste0("B", str_pad(as.character(i),width=3,pad="0"))
  gastos$tpub_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$tpub_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==0 & is.na(decena)!=TRUE)]/d611w08
gastos$tpub_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==1 & is.na(decena)!=TRUE)]/d611w08
gastos$tpub_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==2 & is.na(decena)!=TRUE)]/d611w08
gastos$tpub_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==3 & is.na(decena)!=TRUE)]/d611w09
gastos$tpub_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==4 & is.na(decena)!=TRUE)]/d611w09
gastos$tpub_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==5 & is.na(decena)!=TRUE)]/d611w09
gastos$tpub_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==6 & is.na(decena)!=TRUE)]/d611w10
gastos$tpub_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==7 & is.na(decena)!=TRUE)]/d611w10
gastos$tpub_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==8 & is.na(decena)!=TRUE)]/d611w10
gastos$tpub_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$tpub_m[(decena==9 & is.na(decena)!=TRUE)]/d611w11

### Rubro 10 (semestral): Transporte foráneo, aereo y otros tipos de transporte: 
### adquisición de vehículos y mantenimiento de vehículos, refacciones y partes.

for(i in 1:18){
  string=paste0("M", str_pad(as.character(i),width=3,pad="0"))
  gastos$tfor_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 7:14){
  string=paste0("F", str_pad(as.character(i),width=3,pad="0"))
  gastos$tfor_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$tfor_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==0 & is.na(decena)!=TRUE)]/d6s02
gastos$tfor_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==1 & is.na(decena)!=TRUE)]/d6s02
gastos$tfor_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==2 & is.na(decena)!=TRUE)]/d6s03
gastos$tfor_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==3 & is.na(decena)!=TRUE)]/d6s03
gastos$tfor_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==4 & is.na(decena)!=TRUE)]/d6s03
gastos$tfor_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==5 & is.na(decena)!=TRUE)]/d6s04
gastos$tfor_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==6 & is.na(decena)!=TRUE)]/d6s04
gastos$tfor_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==7 & is.na(decena)!=TRUE)]/d6s04
gastos$tfor_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==8 & is.na(decena)!=TRUE)]/d6s05
gastos$tfor_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$tfor_m[(decena==9 & is.na(decena)!=TRUE)]/d6s05

### Rubro 11 (mensual): Comunicaciones: teléfono, fax

gastos$com_m<-NA
for(i in 1:6){
  string=paste0("F", str_pad(as.character(i),width=3,pad="0"))
  gastos$com_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(gastos$clave==string & is.na(gastos$clave)!=TRUE)]}

for(i in 5:8){
  string=paste0("R", str_pad(as.character(i),width=3,pad="0"))
  gastos$com_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 10:11){
  string=paste0("R", str_pad(as.character(i),width=3,pad="0"))
  gastos$com_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$com_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==0 & is.na(decena)!=TRUE)]/d6m07
gastos$com_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==1 & is.na(decena)!=TRUE)]/d6m07
gastos$com_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==2 & is.na(decena)!=TRUE)]/d6m08
gastos$com_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==3 & is.na(decena)!=TRUE)]/d6m08
gastos$com_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==4 & is.na(decena)!=TRUE)]/d6m08
gastos$com_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==5 & is.na(decena)!=TRUE)]/d6m09
gastos$com_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==6 & is.na(decena)!=TRUE)]/d6m09
gastos$com_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==7 & is.na(decena)!=TRUE)]/d6m09
gastos$com_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==8 & is.na(decena)!=TRUE)]/d6m10
gastos$com_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$com_m[(decena==9 & is.na(decena)!=TRUE)]/d6m10

### Rubro 12 (mensual): Educación y esparcimiento ###

for(i in 1:34){
  string=paste0("E", str_pad(as.character(i),width=3,pad="0"))
  gastos$edre_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 134:135){
  string=paste0("H", str_pad(as.character(i),width=3,pad="0"))
  gastos$edre_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 1:29){
  string=paste0("L", str_pad(as.character(i),width=3,pad="0"))
  gastos$edre_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 3:5){
  string=paste0("N", str_pad(as.character(i),width=3,pad="0"))
  gastos$edre_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$edre_m[(clave=="R009" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="R009" & is.na(clave)!=TRUE)]

gastos$edre_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==0 & is.na(decena)!=TRUE)]/d7m07
gastos$edre_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==1 & is.na(decena)!=TRUE)]/d7m07
gastos$edre_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==2 & is.na(decena)!=TRUE)]/d7m08
gastos$edre_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==3 & is.na(decena)!=TRUE)]/d7m08
gastos$edre_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==4 & is.na(decena)!=TRUE)]/d7m08
gastos$edre_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==5 & is.na(decena)!=TRUE)]/d7m09
gastos$edre_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==6 & is.na(decena)!=TRUE)]/d7m09
gastos$edre_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==7 & is.na(decena)!=TRUE)]/d7m09
gastos$edre_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==8 & is.na(decena)!=TRUE)]/d7m10
gastos$edre_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$edre_m[(decena==9 & is.na(decena)!=TRUE)]/d7m10

for(i in 2:3){
  string=paste0("E", str_pad(as.character(i),width=3,pad="0"))
  gastos$edba_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 134:135){
  string=paste0("H", str_pad(as.character(i),width=3,pad="0"))
  gastos$edba_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$edba_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==0 & is.na(decena)!=TRUE)]/d7m07
gastos$edba_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==1 & is.na(decena)!=TRUE)]/d7m07
gastos$edba_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==2 & is.na(decena)!=TRUE)]/d7m08
gastos$edba_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==3 & is.na(decena)!=TRUE)]/d7m08
gastos$edba_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==4 & is.na(decena)!=TRUE)]/d7m08
gastos$edba_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==5 & is.na(decena)!=TRUE)]/d7m09
gastos$edba_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==6 & is.na(decena)!=TRUE)]/d7m09
gastos$edba_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==7 & is.na(decena)!=TRUE)]/d7m09
gastos$edba_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==8 & is.na(decena)!=TRUE)]/d7m10
gastos$edba_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$edba_m[(decena==9 & is.na(decena)!=TRUE)]/d7m10

### Rubro 13 (mensual): Artículos y servicios para el cuidado personal ###

for(i in 1:26){
  string=paste0("D", str_pad(as.character(i),width=3,pad="0"))
  gastos$cuip_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$cuip_m[(clave=="H132" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="H132" & is.na(clave)!=TRUE)]

gastos$cuip_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==0 & is.na(decena)!=TRUE)]/d23m07
gastos$cuip_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==1 & is.na(decena)!=TRUE)]/d23m07
gastos$cuip_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==2 & is.na(decena)!=TRUE)]/d23m08
gastos$cuip_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==3 & is.na(decena)!=TRUE)]/d23m08
gastos$cuip_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==4 & is.na(decena)!=TRUE)]/d23m08
gastos$cuip_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==5 & is.na(decena)!=TRUE)]/d23m09
gastos$cuip_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==6 & is.na(decena)!=TRUE)]/d23m09
gastos$cuip_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==7 & is.na(decena)!=TRUE)]/d23m09
gastos$cuip_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==8 & is.na(decena)!=TRUE)]/d23m10
gastos$cuip_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$cuip_m[(decena==9 & is.na(decena)!=TRUE)]/d23m10

### Rubro 14 (trimestral): Accesorios y efectos personales ###

for(i in 123:131){
  string=paste0("H", str_pad(as.character(i),width=3,pad="0"))
  gastos$accp_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$accp_m[(clave=="H133" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="H133" & is.na(clave)!=TRUE)]

gastos$accp_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==0 & is.na(decena)!=TRUE)]/d23t05
gastos$accp_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==1 & is.na(decena)!=TRUE)]/d23t05
gastos$accp_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==2 & is.na(decena)!=TRUE)]/d23t06
gastos$accp_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==3 & is.na(decena)!=TRUE)]/d23t06
gastos$accp_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==4 & is.na(decena)!=TRUE)]/d23t06
gastos$accp_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==5 & is.na(decena)!=TRUE)]/d23t07
gastos$accp_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==6 & is.na(decena)!=TRUE)]/d23t07
gastos$accp_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==7 & is.na(decena)!=TRUE)]/d23t07
gastos$accp_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==8 & is.na(decena)!=TRUE)]/d23t08
gastos$accp_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$accp_m[(decena==9 & is.na(decena)!=TRUE)]/d23t08

### Rubro 15 (semestral): Otros gastos  diversos y transferencias ###

for(i in 6:16){
  string=paste0("N", str_pad(as.character(i),width=3,pad="0"))
  gastos$otr_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

for(i in 901:915){
  string=paste0("T", str_pad(as.character(i),width=3,pad="0"))
  gastos$otr_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$otr_m[(clave=="R012" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="R012" & is.na(clave)!=TRUE)]
gastos$otr_m[(clave=="N001" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="N001" & is.na(clave)!=TRUE)]
gastos$otr_m[(clave=="N002" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="N002" & is.na(clave)!=TRUE)]

gastos$otr_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==0 & is.na(decena)!=TRUE)]/dINPCs02
gastos$otr_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==1 & is.na(decena)!=TRUE)]/dINPCs02
gastos$otr_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==2 & is.na(decena)!=TRUE)]/dINPCs03
gastos$otr_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==3 & is.na(decena)!=TRUE)]/dINPCs03
gastos$otr_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==4 & is.na(decena)!=TRUE)]/dINPCs03
gastos$otr_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==5 & is.na(decena)!=TRUE)]/dINPCs04
gastos$otr_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==6 & is.na(decena)!=TRUE)]/dINPCs04
gastos$otr_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==7 & is.na(decena)!=TRUE)]/dINPCs04
gastos$otr_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==8 & is.na(decena)!=TRUE)]/dINPCs05
gastos$otr_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$otr_m[(decena==9 & is.na(decena)!=TRUE)]/dINPCs05

### Regalos otorgados ###

for(i in 901:915){
  string=paste0("T", str_pad(as.character(i),width=3,pad="0"))
  gastos$reda_m[(clave==string & is.na(clave)!=TRUE)]=gastos$aponm[(clave==string & is.na(clave)!=TRUE)]}

gastos$reda_m[(clave=="N013" & is.na(clave)!=TRUE)]=gastos$aponm[(clave=="N013" & is.na(clave)!=TRUE)]

gastos$reda_m[(decena==0 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==0 & is.na(decena)!=TRUE)]/dINPCs02
gastos$reda_m[(decena==1 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==1 & is.na(decena)!=TRUE)]/dINPCs02
gastos$reda_m[(decena==2 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==2 & is.na(decena)!=TRUE)]/dINPCs03
gastos$reda_m[(decena==3 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==3 & is.na(decena)!=TRUE)]/dINPCs03
gastos$reda_m[(decena==4 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==4 & is.na(decena)!=TRUE)]/dINPCs03
gastos$reda_m[(decena==5 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==5 & is.na(decena)!=TRUE)]/dINPCs04
gastos$reda_m[(decena==6 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==6 & is.na(decena)!=TRUE)]/dINPCs04
gastos$reda_m[(decena==7 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==7 & is.na(decena)!=TRUE)]/dINPCs04
gastos$reda_m[(decena==8 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==8 & is.na(decena)!=TRUE)]/dINPCs05
gastos$reda_m[(decena==9 & is.na(decena)!=TRUE)]=gastos$reda_m[(decena==9 & is.na(decena)!=TRUE)]/dINPCs05

detach(gastos)
gastos%>%arrange(folioviv,foliohog)%>%as.data.frame()%>%write.dta('Bases/Bases_R/ing_nomon.dta')
rm(list=ls())

gastos<-read.dta('Bases/Bases_R/ing_nomon.dta')
gastos=gastos%>%filter(remesp==1)%>%as.data.frame()     

L=summaryBy(ali_m+alta_m+veca_m+viv_m+lim_m+cris_m+ens_m+sal_m+tpub_m+tfor_m+com_m+edre_m+edba_m+cuip_m+accp_m+otr_m+reda_m ~ folioviv+foliohog,FUN=c(sum),data=gastos,na.rm=TRUE)

L%>%rename(ali_nmre=ali_m.sum)%>%
  rename(alta_nmre=alta_m.sum)%>%
  rename(veca_nmre=veca_m.sum)%>%
  rename(viv_nmre=viv_m.sum)%>%
  rename(lim_nmre=lim_m.sum)%>%
  rename(cris_nmre=cris_m.sum)%>%
  rename(ens_nmre=ens_m.sum)%>%
  rename(sal_nmre=sal_m.sum)%>%
  rename(tpub_nmre=tpub_m.sum)%>%
  rename(tfor_nmre=tfor_m.sum)%>%
  rename(com_nmre=com_m.sum)%>%
  rename(edre_nmre=edre_m.sum)%>%
  rename(edba_nmre=edba_m.sum)%>%
  rename(cuip_nmre=cuip_m.sum)%>%
  rename(accp_nmre=accp_m.sum)%>%
  rename(otr_nmre=otr_m.sum)%>%
  rename(reda_nmre=reda_m.sum)%>%   
  arrange(folioviv,foliohog)%>%       
  as.data.frame()%>%
  write.dta('Bases/Bases_R/remunesp.dta')

rm(list=ls())

ing_nom<-read.dta('Bases/Bases_R/ing_nomon.dta')
ing_nom1<-ing_nom%>%filter(reg==1)%>%as.data.frame()
L=summaryBy(ali_m+alta_m+veca_m+viv_m+lim_m+cris_m+ens_m+sal_m+tpub_m+tfor_m+com_m+edre_m+edba_m+cuip_m+accp_m+otr_m+reda_m ~ folioviv+foliohog,FUN=c(sum),data=ing_nom1,na.rm=TRUE)

L%>%rename(ali_nmte=ali_m.sum)%>%
  rename(alta_nmte=alta_m.sum)%>%
  rename(veca_nmte=veca_m.sum)%>%
  rename(viv_nmte=viv_m.sum)%>%
  rename(lim_nmte=lim_m.sum)%>%
  rename(cris_nmte=cris_m.sum)%>%
  rename(ens_nmte=ens_m.sum)%>%
  rename(sal_nmte=sal_m.sum)%>%
  rename(tpub_nmte=tpub_m.sum)%>%
  rename(tfor_nmte=tfor_m.sum)%>%
  rename(com_nmte=com_m.sum)%>%
  rename(edre_nmte=edre_m.sum)%>%
  rename(edba_nmte=edba_m.sum)%>%
  rename(cuip_nmte=cuip_m.sum)%>%
  rename(accp_nmte=accp_m.sum)%>%
  rename(otr_nmte=otr_m.sum)%>%
  rename(reda_nmte=reda_m.sum)%>%   
  arrange(folioviv,foliohog)%>%       
  as.data.frame()%>%
  write.dta('Bases/Bases_R/transfesp.dta')

##########################
##### INGRESO  TOTAL ##### 
##########################

### Unión de Bases ###

rm(list=ls())
con_hogar<-read.dta('Bases/Datos_R/concentradohogar.dta')
datos=select(con_hogar,folioviv, foliohog, tam_loc, tot_integ, factor, ubica_geo, est_dis, upm)
rm(con_hogar)
datos=arrange(datos,folioviv,foliohog)
datos=mutate(datos,c_mun=substr(ubica_geo,1,5))
datos$c_mun<-as.numeric(datos$c_mun)

#Se cargan las bases
ing_mon<-read.dta('Bases/Bases_R/ing_mon.dta')
remunesp<-read.dta('Bases/Bases_R/remunesp.dta')
transfesp<-read.dta('Bases/Bases_R/transfesp.dta')


datos1<-full_join(datos,ing_mon,by=c("folioviv","foliohog"))
datos1=arrange(datos1,folioviv,foliohog)
datos2<-full_join(datos1,remunesp,by = c("folioviv", "foliohog"))
datos2=arrange(datos2,folioviv,foliohog)
datos3<-full_join(datos2,transfesp,by=c("folioviv","foliohog"))
datos3=arrange(datos3,folioviv,foliohog)

concentrado<-datos3
rm("datos","datos1","datos2","datos3","ing_mon","remunesp","transfesp")

### Estratos ###
#Rural: localidades con menos de 2,500 habitantes 

concentrado$tam_loc<-as.numeric(concentrado$tam_loc)

concentrado$rururb[ concentrado$tam_loc==4 & is.na(concentrado$tam_loc)!=TRUE]=1
concentrado$rururb[concentrado$tam_loc<=3]=0

attr(concentrado$rururb,"Descripción")<-"0 estrato urbano, 1 al estrato rural"
attributes(concentrado$rururb)
table(concentrado$rururb,dnn="Urbano y Rural")

#### Ingreso no monetario total ###

attach(concentrado)
concentrado$pago_esp=rowSums(data.frame(ali_nmre,alta_nmre,veca_nmre,viv_nmre,lim_nmre,cris_nmre,ens_nmre,
                                        sal_nmre,tpub_nmre,tfor_nmre,com_nmre,edre_nmre,cuip_nmre,accp_nmre,otr_nmre), na.rm = TRUE)
concentrado$reg_esp=rowSums(data.frame(ali_nmte,alta_nmte,veca_nmte,viv_nmte,lim_nmte,ens_nmte,cris_nmte,
                                       sal_nmte,tpub_nmte,tfor_nmte,com_nmte,edre_nmte,cuip_nmte,accp_nmte,otr_nmte), na.rm = TRUE)
detach(concentrado)

concentrado$nomon=rowSums(data.frame(concentrado$pago_esp,concentrado$reg_esp), na.rm = TRUE)

### Ingreso Corriente Total (ICT) ###
concentrado$ict=rowSums(data.frame(concentrado$ing_mon,concentrado$nomon),na.rm=TRUE)
attr(concentrado$ict,"Descripción")<-"Ingreso corriente total"
attributes(concentrado$ict)

### Ingreso corriente total según escalas de equivalencia ###
equivalencia<-read.dta('Bases/Bases_R/escala_equivalencia.dta')
concentrado_2<-full_join(concentrado,equivalencia,by=c("folioviv","foliohog"))
concentrado_2$ictpc<-concentrado_2$ict/concentrado_2$escala

attr(concentrado_2$ictpc,"Descripción")<-"Ingreso corriente total per capita por escalas de equivalencia"
attributes(concentrado_2$ictpc)

concentrado_2$ictpc<-recode(concentrado_2$ictpc,"NA=0")

#### Factor de expansión para personas ###
concentrado_2$factorp=concentrado_2$factor*concentrado_2$tot_integ
attr(concentrado_2$factor,"Descripción")<-"Factor de expansión para hogares"
attributes(concentrado_2$factor)

attr(concentrado_2$factorp,"Descripción")<-"Factor de expansión para hogares"
attributes(concentrado_2$factorp)

###Incidencial ###

# Pobreza por ingresos
# Líneas de pobreza

#Bienestar mínimo
LPU1 = 1281.24
LPR1= 910.28

#Bienestar
LPU= 2600.79
LPR= 1673.52

attach(concentrado_2)
concentrado_2$pob[ictpc<LPU & rururb==0]=1
concentrado_2$pob[ictpc<LPR & rururb==1]=1
concentrado_2$pob[ictpc>LPU & rururb==0 & is.na(ictpc)==FALSE]=0
concentrado_2$pob[ictpc>LPR & rururb==1 & is.na(ictpc)==FALSE]=0
attr(concentrado_2$pob,"Descripción")<-"CONEVAL pobreza por ingresos"
attributes(concentrado_2$pob)
detach(concentrado_2)

#####	Pobreza extrema por ingresos
attach(concentrado_2)
concentrado_2$pobex[ictpc<LPU1 & rururb==0]=1
concentrado_2$pobex[ictpc>LPU1 & rururb==0 & is.na(ictpc)==FALSE]=0
concentrado_2$pobex[ictpc<LPR1 & rururb==1]=1
concentrado_2$pobex[ictpc>LPR1 & rururb==1 & is.na(ictpc)==FALSE]=0
attr(concentrado_2$pobex,"Descripción")<-"CONEVAL pobreza extrema por ingresos" 
attributes(concentrado_2$pobex)
detach(concentrado_2)

concentrado_2%>%
  select(folioviv,foliohog,factor,tam_loc,tot_integ,ict,escala,rururb,ictpc,pob,pobex, 
         starts_with("ing"),nomon,ends_with("_esp"),c_mun,est_dis, upm)%>%
  arrange(folioviv,foliohog)%>%
  as.data.frame()%>%
  write.dta('Bases/Bases_R/pingresos.dta')

rm(list=ls())

### Integración de base final ###

### Carga de las Bases ###

peducacion=read.dta('Bases/Bases_R/peducacion.dta')
pasalud=read.dta('Bases/Bases_R/pasalud.dta')
pss=read.dta('Bases/Bases_R/pss.dta')
pviviendac=read.dta('Bases/Bases_R/pviviendac.dta')
pviviendas=read.dta('Bases/Bases_R/pviviendas.dta')
palimentacion=read.dta('Bases/Bases_R/palimentacion.dta')
pingresos=read.dta('Bases/Bases_R/pingresos.dta')

### Construcción ###
base_pobreza= full_join(peducacion, pasalud,by=c("folioviv", "foliohog", "numren" ))
base_pobreza=arrange(base_pobreza,folioviv,foliohog,numren)

base_pobreza= full_join(base_pobreza, pss, by=c("folioviv","foliohog", "numren" ))
base_pobreza=arrange(base_pobreza,folioviv,foliohog)

base_pobreza= full_join(base_pobreza, pviviendac, by=c("folioviv", "foliohog"))
base_pobreza=arrange(base_pobreza,folioviv,foliohog)

base_pobreza= full_join(base_pobreza, pviviendas, by=c("folioviv", "foliohog"))
base_pobreza=arrange(base_pobreza,folioviv,foliohog)

base_pobreza= full_join(base_pobreza, palimentacion, by=c("folioviv", "foliohog"))
base_pobreza=arrange(base_pobreza,folioviv,foliohog)

base_pobreza= full_join(base_pobreza, pingresos, by=c("folioviv", "foliohog"))
base_pobreza=arrange(base_pobreza,folioviv,foliohog)
rm("palimentacion","pasalud","peducacion","pingresos","pss","pviviendac","pviviendas")

attach(base_pobreza)
base_pobreza$ing_1[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_1[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_2[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_2[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_3[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_3[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_4[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_4[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_5[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_5[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_6[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_6[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_tri[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_tri[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_mens[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_mens[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_mon[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_mon[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_lab[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_lab[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_rem[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_rem[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_neg[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_neg[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_otro[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_otro[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_rent[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_rent[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
base_pobreza$ing_trans[is.na(ing_mon)==TRUE | ictpc!=0]<-recode(base_pobreza$ing_trans[is.na(ing_mon)==TRUE | ictpc!=0],"NA=0")
detach(base_pobreza)

### Depuración de la base ###

### Duplicados ###

anyDuplicated(base_pobreza)

#Eliminacion de Duplicados de los campos folioviv, foliohog y numren
M=base_pobreza[,c("folioviv","foliohog","numren")]
base_pobreza=base_pobreza[!duplicated(M),]

### Base_Pobreza ###
col_iniciales=c("folioviv","foliohog","numren","factor","tam_loc","tot_integ","rururb","ict","escala",
                "ictpc","pob","pobex","ic_rezedu","ic_asalud","ic_ss","ic_cv","ic_sbv","ic_ali")
columnas<-names(base_pobreza)
pobreza2015<-base_pobreza[c(col_iniciales,setdiff(columnas,col_iniciales))]

pobreza2015%>%as.data.frame()%>%write.dta('Bases/Bases_R/pobreza15.dta')

##################################################	     
##########	II. INCIDENCIA DE POBREZA		##########	     
##################################################	     

rm(list=ls())

pobreza_15<-read.dta("Bases/Bases_R/pobreza15.dta")

### Indicadores de privación social ###
pobreza_15<-arrange(pobreza_15,c_mun)
cnch<-read.dta("Bases/Datos_R/cnch.dta")

pobreza<-full_join(pobreza_15,cnch,by="c_mun")

#Revisas como se hace la recodificación
pobreza$cnch<-recode(pobreza$cnch,"NA=0")
pobreza<-pobreza[!is.na(pobreza$folioviv),]
pobreza<-arrange(pobreza,folioviv,foliohog,numren)

#Codigo Municipio de mas
pobreza$ent<-NULL
pobreza$ent=as.numeric(substr(pobreza$folioviv,1,2))

pobreza$decil<-NULL
pobreza$decil<-ntile(pobreza$ictpc,10)

polarizacion<-read.dta("Bases/Datos_R/polarizacion.dta")
pobreza=arrange(pobreza,ent)

pobreza_1<-inner_join(pobreza,polarizacion,by="ent")
pobreza_1=arrange(pobreza_1,folioviv,foliohog,numren)
rm(pobreza_15,pobreza,cnch,polarizacion)

pobreza_1$q=1

pobreza_1<-pobreza_1%>%group_by(pol)%>%mutate(tot_1=n()*factor,tot_2=3766*factor)%>%arrange(pol)%>%as.data.frame()
pobreza_1$pp<-pobreza_1$tot_1/pobreza_1$tot_2

### Tabla ###
summaryBy(pp~pol,data=pobreza_1,FUN=mean)
L=summaryBy(pp~pol,data=pobreza_1,FUN=mean)

### Medias de pp ###
pol1_pp=L[1,2]
pol2_pp=L[2,2]
pol3_pp=L[3,2]
pol4_pp=0

pobreza_1$tot_1<-NULL
pobreza_1$tot_2<-NULL
pobreza_1$q<-NULL
pobreza_1<-arrange(pobreza_1,ent)

redes<-read.dta("Bases/Datos_R/redes15.dta")

### Depuración de archivo redes15.dta para R ### 
redes_1<-redes[!duplicated(redes),]

pobreza_2<-inner_join(pobreza_1,redes_1,by="ent")
pobreza<-pobreza_2
rm(L,pobreza_1,pobreza_2,redes,redes_1)

attach(pobreza)
pobreza$ing_monpc=ing_mon/escala
pobreza$ing_rempc=ing_rem/escala
pobreza$ing_negpc=ing_neg/escala
pobreza$ing_rentpc=ing_rent/escala
pobreza$ing_otrospc=ing_otro/escala
pobreza$ing_transpc=ing_trans/escala
pobreza$nomonpc=nomon/escala
pobreza$pagopc=pago_esp/escala
pobreza$esppc=reg_esp/escala
detach(pobreza)

LPU1 = 1281.24
LPR1= 910.28

#Bienestar
LPU= 2600.79
LPR= 1673.52

### Privación social ###
#pobreza$ic_cv<-recode(pobreza$ic_cv,"1=NA")
pobreza$ips=rowSums(data.frame(pobreza$ic_rezedu, pobreza$ic_asalud,pobreza$ic_ss, pobreza$ic_cv,pobreza$ic_sbv,pobreza$ic_ali),na.rm=TRUE)
pobreza$ips[is.na(pobreza$ic_rezedu)==TRUE | is.na(pobreza$ic_asalud)==TRUE | is.na(pobreza$ic_ss)==TRUE | is.na(pobreza$ic_cv)==TRUE 
            | is.na(pobreza$ic_sbv)==TRUE | is.na(pobreza$ic_ali)==TRUE]= NA

### Ingresos ### 

#Función para replicar salida de tablas tipo Stata
Tabla_Stata<-function(vec_valores){
           #Se obtiene una tabla con las frecuencias multiplicadas por el factor 13
           #y con el porcentaje correspondiente
  vec<-as.vector(vec_valores)
  w=table(vec)
  class(w)
  Tabla<-as.data.frame(w)
  var1<-as.numeric(as.character(Tabla$Var1))
  var2<-as.numeric(Tabla$Freq)
  Salida<-data.frame(Valores=c(0,1),
                     Frecuencia=c(var2[1],var2[2]),
                     Porcentage=Tabla$Freq/212005*100)
  return(Salida)
}

### Número y porcentaje de hogares en pobreza ###
Tabla_Stata(pobreza$pob)

### Número y porcentaje de hogares en pobreza extrema ###
Tabla_Stata(pobreza$pobex)

### Población en situación de pobreza multidimensional ###

pobreza$pob_multi[(pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE) & pobreza$pob==1]=1
pobreza$pob_multi[(pobreza$pob==0 | pobreza$ips==0) & (is.na(pobreza$pob)==FALSE & is.na(pobreza$ips)==FALSE)]=0
attr(pobreza$pob_multi,"Descripción")<-"Población en situación de pobreza multidimensional"
attributes(pobreza$pob_multi)

### Población en situación de pobreza multidimensional extrema ###
pobreza$pob_multiex[pobreza$ips>=3  & is.na(pobreza$ips)!=TRUE & pobreza$pobex==1]=1
pobreza$pob_multiex[(pobreza$pobex==0 | pobreza$ips<3) & (is.na(pobreza$pobex)!=TRUE & is.na(pobreza$ips)!=TRUE)]=0
attr(pobreza$pob_multiex,"Descripción")<-"Población en situación de pobreza multidimensional extrema"
attributes(pobreza$pob_multiex)
table(pobreza$pob_multiex,dnn="Población en situación de pobreza multidimensional extrema")

### Población en situación de pobreza multidimensional moderada ###
pobreza$pob_multimod[pobreza$pob_multiex==0 & is.na(pobreza$ips)!=TRUE & pobreza$pob_multi==1]=1
pobreza$pob_multimod[(pobreza$pob_multi==1 & pobreza$pob_multiex==1) | pobreza$pob_multi==0]=0
attr(pobreza$pob_multimod,"Descripción")<-"Población en situación de pobreza multidimensional moderada"
attributes(pobreza$pob_multimod)
table(pobreza$pob_multimod,dnn="Población en situación de pobreza multidimensional moderada")

### Vulnerables por carencias
pobreza$vul_car[pobreza$pob==0 & (pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE)]=1
pobreza$vul_car[(pobreza$pob==1 | pobreza$ips==0) & (is.na(pobreza$pob)!=TRUE & is.na(pobreza$ips)!=TRUE)]=0
attr(pobreza$vul_car,"Descripción")<-"Población vulnerable por carencias"
attributes(pobreza$vul_car)
table(pobreza$vul_car,dnn="Población vulnerable por carencias")

### Vulnerables por ingresos
pobreza$vul_ing[pobreza$pob==1 & (pobreza$ips==0)]=1
pobreza$vul_ing[pobreza$pob==0  | (pobreza$pob==1 & pobreza$ips>=1 & is.na(pobreza$ips)==FALSE)]=0

#No pobre y no vulnerable
pobreza$nopob_nov[pobreza$pob==0 & pobreza$ips==0]=1
pobreza$nopob_nov[(pobreza$pob==1  & is.na(pobreza$ips)!=TRUE) | (pobreza$pob==0 & (pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE))]=0

#label var nopob_nov "Población no pobre y no vulnerable"

pobreza$carencias[pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE]=1
pobreza$carencias[pobreza$ips==0]=0

pobreza$carencias_3[pobreza$ips>=3 & is.na(pobreza$ips)!=TRUE]= 1
pobreza$carencias_3[pobreza$ips<3 & is.na(pobreza$ips)!=TRUE]= 0

### Población con al menos una carencia y cuyo ingreso es menor a la línea de pobreza por ingresos (I)
pobreza$car1_pob1[(pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE) & pobreza$pob==1]=1

### Población con al menos una carencia y cuyo ingreso es mayor de la línea de pobreza por ingresos (II)

pobreza$car1_pob0=0
pobreza$car1_pob0[(pobreza$ips>=1 & is.na(pobreza$ips)!=TRUE) & pobreza$pob==0]=1

### Población con cero carencias y cuyo ingreso es menor a la línea de pobreza por ingresos (III)
pobreza$car0_pob1[pobreza$ips==0 & pobreza$pob==1]=1

### Población con cero carencias y cuyo ingreso es mayor de la línea de pobreza por ingresos (IV)
pobreza$car0_pob0[pobreza$ips==0 & pobreza$pob==0]=1

#Profundidad 
#FGT (a=1) - Línea bienestar

attach(pobreza)
pobreza$fgt1_b[rururb==1 & pob==1]=(LPR-ictpc[rururb==1 & pob==1])/(LPR)
pobreza$fgt1_b[rururb==0 & pob==1]=(LPU-ictpc[rururb==0 & pob==1])/(LPU)
pobreza$fgt1_b[is.na(pobreza$ictpc)!=TRUE]<-recode(pobreza$fgt1_b[is.na(pobreza$ictpc)!=TRUE],"NA=0")

#FGT (a=1) - Línea bienestar mínimo
pobreza$fgt1_bm[rururb==1 & pobex==1]=(LPR1-ictpc[rururb==1 & pobex==1])/(LPR1)
pobreza$fgt1_bm[rururb==0 & pobex==1]=(LPU1-ictpc[rururb==0 & pobex==1])/(LPU1)
pobreza$fgt1_bm[is.na(pobreza$ictpc)!=TRUE]<-recode(pobreza$fgt1_bm[is.na(pobreza$ictpc)!=TRUE],"NA=0")

##########################################
#Intensidad 
#FGT (a=2) - Línea bienestar
pobreza$fgt2_b[rururb==1 & pob==1]=((LPR-ictpc[rururb==1 & pob==1])/(LPR))^2
pobreza$fgt2_b[rururb==0 & pob==1]=((LPU-ictpc[rururb==0 & pob==1])/(LPU))^2
pobreza$fgt2_b[is.na(pobreza$ictpc)!=TRUE]<-recode(pobreza$fgt2_b[is.na(pobreza$ictpc)!=TRUE],"NA=0")

#FGT (a=2) - Línea bienestar mínimo
pobreza$fgt2_bm[rururb==1 & pobex==1]=((LPR1-ictpc[rururb==1 & pobex==1])/(LPR1))^2
pobreza$fgt2_bm[rururb==0 & pobex==1]=((LPU1-ictpc[rururb==0 & pobex==1])/(LPU1))^2
pobreza$fgt2_bm[is.na(pobreza$ictpc)!=TRUE]<-recode(pobreza$fgt2_bm[is.na(pobreza$ictpc)!=TRUE],"NA=0")

detach(pobreza)
#Profundidad de la privación social
pobreza$profun=pobreza$ips/6

#Intensidad de la privación social
#Pobres
pobreza$int_pob=pobreza$profun*pobreza$pob_multi

#Pobres extrema
pobreza$int_pobe=pobreza$profun*pobreza$pob_multiex

#Población vulnerable por carencias
pobreza$int_vulcar=pobreza$profun*pobreza$vul_car

#Población carenciada
pobreza$int_caren=pobreza$profun*pobreza$carencias

#Desagregación
pobreza$r1[pobreza$pob_multi==1]=sum(pobreza$ips[pobreza$pob_multi==1])*13
pobreza$r1[pobreza$pob_multi!=1]=NA
pobreza$r2[pobreza$pob_multi==1]=sum(pobreza$ic_rezedu[pobreza$pob_multi==1])*13
pobreza$r2[pobreza$pob_multi!=1]=NA
pobreza$r3[pobreza$pob_multi==1]=sum(pobreza$ic_asalud[pobreza$pob_multi==1])*13
pobreza$r3[pobreza$pob_multi!=1]=NA
pobreza$r4[pobreza$pob_multi==1]=sum(pobreza$ic_ss[pobreza$pob_multi==1])*13
pobreza$r4[pobreza$pob_multi!=1]=NA
pobreza$r5[pobreza$pob_multi==1]=sum(pobreza$ic_cv[pobreza$pob_multi==1])*13
pobreza$r5[pobreza$pob_multi!=1]=NA
pobreza$r6[pobreza$pob_multi==1]=sum(pobreza$ic_sbv[pobreza$pob_multi==1])*13
pobreza$r6[pobreza$pob_multi!=1]=NA
pobreza$r7[pobreza$pob_multi==1]=sum(pobreza$ic_ali[pobreza$pob_multi==1])*13
pobreza$r7[pobreza$pob_multi!=1]=NA

attach(pobreza)             
pobreza$rr1=r2/r1
pobreza$rr2=r3/r1
pobreza$rr3=r4/r1
pobreza$rr4=r5/r1
pobreza$rr5=r6/r1
pobreza$rr6=r7/r1
detach(pobreza)

pobreza_2<-pobreza%>%arrange(ent)%>%group_by(ent)%>%
  mutate(e1=ifelse(pob_multi==1,sum(ips*factor),NA),
         e2=ifelse(pob_multi==1,sum(ic_rezedu*factor),NA),
         e3=ifelse(pob_multi==1,sum(ic_asalud*factor),NA),
         e4=ifelse(pob_multi==1,sum(ic_ss*factor),NA),
         e5=ifelse(pob_multi==1,sum(ic_cv*factor),NA),
         e6=ifelse(pob_multi==1,sum(ic_sbv*factor),NA),
         e7=ifelse(pob_multi==1,sum(ic_ali*factor),NA))%>%
  as.data.frame()

attach(pobreza_2)		 		 
pobreza_2$ee1=e2/e1
pobreza_2$ee2=e3/e1
pobreza_2$ee3=e4/e1
pobreza_2$ee4=e5/e1
pobreza_2$ee5=e6/e1
pobreza_2$ee6=e7/e1
detach(pobreza_2)		

pobreza_2$espacio=NA
a=100
z=1000000
pobreza=pobreza_2
rm(pobreza_2)

#Base con todas las variables generadas
write.dta(pobreza,file='Bases/Bases_R/pob2015.dta')

##################################################################################
#CUADRO 1

varlist<-c("pob_multi", "pob_multimod", "pob_multiex", "vul_car","vul_ing","nopob_nov", "espacio", "carencias", "carencias_3","espacio", "ic_rezedu", "ic_asalud", "ic_ss","ic_cv", "ic_sbv",
           "ic_ali","espacio","pobex", "pob")
valores<-unname(unlist(lapply(pobreza[is.na(pobreza$pob_multi)==FALSE,varlist],function(i)weighted.mean(x=i,w=pobreza$factor[is.na(pobreza$pob_multi)==FALSE],na.rm=TRUE))))
nombres<-unname(sapply(varlist,function(x)paste0(x,"_pp")))
tabla1<-data.frame(Variables=nombres,Medias=valores)
print(tabla1)

varlist<-c("pob_multi","pob_multimod","pob_multiex","vul_car","vul_ing", "nopob_nov", "espacio", "carencias","carencias_3","espacio", "ic_rezedu", "ic_asalud", 
           "ic_ss","ic_cv","ic_sbv","ic_ali","espacio","pobex","pob")
valores<-unname(unlist(lapply(pobreza[is.na(pobreza$pob_multi)==FALSE,varlist],function(i)sum(i*pobreza$factor[is.na(pobreza$pob_multi)==FALSE],na.rm=TRUE))))
nombres<-unname(sapply(varlist,function(x)paste0(x,"_p")))
tabla2<-data.frame(Variables=nombres,Suma=valores)
print(tabla2)

pob_multi_m<-weighted.mean(x=pobreza$ips[pobreza$pob_multi==1],w=pobreza$factor[pobreza$pob_multi==1],na.rm=TRUE)
pob_multimod_m<-weighted.mean(x=pobreza$ips[pobreza$pob_multimod==1],w=pobreza$factor[pobreza$pob_multimod==1],na.rm=TRUE)
pob_multiex_m<-weighted.mean(x=pobreza$ips[pobreza$pob_multiex==1],w=pobreza$factor[pobreza$pob_multiex==1],na.rm=TRUE)
vul_car_m<-weighted.mean(x=pobreza$ips[pobreza$vul_car==1],w=pobreza$factor[pobreza$vul_car==1],na.rm=TRUE)
vul_ing_m<-weighted.mean(x=pobreza$ips[pobreza$vul_ing==1],w=pobreza$factor[pobreza$vul_ing==1],na.rm=TRUE)
nopob_nov_m<-weighted.mean(x=pobreza$ips[pobreza$nopob_nov==1],w=pobreza$factor[pobreza$nopob_nov==1],na.rm=TRUE)
espacio_pp<-weighted.mean(x=pobreza$ips[pobreza$espacio==1],w=pobreza$factor[pobreza$espacio==1],na.rm=TRUE)
carencias_m<-weighted.mean(x=pobreza$ips[pobreza$carencias==1],w=pobreza$factor[pobreza$carencias==1],na.rm=TRUE)
carencias_3_m<-weighted.mean(x=pobreza$ips[pobreza$carencias_3==1],w=pobreza$factor[pobreza$carencias_3==1],na.rm=TRUE)
espacio_pp<-weighted.mean(x=pobreza$ips[pobreza$espacio==1],w=pobreza$factor[pobreza$espacio==1],na.rm=TRUE)
ic_rezedu_m<-weighted.mean(x=pobreza$ips[pobreza$ic_rezedu==1],w=pobreza$factor[pobreza$ic_rezedu==1],na.rm=TRUE)
ic_asalud_m<-weighted.mean(x=pobreza$ips[pobreza$ic_asalud==1],w=pobreza$factor[pobreza$ic_asalud==1],na.rm=TRUE)
ic_ss_m<-weighted.mean(x=pobreza$ips[pobreza$ic_ss==1],w=pobreza$factor[pobreza$ic_ss==1],na.rm=TRUE)
ic_cv_m<-weighted.mean(x=pobreza$ips[pobreza$ic_cv==1],w=pobreza$factor[pobreza$ic_cv==1],na.rm=TRUE)
ic_sbv_m<-weighted.mean(x=pobreza$ips[pobreza$ic_sbv==1],w=pobreza$factor[pobreza$ic_sbv==1],na.rm=TRUE)
ic_ali_m<-weighted.mean(x=pobreza$ips[pobreza$ic_ali==1],w=pobreza$factor[pobreza$ic_ali==1],na.rm=TRUE)
espacio_pp<-weighted.mean(x=pobreza$ips[pobreza$espacio==1],w=pobreza$factor[pobreza$espacio==1],na.rm=TRUE)
pobex_m<-weighted.mean(x=pobreza$ips[pobreza$pobex==1],w=pobreza$factor[pobreza$pobex==1],na.rm=TRUE) 
pob_m<-weighted.mean(x=pobreza$ips[pobreza$pob==1],w=pobreza$factor[pobreza$pob==1],na.rm=TRUE)
nombres<-c("pob_multi_m","pob_multimod_m","pob_multiex_m","vul_car_m","vul_ing_m", "nopob_nov_m", "espacio_pp", 
           "carencias_m","carencias_3_m","espacio_pp","ic_rezedu_m","ic_asalud_m","ic_ss_m","ic_cv_m","ic_sbv_m",
           "ic_ali_m", "espacio_pp","pobex_m", "pob_m")  
Medias_val<-c(pob_multi_m,pob_multimod_m,pob_multiex_m,vul_car_m,vul_ing_m,nopob_nov_m, espacio_pp, 
              carencias_m,carencias_3_m,espacio_pp,ic_rezedu_m,ic_asalud_m,ic_ss_m,ic_cv_m,ic_sbv_m,
              ic_ali_m, espacio_pp,pobex_m, pob_m)  

tabla3=data.frame(Variables=nombres,Medias=Medias_val)

print(tabla3)

Tabla1_1=data.frame(Medias=tabla1[,2]*a)
Tabla2_1=data.frame(Sumas=tabla2[,2]/z)
Tabla3_1=data.frame(Medias=Medias_val)

print(Tabla1_1)
print(Tabla2_1)
print(Tabla3_1)


wb=loadWorkbook("Bases/Anexo estadístico 2010-2015.xlsx",create=TRUE)
createSheet(wb,name="Cuadro 1")
writeWorksheet(wb,Tabla1_1,sheet="Cuadro 1",startRow = 11,startCol = 6)
writeWorksheet(wb,Tabla2_1,sheet="Cuadro 1",startRow = 11,startCol = 11)
writeWorksheet(wb,Tabla3_1,sheet="Cuadro 1",startRow = 11,startCol = 16)
saveWorkbook(wb)

rm(list=setdiff(ls(),c("a","z","pobreza","espacio_pp","pol1_pp","pol2_pp","pol3_pp","pol4_pp")))

