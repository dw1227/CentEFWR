#' CentEFWR:
#' Calculate customized centiles of estimated fetal weight (EFW) based on WHO, NICHD, PRB/NICHD, Intergrowth 21, Fetal Medicine Foundation, and Hadlock growth charts.
#'
#' @param data: A data frame with the following columns:
#'
#'     GA: Gestational Age (days)
#'
#'     wt: Maternal Weight (kg). May be left blank if not available.
#'
#'     ht: Maternal Height (cm). May be left blank if not available.
#'
#'     parity: Number of previous deliveries (Whole number).May be left blank if not available.
#'
#'     Sex: Fetal sex (Male or Female or left blank).
#'
#'     EFW: Estimated Fetal Weight (g) Based on Hadlock's formula; If this column is not present in the data frame,
#'     then the following columns must be provided to calculate EFW internally:
#'
#'     AC: Abdominal Circumference (mm)
#'
#'     HC: Head Circumference (mm)
#'
#'     FL: Femur Diaphysis Length (mm) and/or
#'
#'     BPD: Biparietal Diameter (mm)
#'
#' @param saveToFile : Logical value indicating whether or not to export the data to a  comma-separated values (CSV) file.
#' @param filename: Name of the CSV file; Only required if saveToFile = TRUE.
#'
#' @return The input data frame with additional columns for the estimated fetal weight and it's percentiles according to six growth standards.
#' @description   This package provides functionality to calculate the customized percentile
#'  of Estimated Fetal Weight (EFW) based on six growth standards.
#' @examples
#' # Generate sample data with EFW
#' data=data.frame(GA=c(240,210,266),wt=c(88,92,100),
#' ht=c(165,165,170),parity=c(0,5,2),
#' Sex=c("Male","Female",""),Race=c("Black","","Asian"),
#' EFW=c(2000,2500,2600))
#' # Compute the EFW centiles for the sample data
#'  CentEFWR(data)
#'
#'  # Generate sample data without EFW
#' data=data.frame(GA=c(200,210,266),wt=c(88,92,100),
#' ht=c(165,165,170),parity=c(0,5,2),
#' Sex=c("Male","Female",""),Race=c("Black","","Asian"),
#' AC=c(220,270,300),
#' HC=c(250,290,320),
#' FL=c(55,60,75))
#'
#' # Compute the EFW centiles for the sample data
#'  CentEFWR(data)
#'
#' @references
#' Tarca, A.L., et al., A new customized fetal growth standard for African American women: the PRB/NICHD Detroit study. Am J Obstet Gynecol, 2018. 218(2s): p. S679-S691.e4.
#'
#' Grantz, K.L., et al., Fetal growth standards: the NICHD fetal growth study approach in context with INTERGROWTH-21st and the World Health Organization Multicentre Growth Reference Study. Am J Obstet Gynecol, 2018. 218(2s): p. S641-S655.e28.
#'
#' Nicolaides, K.H., et al., Fetal Medicine Foundation fetal and neonatal population weight charts. Ultrasound in Obstetrics & Gynecology, 2018. 52(1): p. 44-51.
#'
#' Stirnemann, J., et al., International estimated fetal weight standards of the INTERGROWTH-21(st) Project. Ultrasound Obstet Gynecol, 2017. 49(4): p. 478-486.
#'
#' Kiserud, T., et al., The World Health Organization Fetal Growth Charts: A Multinational Longitudinal Study of Ultrasound Biometric Measurements and Estimated Fetal Weight. PLoS medicine, 2017. 14(1): p. e1002220-e1002220.
#'
#' Hadlock, F.P., R.B. Harrist, and J. Martinez-Poyer, In utero analysis of fetal growth: a sonographic weight standard. Radiology, 1991. 181(1): p. 129-33.
#' @export

CentEFWR<- function (data,saveToFile =FALSE,filename="efwCentiles.csv")
{

  data$mid=paste0("m",seq(1,nrow(data)))
  rownames(data)=data$mid
  origdata=data

  data$Sex=toupper(as.character(data$Sex))
  data$Race=toupper(as.character(data$Race))
  data$GA=as.numeric(as.character(data$GA))/7
  data$wt=as.numeric(as.character(data$wt))
  data$ht=as.numeric(as.character(data$ht))
  data$ht[data$ht<0]=NA
  data$wt[data$wt<0]=NA
  data$GA[data$GA<0]=NA

  data$parity=as.numeric(as.character(data$parity))


  if(("EFW" %in% colnames(origdata)))
  {
    data$EFW=as.numeric(as.character(data$EFW))

    # data$EFW3= data$EFW
    # data$EFW4 = data$EFW
    data$EFW_int= data$EFW
    data$Hadlock_EFW=data$EFW

    data$INTERGROWTH21_EFW=data$EFW_int
  }




  if(!("EFW" %in% colnames(origdata)))
  {
    if(!(all(c("AC","HC","FL") %in% colnames(data))))
    {
      stop("Not enough information is provided to calculate estimated fetal weight")
    }
     if(!("BPD" %in% colnames(data)))
    {
       data$BPD=NA
    }

    data$AC=as.numeric(as.character(data$AC))
    data$FL=as.numeric(as.character(data$FL))
    data$HC=as.numeric(as.character(data$HC))
    data$BPD=as.numeric(as.character(data$BPD))

    data3= data[!is.na(data$AC)&!is.na(data$FL)&!is.na(data$HC)&is.na(data$BPD),]

    data4= data[!is.na(data$AC)&!is.na(data$FL)&!is.na(data$HC)&!is.na(data$BPD),]


    data2= data[!data$mid %in% c(data3$mid,data4$mid),]


    #################### Hadlock 3 parameter
    data3$EFW=round(with(data3,10^(1.326 +0.0438* AC/10 + 0.158* FL/10  + 0.0107* HC/10 - 0.00326*( AC/10)*( FL/10))),1)
    #################### Hadlock 4 parameter
    data4$EFW = round(with(data4,10^(1.3596 + 0.0064* HC/10 + 0.0424* AC/10 + 0.174* FL/10 + 0.00061*(BPD/10)*(AC/10) - 0.00386*( AC/10)*( FL/10))),1)

   if(nrow(data2)>0) {data2$EFW=NA}

    data=rbind(data3,data4,data2)
        ################## Intergrowth EFW
    data$EFW_int= round(with(data,exp(5.08482 - 54.06633 * ((AC / 1000) ^ 3) - 95.80076 * ((AC / 1000) ^ 3) * log(AC / 1000) + 3.13637 * (HC / 1000))),1)


      # data$"EFW[Hadlock(AC,FL,HC)]"=data$EFW3
      # data$"EFW[Hadlock(AC,FL,HC,BPD)]"=data$EFW4
      # data$"EFW[Intergrowth21(AC,HC)]"=data$EFW_int

  }

  if(any(!data$Race %in% toupper(c("White","Black","Hispanic","Asian",""))))
  {

    stop("Race should be White, Black, Hispanic, Asian  or left blank")
  }



  if(any(!data$Sex %in% c("MALE","FEMALE","")))
  {

    stop("Sex should be Male, Female or left blank")
  }

  data$Sex=ifelse(data$Sex=="MALE","Male",ifelse(data$Sex=="FEMALE","Female",""))
  data$Race=ifelse(data$Race=="WHITE","White",ifelse(data$Race=="BLACK","Black",
 ifelse(data$Race=="HISPANIC","Hispanic",ifelse(data$Race=="ASIAN","Asian",""))))


  datasafe =data


  #########################Hadlock 3 and 4 parameter
  #datasafe$"Hadlock(AC,FL,HC)"=""
  # datasafe$"Hadlock(AC,FL,HC,BPD)"=""
  datasafe$Hadlock=""
  data=data[data$GA<=41 & data$GA>=10 & !is.na(data$EFW),]
  if(nrow(data)>0)
  {
  data$mu=with(data,exp(0.578+0.332*GA-0.00354*GA^2)) #Hadlock EFW means at each GA  (in week)
  data$sd=with(data,0.127*mu)  #variability as a percentage of the mean EFW
  data$zga=with(data,(EFW-mu)/sd) # Z scores for the EFW at each ultrasound scan
  data$Hadlock= with(data,round(pnorm(zga)*100,1))
  # data$zga=with(data,(EFW4-mu)/sd) # Z scores for the EFW at each ultrasound scan
  # data$"Hadlock(AC,FL,HC,BPD)"= with(data,round(pnorm(zga)*100,1))
  data$zga=NULL;data$sd=NULL;data$mu=NULL
  datasafe[rownames(data),"Hadlock"]=data$Hadlock

  # datasafe[rownames(data),"Hadlock(AC,FL,HC)"]=data$"Hadlock(AC,FL,HC)"
  # datasafe[rownames(data),"Hadlock(AC,FL,HC,BPD)"]=data$"Hadlock(AC,FL,HC,BPD)"
  }
  data=datasafe


  #Fetal medicine foundation
  datasafe$FetalMedicineFoundation=""

  data=data[data$GA<43 & data$GA>=20,]
  if(nrow(data)>0)
  {
  data$GAd=data$GA*7;data$x=data$GAd-199 #offset
  data$mn=with(data,(3.0893 + (0.00835*x) - (0.00002965*(x^2)) - (0.00000006062*(x^3))))
  data$sde=with(data,0.02464 + 0.00005640*GAd) #for efw
  data$EFWt=log10(data$EFW)
  data$zga=with(data,(EFWt-mn)/sde) # Z scores for the EFW at each ultrasound scan
  data$FetalMedicineFoundation= with(data,round(pnorm(zga)*100,1) )
  datasafe[rownames(data),"FetalMedicineFoundation"]=data$FetalMedicineFoundation
  }
  data=datasafe

  #########################Intergrowth
  datasafe$Intergrowth21=""
  data=data[data$GA<=40 & data$GA>=22 & !is.na(data$EFW_int),]
  if(nrow(data)>0)
  {
  #Skewness
  data$l = with(data,-4.257629-(2162.234 * GA^(-2)) + (0.0002301829 * GA^3 ))
  #Mean
  data$mu= with(data,4.956737 + (0.0005019687 * GA^3) - (0.0001227065 * GA^3 * log(GA)))
  #Coefficient of variation
  data$s = with(data,10^(-4) * (-6.997171 + (0.057559 * GA^3) - (0.01493946 * GA^3 * log(GA))))
  #centile
  data$zga = with(data,ifelse(l== 0,  s^(-1)* log(log(EFW_int)/mu),((s*l)^(-1))*(((log(EFW_int)/mu)^l) - 1)))
  data$Intergrowth= with(data,round(pnorm(zga)*100,1))
  datasafe[rownames(data),"Intergrowth21"]=data$Intergrowth
  }
  data=datasafe



  #interpolate
  interp=function(z,standard){
    y=as.numeric(as.character(z[-1]));
    if(standard=="prb")
    {
      x1=log(as.numeric(as.character(z[1])))
      #x1=as.numeric(as.character(z[1]))
      names(y)=as.character(c(2.5,5,10,25,50,75,90,95,97.5));
    }
    if(standard=="who")
    {
      x1=as.numeric(as.character(z[1]))
      names(y)=as.character(c(2.5,5,10,25,50,75,90,95,97.5));
    }

    if(standard=="nichd")
    {
      x1=as.numeric(as.character(z[1]));
      names(y)=c("3","5","10","50","90","95","97");
    }
    y=y[!is.na(y)];
    cent=as.numeric(as.character(names(y)));
    lcent=length(cent);

    if(all(y-x1<=0)){
      le=cent[c(lcent-1)]
      ri=cent[(lcent)]
    }else if(all(y-x1>=0)){
      le=cent[1]
      ri=cent[2]
    }else{
      le=max(cent[y-x1<=0])
      ri=min(cent[y-x1>=0])
    }
    if(le != ri)
    {
    mx=as.numeric(as.character(c(y[cent==le],y[cent==ri])))
    my=as.numeric(as.character(c(le,ri)))

    mod=lm(my~mx,data.frame(my,mx))
    val=round(predict(mod,data.frame(mx=x1)),1)
    }
    if (le == ri)
    {
      val=le
    }
    if(val>100){val=100}
    if(val<0){val=0}
    return(val)
  }

  #################PRB centiles
  datasafe$"PRBNICHD"=""
  data=data[!is.na(data$wt)&!is.na(data$ht)&!is.na(data$parity)&!is.na(data$EFW),]
  data=data[as.character(data$Sex) %in% c("Male","Female") & as.character(data$Race) %in% "Black",]
  data=data[data$GA<=40 & data$GA>=14,]
  if(nrow(data)>0)
  {

  bmilo=20.5;bmihi=40.4
  makeX=function(cof,ano){

    for(n in rownames(cof)){
      if(n=="(Intercept)"){
        X=rep(1,dim(ano)[1])
      }
      if(n%in%names(ano)){
        X=cbind(X,ano[,n])
      }
      if(grepl(":",n)){
        v1=unlist(strsplit(n,split=":"))[1]
        v2=unlist(strsplit(n,split=":"))[2]
        X=cbind(X,ano[,v1]*ano[,v2])
      }
    }
    X
  }
  ano=data
  ano$x=(ano$GA-40)/10
  ano$x2=ano$x^2
  ano$x3=ano$x^3
  ano$Sexx=ifelse(ano$Sex=="Male",1,0)
  ano$Para=ano$parity
  ano$Para[ano$Para>=3]<-3
  ano$Para=factor(ano$Para)
  ano$hc=(ano$ht-163)/10
  ano$Para1=ifelse(ano$Para=="1",1,0)
  ano$Para2=ifelse(ano$Para=="2",1,0)
  ano$Para3=ifelse(ano$Para=="3",1,0)
  ano$lowlim = bmilo * ano$ht ^ 2 / 10000
  ano$higlim = bmihi * ano$ht ^ 2 / 10000
  ano$wt2=ano$wt
  ano$wt2[ano$wt2<ano$lowlim]<-ano$lowlim[ano$wt2<ano$lowlim]
  ano$wt2[ano$wt2>ano$higlim]<-ano$higlim[ano$wt2>ano$higlim]
  ano$wt=ano$wt2
  ano$wc=(ano$wt-64)/10
  pmat=makeX(cof,ano=ano)
  qua=pmat%*%cof
  dat2=cbind(data$EFW,(qua))

  data$PRBNICHD=apply(dat2,1,interp,standard="prb")
  datasafe[rownames(data),"PRBNICHD"]=data$PRBNICHD
  }
  data=datasafe

  #################NICHD centiles
  datasafe$NICHD=""
  srcdata=NICHD_srcdata;rownames(srcdata)=paste(srcdata$Race,srcdata$GA)
  data=datasafe[datasafe$Race %in% c("White","Black","Asian","Hispanic") & !is.na(data$EFW),]
  data=data[data$GA<=42 & data$GA>=10,]

  if(nrow(data)>0)
  {
  data$GA_i=floor(data$GA)
  data$index=paste(data$Race,data$GA_i)
  tmp=(cbind(EFW=data$EFW,srcdata[data$index,as.character(c(3,5,10,50,90,95,97))]))
  colnames(tmp)[-1]= paste0("q",colnames(tmp[-1]))
  data$NICHD=apply(tmp,1,interp,"nichd")
  datasafe[rownames(data),"NICHD"]=data$NICHD
  }
  data=datasafe
  #################WHO centiles
  datasafe$WHO=""
  srcdata=WHO_srcdata;rownames(srcdata)=paste(srcdata$FBM,srcdata$GA,sep="_")
  data=data[data$GA<=40 & data$GA>=14 & !is.na(data$EFW),]
  if(nrow(data)>0)
  {
  data$GA_i=floor(data$GA)
  data$index=paste(ifelse(data$Sex%in%"Male","EFWT_M",ifelse(data$Sex%in%"Female","EFWT_FE","EFWT")),
                   data$GA_i,sep="_")
  tmp=(cbind(EFW=data$EFW,srcdata[data$index,as.character(c(2.5,5,10,25,50,75,90,95,97.5))]))
  colnames(tmp)[-1]= paste0("q",colnames(tmp[-1]))
  data$WHO=apply(tmp,1,interp,"who")
  datasafe[rownames(data),"WHO"]=data$WHO
  }
  data=datasafe



  data[,"GA"]=origdata[rownames(data),"GA"];
  data=data[origdata$mid,]
  data$wt=origdata$wt;data$GA=origdata$GA;data$ht=origdata$ht;

  #data$EFW_int=NULL;
  #data$INTERGROWTH21_EFW=NULL;
   data$Hadlock_EFW=NULL
  # data$Hadlock_EFW=data$EFW;
  data$EFW_int=NULL;
  data$mid=NULL;
  rownames(data)=NULL

#
#   if(!"BPD" %in% colnames(origdata))
#   {
#     data$"EFW[Hadlock(AC,FL,HC,BPD)]"=NULL
#     data$"Hadlock(AC,FL,HC,BPD)"=NULL
#    data$BPD=NULL
#   }
#   if("EFW" %in% colnames(origdata))
#   {
#     colnames(data)=gsub("Hadlock\\(AC,FL,HC)","HAdlock",colnames(data))
#   }


  data[is.na(data)]=""
  if(saveToFile ==TRUE){write.csv(data,file=filename)}
  return(data)

}
