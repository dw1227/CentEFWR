# CentEFWR
Percentile calculator based on estimated fetal weight growth charts.

# Examples
# Generate sample data with EFW
data=data.frame(GA=c(240,210,266),wt=c(88,92,100),
ht=c(165,165,170),parity=c(0,5,2),
Sex=c("Male","Female",""),Race=c("Black","","Asian"),
EFW=c(2000,2500,2600))
# Compute the EFW centiles for the sample data
 CentEFWR(data)

 # Generate sample data without EFW
data=data.frame(GA=c(200,210,266),wt=c(88,92,100),
ht=c(165,165,170),parity=c(0,5,2),
Sex=c("Male","Female",""),Race=c("Black","","Asian"),
AC=c(220,270,300),
HC=c(250,290,320),
FL=c(55,60,75))
# Compute the EFW centiles for the sample data
 CentEFWR(data)
 
#References
Tarca, A.L., et al., A new customized fetal growth standard for African American women: the PRB/NICHD Detroit study. Am J Obstet Gynecol, 2018. 218(2s): p. S679-S691.e4.

Grantz, K.L., et al., Fetal growth standards: the NICHD fetal growth study approach in context with INTERGROWTH-21st and the World Health Organization Multicentre Growth Reference Study. Am J Obstet Gynecol, 2018. 218(2s): p. S641-S655.e28.

Nicolaides, K.H., et al., Fetal Medicine Foundation fetal and neonatal population weight charts. Ultrasound in Obstetrics & Gynecology, 2018. 52(1): p. 44-51.

Stirnemann, J., et al., International estimated fetal weight standards of the INTERGROWTH-21(st) Project. Ultrasound Obstet Gynecol, 2017. 49(4): p. 478-486.

Kiserud, T., et al., The World Health Organization Fetal Growth Charts: A Multinational Longitudinal Study of Ultrasound Biometric Measurements and Estimated Fetal Weight. PLoS medicine, 2017. 14(1): p. e1002220-e1002220.

Hadlock, F.P., R.B. Harrist, and J. Martinez-Poyer, In utero analysis of fetal growth: a sonographic weight standard. Radiology, 1991. 181(1): p. 129-33.

