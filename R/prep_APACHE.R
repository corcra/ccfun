# author: Arthur Le Gall
# date: 2016-05-23
# subject: Calculate APACHE II score

# Readme
# ======
# See issue https://github.com/UCL-HIC/paper-brc/issues/15

# Function should be passed a (one row per time-point per episode/patient 2d data)
# Function will expect that the data has been _pre_ aggregated


# Todo
# ====
#
# -- To create a funcion to determine the parameter to use to calculate respiratory failure score
#     - Depending on the FiO2
#             - FiO2 < 0.5 : PaO2 is the considered variable
#             - Fio2 >= 0.5 : Alveolo-arterial gradient [ Grad.(A-a) ] is the considered variable
#
#
# -- To create a function to determine the parameter to use to calculate metabolic failure
#     - Depending on the availability of the pH variable
#
#
# -- To create a function to determine the parameter to use to calculate renal failure
#     - Depending on the AKI presence or absence
#
#
# -- To create a function that will find the presence or absence of chronic diseases, such as:
#             - Hepatopathie :
#                   - Documented Cirrhosis
#                   - Documented Portal Hypertension
#                   - Upper Gastro-Intestinal Bleeding due to Portal Hypertension
#                   - Hepatic encephalopathy or Hepatic comatose
#             - Chronic Heart Failure :
#                   - NYHA IV
#             - Chronic Respiratory Insufficiency :
#                   - Restrictive, Obstructive or Vascular Respiratory Failure leading to impaired physical exercise
#                   - Documented Chronic Hypercarbia or Hypoxia
#                   - Secondary Polycythemia
#                   - Severe Pulmonary Hypertension
#                   - Dependance to respiratory support
#             - Chronic Renal Failure :
#                   - Chronic Hemodialysis
#             - Immunodepression :
#                   - Immunosuppressive Chemotherapy
#                   - Radiotherapy
#                   - Long term Corticosteroid Therapy
#                   - Leucemia
#                   - Lymphoma
#                   - AIDS



# Log
# ===
# 2016-05-10
# - file created

# Items for calculation of APACHE score
# ===
# 2016-05-23



# -------------------------------------------------------------------


###--- Respiratory Rate  Component

# APACHE Respiratory Rate (cycles/min)
#            - 0: 12-24
#            - 1: 25-34
#            - 2: 10-11
#            - 3: 6-9 || 35-49
#            - 4: <= 5 || >= 50


gen_APACHE_RR <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  if (!header)
    {
    header<- c("Spontaneous Respiratory Rate","Total respiratory rate (monitor)","Mandatory Respiratory Rate","Total respiratory rate (ventilator)")
    dat1 <- dat[,header,with=F]
    }
  else
    {
    dat1<-dat[,.(header),with=F]
    }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Respiratory Rate
  dat1[,Max_RR:=do.call(pmin.int, c(.SD, na.rm=TRUE)), .SDcols=header]
  setkey(dat1,Max_RR)
  dat1[.(c(seq(12,24,1))),RR_APACHE := 0]
  dat1[.(c(seq(25,34,1))),RR_APACHE := 1]
  dat1[.(c(seq(10,11,1))),RR_APACHE := 2]
  dat1[.(c(seq(6,9,1),seq(35,49,1))),RR_APACHE := 3]
  dat1[(Max_RR %between% c(0,5))|(Max_RR > 49),RR_APACHE := 4]
  dat1[,Max_RR:=NULL]

  dat[,RR_APACHE:=dat1[,RR_APACHE]]
}

#                         ----------------------------------------------------------------------------------------------------------


###--- Temperature Component

# APACHE Temperature Score
#
#            - 0: 36-38.4
#            - 1: 34-35.9 || 38.5-38.9
#            - 2: 32-33.9
#            - 3: 30-31.9 || 39-40
#            - 4: <= 29.9 || >= 41

gen_APACHE_T <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  if (!header)
  {
    header<- c("Temperature - Non-central","Temperature - Central")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Respiratory Rate
  dat1[,Max_T:=do.call(pmax.int, c(.SD, na.rm=TRUE)), .SDcols=header]
  setkey(dat1,Max_T)
  dat1[.(c(seq(36,38.4,0.1))),T_APACHE := 0]
  dat1[.(c(seq(34,35.9,0.1),seq(38.5,38.9,0.1))),T_APACHE := 1]
  dat1[.(c(seq(32,33.9,0.1))),T_APACHE := 2]
  dat1[.(c(seq(30,31.9,0.1), seq(39,40,0.1))),T_APACHE := 3]
  dat1[(Max_T < 29)|(Max_T > 40),T_APACHE := 4]
  dat1[,Max_T:=NULL]

  dat[,T_APACHE:=dat1[,T_APACHE]]
}

#                         ----------------------------------------------------------------------------------------------------------

###---Arterial Pressure Component
#
# APACHE Mean Arterial Pressure (mmHg)
#            - 0: 70-109
#            - 2: 50-69 || 110-129
#            - 3: 130-159
#            - 4: <= 49 || >= 160
#

gen_APACHE_MAP <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  gen_MAP<-function(Sys,Dia){MAP <- round((Sys*(1/3) + Dia*(2/3)),0)}

  if (!header)
  {
    header<- c("Mean arterial blood pressure - NBPMean arterial blood pressure",
               "Mean arterial blood pressure - Art BPMean arterial blood pressure",
               "Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure",
               "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure",
               "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure",
               "Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure")
    MAP<- c("Mean arterial blood pressure - NBPMean arterial blood pressure",
            "Mean arterial blood pressure - Art BPMean arterial blood pressure")

    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,header,with=F]
  }

  # Convert factor to numerical value
  dat1[, header := lapply(.SD,function(x){x<-as.numeric(as.character(x))}),.SDcols= header]

  # Compute APACHE for Respiratory Rate
  dat1[,"Min_C":=do.call(pmin.int, c(.SD, na.rm=TRUE)), .SDcols = MAP]

  setkey(dat1,Min_C)
  dat1[.(NA),"Min_C" := gen_MAP(get("Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure"),get("Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure")),with=F,by = 1:nrow(dat1[.(NA)])]

  setkey(dat1,Min_C)
  dat1[.(NA),"Min_C" := gen_MAP(get("Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure"),get("Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure")),with=F,by = 1:nrow(dat1[.(NA)])]

  setkey(dat1,Min_C)
  dat1[.(c(seq(70,109,1))),C_APACHE := 0]
  dat1[.(c(seq(50,69,1),seq(110,129,1))),C_APACHE := 2]
  dat1[.(c(seq(130,159,1))),C_APACHE := 3]
  dat1[(Min_C < 49)|(Min_C > 160),C_APACHE := 4]

  dat1[,Min_C:=NULL]

  dat[,C_APACHE:=dat1[,C_APACHE]]
}


#                         ----------------------------------------------------------------------------------------------------------

###---Heart Rate Component
#
# - Heart Rate (beats/min)
#            - 0: 70-109
#            - 2: 55-69 || 110-139
#            - 3: 40-54 || 140-179
#            - 4: <= 39 || >= 180


gen_APACHE_HR <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  if (!header)
  {
    header<- c("Heart rate")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Respiratory Rate
  dat1[,Max_RF:=do.call(pmax.int, c(.SD, na.rm=TRUE)), .SDcols=header]
  setkey(dat1,Max_RF)
  dat1[.(c(seq(70,109,1))),HR_APACHE := 0]
  dat1[.(c(seq(55,69,1),seq(110,139,1))),HR_APACHE := 2]
  dat1[.(c(seq(40,54,1),seq(140,179,1))),HR_APACHE := 3]
  dat1[(Max_RF < 40)|(Max_RF > 179),HR_APACHE := 4]
  dat1[,Max_RF:=NULL]

  dat[,HR_APACHE:=dat1[,HR_APACHE]]
}

#                         ----------------------------------------------------------------------------------------------------------


###---Respiratory Failure Component
#
# - Respiratory Failure
#       - FiO2 >= 50% ; Grad(A-a) = ((PAtm-PH2O)*FiO2-(PaCO2/Respiratory Quotient))-PaO2 ; [Default: QR = 1 ; PAtm = 100 ; PH2O = 6.2 ] ; (kPa)
#            - 0: <= 26.6
#            - 2: 26.6-46.4
#            - 3: 46.5-66.3
#            - 4: >= 66.4
#       - FiO2 < 50% ; PaO2 (kPa)
#            - 0: >= 9.3
#            - 1: 8.1-9.3
#            - 3: 7.3-8
#            - 4: <= 7.3


"PaO2/FiO2 ratio"
"Inspired fraction of oxygen"
"PaO2 - ABG"

gen_APACHE_RF <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  if (!header)
  {
    header<- c("PaO2 - ABG","Inspired fraction of oxygen","PaCO2 - ABG")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Respiratory Rate

  setkey(dat1,"Inspired fraction of oxygen","PaCO2 - ABG")


  dat1[.(c(seq(0,49,1)),c(seq(9.3,100,0.1))) ,RF_APACHE := 0]
  dat1[.(c(seq(0,49,1)),c(seq(8.1,9.3,0.1))) ,RF_APACHE := 1]
  dat1[.(c(seq(0,49,1)),c(seq(7.3,8,0.1))) ,RF_APACHE := 3]
  dat1[.(c(seq(0,49,1)),c(seq(0,7.3,0.1))) ,RF_APACHE := 4]


  dat1[.(c(seq(50,100,1))), Grad := lapply(dat1,function(x){x<- 93.8 * (`Inspired fraction of oxygen` - (`PaCO2 - ABG` - `PaO2 - ABG` ))}), by=1:nrow(dat1[.(c(seq(50,100,1))),])]

  setkey(dat1,"Inspired fraction of oxygen","Grad")


  dat1[.(c(seq(50,100,1)),c(seq(0,26.5,0.1))),RF_APACHE := 0 ]
  dat1[.(c(seq(50,100,1)),c(seq(26.6,46.4,0.1))),RF_APACHE := 2]
  dat1[.(c(seq(50,100,1)),c(seq(46.5,66.3,0.1))),RF_APACHE := 3]
  dat1[.(c(seq(50,100,1)),c(seq(66.4,100,0.1))),RF_APACHE := 4]

  dat1[,Grad:=NULL]

  dat[,RF_APACHE:=dat1[,RF_APACHE]]
}

#                         ----------------------------------------------------------------------------------------------------------

# M?tabolic Failure Component

# - Metabolic Failure
#       - pH unavailable ; [HCO3-] (mmol/l)
#            - 0: 22-31.9
#            - 1: 32-40.9
#            - 2: 18-21.9
#            - 3: 15-17.9 || 41-51.9
#            - 4: <= 15 || >= 52
#       - pH available
#            - 0: 7.33-7.49
#            - 1: 7.5-7.59
#            - 2: 7.25-7.32
#            - 3: 7.15-7.24 || 7.6-7.69
#            - 4: <=7.15 || >= 7.7
#

gen_APACHE_MF <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with respiratory rate
  if (!header)
  {
    header<- c("pH - ABG / VBG","HCO3 - ABG / VBG")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  setkey(dat1,"pH - ABG / VBG","HCO3 - ABG / VBG")


  dat1[.(NA,c(seq(22,31.9,0.1))) ,MF_APACHE := 0]
  dat1[.(NA,c(seq(32,40.9,0.1))) ,MF_APACHE := 1]
  dat1[.(NA,c(seq(18,21.9,0.1))) ,MF_APACHE := 2]
  dat1[.(NA,c(seq(15,17.9,0.1),seq(41,51.9,0.1))) ,MF_APACHE := 3]
  dat1[is.na("pH - ABG / VBG") & ("HCO3 - ABG / VBG" < 15 | "HCO3 - ABG / VBG" > 51.9) ,MF_APACHE := 4]



  dat1[.(c(seq(7.33,7.49,0.01))),MF_APACHE := 0 ]
  dat1[.(c(seq(7.5,7.59,0.01))),MF_APACHE := 1]
  dat1[.(c(seq(7.25,7.32,0.01))),MF_APACHE := 2]
  dat1[.(c(seq(7.15,7.24,0.01),seq(7.6,7.69,0.01))),MF_APACHE := 3]
  dat1[!is.na("pH - ABG / VBG") & ("ph - ABG / VBG" < 7.15 | "ph - ABG / VBG" > 7.69) ,MF_APACHE := 4]



  dat[,MF_APACHE:=dat1[,MF_APACHE]]
}



#                         ----------------------------------------------------------------------------------------------------------


###--- Sodium Component

# - Natremia (mmol/l)
#            - 0: 130-149
#            - 1: 150-154
#            - 2: 120-129 || 155-159
#            - 3: 111-119 || 160-179
#            - 4: <= 110 || >= 180
#


gen_APACHE_Na <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with sodium level
  if (!header)
  {
    header<- c("Sodium")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Sodium level

  setkey(dat1,header)
  dat1[.(c(seq(130,149,1))),Na_APACHE := 0]
  dat1[.(c(seq(150,154,1))),Na_APACHE := 1]
  dat1[.(c(seq(120,129,1),seq(155,159,1))),Na_APACHE := 2]
  dat1[.(c(seq(111,119,1),seq(160,179,1))),Na_APACHE := 3]
  dat1[(header < 111)|(header > 179),Na_APACHE := 4,with=F]

  dat[,Na_APACHE:=dat1[,Na_APACHE]]
}


#                         ----------------------------------------------------------------------------------------------------------


###--- Potassium Component

# - Kaliemia (mmol/l)
#            - 0: 3.5-5.4
#            - 1: 3-3.4
#            - 2: 2.5-2.9 || 5.5-5.9
#            - 3: 6-6.9
#            - 4: <= 2.5 || >=7


gen_APACHE_K <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with sodium level
  if (!header)
  {
    header<- c("Potassium")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Potassium level

  setkey(dat1,header)
  dat1[.(c(seq(3.5,5.4,0.1))),K_APACHE := 0]
  dat1[.(c(seq(3,3.4,0.1))),K_APACHE := 1]
  dat1[.(c(seq(2.5,2.9,0.1),seq(5.5,5.9,0.1))),K_APACHE := 2]
  dat1[.(c(seq(6,6.9,0.1))),K_APACHE := 3]
  dat1[(header < 2.5)|(header > 6.9),K_APACHE := 4,with=F]

  dat[,K_APACHE:=dat1[,K_APACHE]]
}



#                         ----------------------------------------------------------------------------------------------------------


###--- Acute Kidney Injury Component

# - Creatininemia (?mol/l)
#         - AKI +
#            - 0: 54-129
#            - 4: <= 54 || 130-169
#            - 6: 170-304
#            - 8: >= 305
#         - chronic  (this information have to be extracted from the data1d table)
#            - 0: 54-129
#            - 2: < 54 || 130-169
#            - 3: 170-304
#            - 4: >= 305
#


gen_APACHE_AKI <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with sodium level
  if (!header)
  {
    header<- c("Creatinine","CRF")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for AKI

  setkey(dat1,"Creatinine","CRF")
  dat1[.(c(seq(54,129,1))),AKI_APACHE := 0]
  dat1[("Creatinine" < 54 | "Creatinine" %between% c(130-169)) & "CRF"== "yes",AKI_APACHE := 2]
  dat1[.(c(seq(170,304,1),c("yes"))),AKI_APACHE := 3]
  dat1[ "Creatinine" > 304 & "CRF"== "yes",AKI_APACHE := 4,with=F]
  dat1[("Creatinine" < 54 | "Creatinine" %between% c(130-169)) & "CRF"== "no",AKI_APACHE := 4]
  dat1[.(c(seq(170,304,1),c("no"))),AKI_APACHE := 6]
  dat1[ "Creatinine" > 304 & "CRF"== "no",AKI_APACHE := 8,with=F]


  dat[,AKI_APACHE:=dat1[,AKI_APACHE]]
}



#                         ----------------------------------------------------------------------------------------------------------


###--- Hematocrit Component

# - Hematocrite (%)
#            - 0: 30-45.9
#            - 1: 46-49.9
#            - 2: 20-29.9 || 50-59.9
#            - 4: <20 || >=60
#


gen_APACHE_Hemo <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with hematocrit level
  if (!header)
  {
    header<- c("Hematocrit")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Hematocrit level

  setkey(dat1,header)
  dat1[.(c(seq(30,45.9,0.1))),Hemo_APACHE := 0]
  dat1[.(c(seq(46,49.9,0.1))),Hemo_APACHE := 1]
  dat1[.(c(seq(20,29.9,0.1),seq(50,59.9,0.1))),Hemo_APACHE := 2]
  dat1[(header < 20)|(header > 59.9),Hemo_APACHE := 4,with=F]

  dat[,Hemo_APACHE:=dat1[,Hemo_APACHE]]
}


#                         ----------------------------------------------------------------------------------------------------------


###--- Leucocytes Component

# - Leucocytes count (/mm3)
#            - 0: 3000-14900
#            - 1: 15000-19900
#            - 2: 1000-2900 || 20000-39900
#            - 4: < 1000 || >= 40000
#


gen_APACHE_Leuco <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with Leucocytes level
  if (!header)
  {
    header<- c("Leucocytes")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for Leucocytes level

  setkey(dat1,header)
  dat1[.(c(seq(3000,4000,1))),Leuco_APACHE := 0]
  dat1[.(c(seq(15000,19000,1))),Leuco_APACHE := 1]
  dat1[.(c(seq(1000,2900,1),seq(20000,39900,1))),Leuco_APACHE := 2]
  dat1[(header < 1000)|(header > 40000),Leuco_APACHE := 4,with=F]

  dat[,Leuco_APACHE:=dat1[,Leuco_APACHE]]
}




#                         ----------------------------------------------------------------------------------------------------------

# - Glasgow Coma Scale Score
#            - Score = 15-GCS
#

gen_APACHE_GCS <- function(dat,header = FALSE){
  require(data.table)

  # First step is to find patterns associated with GCS score
  if (!header)
  {
    header<- c("GCS - total")
    dat1 <- dat[,header,with=F]
  }
  else
  {
    dat1<-dat[,.(header),with=F]
  }

  # Convert factor to numerical value
  dat1[, (header) := (lapply(.SD,function(x){x<-as.numeric(as.character(x))})),.SDcols=header]

  # Compute APACHE for GCS Score level
  dat1[, GCS_APACHE := (lapply(.SD,function(x){x<- 15-x})),.SDcols=header]

  dat[,GCS_APACHE:=dat1[,GCS_APACHE]]
}


#                         ----------------------------------------------------------------------------------------------------------



# Calculate the APACHE score using previous functions


gen_APACHE <- function(dat){

  dat1<-dat
  dat1<-ifelse(class(try(gen_APACHE_GCS(dat1),T))=="try-error",function(x){return(x)
    print("Function can't succeed to calculate APACHE for GCS score")},gen_APACHE_GCS(dat1))
  dat1<-gen_APACHE_Leuco
  dat1<-gen_APACHE_Hemo
  dat1<-gen_APACHE_AKI
  dat1<-gen_APACHE_K
  dat1<-gen_APACHE_Na
  dat1<-gen_APACHE_MF
  dat1<-gen_APACHE_RF
  dat1<-gen_APACHE_HR
  dat1<-gen_APACHE_RR
  dat1<-gen_APACHE_T
  dat1<-gen_APACHE_MAP
}


