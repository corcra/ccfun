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




# Log
# ===
# 2016-05-10
# - file created


# ----------------------------------------------------------------------------------------------------------
# Items and function needed for calculation of APACHE score:
# ===
# 2016-05-23

# Functions and Libraries ----------------------------------------------------------------------------------
library(ccdata)
library(ccfun)
library(apacher)
library(assertthat)


# Path and setup -------------------------------------------------------------------------------------------
    # Save the workspace    
        wdbackup <- getwd()
        
    # Defining the Path to Find the Yaml file
        DictionaryPath <- "G:/UCLH/paper-brc/data/"
        Dictionary <- "ANALYSIS_APACHE.yaml"
    
    # Defining the Path to Find the Data Environment  
        DataPath <- "G:/UCLH/paper-brc/data/"
        DataName <- "delta_num.Rdata"
    
    # Naming the Datatable  
        data <- "dt"
    
    # Renaming column of the DataTable
        Renaming <- "yes"
        ColInput <- "NHICcode"
        ColOutput <- "dataItem"


# Dataset Management-----------------------------------------------------------------------------------------
    # Loading environment  
        if (!exists("ccd_delta_num")){
            load(paste(DataPath,DataName, sep = ""))
        }
        
    # Creating the DataTable
        data <- create2dclean(ccd_delta_num, paste(DictionaryPath,Dictionary, sep = ""), nchunks=5)
        
    # Free memory space
        rm(ccd_delta_num)
        gc()
        
    # Translating the DataTable
        ifelse(Renaming == "yes", relabel_cols(wdt,ColInput ,ColOutput), relabel_cols(wdt,ColInput ,ColInput))


# Variables Name needed for APACHE computation -------------------------------------------------------------
      Variables <- NULL
      Variables$age             <- ""
      Variables$admission       <- "Admission type"
      Variables$surgery         <- "classification of surgery"
      Variables$Comorbidity     <- "Comorbidity"
      Variables$rr              <- "Total respiratory rate (monitor)"  
                          # Can be either : "Total respiratory rate (ventilator)" ;
                          #                 "Spontaneous Respiratory Rate"        ;
                          #                 "Mandatory Respiratory Rate"          ;
      Variables$temperature     <- "Temperature - Central"    
                          # Can be either : "Temperature - Non-central            ;
      Variables$map             <- "Mean arterial blood pressure - Art BPMean arterial blood pressure"
                          # Can be either : "Mean arterial blood pressure - NBPMean arterial blood pressure"
      Variables$sap             <- "Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure"
                          # Can be either : "Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure"
      Variables$dap             <- "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure"  
                          # Can be either : "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure"
      Variables$hr              <- "Heart rate"
      Variables$fio2            <- "PaO2/FiO2 ratio"
      Variables$pao2            <- "PaO2 - ABG"
      Variables$paco2           <- "PaCO2 - ABG"
      Variables$pfratio         <- "PaO2/FiO2 ratio"
      Variables$ph              <- "pH - ABG / VBG"
      Variables$hco3            <- "HCO3 - ABG / VBG"
      Variables$sodium          <- "Sodium"
      Variables$potassium       <- "Potassium"
      Variables$creatinine      <- "Creatinine"
      Variables$haemoglobin     <- "Haemoglobin"
      Variables$leucocytes      <- "White cell count"
      Variables$crrt            <- "Chronic renal replacement therapy"
      Variables$gcs             <- "GCS - total"
      
      Comorbidities             <- list("Portal hypertension",
                                        "Heaptic encephalopathy",
                                        "Very severe cardiovascular disease",
                                        "Chronic myelogenous /lymphocytic leukaemia",
                                        "Steroid treatment",
                                        "Acute myeloid/lymphocytic leukaemia or myeloma",
                                        "Severe respiratory disease",
                                        "Congenital immunohumoral or cellular immune deficiency state",
                                        "Chronic renal replacement therapy",
                                        "Biopsy proven cirrhosis",
                                        "Home ventilation",
                                        "Radiotherapy",
                                        "HIV/AIDS",
                                        "Lymphoma",
                                        "Chemotherapy (within the last 6months) steroids alone excluded")


# Non-Time evolutive variables -----------------------------------------------------------------------------

      # APACHE Age (years old) 
      #            - 0: <= 44
      #            - 2: 45-54
      #            - 3: 55-64
      #            - 5: 65-74
      #            - 6: >= 75
data <- gen_apache_age(dt = data, input = Variables$age, output = NULL)

      # APACHE Chronic Organ Failure:
      #            - At least one condition among:
      #                 - Chronic Heart Failure : NYHA 4
      #                 - Hepatic Disease : Biopsy Proven Cirrhosis ; 
      #                                     Documented Portal Hypertension ;
      #                                     Upper Gastro-Intestinal Bleedings due to Portal Hypertension ;
      #                                     Hepatic encephalopathy or comatose episode.
      #                 - Chroncic respiratory insufficiency: Restrictive or Obstructive or Vascular Disease
      #                                                       with severe impairement of physical activities ;
      #                                                       Secondary Polycythemia ;
      #                                                       Documented Chronic Hypoxia or Hypercarbia ; 
      #                                                       Severe Pulmonary Hypertension or
      #                                                       Long-Term Oxygenotherapy.
      #                 - Chronic Renal Replacement Therapy
      #                 - Imunosuppression :  Immunosuppressive Therapy ;
      #                                       Chemotherapy ;
      #                                       Radiotherapy ;
      #                                       Long Lasting Corticotherapy ;
      #                                       Oncohaematologic Disease ;
      #                                       AIDS.
      #            - AND one of the following criteria for admission type:
      #                 - 2: + Elective Surgery
      #                 - 5: + Medical Reason for Admission || + Emergent Surgery
      
data <- gen_comorbidity(dt = data, input = Comorbidities)
data <- gen_apache_chronic(dt = data, admission = Variables$admission, surgery = Variables$surgery, 
                          Comorbidity = Variables$Comorbidity, output = NULL)

# Time evolutive variables ---------------------------------------------------------------------------------

###-- Respiratory Rate  Component
      # APACHE Respiratory Rate (cycles/min)
      #            - 0: 12-24 
      #            - 1: 25-34 
      #            - 2: 10-11 
      #            - 3: 6-9 || 35-49 
      #            - 4: <= 5 || >= 50
data <- gen_apache_rr(dt = data, input = Variables$rr, output = NULL)

###-- Temperature Component
      # APACHE Temperature Score
      #            - 0: 36-38.4 
      #            - 1: 34-35.9 || 38.5-38.9
      #            - 2: 32-33.9
      #            - 3: 30-31.9 || 39-40 
      #            - 4: <= 29.9 || >= 41
data <- gen_apache_temp(dt = data, input = Variables$temperature, output = NULL)

###-- Arterial Pressure Component
      # APACHE Mean Arterial Pressure (mmHg)
      #            - 0: 70-109 
      #            - 2: 50-69 || 110-129 
      #            - 3: 130-159 
      #            - 4: <= 49 || >= 160 
data <- gen_apache_map(dt = data, input = Variables$map, output = NULL)

###-- Heart Rate Component
      # APACHE Heart Rate (beats/min)
      #            - 0: 70-109 
      #            - 2: 55-69 || 110-139 
      #            - 3: 40-54 || 140-179 
      #            - 4: <= 39 || >= 180 
data <- gen_apache_hr(dt = data, input = Variables$hr, output = NULL)

###-- Respiratory Failure Component
      # APACHE Respiratory Failure 
      #       - FiO2 >= 50% ; Grad(A-a) = ((PAtm-PH2O)*FiO2-(PaCO2/Respiratory Quotient))-PaO2 ; 
      #       [Default: QR = 1 ; PAtm = 100 ; PH2O = 6.2 ] ; (kPa):
      #            - 0: <= 26.6
      #            - 2: 26.6-46.4
      #            - 3: 46.5-66.3
      #            - 4: >= 66.4
      #       - FiO2 < 50% ; PaO2 (kPa)
      #            - 0: >= 9.3
      #            - 1: 8.1-9.3
      #            - 3: 7.3-8
      #            - 4: <= 7.3
gen_alveolar_gradient(dt = data, input = Variables$pfratio, pao2 = Variables$pao2, 
                              paco2 = Variables$paco2, pfratio = F, percentage = T, units = "kpa",
                              output = NULL)

data <- gen_apache_rf(dt = data, fio2 = Variables$pfratio, Gradient = "Grad", output = NULL)

###-- Metabolic Failure Component
      # APACHE Metabolic Failure
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
data <- gen_apache_mf(dt = data, hco3 = Variables$hco3, ph = Variables$ph, output = NULL)  

###-- Sodium Component
      # APACHE Natremia (mmol/l)
      #            - 0: 130-149
      #            - 1: 150-154
      #            - 2: 120-129 || 155-159
      #            - 3: 111-119 || 160-179
      #            - 4: <= 110 || >= 180 
data <- gen_apache_Na(dt = data, input = Variables$sodium, output = NULL)   

###-- Potassium Component
      # APACHE Kaliemia (mmol/l)
      #            - 0: 3.5-5.4
      #            - 1: 3-3.4
      #            - 2: 2.5-2.9 || 5.5-5.9
      #            - 3: 6-6.9
      #            - 4: <= 2.5 || >=7
data <- gen_apache_K(dt = data, input = Variables$potassium, output = NULL)

###-- Acute Kidney Injury Component
      # APACHE Creatininemia (µmol/l)
      #         - AKI +
      #            - 0: 54-129
      #            - 4: <= 54 || 130-169
      #            - 6: 170-304
      #            - 8: >= 305
      #         - chronic  (this information has to be extracted from the data1d table)
      #            - 0: 54-129
      #            - 2: < 54 || 130-169
      #            - 3: 170-304
      #            - 4: >= 305
data <- gen_apache_aki(dt = data, input = Variables$creatinine, crrt = Variables$crrt , output = NULL)

###-- Hematocrit Component
      # APACHE Hematocrite (%)
      #            - 0: 30-45.9
      #            - 1: 46-49.9
      #            - 2: 20-29.9 || 50-59.9
      #            - 4: <20 || >=60
data <- gen_apache_ht(dt = data, input = Variables$haemoglobin, haemoglobin = TRUE, vgm = 0.3, output = NULL)

###-- Leucocytes Component
      # APACHE Leucocytes count (1.e-03/mm3)
      #            - 0: 3000-14900
      #            - 1: 15000-19900
      #            - 2: 1000-2900 || 20000-39900
      #            - 4: < 1000 || >= 40000
data <- gen_apache_wbc(dt = data, input = Variables$leucocytes, output = NULL)

###-- Neurologic Component
      # APACHE Glasgow Coma Scale Score
      #            - 0  : GCS = 15
      #            - 1  : GCS = 14
      #            - 2  : GCS = 13
      #            - 3  : GCS = 12
      #            - 4  : GCS = 11
      #            - 5  : GCS = 10
      #            - 6  : GCS = 9
      #            - 7  : GCS = 8
      #            - 8  : GCS = 7
      #            - 9  : GCS = 6
      #            - 10 : GCS = 5
      #            - 11 : GCS = 4
      #            - 12 : GCS = 3
data <- gen_apache_gcs(dt = data, input = Variables$gcs)


###-- Global Apache Score
data[, apache_score := (apache_chronic + apache_rr + apache_temp + apache_map + apache_hr +
                          apache_gcs + apache_wbc + apache_ht + apache_aki + apache_K + apache_Na +
                          apache_mf + apache_rf)]

data[, .N, by = .(apache_chronic , apache_rr , apache_temp , apache_map , apache_hr ,
                          apache_gcs , apache_wbc , apache_ht , apache_aki , apache_K , apache_Na ,
                          apache_mf , apache_rf)]

