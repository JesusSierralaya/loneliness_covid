
# Directorio base
BASE_DIR <- "~/../../UAM"

#### BDD maestra #### 

# Carpeta maestra
DB_PATH_MASTER <- file.path(BASE_DIR, "marta.miret@uam.es - Bases de datos maestras Edad con Salud")

# Archivo rawdata c2019w1
DB_FILE_PRE <- file.path(DB_PATH_MASTER, "Ola_3/Cohorte_2019", "rawdata_c2019w1.dta")

# Archivo sub estudio covid
DB_FILE_POST <- file.path(DB_PATH_MASTER, "Subestudio_COVID", "Edad_con_salud_Fichero_Completo.dta")

# Archivo de pesos
DB_FILE_WEIGHTS <- file.path(DB_PATH_MASTER, "Ola_3/Cohorte_2019/Pesos", "pesos_norm.dta")

#### BDD documentación #### 

# Carpeta documentación
DB_PATH_DOC <- file.path(BASE_DIR, "marta.miret@uam.es - Documentacion Edad con Salud")

# Fichero outcomes pre y post
CB_FILE_OUTCOMES_PRE <- file.path(DB_PATH_DOC, "Edad con salud - Ola 3/Outcomes/Cohorte 2019/Outcome datasets")
CB_FILE_OUTCOMES_POST <- file.path(DB_PATH_DOC, "Edad con salud - Subestudio COVID/Outcomes/Outcome datasets")

