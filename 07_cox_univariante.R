# COX UNIVARIANTE (N=125)
datos_cox <- datos_surv %>%
  mutate(
    metastasis = factor(tolower(metastasis), levels = c("no","si")),
    
    cirugia = factor(tolower(cirugia), levels = c("no","si")),
    
    sexo = factor(tolower(sexo), levels = c("hombre","mujer")),
    
    ca_cat = case_when(
      ca_19_9_al_dx <= 37 ~ "≤37",
      ca_19_9_al_dx <= 500 ~ "38–500",
      ca_19_9_al_dx > 500 ~ ">500"
    ),
    
    ca_cat = factor(ca_cat, levels = c("≤37","38–500",">500")),
    
    # 🔹 TAMAÑO TUMORAL
    tam_cat = case_when(
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm <= 2 ~ "≤2 cm",
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm > 2 ~ ">2 cm"
    ),
    
    tam_cat = factor(tam_cat, levels = c("≤2 cm",">2 cm"))
  )
datos_cox_principal <- datos_cox %>%
  drop_na(
    tiempo,
    evento,
    edad,
    metastasis,
    cirugia,
    sexo,
    ca_cat
  )

tabla_cox_uni_principal <- tbl_uvregression(
  datos_cox_principal %>%
    select(
      tiempo,
      evento,
      edad,
      metastasis,
      cirugia,
      sexo,
      ca_cat
    ),
  method = coxph,
  y = Surv(tiempo, evento),
  exponentiate = TRUE
) %>%
  bold_labels()

tabla_cox_uni_principal

nrow(datos_cox_principal)


#-------------------------------------------

# COX UNIVARIANTE (Con tam_cat-muestra reducida)

datos_cox_modelo <- datos_cox %>%
  drop_na(
    tiempo,
    evento,
    edad,
    metastasis,
    cirugia,
    sexo,
    ca_cat,
    tam_cat
  )

# Tabla COX univariante (con tam_cat-reducción de muestra)

tabla_cox_uni <- tbl_uvregression(
  datos_cox_modelo %>%
    select(
      tiempo,
      evento,
      edad,
      metastasis,
      cirugia,
      sexo,
      ca_cat,
      tam_cat
    ),
  method = coxph,
  y = Surv(tiempo, evento),
  exponentiate = TRUE
)

tabla_cox_uni

nrow(datos_cox_modelo)