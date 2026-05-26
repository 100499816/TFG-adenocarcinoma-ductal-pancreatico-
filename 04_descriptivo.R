#Tabla Tipo histológico
N <- nrow(df)
tabla_histo <- bind_rows(
  df %>%
    count(tipo_histologico) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number() == 1, "Tipo histológico", ""),
      Categoria = paste0("  ", as.character(tipo_histologico))
    ) %>%
    select(Variable, Categoria, Resultado)
  
)

tabla_histo %>%
  kable(
    col.names = c("Variable", "Categoría", "n (%)"),
    align = c("l","l","r"),
    caption = paste0("Tipo histológico de los pacientes (N = ", N, ")")
  )



#Tabla 2. Características basales de los pacientes (N = 140)

Ndatos <- nrow(datos_surv)

tabla_basal_manual1 <- bind_rows(
  
  data.frame(
    Variable = "Tamaño muestral",
    Categoria = "Pacientes incluidos",
    Resultado = as.character(nrow(datos_surv))
  ),
  
  data.frame(
    Variable = "Edad al diagnóstico (años)",
    Categoria = "Media ± DE",
    Resultado = paste0(
      round(mean(datos_surv$edad, na.rm = TRUE),1), " ± ",
      round(sd(datos_surv$edad, na.rm = TRUE),1)
    )
  ),
  
  datos_surv %>%
    count(sexo) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Sexo",""),
      Categoria = paste0("  ", sexo)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  datos_surv %>%
    count(metastasis) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Metástasis al diagnóstico",""),
      Categoria = paste0("  ", metastasis)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  datos_surv %>%
    count(localizacion_simple) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Localización tumoral",""),
      Categoria = paste0("  ", localizacion_simple)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  data.frame(
    Variable = "Tamaño tumoral (cm)",
    Categoria = "Mediana [P25–P75]",
    Resultado = paste0(
      round(median(datos_surv$tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm, na.rm=TRUE),1),
      " [",
      round(quantile(datos_surv$tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm,0.25,na.rm=TRUE),1),
      "–",
      round(quantile(datos_surv$tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm,0.75,na.rm=TRUE),1),
      "]"
    )
  ),
  
  datos_surv %>%
    count(tam_cat) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Tamaño tumoral categorizado",""),
      Categoria = paste0("  ", tam_cat)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  data.frame(
    Variable = "CA 19-9 (U/mL)",
    Categoria = "Mediana [P25–P75]",
    Resultado = paste0(
      round(median(datos_surv$ca_19_9_al_dx, na.rm=TRUE),1),
      " [",
      round(quantile(datos_surv$ca_19_9_al_dx,0.25,na.rm=TRUE),1),
      "–",
      round(quantile(datos_surv$ca_19_9_al_dx,0.75,na.rm=TRUE),1),
      "]"
    )
  ),
  
  datos_surv %>%
    count(ca_cat) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"CA 19-9 categorizado",""),
      Categoria = paste0("  ", ca_cat)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  datos_surv %>%
    count(cirugia) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Cirugía",""),
      Categoria = paste0("  ", cirugia)
    ) %>%
    select(Variable, Categoria, Resultado)
)

tabla_basal_manual1 %>%
  kable(
    col.names = c("Variable", "Categoría / Resumen", "n (%) o estadístico"),
    align = c("l","l","r"),
    caption = paste0("Tabla 1. Características basales de los pacientes (N = ", Ndatos, ")"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "5cm") %>%
  column_spec(2, width = "6cm") %>%
  column_spec(3, width = "4cm") %>%
  collapse_rows(columns = 1, valign = "top") %>%
  add_footnote(
    "Los datos se presentan como n (%) o mediana [P25–P75], salvo que se indique lo contrario.",
    notation = "none"
  )


#TABLA ANEXO
tabla_basal_manual1 %>%
  kable(
    col.names = c("Variable", "Categoría / Resumen", "n (%) o estadístico"),
    align = c("l","l","r"),
    caption = paste0("Tabla 1. Características basales de los pacientes (N = ", Ndatos, ")")
  )



# TABLA PACIENTES METÁSTASIS

datos_met <- datos_surv %>%
  filter(tolower(metastasis) == "si")

N_met <- nrow(datos_met)

# 2. Crear tabla
tabla_metastasis <- data.frame(
  Variable = c("Hepática", "Peritoneal", "Pulmonar", "Cerebral", "Ósea"),
  
  n = c(
    sum(datos_met$hepatica == "si", na.rm = TRUE),
    sum(datos_met$peritoneal == "si", na.rm = TRUE),
    sum(datos_met$pulmonar == "si", na.rm = TRUE),
    sum(datos_met$cerebral == "si", na.rm = TRUE),
    sum(datos_met$huesos == "si", na.rm = TRUE)
  )
) %>%
  mutate(
    pct = round(100 * n / N_met, 1),
    Resultado = paste0(n, " (", pct, "%)")
  ) %>%
  select(Variable, Resultado)


#Tabla 3. Distribución de metástasis al diagnóstico (N = 74)

tabla_metastasis %>%
  kable(
    col.names = c("Localización metastásica", "n (%)"),
    align = c("l","r"),
    caption = paste0("Tabla 2. Distribución de metástasis al diagnóstico (N = ", N_met, ")"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%  # encabezado en negrita
  column_spec(1, bold = TRUE, width = "7cm") %>%  # nombres más visibles
  column_spec(2, width = "4cm") %>%
  add_footnote(
    "Los porcentajes se calculan sobre el total de pacientes con enfermedad metastásica.",
    notation = "none"
  )


#TABLA ANEXO
tabla_metastasis %>%
  kable(
    col.names = c("Localización metastásica", "n (%)"),
    align = c("l","r"),
    caption = paste0("Distribución de metástasis al diagnóstico (N = ", N_met, ")")
  )




# TABLA PACIENTES OPERADOS INTENCIÓN CURATIVA
datos_qx <- datos_surv %>%
  filter(tolower(cirugia_intencion_curativa) == "si")

N_qx <- nrow(datos_qx)

# 🔹 Crear variables limpias + categorizadas
datos_qx <- datos_qx %>%
  mutate(
    neoadyuvancia = factor(tolower(neoadyuvancia), levels = c("no","si")),
    
    tipo_cirugia = factor(
      tipo_de_cirugia,
      levels = c("DPC","Distal")
    ),
    
    complicacion_bin = ifelse(
      tolower(complicacion_postquirurgica) == "no", "no", "si"
    ),
    
    adyuvancia = factor(tolower(adyuvancia_si_no), levels = c("no","si")),
    
    recaida = factor(
      tolower(recaida),
      levels = c("no","locorregional","a distancia")
    ),
    
    # 🔹 VARIABLES CATEGORIZADAS
    tam_qx_cat = case_when(
      is.na(tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm) ~ NA_character_,
      tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm <= 2 ~ "≤2 cm",
      tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm > 2 ~ ">2 cm"
    ),
    tam_qx_cat = factor(tam_qx_cat, levels = c("≤2 cm", ">2 cm")),
    
    ganglios_cat = case_when(
      is.na(no_ganglios_resecados) ~ NA_character_,
      no_ganglios_resecados >= 15 ~ "≥15",
      no_ganglios_resecados < 15 ~ "<15"
    ),
    ganglios_cat = factor(ganglios_cat, levels = c("≥15", "<15")),
    
    adenopatias_cat = case_when(
      is.na(no_adenopatias_positivas_pieza_qx) ~ NA_character_,
      no_adenopatias_positivas_pieza_qx == 0 ~ "=0",
      no_adenopatias_positivas_pieza_qx > 0 ~ ">0"
    ),
    adenopatias_cat = factor(adenopatias_cat, levels = c("=0", ">0"))
  )


tabla_qx <- bind_rows(
  # Tipo cirugía
  datos_qx %>%
    count(tipo_cirugia) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Tipo de cirugía",""),
      Categoria = paste0("  ", tipo_cirugia)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  # Tamaño continuo
  data.frame(
    Variable = "Tamaño tumoral (pieza quirúrgica, cm)",
    Categoria = "Mediana [P25–P75]",
    Resultado = paste0(
      round(median(datos_qx$tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm, na.rm=TRUE),1)," [",
      round(quantile(datos_qx$tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm,0.25,na.rm=TRUE),1),"–",
      round(quantile(datos_qx$tamano_tumoral_en_pieza_quirurgica_diametro_mayor_en_cm,0.75,na.rm=TRUE),1),"]"
    )
  ),
  
  # 🔹 Tamaño categorizado
  datos_qx %>%
    count(tam_qx_cat) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Tamaño tumoral categorizado",""),
      Categoria = paste0("  ", tam_qx_cat)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  # Ganglios continuo
  data.frame(
    Variable = "Ganglios resecados",
    Categoria = "Mediana [P25–P75]",
    Resultado = paste0(
      median(datos_qx$no_ganglios_resecados, na.rm=TRUE)," [",
      quantile(datos_qx$no_ganglios_resecados,0.25,na.rm=TRUE),"–",
      quantile(datos_qx$no_ganglios_resecados,0.75,na.rm=TRUE),"]"
    )
  ),
  
  # 🔹 Ganglios categorizado
  datos_qx %>%
    count(ganglios_cat) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Ganglios resecados categorizados",""),
      Categoria = paste0("  ", ganglios_cat)
    ) %>%
    select(Variable, Categoria, Resultado),
  
  # Adenopatías continuo
  data.frame(
    Variable = "Adenopatías positivas",
    Categoria = "Mediana [P25–P75]",
    Resultado = paste0(
      median(datos_qx$no_adenopatias_positivas_pieza_qx, na.rm=TRUE)," [",
      quantile(datos_qx$no_adenopatias_positivas_pieza_qx,0.25,na.rm=TRUE),"–",
      quantile(datos_qx$no_adenopatias_positivas_pieza_qx,0.75,na.rm=TRUE),"]"
    )
  ),
  
  # 🔹 Adenopatías categorizado
  datos_qx %>%
    count(adenopatias_cat) %>%
    mutate(
      pct = round(100*n/sum(n),1),
      Resultado = paste0(n," (",pct,"%)"),
      Variable = ifelse(row_number()==1,"Adenopatías positivas categorizadas",""),
      Categoria = paste0("  ", adenopatias_cat)
    ) %>%
    select(Variable, Categoria, Resultado),
)


#TABLA BONITA
tabla_qx %>%
  kable(
    col.names = c("Variable","Categoría / Resumen","n (%) o estadístico"),
    align = c("l","l","r"),
    caption = paste0("Tabla A3. Características de los pacientes operados (N = ", N_qx, ")"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%  # encabezado
  column_spec(1, bold = TRUE, width = "6cm") %>%  # variable destacada
  column_spec(2, width = "6cm") %>%
  column_spec(3, width = "4cm") %>%
  add_footnote(
    "Los datos se presentan como n (%) o mediana [P25–P75].",
    notation = "none"
  )


#TABLA ANEXO 
tabla_qx %>%
  kable(
    col.names = c("Variable","Categoría / Resumen","n (%) o estadístico"),
    align = c("l","l","r"),
    caption = paste0("Tabla A3. Características de los pacientes operados   (N = ", N_qx, ")")
  )




#ASIMETRÍA
skewness(datos$ca_19_9_al_dx, na.rm = TRUE)
skewness(datos$tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm, na.rm = TRUE)
boxplot(datos$ca_19_9_al_dx, main="CA 19-9")
boxplot(datos$tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm, main="Tamaño tumoral")
