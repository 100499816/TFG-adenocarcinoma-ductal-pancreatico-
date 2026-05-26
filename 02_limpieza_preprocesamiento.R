df <- basededatos %>%
  clean_names() %>%
  mutate(
    
    # 🔹 convertir fechas correctamente SIN str_trim
    fecha_nacimiento = as.Date(fecha_nacimiento, origin = "1899-12-30"),
    fecha_diagnostico_ap = as.Date(fecha_diagnostico_ap, origin = "1899-12-30"),
    
    # 🔹 edad
    edad = as.numeric(difftime(fecha_diagnostico_ap, fecha_nacimiento, units = "days")) / 365.25,
    
    # 🔹 CA 19-9
    ca_19_9_al_dx = as.numeric(ca_19_9_al_dx),
    ca_19_9_al_dx = na_if(ca_19_9_al_dx, 99999),
    
    # 🔹 tamaño tumoral
    tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm =
      as.numeric(tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm) %>%
      na_if(99),
    
    # 🔹 factores
    sexo = factor(sexo_hombre_mujer),
    metastasis = factor(metastasis_al_dx),
    cirugia = factor(cirugia_intencion_curativa),
    localizacion = factor(localizacion_tumor),
    tipo_histologico = factor(tipo_histologico),
    exitus = factor(exitus)
    
  )



datos <- df %>%
  clean_names() %>%
  filter(tipo_histologico == "adenocarcinoma") %>%
  mutate(
    
    edad = as.numeric(difftime(fecha_diagnostico_ap, fecha_nacimiento, units = "days"))/365.25,
    
    # CA 19-9
    ca_19_9_al_dx = as.numeric(ca_19_9_al_dx),
    ca_19_9_al_dx = na_if(ca_19_9_al_dx, 99999),
    
    # Tamaño
    tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm =
      as.numeric(tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm) %>%
      na_if(99),
    
    # 🔹 Factores ORDENADOS
    sexo = factor(sexo_hombre_mujer),
    
    metastasis = factor(
      metastasis_al_dx,
      levels = c("si","no","desconocido")
    ),
    
    cirugia = factor(
      cirugia_intencion_curativa,
      levels = c("si","no")
    ),
    
    # 🔹 Localización agrupada
    localizacion_simple = case_when(
      localizacion_tumor %in% c("cabeza","cabeza y cuerpo","cabeza y cola","cabeza, cuello y cuerpo") ~ "cabeza",
      localizacion_tumor %in% c("cuerpo","cuello","cuello y cuerpo","cuerpo y cola") ~ "cuerpo",
      localizacion_tumor == "cola" ~ "cola",
      TRUE ~ NA_character_
    ),
    
    localizacion_simple = factor(
      localizacion_simple,
      levels = c("cabeza","cuerpo","cola")
    ),
    
    # 🔹 Tamaño categorizado
    tam_cat = case_when(
      is.na(tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm) ~ NA_character_,
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm <= 2 ~ "≤2 cm",
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm > 2 ~ ">2 cm"
    ),
    
    tam_cat = factor(tam_cat, levels = c("≤2 cm",">2 cm")),
    
    # 🔹 CA categorizado
    ca_cat = case_when(
      ca_19_9_al_dx <= 37 ~ "≤37",
      ca_19_9_al_dx <= 500 ~ "38–500",
      ca_19_9_al_dx > 500 ~ ">500"
    ),
    
    ca_cat = factor(ca_cat, levels = c("≤37","38–500",">500"))
  )
