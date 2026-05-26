#KAPLAN-MEIER GLOBAL

Surv_obj <- Surv(time = datos_surv$tiempo, event = datos_surv$evento)
km_fit <- survfit(Surv_obj ~ 1, data = datos_surv)


mediana <- summary(km_fit)$table

mediana_surv <- data.frame(
  Mediana_meses = round(mediana["median"],1),
  IC95_inf = round(mediana["0.95LCL"],1),
  IC95_sup = round(mediana["0.95UCL"],1)
)

mediana_surv



tiempos <- c(6, 12, 24)

surv_tiempos <- summary(km_fit, times = tiempos)

tabla_supervivencia <- data.frame(
  Tiempo = c("6 meses", "12 meses", "24 meses"),
  Supervivencia = paste0(round(surv_tiempos$surv * 100,1), "%")
)

tabla_supervivencia


ggsurvplot(
  km_fit,
  data = datos_surv,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # Color único profesional
  palette = c("#0072B2"),
  
  # Sin leyenda (solo una curva)
  legend = "none",
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)


#KAPLAN MEIER METÁSTASIS
datos_surv_met <- datos_surv %>%
  filter(tolower(metastasis) %in% c("si","no")) %>%
  mutate(
    metastasis = factor(tolower(metastasis), levels = c("no","si"))
  )

Surv_obj <- Surv(time = datos_surv_met$tiempo,
                 event = datos_surv_met$evento)

km_met <- survfit(Surv_obj ~ metastasis, data = datos_surv_met)


summary(km_met)$table

logrank <- survdiff(Surv_obj ~ metastasis, data = datos_surv_met)

p_valor <- 1 - pchisq(logrank$chisq, df = 1)
p_valor


ggsurvplot(
  km_met,
  data = datos_surv_met,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # p valor
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  # Colores profesionales
  palette = c("#0072B2","#E69F00"),
  
  # Leyenda
  legend.title = "Metástasis al diagnóstico",
  legend.labs = c("No","Sí"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)

#KAPLAN MEIER CIRUGÍA
datos_surv_cx <- datos_surv %>%
  filter(tolower(cirugia) %in% c("si","no")) %>%
  mutate(
    cirugia = factor(tolower(cirugia), levels = c("no","si"))
  )

Surv_obj_cx <- Surv(time = datos_surv_cx$tiempo,
                    event = datos_surv_cx$evento)

km_cx <- survfit(Surv_obj_cx ~ cirugia, data = datos_surv_cx)
summary(km_cx)$table
logrank_cx <- survdiff(Surv_obj_cx ~ cirugia, data = datos_surv_cx)

p_valor_cx <- 1 - pchisq(logrank_cx$chisq, df = 1)
p_valor_cx


ggsurvplot(
  km_cx,
  data = datos_surv_cx,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # p valor
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  # Colores profesionales
  palette = c("#E69F00","#0072B2"),
  
  # Leyenda
  legend.title = "Cirugía",
  legend.labs = c("No","Sí"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)

#KAPLAN MEIER SEXO
datos_surv_sexo <- datos_surv %>%
  filter(!is.na(sexo)) %>%
  mutate(
    sexo = factor(tolower(sexo), levels = c("hombre","mujer"))
  )

Surv_obj_sexo <- Surv(time = datos_surv_sexo$tiempo,
                      event = datos_surv_sexo$evento)

km_sexo <- survfit(Surv_obj_sexo ~ sexo, data = datos_surv_sexo)
summary(km_sexo)$table

logrank_sexo <- survdiff(Surv_obj_sexo ~ sexo, data = datos_surv_sexo)

p_valor_sexo <- 1 - pchisq(logrank_sexo$chisq, df = 1)
p_valor_sexo


ggsurvplot(
  km_sexo,
  data = datos_surv_sexo,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # p valor
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  # Colores profesionales
  palette = c("#0072B2","#E69F00"),
  
  # Leyenda
  legend.title = "Sexo",
  legend.labs = c("Hombre","Mujer"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)


#KAPLAN MEIER CA19_9 categorizado

datos_surv_ca <- datos_surv %>%
  filter(!is.na(ca_19_9_al_dx)) %>%   
  mutate(
    ca_cat = case_when(
      ca_19_9_al_dx <= 37 ~ "≤37",
      ca_19_9_al_dx <= 500 ~ "38–500",
      ca_19_9_al_dx > 500 ~ ">500"
    ),
    ca_cat = factor(ca_cat, levels = c("≤37","38–500",">500"))
  )


Surv_obj_ca <- Surv(time = datos_surv_ca$tiempo,
                    event = datos_surv_ca$evento)

km_ca <- survfit(Surv_obj_ca ~ ca_cat, data = datos_surv_ca)

summary(km_ca)$table
logrank_ca <- survdiff(Surv_obj_ca ~ ca_cat, data = datos_surv_ca)

p_valor_ca <- 1 - pchisq(logrank_ca$chisq, df = 2)
p_valor_ca

ggsurvplot(
  km_ca,
  data = datos_surv_ca,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # p valor
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  # Colores profesionales
  palette = c("#0072B2","#E69F00","#009E73"),
  
  # Leyenda
  legend.title = "CA 19-9",
  legend.labs = c("≤37","38–500",">500"),
  legend = c(0.8,0.7),
  
  # Color y tamaño texto leyenda
  font.legend = c(9),
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)


# KAPLAN-MEIER tamaño tumoral categorizado

datos_surv_tam <- datos_surv %>%
  filter(!is.na(tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm)) %>%
  mutate(
    tam_cat = case_when(
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm <= 2 ~ "≤2 cm",
      tamano_tumoral_al_dx_por_tc_diametro_mayor_en_cm > 2 ~ ">2 cm"
    ),
    tam_cat = factor(tam_cat, levels = c("≤2 cm",">2 cm"))
  )

Surv_obj_tam <- Surv(
  time = datos_surv_tam$tiempo,
  event = datos_surv_tam$evento
)

km_tam <- survfit(
  Surv_obj_tam ~ tam_cat,
  data = datos_surv_tam
)

summary(km_tam)$table

logrank_tam <- survdiff(
  Surv_obj_tam ~ tam_cat,
  data = datos_surv_tam
)

p_valor_tam <- 1 - pchisq(logrank_tam$chisq, df = 1)

ggsurvplot(
  km_tam,
  data = datos_surv_tam,
  
  # Curva
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  # Intervalos confianza
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  # Tabla riesgo
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  # p valor
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  # Colores profesionales
  palette = c("#0072B2","#E69F00"),
  
  # Leyenda
  legend.title = "Tamaño tumoral",
  legend.labs = c("≤2 cm",">2 cm"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  # Ejes
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  # Tema
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)




#----------------------------------------------------------------------------
#Kaplan meier CIRUGÍA
datos_qx <- datos_qx %>%
  mutate(
    evento = case_when(
      tolower(exitus) == "si" ~ 1,
      tolower(exitus) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    
    fecha_fin = coalesce(fecha_exitus, fecha_ultima_revision_si_no_exitus),
    
    tiempo = as.numeric(difftime(fecha_fin, fecha_diagnostico_ap, units = "days")) / 30.44
  ) %>%
  filter(!is.na(evento) & !is.na(tiempo) & tiempo >= 0)


#KM Tipo de cirugía
datos_km <- datos_qx %>%
  filter(!is.na(tipo_cirugia)) %>%
  mutate(
    tipo_cirugia = factor(
      tipo_cirugia,
      levels = c("DPC","Distal")
    )
  )

Surv_obj <- Surv(
  datos_km$tiempo,
  datos_km$evento
)

km_cx <- survfit(
  Surv_obj ~ tipo_cirugia,
  data = datos_km
)

summary(km_cx)$table

logrank <- survdiff(
  Surv_obj ~ tipo_cirugia,
  data = datos_km
)

p_valor <- 1 - pchisq(
  logrank$chisq,
  df = 1
)
p_valor

ggsurvplot(
  km_cx,
  data = datos_km,
  
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  palette = c("#0072B2","#E69F00"),
  
  legend.title = "Tipo de cirugía",
  legend.labs = c("DPC","Distal"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)


#KM Tamaño pieza qx
datos_km <- datos_qx %>%
  filter(!is.na(tam_qx_cat)) %>%
  mutate(
    tam_qx_cat = factor(tam_qx_cat, labels = c("<=2 cm", ">2 cm"))
  )

Surv_obj <- Surv(datos_km$tiempo, datos_km$evento)

km_tam <- survfit(Surv_obj ~ tam_qx_cat, data = datos_km)

summary(km_tam)$table

logrank <- survdiff(Surv_obj ~ tam_qx_cat, data = datos_km)
p_valor <- 1 - pchisq(logrank$chisq, df = 1)
p_valor

ggsurvplot(
  km_tam,
  data = datos_km,
  
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  palette = c("#0072B2","#E69F00"),
  
  legend.title = "Tamaño tumoral",
  legend.labs = c("≤2 cm",">2 cm"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)


#KM Gnaglios resecados
datos_km <- datos_qx %>%
  filter(!is.na(ganglios_cat)) %>%
  mutate(
    ganglios_cat = factor(
      ganglios_cat,
      levels = c("≥15","<15")
    )
  )

Surv_obj <- Surv(
  datos_km$tiempo,
  datos_km$evento
)

km_gang <- survfit(
  Surv_obj ~ ganglios_cat,
  data = datos_km
)

summary(km_gang)$table

logrank <- survdiff(
  Surv_obj ~ ganglios_cat,
  data = datos_km
)
table(datos_qx$ganglios_cat, useNA = "ifany")
p_valor <- 1 - pchisq(
  logrank$chisq,
  df = 1
)

p_valor
ggsurvplot(
  km_gang,
  data = datos_km,
  
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  palette = c("#0072B2","#E69F00"),
  
  legend.title = "Ganglios resecados",
  legend.labs = c("≥15","<15"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)



#KM Adenopatías positivas
datos_km <- datos_qx %>%
  filter(!is.na(adenopatias_cat))

Surv_obj <- Surv(datos_km$tiempo, datos_km$evento)

km_adeno <- survfit(Surv_obj ~ adenopatias_cat, data = datos_km)

summary(km_adeno)$table

logrank <- survdiff(Surv_obj ~ adenopatias_cat, data = datos_km)
p_valor <- 1 - pchisq(logrank$chisq, df = 1)
p_valor

ggsurvplot(
  km_adeno,
  data = datos_km,
  
  size = 1.2,
  censor.size = 3,
  censor.shape = 124,
  
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.fontsize = 3.5,
  risk.table.y.text = FALSE,
  
  pval = TRUE,
  pval.coord = c(10,0.2),
  pval.size = 5,
  
  palette = c("#0072B2","#E69F00"),
  
  legend.title = "Adenopatías positivas",
  legend.labs = c("=0",">0"),
  legend = c(0.8,0.8),
  font.legend = c(9),
  
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de supervivencia",
  xlim = c(0,100),
  break.time.by = 12,
  
  surv.scale = "percent",
  
  ggtheme = theme_classic(base_size = 10),
  
  font.title = c(10,"bold"),
  font.x = c(11),
  font.y = c(9),
  font.tickslab = c(10),
  
  tables.theme = theme_cleantable()
)