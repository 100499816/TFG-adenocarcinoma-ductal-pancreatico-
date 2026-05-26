# COX MULTIVARIANTE (N=125. Muestra completa)
datos_cox_multi <- datos_surv %>%
  mutate(
    metastasis = factor(tolower(metastasis), levels = c("no","si")),
    cirugia = factor(tolower(cirugia), levels = c("no","si")),
    sexo = factor(tolower(sexo), levels = c("hombre","mujer")),
    ca_cat = case_when(
      ca_19_9_al_dx <= 37 ~ "≤37",
      ca_19_9_al_dx <= 500 ~ "38–500",
      ca_19_9_al_dx > 500 ~ ">500"
    ),
    ca_cat = factor(ca_cat, levels = c("≤37","38–500",">500"))
  ) %>%
  drop_na(tiempo, evento, edad, metastasis, cirugia, ca_cat)

# Modelo
cox_multi <- coxph(
  Surv(tiempo, evento) ~ edad + metastasis + cirugia + ca_cat,
  data = datos_cox_multi
)

summary(cox_multi)


tabla_cox_multi <- tbl_regression(
  cox_multi,
  exponentiate = TRUE
) %>%
  bold_labels()

tabla_cox_multi

nrow(datos_cox_multi)



#Análisis de sensibilidad (con Sexo)

cox_sensibilidad <- coxph(
  Surv(tiempo, evento) ~ edad + metastasis + cirugia + ca_cat + sexo,
  data = datos_cox_multi
)

summary(cox_sensibilidad)


tabla_cox_sensibilidad <- tbl_regression(
  cox_sensibilidad,
  exponentiate = TRUE
) %>%
  bold_labels()

tabla_cox_sensibilidad
