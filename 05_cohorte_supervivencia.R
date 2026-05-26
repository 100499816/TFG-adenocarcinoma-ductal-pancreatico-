#=================================================
# DEFINICIÓN DEL EVENTO Y CENSURA
#=================================================

datos_surv <- datos %>%
  mutate(
    
    fecha_dx = fecha_diagnostico_ap,
    
    evento = case_when(
      tolower(exitus) == "si" ~ 1,
      tolower(exitus) == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    
    fecha_fin = coalesce(
      fecha_exitus,
      fecha_ultima_revision_si_no_exitus
    ),
    
    tiempo = as.numeric(
      difftime(
        fecha_fin,
        fecha_dx,
        units = "days"
      )
    ) / 30.44
    
  ) %>%
  filter(
    !is.na(evento),
    !is.na(tiempo),
    tiempo >= 0
  )

# Resumen evento/censura
N_total <- nrow(datos_surv)

n_eventos <- sum(datos_surv$evento == 1)

n_censurados <- sum(datos_surv$evento == 0)


#DIAGRAMA DE FLUJO DE SELECCIÓN DE COHORTE
grViz("
digraph flujo {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = rounded, fontname = Helvetica]

  A [label = 'Registros identificados (n = 151)']
  
  B [label = 'Exclusiones (n = 11)\n- Histología no elegible (n = 7)\n- Datos ausentes/inconsistentes (n = 4)']
  
  C [label = 'Cohorte elegible (n = 144)']
  
  D [label = 'Cohorte analítica (n = 140)']
  
  E [label = 'Eventos (n = 129)']
  
  F [label = 'Censurados (n = 11)']

  A -> B -> C -> D
  D -> E
  D -> F
}
")