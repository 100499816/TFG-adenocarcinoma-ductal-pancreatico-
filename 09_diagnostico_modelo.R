#Comprobación de riesgos proporcionales
test_ph <- cox.zph(cox_multi)

test_ph
plot(test_ph)