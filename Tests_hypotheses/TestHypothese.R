# Test Hypothese: X suis un loi Normal et variance
# de la population  sigma² ou n<30

# données
x_moy = 0.3
esp_theo = 0.45
esti_variance_echan = 0.4
nb_element = 40
risque=0.05

# RAPPEL: (sigma)ecart type = sqrt(variance)

# suis une loi N(esp_theo,esti_variance_echan²)  
# calcule de t abs
ecart = abs(x_moy-esp_theo)
t_abs = ecart/((esti_variance_echan)/sqrt(nb_element))
t_abs
demi_p = pnorm(t_abs, mean = 0, sd = 1, lower.tail = TRUE)
demi_p = (1-demi_p)
demi_p
p = demi_p*2
p

if(p<risque){
  print("p<risque --> On refuse H0")
} else{
  print("p>risque --> On ne peut pas refuser H0") 
}

var = (1-risque/2)
var

t_theo = demi_p = qnorm(var)
t_theo

if (t_abs<t_theo){
  print("t_abs<t_theo, Garde l'hyopthese H0, l'échantillon est représentatif")
}else{
  print("Refuse l'hypothese")
}

# ----------------------------------------------
# Version avec Calcul intervalle de confiance à X% de la moyenne
# Rappel: risque = 100% - %confiance

# données
x_moy = 0.3
esp_theo = 0.45
esti_variance_echan = 0.4
nb_element = 40
risque=0.05

# Rappel: 
# I = [x_moy -t(sigma_chap/sqrt(n));x_moy +t(sigma_chap/sqrt(n))]
# I = [I0;I1]

sigma_chap = sqrt(nb_element/(nb_element-1))*sqrt(esti_variance_echan)
sigma_chap
t = abs(qnorm(risque/2))
t
var= t*(sigma_chap/sqrt(nb_element))
I0 = esp_theo - var
I0
I1 = esp_theo + var
I1
reponse = paste("intervalle = [",I0,";",I1,"]")
reponse