# Title     : Get Double from User Input
# Objective : Get Double from User Input
# Created by: natov
# Created on: 12/05/2020

# Déclaration de la fonction pour récupérer un double de la commande utilisateur
getDoublePrompt <- function(prompt) {
  # Read user input
  x <- readline(prompt = prompt)

  # Réalise le cast de l'input en double
  x <- tryCatch({
          return <- as.double(x)
        },
        error=function(cond) {
          # Dans le cas ou il y a une erreur dans le cast en double
          message("Erreur lors du cast en double")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        },
        warning=function(cond) {
          # Dans le cas d'un warning lors du cast en double
          message("Warning lors du cast en double")
          message(cond)
          # Choose a return value in case of warning
          return(NULL)
        }
  )

  # Si jamais le retour du try catch n'est pas un double je stop le processus
  if(!is.double(x)) {
    stop("Erreur lors de la saisie utilisateur")
  }

  # Si jamais le retour est bien un double alors je retourne la valeur
  return <- x
}
