rm(list=ls())

nb_jour <- 100 #nombre de jours que dure l'expérience
i_jour <- 1 #compteur du nombre de jours que dure l'exp
duree_cycle <- 40 #nb jours du cycle des femelles
nb_mal <- 10 #nombre de femelles
nb_fem <- 10 #nombre de males
mal_partner <- numeric(nb_mal) #vecteur qui contient les n° des femelles correspondant aux males
fem_partner <- numeric(nb_fem) #vecteur qui contient les n° des males correspondant aux femelles
jour_cycle <- as.integer(runif(nb_fem,0,duree_cycle-1)) #vecteur qui contient le jour du cycle des femelles
fem_size <- rnorm(nb_fem,mean=2,sd=0.2) #vecteur avec tailles des femelles (loi normale)
mal_size <- rnorm(nb_mal,mean=2.75,sd=0.2) #vecteur avec tailles des males (loi normale)

while (i_jour <= nb_jour){
  i <- 1
  while(i<nb_mal){ #on regarde toute la pop male
    i2<-1
    while(mal_partner[i]==0){ #tant que le male n'a pas de partenaire
      
      fem_rand <- sample(nb_mal,1) #on tire une femelle au hasard
      
      if(mal_size[i]>=fem_size[fem_rand] & fem_partner[fem_rand]==0){
        
        mal_partner[i] <- fem_rand
        fem_partner[fem_rand] <- i
      }
      if(i2>nb_fem)next #si on a vu toute la pop fem, on passe au male suivant
      
      i2 <- i2+1
    }
    
    i <- i+1
  }
  
  
  jour_cycle <- jour_cycle +1 #pour augmenter le jour des cycles des femelles et vérifier qu'il soit <40
  if (max(jour_cycle) == (duree_cycle)){
    while(max(jour_cycle)==(duree_cycle)){
      maxi <- which.max(jour_cycle==(duree_cycle))
      jour_cycle[maxi] <- 0
    }
  }
  i_jour <- i_jour+1 
}
plot(1)
