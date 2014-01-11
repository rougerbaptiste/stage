rm(list=ls())

pop_fem <- 100000 #nb individus pop femelle
pop_mal <- 100000 #nb individus pop male

fem_num <- 1:pop_fem #vecteur contenant les numeros des femelles
mal_num <- 1:pop_mal #vecteur contenant les numeros des males

fem_size <- rnorm(pop_fem,mean=2,sd=0.2) #vecteur avec tailles des femelles (loi normale)
mal_size <- rnorm(pop_mal,mean=2,sd=0.2) #vecteur avec tailles des males (loi normale)

fem_partner <- rep(0,pop_fem) #vecteur avec numero du partenaire
mal_partner <- numeric(pop_mal) #vecteur avec numero de la partenaire

fem_partner_size <- numeric(pop_fem) #vecteur avec la taille du partenaire male


i <- 1
while(i <= pop_fem){#on regarde toute la pop
  i2 <- 1
    while(fem_partner[i]==0 & i2 <= 100){ #tant que la femelle n'a pas de partenaire et qu'on a pas cherche 100 fois
        mal_rand <- sample(mal_num,1,replace=T) #on tire un male au hasard
    
        if(fem_size[i] <= mal_size[mal_rand] & mal_partner[mal_rand]==0){#si la taille de la femelle <= à celle du male et que le male n'a pas de partenaire
          fem_partner[i] <- mal_rand 
          fem_partner_size[i] <- mal_size[mal_rand]
          mal_partner[mal_rand] <- i
          break
        }
        i2 <- i2+1
      }
  i <- i+1
}
plot(fem_partner_size,fem_size)
abline(0,1, col = "red", lty = 3)
