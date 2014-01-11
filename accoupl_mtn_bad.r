rm(list=ls())

pop_fem <- 10 #nb individus pop femelle
pop_mal <- 10 #nb individus pop male

fem_num <- 1:pop_fem #vecteur contenant les numeros des femelles
mal_num <- 1:pop_mal #vecteur contenant les numeros des males

fem_size <- rnorm(pop_fem,mean=2,sd=0.2) #vecteur avec tailles des femelles (loi normale)
mal_size <- rnorm(pop_mal,mean=2,sd=0.2) #vecteur avec tailles des males (loi normale)

fem_partner <- rep(0,pop_fem) #vecteur avec numero du partenaire
mal_partner <- numeric(pop_mal) #vecteur avec numero de la partenaire

fem_partner_size <- numeric(pop_fem) #vecteur avec la taille du partenaire male


min_fem <- function(fem_partner){
  a <- as.numeric(c(fem_partner==0)) #on trouve toutes les fem libres
  e <- c()
  i <- 1
  while(i <= length(fem_partner)){
    
    if(a[i]!=0){ #si elle est libre
      
      e <- c(e,i) #on la rajoute aux "libres"
    }
    i <- i+1
  }
  
  if(length(e)!=0){ #on vérifie qu'il existe des libres
    f_o <- min(fem_size[e]) #on cherche la plus petite femelle
  }else{
    f_o <-10 #sinon on met une valeur qui dépasse la taille de tous les males
  }
  return(f_o)
}

max_mal <- function(mal_partner){
  a <- as.numeric(c(mal_partner==0)) #on trouve toutes les fem libres
  e <- c()
  i <- 1
  while(i <= length(mal_partner)){
    
    if(a[i]!=0){ #si elle est libre
      
      e <- c(e,i) #on la rajoute aux "libres"
    }
    i <- i+1
  }
  
  if(length(e)!=0){ #on vérifie qu'il existe des libres
    m_o <- min(mal_size[e]) #on cherche la plus petite femelle
  }else{
    m_o <-0 #sinon on met une valeur qui dépasse la taille de tous les males
  }
  return(m_o)
}

mini_fem <- min_fem(fem_partner)
maxi_mal <- max_mal(mal_partner)

i <- 1
while(i <= pop_fem & mini_fem < maxi_mal){#on regarde toute la pop
  while(fem_partner[i]==0){ #tant que la femelle n'a pas de partenaire
    
    mal_rand <- sample(mal_num,1,replace=T) #on tire un male au hasard
    
    if(fem_size[i] <= mal_size[mal_rand] & mal_partner[mal_rand]==0){#si la taille de la femelle <= à celle du male et que le male n'a pas de partenaire
      fem_partner[i] <- mal_rand 
      fem_partner_size[i] <- mal_size[mal_rand]
      mal_partner[mal_rand] <- i
    }
  }
  mini_fem <- min_fem(fem_partner)
  maxi_mal <- max_mal(mal_partner)
  i <- i+1
}
plot(fem_partner_size,fem_size)
abline(0,1, col = "red", lty = 3)
