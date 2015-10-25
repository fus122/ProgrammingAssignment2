library(da
        ## Duas funcoes que retornarao do cache matriz invertida
        ## makeCacheMatrix cria uma matriz especial a ser invertida
        
        ##
        
        makeCacheMatrix <- function( g = matrix() ) {
                
                ## seta a propriedade de inversao
                
                a <- NULL
                
                ## Definicao da Matriz
                
                set <- function( matrix ) {
                       
                        a <<- NULL
                        g <<- matrix
                }
                
                ## Obtencao da Matriz
                
                get <- function() {
                        
                        g
                }
                
                ## Metodo para inversao
                
                SetMatrizInversa <- function(inverse) {
                        
                        a <<- inverse
                }
                
                ## Obtencao da matriz inversa
                getMatrizInversa <- function() {
                        
                        a
                }
                
                ## lista os metodos
                
                list(set = set, get = get,
                     SetMatrizInversa = SetMatrizInversa,
                     getMatrizInversa = getMatrizInversa)
        }
        ## Calcula a o inverso da Matriz obtida no MakeCacheMatrix 
        ## Caso o inverso ja tenha sido calculado a e a matriz nao tenha se alterado esta funcao devera retornar o inverso do cache
        
        cacheSolve <- function(g, ...) {
                
                
                g <- g$getMatrizInversa()
                
                ## Houve mudanca?
                
                if( !is.null(g) ) {
                        message("Obtendo o dado do Cache")
                        return(g)
                }
                
                ## Busca a matriz 
                
                data <- g$get()
                
                ## calcula o inverso
                
                g <- solve(data) %*% data
                
                ## estabele o inverso do objeto g
                
                g$SetMatrizInversa(g)
                
                ## retorna a matriz
                
                g
        } 
        
