#' @export

getPi <- function (P){

    if(length(P[1,]) != length(P[,1])){     #check matrix dimension

        stop('Matrix P must be squared: number of rows equal to the number o columns')

    }

    for(row in 1:length(P[,1])){
        if(sum(P[row,])!=1){
            stop('All rows in P must sum up to 1')
        }
    }


    n_states = length(P[1,])

    A = t(P)                            #transpose matrix

    for(row in 1:n_states){               #canonical form
        for(column in 1:n_states){
            if(row == column){
                A[row, column] = A[row, column]-1
            }
        }
    }

    B = rbind(A, rep(1, n_states))      #adding last equation

    rhs = c(rep(0,n_states),1)          #Right hand side of the system

    pi_vector = matlib::echelon(B, rhs)[1:n_states,n_states+1]

    return(pi_vector)
}

