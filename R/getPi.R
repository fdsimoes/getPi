#' @export

getPi <- function (P){
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

