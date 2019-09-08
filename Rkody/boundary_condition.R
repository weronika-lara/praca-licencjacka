

schemat_brzegowy = function(f, c , t0 , tN, x0, xN , N)
{
  h = (tN-t0)/N
  t = seq(from=t0, to=t0+(N-2)*h,length.out = N-1)
  A = diag(-2-c*h^2,nrow=N-1,ncol=N-1)
  A[row(A) - col(A) == -1] = 1
  A[row(A) - col(A) == 1] = 1
  b = -1 * f(t) * h
  b[1] = b[1] - x0
  b[N-1] = b[N-1] - xN
  x = solve(A, b)
  result = list(time=c(t0, t, tN), values = c(x0, x, xN))
  return(result)
}

f = function(t){
  return(rep(0,times = length(t)))
}
c = -1
t0 = 0
tN = 5*pi/2
x0 = 0
xN = 1
N =100

solution = schemat_brzegowy(f,c, t0, tN, x0, xN, N)
solution


t = seq(0,to=4*pi, length.out = 1000)
plot(t, sin(t),type = 'l')
lines(solution$time, solution$values,col='red')
