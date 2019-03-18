#Modeling Passthrough

#Initiate
require(data.table)
Principal = 500000000
WAC = 8.75/100 #weighted average coupon
PT.rate = 8/100 #passthrough rate
PSA = 75/100 
WAM = 360 #weighted average maturity
seasoning = 0 #seasoning of the pool

#define annuity function
pmt <- function(r,nper,pv,fv){
  return(pv*r*(1+r)^nper/((1+r)^nper-1))
}
#Calculate Constant Prepayment Rate(CPR) and Single Monthly Mortality(SMM)
CPR = matrix(seq(1,WAM))
colnames(CPR) <- c('Month')
CPR <- data.table(CPR)
CPR[Month<(30-seasoning),cpr:= PSA*0.06*(Month+seasoning)/30]
CPR[Month>=(30-seasoning),cpr:= PSA*0.06]
CPR[,smm:= 1-(1-cpr)^(1/12)]
#modeling total cf for the pool(pass-through)
pt = matrix(NA,WAM,1)
colnames(pt) = c('Month')
pt = data.table(pt)
pt$Month = seq(1,WAM)
pt[1,OutstandingBalance:=Principal] #set initial balance
pt$SMM = CPR$smm

for(month in 1:(WAM-1)){
  pt[month,MortgagePayment:= pmt(WAC/12,WAM-Month+1,OutstandingBalance,0)]
  pt[month,NetI:= OutstandingBalance*PT.rate/12] #Net Interest
  pt[month,ScheduledPrincipal:= MortgagePayment-OutstandingBalance*WAC/12]
  pt[month,Prepayment:= SMM*(OutstandingBalance-ScheduledPrincipal)]
  pt[month,TotalPrincipal:= ScheduledPrincipal+Prepayment]
  pt[month,CF:= TotalPrincipal+NetI]
  pt[month+1,OutstandingBalance := pt[month,2]-pt[month,8]] #pervious month's OutstandingBalance - previous month's TotalPrincipal
}
#cf of the last month
pt[.N,MortgagePayment:= pmt(WAC/12,WAM-Month+1,OutstandingBalance,0)]
pt[.N,NetI:= OutstandingBalance*PT.rate/12]
pt[.N,ScheduledPrincipal:= MortgagePayment-OutstandingBalance*WAC/12]
pt[.N,Prepayment:= SMM*(OutstandingBalance-ScheduledPrincipal)]
pt[.N,TotalPrincipal:= ScheduledPrincipal+Prepayment]
pt[.N,CF:= TotalPrincipal+NetI]

#Calculate duration, WAL
pt[,duration:= CF/(1+PT.rate/12)^Month*Month/Principal]
duration.month = sum(pt$duration)
duration.year = duration.month/12
WAL = sum(pt[,TotalPrincipal*Month/Principal])

#plot prinicipal component
require(ggplot2)
ggplot(pt,(aes(Month))) +
      geom_bar(aes(y= TotalPrincipal,colour = "Principal"),stat='identity') +
      geom_bar(aes(y= NetI,colour = "Interest"),stat='identity')