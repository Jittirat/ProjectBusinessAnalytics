

install.packages("FinancialMath")
library(FinancialMath)
##1. Amortisation table
amort.table(Loan=1000,n=2,i=.005,ic=1,pf=1, plot = TRUE)
            
##1.1

amort.table(Loan=100000, n=10,pmt= NA,i=0.10,ic=1,pf=1)

###2. Amortization Period
amort.period(Loan=100000,n=5,i=.02,t=3)

#2.1 what is the loan amount, principal paid, interest paid, and balance at year 3?
amort.period(n=5, pmt=30, i=.02,t=3, pf=12)

#2.2 To findthe number of payments in each loan or leasing.
amort.period(Loan=1000000, pmt=100000,ic=1,i=.02,t=3)


