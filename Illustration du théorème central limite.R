setwd('/DISQUE/0_Grenoble/0_Enseign/L2_Pharma_S1/Pharmacie_2A_S1_2023_2024_correction/')

N = 3000
# n = 500
# n = 50
n = 31
# n = 15
# n = 7
# n = 3
# n = 1
Nech = 10000

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('./Illustration du théorème central limite_functions.R')

# Pop = rnorm(N,0,1)
Pop = rbeta(N,0.5,0.25)
σ = sd(Pop)
µ = mean(Pop)

PlotPop()

M_ = sampleNech()

PlotHistogramSamples()

CheckSymmetry()

ComparePop_SampleConf(0)
ComparePop_SampleConf(1)
ComparePop_SampleConf(2)
ComparePop_SampleConf(3)
ComparePop_SampleConf(4)
ComparePop_SampleConf(5)
ComparePop_SampleConf(6)
ComparePop_SampleConf(7)


