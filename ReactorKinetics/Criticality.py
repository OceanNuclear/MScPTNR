#!/home/ocean/anaconda3/bin/python3
from numpy import exp, log, cos, arccos, sin, arctan, tan, pi, sqrt; from numpy import array as ary; import numpy as np; tau = 2*pi
from matplotlib import pyplot as plt
from scipy.constants import k, m_n, eV

ρ = -0.000

def speed(energy, mass):
	return sqrt(energy*(2*mass))
barn = 1e-28	#SI

#Using PWR figures, world's most common type of reactor:
#Λ = reproduction time = 1/(2.4*Σ_f*average velocity of ALL neutrons(weighting factor = 1 per neutron) )
#Λ = 1/(2.54*(543*barn)*speed(1*eV,m_n))
#print(Λ)
Λ = 6e-5
λ = 0.08#decay rate of that group's precursor
	#ranges from 0.0001 sec to 10 sec
β = 0.007
n0=1000
def amp1(ρ):
	Δρ=ρ-β
	return (ρ/Δρ)
def amp2(ρ):
	Δρ=ρ-β
	return (-β/Δρ)
def const1(ρ):
	Δρ=ρ-β
	return Δρ/Λ
def const2(ρ):
	Δρ=ρ-β
	return -λ*β/Δρ

t = np.linspace(0,5,100)
n = n0*( amp1(ρ)*exp(const1(ρ) *t) - amp2(ρ)*exp(const2(ρ) *t) )
n1 = n0*(amp1(ρ))*exp(const1(ρ)*t)
n2 = n0*(amp2(ρ))*exp(const2(ρ)*t)
r = np.linspace(β-β,β+β,1000)
#plt.plot(t,n, label="total")
#plt.plot(t,n1,label="first term")
#plt.plot(t,n2,label="second term")
#plt.semilogy(r,(amp1(r)),label="amp1")
#plt.semilogy(r,(amp2(r)),label="amp2")
plt.plot(r,abs(const1(r)),label="const1")
plt.plot(r,abs(const2(r)),label="const2")
#plt.plot(r,log(abs(const1(r)))+log(abs(const2(r))))#This is a constant
plt.legend()
#Check where in the region does the "prompt junp" stay "prompt".

plt.show()