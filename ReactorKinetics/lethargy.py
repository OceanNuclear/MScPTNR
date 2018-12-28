#!/home/ocean/anaconda3/bin/python3
from numpy import exp, log, log10, cos, arccos, sin, arctan, tan, pi, sqrt; from numpy import array as ary; import numpy as np; tau = 2*pi
from matplotlib import pyplot as plt
from scipy import constants as c

def s(num):#turning a floating point number into a reader-friendly format
	assert isinstance(num,float)
	if 1<=log10(abs(num))<3:
		#10 -> 10.0
		#100->100.0
		#999->999.0
		string = "{:.1f}".format(num)
	elif 0<=log10(abs(num))<1 :
		#1  ->  1.00
		string = "{:.2f}".format(num)
	elif -1<=log10(abs(num))<0 :
		#0.1->  0.100
		string = "{:.3f}".format(num)
	else:
		string = ('%3.2e'%num)
		place  = string[-3:]#can only handle numbers between 10^+-100
		string = string[:-3]
		for char in place[:-1]:
			if (char!="+") and (char!="0"):
				string+=char
		string+= place[-1]
	return string
def logspace(llim, ulim, num=50):
	return np.logspace(log10(llim), log10(ulim), num)

#choose a 6 group analysis situation:
G = 6 
#define the upper and lower limits of energy
E0 = 2e6 #fast neutron energy (in eV)
Eth= 25.7e-3  #thermal energy (in eV)
Eg = logspace(Eth,E0,G)[::-1] #in unit eV
u = log(E0/Eg)
print("u_g=",[ s(u[g]) for g in range(len(u)) ])
print("the spacing is as below:")
print([ s(elem) for elem in np.diff(u) ])