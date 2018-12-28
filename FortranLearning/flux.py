#!/home/ocean/anaconda3/bin/python3
from numpy import cos, arccos, sin, arctan, tan, pi, sqrt; from numpy import array as ary; import numpy as np; tau = 2*pi
from matplotlib import pyplot as plt
f = open ('flux.txt','r')
data = f.readlines()[:]
f.close()
flux = ary(data,dtype=float)[::4]
z = 4
dz = z/len(flux)
height = np.linspace(0,z,len(flux))
h2 = height.copy()
height = np.append(height,ary([max(height)+dz]) )
f2 = flux.copy()
flux = np.append(flux,flux[-1])
steps,sflux = [],[]
for i in range(len(flux)-1):
	steps.append(height[i])
	sflux.append(flux  [i])
	steps.append(height[i+1])
	sflux.append(flux  [i])
steps = ary(steps)-dz
plt.plot(steps,sflux,label="Summation without interpolation")
plt.plot(h2,f2,label="Integration with interpolation")
plt.ylabel("flux"+r"($m^{-2}s^{-1}$)")
plt.xlabel("height(m)")
plt.legend()
plt.title("Difference between summation and integration")
#plt.show()
plt.savefig("SumVsInt.png")