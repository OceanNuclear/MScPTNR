#!/home/ocean/anaconda3/bin/python3
from numpy import exp, cos, arccos, sin, arctan, tan, pi, sqrt; from numpy import array as ary; import numpy as np; tau = 2*pi
import matplotlib.pyplot as plt
from scipy.constants import *

k_B = k*Avogadro/(1000* calorie)
def T(temp):
	return convert_temperature(temp, "C","K")
Temp_range = np.linspace(-50, 1600, 200)

mono = exp(-20/(k_B*T(Temp_range)))
di = 12/2 * exp(-(40-7)/(k_B*T(Temp_range)))
selfInterstitial = exp(-90/(k_B*T(Temp_range)))

plt.semilogy(Temp_range,mono, label="monovacancy")
plt.semilogy(Temp_range,di, label="divacancy")
plt.semilogy(Temp_range,selfInterstitial, label="self Interstitial")
plt.legend()
plt.show()