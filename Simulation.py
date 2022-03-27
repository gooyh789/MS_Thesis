# -*- coding: utf-8 -*-
"""
Created on Mon Nov  1 09:55:42 2021

@author: gooyh
"""

# Simulation Terror System
import numpy as np
import math
import matplotlib.pyplot as plt


# Design the random death generator 
# If you want to check the designing procedure, see the thesis Appendix C.
xmin = 7 # Lower bound
alpha = 2.258 # Scale parameter
def death(xmin, alpha):
    u = np.random.uniform(0,1)
    x = 0
    if u <= 0.79:
        while (x == 0): 
            x = round(np.random.exponential(scale = 1/0.53))
            if x <= 7:
                x = x
                return x
            else:
                x = 0
    else:
        while (x == 0):
            v = np.random.uniform(0,1)
            x = round(xmin*(1-v)**(-1/(alpha-1)))
            if x >7 and x < 200:
                x = x
                return x
            else:
                x = 0
   
# Generate the random deaths for 492 hostage takings
D = []
np.random.seed(123)
for i in range(493):
    D.append(death(xmin, alpha))

plt.style.use("ggplot")
plt.title("Distribution of Deaths in simulation", fontsize=15, fontweight="bold", loc="left")
plt.ylabel("frequency")
plt.xlabel("Number of deaths")
plt.text(x=20, y=100, s=f"Total deaths in 492 cases: {sum(D)}", fontsize=15)
plt.hist(D, bins=100)
plt.show()


# Design the rescue operational system simulation
# Input parameter
lam = 0.2763 # Inter-arrival rate (occurrence)
mu = 0.2829 # Successful operation rate
eps = 0.1718 # Terrorist killing rate

# Random Policy set
# (A,B) means : Allocate the B number of rescue teams to the A number of hostage takings. 
policySet = [(0,0), (1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (9,9), (10,10)]

# Design the function for the probability of next event by using Uniformization method.
maxTerror = len(policySet)-1
gam = policySet[-1][0]*eps + policySet[-1][1]*mu+lam

# Next event: new terror occurs or current status remained
def getP1(gam, lam, n, maxTerror):
    if n == maxTerror:
        p1 = 0
    else:
        p1 = lam / gam
    return(p1)

# Next event: current terror end
def getP2(gam, mu, eps, n):
    m = policySet[n][1]
    p2 = (m*mu + n*eps) / gam
    return(p2)

# Next event: current terror end with successful operation or terrorist killing
def getP3(mu, eps, n):
    m = policySet[n][1]
    p3 = m*mu / (m*mu + n*eps)
    return(p3)

n = 0 # number of hostage takings in the system (current)
tE = 0 # event time
t = 0 # current time
NA = 0 # cumulative number of arrival
ND = 0 # cumulative number of departure
NT = [] # number of terror set in the system
AT = [] # arrival time set
DT = [] # departure time set
TT = [] # event time set
Death = [] # deaths set
LT = 365 * 100 # time limit in long run 
S = 0 # check the how many successful operation exist in the simulation
F = 0 # check the how many time terroris kill the hostages in the simulation
np.random.seed(123)
while t < LT:
    # Generate next event time
    tE = -math.log(np.random.rand(1))/gam
    TT.append(tE)
    t = t + tE
    U = np.random.rand(1)
    p1 = getP1(gam, lam, n, maxTerror)
    p2 = getP2(gam, mu, eps, n)
    # new hostage taking occurs
    if U <= p1:
        n = n+1 # The number of current hostage takings is increased
        NA = NA+1
        AT.append(t)
        NT.append(n)
    # current hostage taking end
    elif U <= (p1+p2):
        V = np.random.rand(1)
        p3 = getP3(mu, eps, n)
        # successful operation
        if V <= p3:
            S = S+1
        # terrorists kill the hostages
        else:
            F = F+1
            # generate the random deaths
            rt = death(xmin, alpha)
            Death.append(rt)
        n = n-1 # The number of current hostage takings is decreased. 
        ND = ND+1
        DT.append(t)
        NT.append(n)
    # current hostage taking remained
    else:
        n = n
        NT.append(n)        
    print(f"Simulation processing...{(t/LT)*100}%")

# Proportion of operation execution and terrorist act
print(S / (S+F)) # operation execution
print(F / (S+F)) # terrorist act

len(TT)
len(NT)

stateTime = {}
for num in range(len(NT)):
    if NT[num] in stateTime:
        stateTime[NT[num]] += TT[num]
    else:
        stateTime[NT[num]] = TT[num]
sumTime = sum(stateTime.values())
for key, values in stateTime.items():
    stateTime[key] = values/sumTime

print(stateTime)

# Check the No. of visit in state (No. of hostage takings)
state = {}
for val in NT:
    if val in state:
        state[val] += 1
    else:
        state[val] = 1
sumVal = sum(state.values())
for key, values in state.items():
    state[key] = values/sumVal
print(state)

plt.style.use("ggplot")
plt.title("Distribution of Deaths in simulation", fontsize=15, fontweight="bold", loc="left")
plt.ylabel("frequency")
plt.xlabel("Number of deaths")
plt.text(x=50, y=1400, s=f"Total deaths in 100 years: {sum(Death)}", fontsize=15)
plt.text(x=50, y=1200, s=f"No. of hostage takings: {S+F}", fontsize=15)
plt.text(x=50, y=1000, s=f"No. of successful operations: {S}", fontsize=15)
plt.hist(Death, bins=100)
plt.show()

# Generate random deaths for 492 hostage takings by applying different set.seed
# Then check the Confidence interval and Prediction interval
DD = []
for j in range(999):
    np.random.seed(j)
    D = []
    for i in range(493):
        D.append(death(xmin, alpha))
    DD.append(sum(D))

meanD = sum(DD)/len(DD)

PI = [meanD-1.96*(np.std(DD)/math.sqrt(1+1/len(DD))),meanD+1.96*(np.std(DD)/math.sqrt(1+1/len(DD)))]
print(PI)


CI = [meanD-1.96*(np.std(DD)/math.sqrt(len(DD))),meanD+1.96*(np.std(DD)/math.sqrt(len(DD)))]
print(CI)


plt.title("Distribution of Deaths in simulation", fontsize=15, fontweight="bold", loc="left")
plt.ylabel("frequency")
plt.xlabel("No. deaths in 492 incidents")
plt.axvline(x=2890, color='black')
plt.axvline(x=PI[0], color='green')
plt.axvline(x=PI[1], color='green')
plt.axvline(x=CI[0], color='blue')
plt.axvline(x=CI[1], color='blue')
plt.hist(DD)
plt.show()

