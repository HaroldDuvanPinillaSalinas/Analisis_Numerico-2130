# -*- coding: utf-8 -*-
"""Mano.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1wm5LTktv95Qn8-lYvCwtYpL9NudWQj3L
"""

import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import r2_score
"""
x = np.array([6.7,6.0,5.0,4.3,3.5,2.8,2.3,1.7|,1.8,2.5,3.0,3.6,4.2,4.5,5.2|,5.6,5.3,5.2,5.1,4.9,4.7,4.8,5.2|,5.6,6.0,6.3,|
              7.2,7.3,7.4,7.3,7.8,8.4|,8.8,8.9,9.0,9.1,9.2|,9.6,10.1,10.8,11.5|,11.7,11.4,10.7|,11.4,11.8,12.6,13.0,13.6,
              13.9|,14.0,13.7,13.2,12.9,12.8,12.7,12.5,12.2,12.1,12.0,11.9,11.8,11.6,11.5])
y = np.array([3.0,3.6,4.9,6.3,8.0,8.6,9.7,10.7|,11.3,11.5,11.2,10.7,10.0,9.6,8.6|,9.6,11.0,12.2,13.3,14.0,15.2,16.2,
              16.6|,16.5,15.5,14.0|,11.7,13.8,14.7,16.0,17.4,17.6,|17.3,16.3,15.4,14.0,12.3|,14.5,16.1,16.9,16.5|,15.7,14.4,
              11.2|,12.0,12.4,13.5,14.2,14.4,14.1|,13.7,13.0,12.2,11.6,11.2,10.9,10.5,9.5,8.3,7.6,6.7,6.0,5.0,4.5])
"""
x1 = np.array([6.7,6.0,5.0,4.3,3.5,2.8,2.3,1.7])
y1 = np.array([3.0,3.6,4.9,6.3,8.0,8.6,9.7,10.7])
x2 = np.array([1.7,1.8,2.5,3.0,3.6,4.2,4.5,5.2])
y2 = np.array([10.7,11.3,11.5,11.2,10.7,10.0,9.6,8.6])
x3 = np.array([5.6,5.3,5.2,5.1,4.9])
y3 = np.array([9.6,11.0,12.2,13.3,14.0])
x4 = np.array([4.7,4.8,5.2,5.6,6.0])
y4 = np.array([15.2,16.2,16.6,16.5,15.5])
x5 = np.array([5.6,6.0,6.3,7.2])
y5 = np.array([16.5,15.5,14.0,11.7])
x6 = np.array([7.2,7.3,7.4,7.3,7.8,8.4,8.8])
y6 = np.array([11.7,13.8,14.7,16.0,17.4,17.6,17.3])
x65 = np.array([7.3,7.8,8.4,8.8])
y65 = np.array([16.0,17.4,17.6,17.3])
x7 = np.array([8.8,8.9,9.0,9.1,9.2])
y7 = np.array([17.3,16.3,15.4,14.0,12.3])
x8 = np.array([9.2,9.6,10.1,10.8,11.5])
y8 = np.array([12.3,14.5,16.1,16.9,16.5])
x9 = np.array([11.7,11.4,10.7])
y9 = np.array([15.7,14.4,11.2])
x10 = np.array([10.7,11.4,11.8,12.6])
y10 = np.array([11.2,12.0,12.4,13.5])
x105 = np.array([13.0,13.6,13.9])
y105 = np.array([14.2,14.4,14.1])
x11 = np.array([14.0,13.7,13.2,12.9,12.8,12.7,12.5, 12.2])
y11 = np.array([13.7,13.0,12.2,11.6,11.2,10.9,10.5, 9.5])
x12 = np.array([12.2,12.1,12.0,11.9,11.8,11.6,11.5])
y12 = np.array([9.5,8.3,7.6,6.7,6.0,5.0,4.5])

mymodel1 = np.poly1d(np.polyfit(x1, y1, 3))
myline1 = np.linspace(1.55, 6.8, 50)
mymodel2 = np.poly1d(np.polyfit(x2, y2, 3))
myline2 = np.linspace(1.55, 5.8, 50)
mymodel3 = np.poly1d(np.polyfit(x3, y3, 2))
myline3 = np.linspace(4.7, 5.82, 50)
mymodel4 = np.poly1d(np.polyfit(x4, y4, 3))
myline4 = np.linspace(4.7, 5.7, 50)
mymodel5 = np.poly1d(np.polyfit(x5, y5, 2))
myline5 = np.linspace(5.6, 7.12, 50)
mymodel6 = np.poly1d(np.polyfit(x6, y6, 3))
myline6 = np.linspace(7.15, 7.6, 50)
mymodel65 = np.poly1d(np.polyfit(x65, y65, 2))
myline65 = np.linspace(7.6, 8.8, 50)
mymodel7 = np.poly1d(np.polyfit(x7, y7, 2))
myline7 = np.linspace(8.8, 9.2, 50)
mymodel8 = np.poly1d(np.polyfit(x8, y8, 2))
myline8 = np.linspace(9.2, 11.8, 50)
mymodel9 = np.poly1d(np.polyfit(x9, y9, 2))
myline9 = np.linspace(10.7, 11.7, 50)
mymodel10 = np.poly1d(np.polyfit(x10, y10, 2))
myline10 = np.linspace(10.8, 12.6, 50)
mymodel105 = np.poly1d(np.polyfit(x105, y105, 2))
myline105 = np.linspace(12.6, 14.1, 50)
mymodel11 = np.poly1d(np.polyfit(x11, y11, 2))
myline11 = np.linspace(12.2, 14.1, 50)
mymodel12 = np.poly1d(np.polyfit(x12, y12, 2))
myline12 = np.linspace(11.5, 12.2, 50)

print("1: \n")
print(mymodel1)
print(r2_score(y1, mymodel1(x1)),'\n')
print("2: \n")
print(mymodel2)
print(r2_score(y2, mymodel2(x2)),'\n')
print("3: \n")
print(mymodel3)
print(r2_score(y3, mymodel3(x3)),'\n')
print("4: \n")
print(mymodel4)
print(r2_score(y4, mymodel4(x4)),'\n')
print("5: \n")
print(mymodel5)
print(r2_score(y5, mymodel5(x5)),'\n')
print("6: \n")
print(mymodel6)
print(r2_score(y6, mymodel6(x6)),'\n')
print("7: \n")
print(mymodel7)
print(r2_score(y7, mymodel7(x7)),'\n')
print("8: \n")
print(mymodel8)
print(r2_score(y8, mymodel8(x8)),'\n')
print("9: \n")
print(mymodel9)
print(r2_score(y9, mymodel9(x9)),'\n')
print("10: \n")
print(mymodel10)
print(r2_score(y10, mymodel10(x10)),'\n')
print("11: \n")
print(mymodel11)
print(r2_score(y11, mymodel11(x11)),'\n')

plt.scatter(x1, y1)
plt.scatter(x2, y2)
plt.scatter(x3, y3)
plt.scatter(x4, y4)
plt.scatter(x5, y5)
plt.scatter(x6, y6)
plt.scatter(x65, y65)
plt.scatter(x7, y7)
plt.scatter(x8, y8)
plt.scatter(x9, y9)
plt.scatter(x10, y10)
plt.scatter(x105, y105)
plt.scatter(x11, y11)
plt.scatter(x12, y12)

plt.plot(myline1, mymodel1(myline1))
plt.plot(myline2, mymodel2(myline2))
plt.plot(myline3, mymodel3(myline3))
plt.plot(myline4, mymodel4(myline4))
plt.plot(myline5, mymodel5(myline5))
plt.plot(myline6, mymodel6(myline6))
plt.plot(myline65, mymodel65(myline65))
plt.plot(myline7, mymodel7(myline7))
plt.plot(myline8, mymodel8(myline8))
plt.plot(myline9, mymodel9(myline9))
plt.plot(myline10, mymodel10(myline10))
plt.plot(myline105, mymodel105(myline105))
plt.plot(myline11, mymodel11(myline11))
plt.plot(myline12, mymodel12(myline12))

#plt.scatter(x, y, color="blue")
plt.show()