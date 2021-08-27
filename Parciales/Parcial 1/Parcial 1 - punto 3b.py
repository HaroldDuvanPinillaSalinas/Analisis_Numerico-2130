# -*- coding: utf-8 -*-

import numpy as np
from matplotlib import pyplot

def puntofijo(gx, a, tol, max_iteraciones):
    i = 1 # iteración
    b = gx(a)
    tramo = abs(b-a)
    
    while(tramo >= tol and i <= max_iteraciones):
        a = b
        b = gx(a)
        tramo = abs(b-a)
        print("iteracion: ", i, "\tvalor: ", b, "\terror: ", tramo)
        i = i + 1
    result = b
    
    print("iteracion: ", i, "\tvalor: ", b, "\terror: ", tramo)
    
    # Validar respuesta
    if (i >= max_iteraciones ):
        result = np.nan
        
    print("\nEl número de iteraciones fueron: ", i)
    return(result)


# Funcion
fx = lambda x: x - np.cos(x)
gx = lambda x: np.cos(x) # Recta identidad

a = 0
b = 1
tol = 10**-5
max_iteraciones = 100  # iteraciones máximas

# PROCEDIMIENTO
if(fx(a)*fx(b) < 0):
    result = puntofijo(gx,a,tol,max_iteraciones)
    print("El valor de la raíz encontrada fue: ", result)
else: 
    print("El algoritmo no encuentra raices entre el intervalo [", a, ", ", b, "] Intente con otro intervalo")


#GRAFICA

# Valores del eje X que toma el gráfico.
x = range(-5,5)

# Graficar ambas funciones.
pyplot.plot(x, [fx(i) for i in x])

# Establecer el color de los ejes.
pyplot.axhline(0, color="black")
pyplot.axvline(0, color="black")

# Linea vertical en la raíz
pyplot.axvline(result, color="green")

# Limitar los valores de los ejes.
pyplot.xlim(-4, 4)
pyplot.ylim(-4, 4)

# Guardar gráfico como imágen PNG.
pyplot.savefig("output.png")

# Mostrarlo.
pyplot.show()



