#include <iostream>
#include "Fraccion.h"
using namespace std;

Fraccion consFraccion(int numerador, int denominador)
{
    struct Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;

    return f;
}


int numerador(Fraccion f)
{
    return f.numerador;
}

int denominador(Fraccion f)
{
    return f.denominador;
}

float division(Fraccion f)
{
    return f.numerador / f.denominador;

}

Fraccion multF(Fraccion f1, Fraccion f2)
{
    struct Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f1.denominador;

    return f;
}

Fraccion sumF(Fraccion f1, Fraccion f2)
{
    struct Fraccion f;
    f.numerador = (f1.numerador * f2.denominador) + (f1.denominador * f2.numerador);
    f.denominador = f1.denominador * f2.denominador;

    return f;
}

int resto(int x,int y)
{
    return x % y;
}

Fraccion simplificada(Fraccion p)
{
    if (resto(p.numerador,p.denominador) == 0)
    {
        p.numerador = p.numerador / p.denominador;
        p.denominador = p.denominador / p.denominador;

        return p;
    }
    else if(resto(p.denominador,p.numerador) == 0)
    {
         p.numerador = p.numerador / p.numerador;
         p.denominador = p.denominador / p.numerador;

         return p;
    }

    return p;
}
