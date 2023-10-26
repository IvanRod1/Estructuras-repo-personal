#include <iostream>
using namespace std;
#include "Persona.h"

Persona consPersona(string nombre, int edad)
{
    PersonaSt* nPersona = new PersonaSt;
    nPersona->nombre = nombre;
    nPersona->edad = edad;

    return nPersona; 
}

string nombre(Persona p)
{
    return p->nombre;
}

void crecer(Persona p)
{
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p)
{
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2)
{
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2)
{
    if(esMayorQueLaOtra(p1,p2))
    {
        return p1;
    }
    else 
    {
        return p2;
    }
}

int edad(Persona p)
{
    return p->edad;
}
