#include "Persona.h"
#include <iostream>
#include <iomanip>

struct PersonaSt {
 string nombre;
 int edad;
};

Persona consPersona(string nombre, int edad)
{
    PersonaSt* p = new Persona;
    (*p).nombre = nombre;
    (*p).edad = edad;

    return p; 
}

string nombre(Persona p)
{
    return p -> nombre;
}

int edad(Persona p)
{
    return p ->edad;
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
    
    return p2;
}






