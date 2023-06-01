#include "Persona.h"
using namespace std;
struct PersonaSt
{
    int edad;
    string nombre;
};

Persona consPersona(string nombreP, int edadP)
{
    PersonaSt* p = new PersonaSt;
    p->edad = edadP;
    p->nombre = nombreP;

    return p;
}

string nombre(Persona p)
{
    return p->nombre;
}

int edad(Persona p)
{
    return p->edad;
    //return (*p).edad
}

void crecer(Persona p)
{
    p->edad++;
}

void cambioDeNombre(string nombreC, Persona p)
{
    p->nombre = nombreC;
}

bool esMayorQueLaOtra(Persona p1, Persona p2)
{
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2)
{
    if (esMayorQueLaOtra(p1,p2))
    {
        return p1;
    }

    return p2;
}

int main()
{
    consPersona("Pedro", 22);
    return 0;
}

//g++ -o prueba1 Persona.cpp
