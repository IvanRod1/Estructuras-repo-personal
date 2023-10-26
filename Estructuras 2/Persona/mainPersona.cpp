#include <iostream>
#include "Persona.h"
using namespace std;

int main()
{
    Persona p1 = consPersona("Ivan",22);
    cout << nombre(p1) << edad(p1) << endl;
    cambioDeNombre("Rodolfo",p1);
    cout << nombre(p1) << edad(p1) << endl;
    Persona p2 = consPersona("Michael",56);
    cout << esMayorQueLaOtra(p1,p2) << endl;
    cout << esMayorQueLaOtra(p2,p1) << endl;
    cout << (laQueEsMayor(p1,p2))->nombre << endl;



}