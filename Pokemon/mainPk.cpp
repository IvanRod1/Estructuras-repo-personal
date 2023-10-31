#include <iostream>
using namespace std;
#include "pokemonSt.h"


int main()
{
    Pokemon pk1 = consPokemon("Fuego");
    Pokemon pk2 = consPokemon("Planta");

    cout << superaA(pk1,pk2) << endl;
}