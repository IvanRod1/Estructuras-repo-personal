#include <iostream>
using namespace std;
#include "Pokemon.h"
#include "Entrenador.h"

struct EntrenadorSt {
string nombre;
Pokemon* pokemon;
int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon)
{
    EntrenadorSt* trainerP = new EntrenadorSt;
    trainerP ->nombre = nombre;
    trainerP ->cantPokemon = cantidad;
    trainerP ->pokemon = new Pokemon [trainerP->cantPokemon];
    return trainerP;
}

string nombreDeEntrenador(Entrenador e)
{
    return e->nombre;
}

int cantidadDePokemon(Entrenador e)
{
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e)
{
    int contadorPokemon = 0;
    for(int i = 0; i <= e->cantPokemon; i++)
    {
        if(tipoDePokemon(e->pokemon[i]) == tipo)
        {
            contadorPokemon++;
        }
    }
    return contadorPokemon;
}

Pokemon pokemonNro(int i, Entrenador e)
{
    if(i > 0 && i <= e->cantPokemon)
    {
        return e->pokemon[i - 1];
    }
    return NULL;
}

bool leGanaATodos(Entrenador e1, Entrenador e2)
{
    for(int i = 0; i <= e1->cantPokemon && i <= e2->cantPokemon;i++)
    {
        if(superaA(e1->pokemon[i],e2->pokemon[i]))
        {
            i++;
        }
    }
}
   

