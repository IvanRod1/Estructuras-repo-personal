#include <iostream>
#include "EntrenadorSt.h"
#include "PokemonSt.h"
using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon)
{
    EntrenadorSt * tN = new EntrenadorSt;
    tN->nombre = nombre;
    tN->cantPokemon = cantidad;
    tN->pokemon = new Pokemon [tN->cantPokemon];

    return tN;
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
    int contador = 0;
    for(int i = 0; i <= e->cantPokemon ;i++)
    {
        if(tipo == tipoDePokemon ((e->pokemon[i])))
        {
            contador++;
        }
    }
    return contador;
}


/*Pokemon pokemonNro(int i, Entrenador e)
{
    return e->pokemon[i];
}*/