#include <iostream>
#include "Pokemon.h"
using namespace std;


struct PokeSt {
TipoDePokemon tipo;
int vida;
};


Pokemon consPokemon(TipoDePokemon tipoP)
{
    PokeSt* nPokemon = new PokeSt;
    nPokemon ->tipo = tipoP;
    nPokemon ->vida = 100;
    return nPokemon;
}

tipo tipoDePokemon(Pokemon p)
{
    return p->tipo;
}

int energia(Pokemon p)
{
    return p->vida;
}

void perderEnergia(int energia, Pokemon p)
{
    p->vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2)
{
    if(tipoDePokemon(p1) == "Agua")
    {
        return(tipoDePokemon(p2) != "Planta");
    }
    else if (tipoDePokemon(p1) == "Planta")
    {
        return(tipoDePokemon(p2) != "Fuego");
    }
    else{
        return (tipoDePokemon(p2) != "Agua");
    }
}


