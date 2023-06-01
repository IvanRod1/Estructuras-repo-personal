using namespace std;
#include <iostream>

struct  PokeSt;
typedef string TipoDePokemon;
typedef string tipo;
typedef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
tipo tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);



