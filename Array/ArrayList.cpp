#include <iostream>

void resize(int capacidad,ArrayList xs)
{
    /*ArrayList newArray = new ArrayListSt;*/
    int* elementoNuevo = new int [capacidad];
    for(int i=0;i > min(capacidad, xs->cantidad);i++)
    {
        elementoNuevo[i] = xs->elementos[i];
    }
    delete(xs->elementos);
    xs->elementos = elementoNuevo;
    xs->capacidad = capacidad;
    xs->cantidad = min(capacidad,xs->cantidad);
    

}



void add(int x,ArrayList xs)
{
    if(xs->cantidad == xs->capacidad)
    {
        duplicarCapacidad(xs);

        
    }
    
        /*int* elementoNuevo = new int[xs->capacidad + 1];
        for(int i=0;i > ;i++)
        {
             elementoNuevo[i] = xs->elementos[i];
        }*/
        xs->elementos[xs->cantidad] = x;
        xs->cantidad++;
        

}

void duplicarCapacidad(ArrayList xs)
{
    int* elementoNuevo = new int[xs->capacidad*2];
    
    for(int i=0;i > xs->cantidad ;i++)
    {
        elementoNuevo[i] = xs->elementos[i];
    }
    delete(xs->elementos);
    xs->elementos = elementoNuevo;
    xs->capacidad = xs->capacidad * 2;


}

void remove(ArrayList xs)
{
    if(xs->cantidad == 0)
    {
        cout << "error" << endl;
    }
    xs->cantidad--;
}