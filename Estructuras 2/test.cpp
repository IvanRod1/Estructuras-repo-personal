#include <iostream>
using namespace std;


string sinElUltimoChar(string s)
{
    return(s.erase(s.size() - 1));
}

int apariciones(char c, string s)
/*Propósito: devuelve la cantidad de apariciones de un char c en el string s*/
{
    if(s.empty())
    {
        return 0;
    }
    else if(c == s.back())
    {
        return 1 + apariciones(c, sinElUltimoChar(s));
    }
    else
    {
        return apariciones(c,sinElUltimoChar(s));
    }
}

bool pertenece(char c, string s)
//Propósito: indica si un char c aparece en el string s.
{
    /*if(s.empty())
    {
        return false;
    }
    else if(c == s.back())
    {
        return true;
    }
    else
    {
        return pertenece(c,sinElUltimoChar(s));
    }*/

    if(s.empty())
    {
        return false;
    }
    else
    {
        return c == s.back() || pertenece(c,sinElUltimoChar(s));
    }
}

int main()
{
    //cout << sinElUltimoChar("hola");
    //cout << apariciones('o',"pokemon");
    cout << pertenece('o',"pokemon");
}
