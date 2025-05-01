/*
 eq / eqv
*/

#include <iostream>

using namespace std;

class obj{
    public:
    int a;
    int b;
    obj(int a, int b):a(a),b(b){}
    bool operator==(const obj& rhs){ // value equal
        return (a == rhs.a && b == rhs.b);
    }
    // bool operator==(const obj& rhs){ // address equal
    //     return &rhs == this;
    // }
};

int main(){
    obj obj1(1,2), obj2(1,2);
    int a = 0;
    int *b = &a; 
    int *c = &a;
    auto ans = (b == c)?"y":"n";
    // auto ans = (obj1 == obj1)?"y":"n";
    
    cout << ans << endl;

}