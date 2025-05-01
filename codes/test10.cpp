#include <iostream>
#include <vector>
#include <unordered_map>

using namespace std;

int main(){
    unordered_map<char, vector<int>> m;

    m['a'] = {1,2};
    m['b'] = {3,4};
    
    // contains
    if(m.count('c')){
        cout << "jskjhsdk" << " ";
    }

    unordered_map<char, bool> mb;
    mb['a'] = true;
    mb['b'] = false;

    // contains
    if(mb.count('c')){
        cout << "jskjhsdk" << " ";
    }

    return 0;
}