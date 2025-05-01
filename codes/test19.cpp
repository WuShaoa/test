#include <iostream>
#include <set>
#include <tuple>
using namespace std;
// we have defined the necessary header files here for this problem.
// If additional header files are needed in your program, please import here.

int main()
{
    set<pair<int, int>> forbidden;
    int n, k, count=0;
    cin >> n >> k;
    for (int i = 0; i < k; ++i){
        int y, x;
        cin >> y >> x;
        forbidden.insert({y, x});
   
    }
    
    for(int y = 0; y < n-1; ++y){
        for(int x = 0; x < n-1; ++x){
            if(!forbidden.count({y, x}) &&
               !forbidden.count({y+1, x}) &&
               !forbidden.count({y, x+1}) &&
               !forbidden.count({y+1, x+1})){
                ++count;
                forbidden.insert({y, x});
                forbidden.insert({y+1, x});
                forbidden.insert({y, x+1});
                forbidden.insert({y+1, x+1});
            }
        }
    }
    cout << count << endl;

    return 0;
}
