#include <iostream>
#include <vector>
#include <set>

using namespace std;
set<int> visited;


void print(vector<vector<int>>& net){
    for(auto i = net.begin(); i != net.end(); ++i){
        for(auto j = i->begin(); j != i->end(); ++j){
            cout << *j << " ";
        }
        cout << endl;
    }
}

// 
int dfs(vector<vector<int>>& net, int current_user, int k){
    if(k == 0) {
        return 0;
        }
    else{
        for(auto pu = net[current_user].begin(); pu != net[current_user].end(); ++pu){
            if (*pu==0 || *pu > 1 || pu - net[current_user].begin() == current_user) continue; //已访问或无联系
            else{
                *pu = 0;
                net[pu - net[current_user].begin()][current_user] = 0; //标记已访问
                visited.insert(pu - net[current_user].begin());
                //print(net);
                cout << "current_user: " << current_user << " pu: " << pu - net[current_user].begin() << endl;
                print(net);
                cout << "----------------" << endl;
                dfs(net, pu - net[current_user].begin(), k-1);
                //*pu = 1;
                //net[*pu][current_user] = 1; //清除标记已访问
            }    
        }
    }
    return 0;
    
}


int main()
{
    int N, M, K;
    cin >> N >> M >> K;
    vector<vector<int>> socialnet(N+1, vector<int>(N+1, 0));
    
    for(int i  = 0; i < N; ++i){
        int X, Y;
        cin >> X >> Y;
        socialnet[X][Y] = 1;
        socialnet[Y][X] = 1;
    }
    
    dfs(socialnet, M, K);
 
    cout << visited.size() << endl;
    
    return 0;
}