// struct Node{
//     int val;
//     Node* next;
// };

// Node* reorderList(Node *head){
//     Node* last = nullptr;
//     Node* current = head;
//     while (last)
//     {
//         /* code */
//     }
    
// }

#include <iostream>
#include <sstream>
#include <unordered_map>
#include <queue>
#include <string>

using namespace std;

int main(){
    unordered_map<string, int> dict;
    string words;
    int max = 0;
    
    // read line
    getline(cin, words);
    stringstream ss(words);
    // words count
    for(string word; ss >> word;){
        dict[word]++;
    }
    priority_queue<int> pq;
    for(auto wp : dict){
        auto temp = wp.second * wp.first.size();
        max = max > temp ? max : temp;
    }
    cout << max << endl;

    return 0;
}