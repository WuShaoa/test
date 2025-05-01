#include <iostream>
#include <vector>
#include <stack>
#include <tuple>

using namespace std;

class Solution {
public:
    vector<int> dailyTemperatures(vector<int>& temperatures) {
        vector<int> ret(temperatures.size(), 0);
        stack<int> des_stack; // 单调栈 （单调减或相等） 记录单调减的位置， 遇见增时pop并记录位置差，直到符合单调规则
        auto p = temperatures.begin();
        des_stack.push(p - temperatures.begin());
        while(p != temperatures.end()){
            if(*p <= temperatures[des_stack.top()]) des_stack.push(p - temperatures.begin());
            else{
                while(!des_stack.empty() && *p > temperatures[des_stack.top()]){
                    ret[des_stack.top()] = p - temperatures.begin() - des_stack.top();
                    des_stack.pop();
                }
                des_stack.push(p - temperatures.begin());
            }
            ++p;
        }
        return ret;
    }
};

int main(){
    Solution s;

    vector<int> temperatures = {73, 74, 75, 71, 69, 72, 76, 73};
    vector<int> ret = s.dailyTemperatures(temperatures);
    for(auto i : ret) cout << i << " ";
    cout << endl;

    return 0;
}