#include <iostream>
#include <string>
#include <sstream>
using namespace std;

class Solution {
public:
    /**
     * 代码中的类名、方法名、参数名已经指定，请勿修改，直接返回方法规定的值即可
     *
     * 
     * @param Ip string字符串 
     * @return int整型
     */
    long long count = 0;
    stringstream ss;
    int RestoreIp(string Ip) {
        
        recRestore(Ip, 3);
        return count;

    }

    void recRestore(const string& sub, int dots){
        if(dots == 0){
            int temp;
            ss << sub;
            ss >> temp;
            ss.clear();
            ss.str("");
            if(sub.length() == 0 or sub.length() > 3 or temp > 255 or 
                (sub[0] == '0' and sub.length() > 1)) // 0开头的数字不合法
                return;
            else {
                ++count;
                return;
            }
        } else {
            int t = sub.length() < 3? sub.length() : 3;
            for(int i = 1; i <= t; ++i){
                if(sub[0] == '0' and i > 1) return; // 0开头的数字不合法
                int temp;
                ss << sub.substr(0, i);
                ss >> temp;
                ss.clear();
                ss.str("");
                if(temp <= 255){
                    recRestore(sub.substr(i), dots - 1);
                }
            }
        }
    }
};

int main(){
    Solution s;
    cout << s.RestoreIp("0000") << endl;
    return 0;
}