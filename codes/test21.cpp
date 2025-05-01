#include <algorithm>
#include <iostream>
#include <numeric>

using namespace std;

class Solution {
public:
    /**
     * 获取两数和
     * @param a int整型一维数组 
     * @param aLen int a数组长度
     * @param b int整型一维数组 
     * @param bLen int b数组长度
     * @return int整型
     */
    int getSnum(int* a, int aLen, int* b, int bLen) {
        int sum2 = 0, ans = 0, posa = -1, posb = -1; 
        // write code here
        if(aLen > bLen){
            for(int i = 0; i < aLen - bLen; ++i){
                
                sum2 = sum2*10 + a[i];
                // cout <<"t1:" <<sum2 << " " << i << endl;
                posa = i;
            }    
        } else {
            for(int i = 0; i < bLen - aLen; ++i){
                
                sum2 = sum2*10 + b[i];
                // cout <<"t2:" <<sum2 << endl;
                posb = i;
            }
        }
        ++posa;
        ++posb;
        for(int i = 0; i < min(aLen, bLen); ++i){
            
            sum2 = sum2*10 + a[i+posa] + b[i+posb];
            // cout <<"t3:" <<sum2 <<" " <<a[i+posa]<< " " << b[i+posb]<< endl;
        }
        //print a, b, posa, posb, sum2
        // cout << "a: ";
        // for(int i = 0; i < aLen; ++i){
        //     cout << a[i] << " ";
        // }
        // cout << endl;
        // cout << "b: ";
        // for(int i = 0; i < bLen; ++i){
        //     cout << b[i] << " ";
        // }
        // cout << endl;
        // cout << "posa: " << posa << endl;
        // cout << "posb: " << posb << endl;
        // cout << "sum2: " << sum2 << endl;
        while(sum2 != 0){
            ans += sum2 % 10;
            sum2 = sum2 / 10;
        }

        return ans;
    }
};

int main(){
    int a[4] = {7,2,4,3};
    int b[3] = {5,6,4};
    Solution s;
    cout << s.getSnum(a, 4, b, 3) << endl;
    return 0;

}