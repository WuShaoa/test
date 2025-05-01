#include <map>
#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

class Solution {
public:
    int fourSumCount(vector<int>& nums1, vector<int>& nums2, vector<int>& nums3, vector<int>& nums4) {
        // 分组哈希
        map<int, int> dict;
        auto size1 = nums1.size();
        auto size2 = nums2.size();
        auto size3 = nums3.size();
        auto size4 = nums4.size();
        int count = 0;

        for(int i = 0; i < size1; ++i){
            for(int j = 0; j < size2; ++j){
                dict[-(nums1[i] + nums2[j])]++;
            }
        }

        for(int k = 0; k < size3; ++k){
            for(int l = 0; l < size4; ++l){
                if(dict[nums3[k] + nums4[l]]){
                    count += dict[nums3[k] + nums4[l]]; //有dict[nums3[k] + nums4[l]]个不同的匹配项
                }
            }
        }

        return count;
    }
};


int main(){
    Solution s;

    vector<int> nums1 = {1, 2};
    vector<int> nums2 = {-2, -1};
    vector<int> nums3 = {-1, 2};
    vector<int> nums4 = {0, 2};
    cout << s.fourSumCount(nums1, nums2, nums3, nums4) << endl;

    return 0;
}