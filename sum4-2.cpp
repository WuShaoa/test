#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

#include <unordered_map>
#include <unordered_set>
#include <iostream>
#include <algorithm>
#include <functional>

using namespace std;

struct v4hash
{
    size_t operator()(const vector<int>& v) const
    {
        auto h = hash<int>();
        std::size_t seed = v.size();
        for(auto& i : v) {
          seed ^= h(i) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};

class Solution {
public:
    vector<vector<int>> fourSum(vector<int> nums, int target) {
        unordered_set<vector<int>, v4hash> rets;
        unordered_map<long, int> hmap;
        
        for(int i = 0; i < nums.size(); ++i){
            for(int j = i+1; j < nums.size(); ++j){
                hmap.clear();
                for(int k = j+1; k < nums.size(); ++k){
                    if(hmap.find((long)nums[k]) != hmap.end()){
                        vector<int> temp{nums[i], nums[j], nums[k], nums[hmap[(long)nums[k]]]};
                        sort(temp.begin(), temp.end()); //确保不同顺序的四元组不会重复
                        rets.insert(temp);
					} else {
                        hmap[(long)target - (long)nums[k] - (long)nums[i] - (long)nums[j]] = k;
                    }
                }
            }
        }
        vector<vector<int>> ret(rets.begin(), rets.end());
        return ret;
    }
};