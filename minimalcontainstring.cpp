/*
有s和t两个字符串，求s中包含t所有字符的最小子串
*/
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <climits>
using namespace std;

string minWindow(string s, string t) {
    unordered_map<char, int> map;
    for (char c : t) {
        map[c]++;
    }
    int left = 0, right = 0, count = t.size(), minLen = INT_MAX, minStart = 0;
    while (right < s.size()) {
        if (map[s[right++]]-- > 0) {//不在t中map减为负数（踩下去）
            count--; // 在t中count减1，因为完整覆盖了一个t中字符
        }
        while (count == 0) {
            if (right - left < minLen) {
                minLen = right - left;
                minStart = left;
            }
            if (map[s[left++]]++ == 0) {//在t中map加为0（抬起来）
                count++;
            }
        }
    }
    return minLen == INT_MAX ? "" : s.substr(minStart, minLen);
}

int main() {
    string s = "DOBECMTEBANCV";
    string t = "ABC";
    cout << minWindow(s, t) << endl;
    return 0;
}

/*滑动窗口策略。初始化map记录字符串t中所有字符及个数；
初始化左右index(left，right)​为0（字符串s的可变滑动窗口的左右边界index）。
count表示对t的覆盖，扩增right并执行map[s[right++]]--，如果执行该语句查出的值大于零
（说明在t中）则count--，否则count不变，对应走过字符在map中的计数自减1（非t中的字符会变为负数）。
当count减到0时，说明完成了对t的全覆盖，记录最小字串，尝试缩减区间（增加left），
当map[s[left++]]++如果执行该语句查出的值恰等于零（根据之前的约定等于零是t中字符，小于零是非t字符）
时增加对应字符的map计数，即需要重新右滑left，去寻找left缩进所增加到map中的那个字符。
right扩增到s串最右端为终止条件​，否则持续扩增right-缩减区间，记录的最小区间即为答案。*/
