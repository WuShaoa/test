#include <iostream>
#include <string>

using namespace std;

//最长回文子串
int main() {
    string s;
    cin >> s;
    int n = s.size();
    int max_len = 0;
    int start = 0;
    for (int i = 0; i < n; i++) {
        int l = i, r = i;
        while (l >= 0 && r < n && s[l] == s[r]) {
            l--;
            r++;
        }
        if (r - l - 1 > max_len) {
            max_len = r - l - 1;
            start = l + 1;
        }
        l = i, r = i + 1;
        while (l >= 0 && r < n && s[l] == s[r]) {
            l--;
            r++;
        }
        if (r - l - 1 > max_len) {
            max_len = r - l - 1;
            start = l + 1;
        }
    }
    cout << s.substr(start, max_len) << endl;
    return 0;
}