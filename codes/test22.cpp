#include <iostream>

using namespace std;

long long pow(int a, int b, int m) {
    int level = a;
    int result = 1;
    while (b != 0) {
        if ((b & 1) == 1) result = result * level % m;
        level = level * level % m;
        b = b >> 1;
    }

    return result % m;
}

int main() {
    int MOD = 1000000007;
    long long n;
    cin >> n;
    n %= MOD;
    long long ans = (n * pow(2, n - 1, MOD)) % MOD;

    cout << ans << endl;

    return 0;
}