/*
max sum_k z_k
where any x_i,x_j \in {x_k}
x_j \not \in [x_i, x_i + y_i]
x_i \not \in [x_j, x_j + y_j]
*/

/*
/*
是的，这个问题可以用动态规划（DP）方法来解决。动态规划的核心思想是通过构建一个状态表来记录每个子问题的最优解，从而逐步构建出整个问题的最优解。

### 动态规划方法步骤

1. **定义状态**：
   - `dp[i][j]` 表示在第 [`i`](command:_github.copilot.openSymbolFromReferences?%5B%22%22%2C%5B%7B%22uri%22%3A%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fhome%2Fkevin%2Ftest6.cpp%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%2C%22pos%22%3A%7B%22line%22%3A35%2C%22character%22%3A13%7D%7D%5D%2C%224265b84e-59ac-4cc4-be11-079f32b70276%22%5D "Go to definition") 个房间中，考虑前 `j` 个表演时的最大不重叠表演数量。

2. **状态转移方程**：
   - 如果第 `j` 个表演不与之前选择的表演重叠，则可以选择这个表演：
     - `dp[i][j] = max(dp[i][j-1], dp[i][k] + 1)`，其中 `k` 是在第 [`i`](command:_github.copilot.openSymbolFromReferences?%5B%22%22%2C%5B%7B%22uri%22%3A%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fhome%2Fkevin%2Ftest6.cpp%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%2C%22pos%22%3A%7B%22line%22%3A35%2C%22character%22%3A13%7D%7D%5D%2C%224265b84e-59ac-4cc4-be11-079f32b70276%22%5D "Go to definition") 个房间中，最后一个不与第 `j` 个表演重叠的表演。
   - 如果第 `j` 个表演与之前选择的表演重叠，则不能选择这个表演：
     - `dp[i][j] = dp[i][j-1]`

3. **初始化**：
   - `dp[i][0] = 0`，表示在第 [`i`](command:_github.copilot.openSymbolFromReferences?%5B%22%22%2C%5B%7B%22uri%22%3A%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fhome%2Fkevin%2Ftest6.cpp%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%2C%22pos%22%3A%7B%22line%22%3A35%2C%22character%22%3A13%7D%7D%5D%2C%224265b84e-59ac-4cc4-be11-079f32b70276%22%5D "Go to definition") 个房间中，没有表演时的最大不重叠表演数量为 0。

4. **结果**：
   - 最终结果是所有房间的最大不重叠表演数量之和。

### 代码实现

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

struct Performance {
    int room, start, end;
    bool operator<(const Performance& other) const {
        if (room != other.room) return room < other.room;
        return end < other.end;
    }
};

int main() {
    int n, m;
    cin >> n >> m;
    vector<Performance> performances(n);
    for (int i = 0; i < n; ++i) {
        cin >> performances[i].room >> performances[i].start >> performances[i].end;
    }

    sort(performances.begin(), performances.end());

    map<int, vector<Performance>> roomPerformances;
    for (const auto& perf : performances) {
        roomPerformances[perf.room].push_back(perf);
    }

    int totalScheduled = 0;

    for (auto& [room, perfs] : roomPerformances) {
        int size = perfs.size();
        vector<int> dp(size, 0);
        dp[0] = 1;

        for (int j = 1; j < size; ++j) {
            dp[j] = dp[j-1];
            for (int k = j-1; k >= 0; --k) {
                if (perfs[k].end <= perfs[j].start) {
                    dp[j] = max(dp[j], dp[k] + 1);
                    break;
                }
            }
        }

        totalScheduled += dp[size-1];
    }

    cout << totalScheduled << endl;

    return 0;
}
```

### 解释
是的，这个问题可以用动态规划（DP）方法来解决。动态规划的核心思想是通过构建一个状态表来记录每个子问题的最优解，从而逐步构建出整个问题的最优解。

### 动态规划方法步骤

1. **定义状态**：
   - `dp[i][j]` 表示在第 [`i`]个房间中，考虑前 `j` 个表演时的最大不重叠表演数量。

2. **状态转移方程**：
   - 如果第 `j` 个表演不与之前选择的表演重叠，则可以选择这个表演：
     - `dp[i][j] = max(dp[i][j-1], dp[i][k] + 1)`，其中 `k` 是在第 [`i`]个房间中，最后一个不与第 `j` 个表演重叠的表演。
   - 如果第 `j` 个表演与之前选择的表演重叠，则不能选择这个表演：
     - `dp[i][j] = dp[i][j-1]`

3. **初始化**：
   - `dp[i][0] = 0`，表示在第 [`i`] 个房间中，没有表演时的最大不重叠表演数量为 0。

4. **结果**：
   - 最终结果是所有房间的最大不重叠表演数量之和。
*/
/*
为了减少时间复杂度，我们可以使用二分查找来优化内层循环。具体来说，在每次查找不重叠的表演时，可以使用二分查找来代替线性查找，从而将时间复杂度从 `O(n^2)` 降低到 `O(n log n)`。

### 优化后的代码

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

struct Performance {
    int room, start, end;
    bool operator<(const Performance& other) const {
        if (room != other.room) return room < other.room;
        return end < other.end;
    }
};

int main() {
    int n, m;
    cin >> n >> m;
    vector<Performance> performances(n);
    for (int i = 0; i < n; ++i) {
        cin >> performances[i].room >> performances[i].start >> performances[i].end;
    }

    sort(performances.begin(), performances.end());

    map<int, vector<Performance>> roomPerformances;
    for (const auto& perf : performances) {
        roomPerformances[perf.room].push_back(perf);
    }

    int totalScheduled = 0;

    for (auto& [room, perfs] : roomPerformances) {
        int size = perfs.size();
        vector<int> dp(size, 0);
        dp[0] = 1;

        for (int j = 1; j < size; ++j) {
            dp[j] = dp[j-1];
            int low = 0, high = j - 1;
            while (low <= high) {
                int mid = (low + high) / 2;
                if (perfs[mid].end <= perfs[j].start) {
                    dp[j] = max(dp[j], dp[mid] + 1);
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }
        }

        totalScheduled += dp[size-1];
    }

    cout

 <<

 totalScheduled << endl;

    return 0;
}
```

### 解释

1. **输入处理**：读取输入的表演数量 `n` 和房间数量 `m`，并存储每个表演的房间号、开始时间和结束时间。
2. **排序**：对表演按照房间号和结束时间进行排序。
3. **按房间分组**：将表演按房间分组存储在 [`roomPerformances`](command:_github.copilot.openSymbolFromReferences?%5B%22%22%2C%5B%7B%22uri%22%3A%7B%22scheme%22%3A%22file%22%2C%22authority%22%3A%22%22%2C%22path%22%3A%22%2Fhome%2Fkevin%2Ftest6.cpp%22%2C%22query%22%3A%22%22%2C%22fragment%22%3A%22%22%7D%2C%22pos%22%3A%7B%22line%22%3A43%2C%22character%22%3A8%7D%7D%5D%2C%225e09386f-a10d-4733-8df2-be842167b8ec%22%5D "Go to definition") 映射中。
4. **动态规划**：对每个房间的表演使用动态规划方法计算最大不重叠表演数量。
   - 使用二分查找来优化查找不重叠表演的过程，从而减少时间复杂度。
5. **结果输出**：输出所有房间的最大不重叠表演数量之和。

通过使用二分查找，我们将内层循环的时间复杂度从 `O(n)` 降低到 `O(log n)`，从而使整个算法的时间复杂度降低到 `O(n log n)`。这样可以显著提高算法的效率。
*/
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

struct Performance
{
    int room, start, end;
    bool operator<(const Performance &other) const
    {
        if (room != other.room)
            return room < other.room;
        return end < other.end;
    }
};

int main()
{
    int n, m;
    cin >> n >> m;
    vector<Performance> performances(n);
    for (int i = 0; i < n; ++i)
    {
        cin >> performances[i].room >> performances[i].start >> performances[i].end;
    }

    sort(performances.begin(), performances.end());

    map<int, vector<Performance>> roomPerformances;
    for (const auto &perf : performances)
    {
        roomPerformances[perf.room].push_back(perf);
    }

    int totalScheduled = 0;

    for (auto &[room, perfs] : roomPerformances) //c++17^ structure binding
    {
        int size = perfs.size();
        vector<int> dp(size, 0);
        dp[0] = 1;

        for (int j = 1; j < size; ++j)
        {
            dp[j] = dp[j - 1];
            for (int k = j - 1; k >= 0; --k)
            {
                if (perfs[k].end <= perfs[j].start)
                {
                    dp[j] = max(dp[j], dp[k] + 1);
                    break;
                }
            }
        }

        totalScheduled += dp[size - 1];
    }

    cout << totalScheduled << endl;

    return 0;
}
