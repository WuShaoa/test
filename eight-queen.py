board = [[0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0]]

def print_board(board, cnt=-1):
    print(f"Board {cnt}:" )
    for row in board:
        print(row)
    print()

def canplace(board, row, col):
    for i in range(row):
        if board[row][i] == 1 or board[i][col] == 1:
            return False
    for i in range(row):
        for j in range(8):
            if (i+j == row+col) or (i-j == row-col):
                if board[i][j] == 1:
                    return False
    return True 

count = 0
iter_count = 0
def recurse(board, row):
    if row == 8:
        global count
        count += 1
        print_board(board, count) 
        return
    for i in range(8):
        if canplace(board, row, i):
            global iter_count
            iter_count += 1
            board[row][i] = 1
            recurse(board, row+1)
            board[row][i] = 0
            
if __name__ == '__main__':
    print_board(board)
    recurse(board, 0)
    print("count:", count)
    print("iter:", iter_count)
    print("Done")
    print_board(board)