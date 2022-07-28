
food_table = {#'food': ['ratio/份 m/100g','Kcalories/千卡',  'carbs/碳水', 'protein/蛋白质','fat/脂肪'],
    "糁汤" : [3.86, 226.75, 24.04, 13.83, 6.97],
    "蒸饺" : [0.215, 268, 27, 6.3, 13.8],
    "肉包" : [0.78, 205, 27.11, 13.4, 5.08],
    "挂面" : [3, 353, 75.1, 11.4, 0.9],
    "鸡蛋" : [0.64, 143, 0.1, 12.1, 10.5],
    "奶粉" : [0.6, 0.51, 39, 24, 28.4],
    "豆奶" : [0.347, 0.434, 59.2, 16.8, 13.2],
}

def get_ratio(food):
    return food_table[food][0]
def get_calories(food):
    return food_table[food][1]
def get_carbs(food):
    return food_table[food][2]
def get_protein(food):
    return food_table[food][3]
def get_fat(food):
    return food_table[food][4]

recipys = [#(food_name : num, heat : Kcalories)
]

def get_recipy(hi, lo, max_num):
    def get_recipy_iter(ft, heat, recipy):
        if len(ft) == 0:
            return None # stop
        else:
            temp = dict.copy(ft)
            name = temp.popitem()[0]
            
            num = 0
            for num in range(max_num):
                h = num * get_ratio(name) * get_calories(name)
                
                recipy[name] = num  
                th = heat + h     
                
                if th > hi:
                    break # 剪枝
                elif th >= lo:
                    list.append(recipys, (recipy, th))
                    break
                else:
                    get_recipy_iter(temp, th, dict.copy(recipy))
    get_recipy_iter(food_table, 0, {})
    
get_recipy(1800, 1600, 10)


for r in recipys:
    carbs, protein, fat = 0, 0, 0
    for name in r[0]:
        carbs += get_carbs(name) * r[0][name] * get_ratio(name)
        protein += get_protein(name) * r[0][name] * get_ratio(name)
        fat += get_fat(name) * r[0][name] * get_ratio(name)
    total = carbs + protein + fat
    if 0.5 <= carbs / total <= 0.62 and 0.12 <= protein / total <= 0.20 and 0.16 <= fat / total <= 0.23:
        print(r[0], "份")
        print(r[1], "千卡")
        print("carbs: %.2f, protein: %.2f, fat: %.2f" % (carbs / total, protein / total, fat / total))
        print("total: %.2f" % total, "克")
        print("\n")
            
            

            

        


                
        
    