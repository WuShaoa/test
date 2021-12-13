import json
import numpy as np
import matplotlib.pyplot as plt

#绘图注意：
# 绘图使用线形区分
# 不要title; x , y 要有轴说明/单位
# 字体和正文字体一致

def plot_data(path):
    fp = open(path, "r")
    tb_data = json.load(fp)
    
    omeg_xl = [float(item["omeg"]["x"]) for item in tb_data]
    omeg_yl = [float(item["omeg"]["y"]) for item in tb_data] 
    omeg_zl = [float(item["omeg"]["z"]) for item in tb_data]
    
    ang_xl = [float(item["ang"]["x"]) for item in tb_data]
    ang_yl = [float(item["ang"]["y"]) for item in tb_data] 
    ang_zl = [float(item["ang"]["z"]) for item in tb_data]

    acc_xl = [float(item["acc"]["x"]) for item in tb_data]
    acc_yl = [float(item["acc"]["y"]) for item in tb_data] 
    acc_zl = [float(item["acc"]["z"]) for item in tb_data]
    
    pres = [float(item["pres"]) for item in tb_data]

    t = np.arange(0, np.size(acc_xl), 1)

    plt.figure()
    plt.title("omeg")

    plt.plot(t,omeg_xl,'r',label="omeg x")
    plt.plot(t,omeg_yl,'g',label="omeg y")
    plt.plot(t,omeg_zl,'b',label="omeg z")
    plt.legend()

    plt.grid()
    #plt.show()

    plt.figure()
    plt.title("ang")

    plt.plot(t,ang_xl,'r',label="ang x")
    plt.plot(t,ang_yl,'g',label="ang y")
    plt.plot(t,ang_zl,'b',label="ang z")
    plt.legend()

    plt.grid()
    #plt.show()

    plt.figure()
    plt.title("acc")

    plt.plot(t,acc_xl,'r',label="acc x")
    plt.plot(t,acc_yl,'g',label="acc y")
    plt.plot(t,acc_zl,'b',label="acc z")
    plt.legend()

    plt.grid()
    #plt.show()

    plt.figure()
    plt.title("pressure")
    plt.plot(t,pres,'y',label="pres")
    plt.legend()

    plt.grid()
    plt.show()
    
plot_data("./data1.json")
plot_data("./data2.json")
plot_data("./data3.json")
