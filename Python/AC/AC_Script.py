#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 25 10:11:02 2017

@author: jsysley
"""

#==============================================================================
# 输入
#多组（不超过 5 组）数据。
#每组输入数据分为三行，第一行有两个数字 n,m($0＜n,m\leq10000$)，分别表示集合 A 和集合 B 的元素个数。
#后两行分别表示集合 A 和集合 B。每个元素为不超出 int 范围的整数，每个元素之间有一个空格隔开。
# 输出
#针对每组数据输出一行数据，表示合并后的集合，要求从小到大输出，每个元素之间有一个空格隔开。
# 样例输入
#1 2
#1
#2 3
#1 2
#1
#1 2 
# 样例输出
#1 2 3
#1 2
#==============================================================================
line = raw_input().strip()
while line:
    n,m = map(lambda x :int(x),line.split(" "))
    line_a = map(lambda x:int(x),raw_input().strip().split(" "))
    line_b = map(lambda x:int(x),raw_input().strip().split(" "))
    line_a = set(line_a)
    line_b = set(line_b)    
    res = line_a|line_b
    res = list(res)
    res.sort()
    res = map(lambda x:str(x),res)
    res = " ".join(res)
    print(res)
    line = raw_input().strip()


#==============================================================================
# 输入
#输入中有多组测试数据。每组测试数据的第一行为两个整数n和m（1=＜n, m=＜1000），
#分别表示价签的数量以及小B的购买清单中所列的物品数。
#第二行为空格分隔的n个正整数，表示货架上各类物品的价格，每个数的大小不超过100000。
#随后的m行为购买清单中物品的名称，所有物品名称为非空的不超过32个拉丁字母构成的字符串，
#保证清单中不同的物品种类数不超过n，且商店有小B想要购买的所有物品。 
# 输出
#对每组测试数据，在单独的行中输出两个数a和b，表示购买清单上所有的物品可能需要的最小和最大费用。
# 样例输入
#5 3
#4 2 1 10 5
#apple
#orange
#mango
#6 5
#3 5 1 6 8 1
#peach
#grapefruit
#banana
#orange
#orange
#  样例输出
#7 19
#11 30
#==============================================================================
line = raw_input().strip()
while line:
    n,m = map(lambda x :int(x),line.split())
    price = map(lambda x:int(x),raw_input().strip().split())
    things = {}
    for i in xrange(m):
        temp = raw_input().strip()
        things[temp] = things.get(temp,0) + 1
    all_fre = things.values()
    all_length = len(things.items())#需要的不同的数的个数
    price.sort()#对价格排序
    number = sorted(all_fre,reverse=True)
    #最小
    price_minlist = [x*y for x,y in zip(price,number)]
    price_min = sum(price_minlist)
    #最大
    price_maxlist = [x*y for x,y in zip(price[::-1],number)]
    price_max = sum(price_maxlist)
    res = [price_min,price_max]
    res = map(lambda x:str(x),res)
    print " ".join(res)
    line = raw_input().strip()


###正确答案
while 1:
    r = raw_input()
    if r != '':
        (n,m) = (int(x) for x in r.split())
        price = [int(x) for x in raw_input().split()]
        wishlist = []
        iter_k = 0
        while iter_k <m:#读入
            want = raw_input()
            if want != '':
                wishlist += want.split()
                iter_k +=1
        number = [wishlist.count(x) for x in list(set(wishlist))]
        price.sort() 
        number.sort(reverse = True)
        min = [x*y for x,y in zip(price,number)]
        cost_min = sum(min)
        max = [x*y for x,y in zip(price[::-1],number)]
        cost_max = sum(max)
        print (str(cost_min) + ' ' +str(cost_max))

#==============================================================================
# 输入描述:
#输入包括多组测试数据。
#每组输入第一行是两个正整数N和M（0 < N <= 30000,0 < M < 5000）,
#分别代表学生的数目和操作的数目。学生ID编号从1编到N。
#第二行包含N个整数，代表这N个学生的初始成绩，其中第i个数代表ID为i的学生的成绩
#接下来又M行，每一行有一个字符C（只取‘Q’或‘U’），和两个正整数A,B,当C为'Q'的时候, 
#表示这是一条询问操作，他询问ID从A到B（包括A,B）的学生当中，
#成绩最高的是多少
#当C为‘U’的时候，表示这是一条更新操作，要求把ID为A的学生的成绩更改为B。
#==============================================================================
import sys
s = sys.stdin.readline().strip()
while s:
    n,m = map(lambda x :int(x),s.split())
    score = [int(x) for x in sys.stdin.readline().strip().split()]
    result=[]
    for i in xrange(m):
        instruction = sys.stdin.readline().strip().split()
        if instruction[0]=="Q":
            ins_left = [int(x) for x in instruction[1:3]]
            up = max(ins_left)
            low = min(ins_left)
            use = score[(low-1):up]
            result.append(max(use))
        if instruction[0]=="U":
            ins_left = [int(x) for x in instruction[1:3]]
            score[ins_left[0]-1] = ins_left[1]
    for i in result:
        sys.stdout.write(str(i)+'\n')
    s = sys.stdin.readline().strip()

#==============================================================================
# C++
#世界上有10种人，一种懂二进制，一种不懂。
#那么你知道两个int32整数m和n的二进制表达，有多少个位(bit)不同么？
#输入例子:
#1999 2299
#
#输出例子:
#7
#==============================================================================
#class Solution {
#public:
#    /**
#     * 获得两个整形二进制表达位数不同的数量
#     * 
#     * @param m 整数m
#     * @param n 整数n
#     * @return 整型
#     */
#    int countBitDiff(int m, int n) 
#    {
#		int len = 32;
#    	int difNum = 0;
#    	for(int i=0;i<32;i++)
#   		{
#        	int m_low = m&1; //取m最后一位，下同
#        	int n_low = n&1;
#        	if((m_low != n_low)) //直接两个数的最后一位对比
#            	difNum++;    //不同就计数+1
#        	m = m >> 1;   //m往右移动一位，下次循环就可以取倒数第二位，下同
#        	n = n >> 1;	
#    }
#    	return difNum;
#    }
#};

#==============================================================================
# 有一个长为n的数组A，求满足0≤a≤b<n的A[b]-A[a]的最大值。
#给定数组A及它的大小n，请返回最大差值。
#测试样例：
#[10,5],2
#返回：0
#==============================================================================
# -*- coding:utf-8 -*-

class LongestDistance:
    def getDis(self, A, n):
        # write code here
        dis = 0#初始化差值
        if n>1:#当n<1时返回0
            min_num = A[0]#记录最小值
            for i in range(n):#遍历n个数
                if A[i] - min_num > dis:
                    dis = A[i] - min_num#更新最大差值
                if min_num > A[i]:
                    min_num = A[i]
        return dis

#==============================================================================
#对于一个有序数组，我们通常采用二分查找的方式来定位某一元素，
#请编写二分查找的算法，在数组中查找指定元素。
#给定一个整数数组A及它的大小n，同时给定要查找的元素val，
#请返回它在数组中的位置(从0开始)，若不存在该元素，返回-1。
#若该元素出现多次，请返回第一次出现的位置。
#测试样例：
#[1,3,5,7,9],5,3
#返回：1
#==============================================================================
# -*- coding:utf-8 -*-

class BinarySearch:
    def getPos(self, A, n, val):
        # write code here
        #默认从小到大排列数字
        
        if n<=0 or len(A)==0:
            return -1
        mid = 0#中间位置
        L=0#左边位置
        R=n-1#右边位置
        while L<R:#只要左边位置小于右边位置
            mid = (L+R)/2#取得中间位置
            if A[mid] > val:
                R = mid-1#mid已经可以排序，mid包括mid的右边都比val大
            elif A[mid] < val:
                L = mid+1
            else:
                R = mid
        if A[L]==val:
            return L
        return -1

#==============================================================================
# 题目描述
#小易经常沉迷于网络游戏.有一次,他在玩一个打怪升级的游戏,
#他的角色的初始能力值为 a.
#在接下来的一段时间内,他将会依次遇见n个怪物,
#每个怪物的防御力为b1,b2,b3...bn. 
#如果遇到的怪物防御力bi小于等于小易的当前能力值c,那么他就能轻松打败怪物,
#并 且使得自己的能力值增加bi;如果bi大于c,那他也能打败怪物,
#但他的能力值只能增加bi 与c的最大公约数.
#那么问题来了,在一系列的锻炼后,小易的最终能力值为多少?
#输入描述:
#对于每组数据,第一行是两个整数n(1≤n<100000)
#表示怪物的数量和a表示小易的初始能力值.
#第二行n个整数,b1,b2...bn(1≤bi≤n)表示每个怪物的防御力
#
#输出描述:
#对于每组数据,输出一行.每行仅包含一个整数,表示小易的最终能力值

#==============================================================================
import sys
line = sys.stdin.readline().strip()
def Max_CD(x1,x2):
    r = x1 % x2
    while r:
        x1 = x2
        x2 = r
        r = x1 % x2
    return x2

while line:
    n,a = [int(x) for x in line.split()]
    monster = [int(x) for x in sys.stdin.readline().strip().split()]
    for i in xrange(n):
        if a >= monster[i]:
            a = a + monster[i]
        else:
            common_div = Max_CD(a,monster[i])
            a = a + common_div
    sys.stdout.write(str(a)+'\n')
    line = sys.stdin.readline().strip()
###错误：数组越界
###改正，
#越界原因是因为上述的while不能用line循环，原因未知
#其二是第二行三个数字的读入是依次的（实际测试案例的数字十分多）
while True:
    try:
        line = sys.stdin.readline().strip()
        n,a = [int(x) for x in line.split()]
        for i in xrange(n):
            monster = int(sys.stdin.readline().strip())
            if a >= monster:
                a = a + monster
            else:
                common_div = Max_CD(a,monster)
                a = a + common_div
        sys.stdout.write(repr(a)+'\n')
    except:
        break
###正确答案
while True:
    def gcd(a,b):
        if b%a==0:
            return a
        return gcd(b%a,a)
    try:
        inp=raw_input()
        inp=inp.split()
        quanlity=int(inp[0])
        ability=int(inp[1])
        for i in range(quanlity):
            inp_def=raw_input()
            inp_def=int(inp_def)
            if ability>=inp_def:
                ability+=inp_def
            else:
                ability+=gcd(ability,inp_def)
        print ability
    except:
        break
    
#==============================================================================
#题目描述
#对于一个字符串，请设计一个高效算法，找到第一次重复出现的字符。
#给定一个字符串(不一定全为字母)A及它的长度n。请返回第一个重复出现的字符。
#保证字符串中有重复字符，字符串的长度小于等于500。
#测试样例：
#"qywyer23tdd",11
#返回：y
#注意：第一次重复出现是从阶段字符串的角度看的，如'kaak'第一个出现重复的字符是a
#==============================================================================
# -*- coding:utf-8 -*-

class FirstRepeat:
    def findFirstRepeat(self, A, n):
        # write code here
        if A is None or n <=0:
            return None
        aSet =set()
        for i in A:
            if i not in aSet:
                aSet.add(i)
            else:
                return i

#==============================================================================
# 题目描述
#春节期间小明使用微信收到很多个红包，非常开心。在查看领取红包记录时发现，某个红包金额出现的次数超过了红包总数的一半。请帮小明找到该红包金额。写出具体算法思路和代码实现，要求算法尽可能高效。
#给定一个红包的金额数组gifts及它的大小n，请返回所求红包的金额。
#若没有金额超过总数的一半，返回0。
#测试样例：
#[1,2,3,2,2],5
#返回：2
#==============================================================================
# -*- coding:utf-8 -*-

class Gift:
    def getValue(self, gifts, n):
        # write code here
        uni = set(gifts)
        flag = False#记录有没有找到
        for x in uni:
            fre = gifts.count(x)
            if fre > n/float(2):
                flag = True
                return x
                break
        if not flag:
            return 0
###高效答案
#先排序，如果一个数出现次数超过一半，排序过后，必然排在中间，
#则最后遍历整个数组查看是否符合即可。
# -*- coding:utf-8 -*-

class Gift:
    def getValue(self, gifts, n):
        # write code here
        gifts.sort()#排序
        flag = False#记录有没有找到
        fre = gifts.count(gifts[n/2])#次数可以循环自己数
        if fre > n/float(2):
            return gifts[n/2]
        else:
            return 0

#==============================================================================
# 题目描述
#对于一个字符串，和字符串中的某一位置，请设计一个算法，
#将包括i位置在内的左侧部分移动到右边，将右侧部分移动到左边。
#给定字符串A和它的长度n以及特定位置p，请返回旋转后的结果。
#测试样例：
#"ABCDEFGH",8,4
#返回："FGHABCDE"
#==============================================================================
# -*- coding:utf-8 -*-

class StringRotation:
    def rotateString(self, A, n, p):
        # write code here
        before = A[:(p+1)]
        after = A[(p+1):]
        return after + before

#==============================================================================
# 题目描述
#有一个数组a[N]顺序存放0~N-1，要求每隔两个数删掉一个数，
#到末尾时循环至开头继续进行，求最后一个被删掉的数的原始下标位置。
#以8个数(N=7)为例:｛0，1，2，3，4，5，6，7｝，
#0->1->2(删除)->3->4->5(删除)->6->7->0(删除),如此循环直到最后一个数被删除。
#
#输入描述:
#每组数据为一行一个整数n(小于等于1000)，为数组成员数,如果大于1000，
#则对a[999]进行计算。
#
#输出描述:
#一行输出最后一个被删掉的数的原始下标位置。
#
#输入例子:
#8
#
#输出例子:
#6
#==============================================================================
import sys
while True:
    try:
        n = int(sys.stdin.readline().strip())
        num = range(n)
        i = 0
        while True:
            i = (i + 2)%len(num)
            num.remove(num[i])
            if len(num)==1:
                break
        print num[0]
    except:
        break


#==============================================================================
# 题目描述
#开发一个简单错误记录功能小模块，能够记录出错的代码所在的文件名称和行号。 
#处理:
#1.记录最多8条错误记录，对相同的错误记录(即文件名称和行号完全匹配)只记录一条，
#错误计数增加；(文件所在的目录不同，文件名和行号相同也要合并)
#2.超过16个字符的文件名称，只记录文件的最后有效16个字符；
#(如果文件名不同，而只是文件名的后16个字符和行号相同，也不要合并)
#3.输入的文件可能带路径，记录文件名称不能带路径

#输入描述:
#一行或多行字符串。每行包括带路径文件名称，行号，以空格隔开。
#
#    文件路径为windows格式
#
#    如：E:\V1R2\product\fpgadrive.c 1325

#输出描述:
#将所有的记录统计并将结果输出，格式：文件名代码行数数目，一个空格隔开，如: fpgadrive.c 1325 1 
#
#    结果根据数目从多到少排序，数目相同的情况下，按照输入第一次出现顺序排序。
#
#    如果超过8条记录，则只输出前8条记录.
#
#    如果文件名的长度超过16个字符，则只输出后16个字符

#输入例子:
#E:\V1R2\product\fpgadrive.c 1325
#
#输出例子:
#fpgadrive.c 1325 1
#==============================================================================
#难点：程序什么时候退出循环，
#读入的路径有转义符怎么处理,数值相同时按照输入的顺序排序（有有序字典）
import sys
import collections
sta = collections.OrderedDict()
while True:
    try:
        x = sys.stdin.readline().strip()
        if not x:
            break#退出程序条件
        i = x.rindex("\\")
        file_name = x[i+1:]
        sta[file_name] = sta.get(file_name,0) + 1
    except:
        break
sta_sorted = sorted(sta.items(),key = lambda x:x[1],reverse=True)

for j in xrange(min(len(sta),8)):#超过8条仅输出前8条
    print(sta_sorted[j][0] +" " + repr(sta_sorted[j][1]))
#错误原因：当值相同时没有考虑加入顺序,没有考虑文件名超过16时
#改正
import sys
import collections
sta = collections.OrderedDict()
while True:
    try:
        x = sys.stdin.readline().strip()
        if not x:
            break#退出程序条件
        i = x.rindex("\\")
        file_name = x[i+1:]
        sta[file_name] = sta.get(file_name,0) + 1
    except:
        break
sta = sta.items()
sta.sort(key = lambda x:x[1],reverse=True)
for j in xrange(min(len(sta),8)):#超过8条仅输出前8条
    t = sta[j][0].split(' ')
    print t[0][-16:], t[1], sta[j][1]
#正确答案
import collections

d = collections.OrderedDict()
while 1:
    try:
        x = raw_input()
        if not x:
            break
        i = x.rfind('\\')
        q = x[i + 1:]
        if q in d:
            d[q] += 1
        else:
            d[q] = 1
    except:
        break
d = d.items()
d.sort(key=lambda k: k[1], reverse=True)
for i in range(min(len(d), 8)):
    t = d[i][0].split(' ')
    print t[0][-16:], t[1], d[i][1]


#==============================================================================
# 题目描述
#兰博教训提莫之后,然后和提莫讨论起约德尔人,谈起约德尔人,自然少不了一个人,
#那 就是黑默丁格------约德尔人历史上最伟大的科学家. 提莫说,
#黑默丁格最近在思考一个问题:黑默丁格有三个炮台,
#炮台能攻击到距离它R的敌人 (两点之间的距离为两点连续的距离,
#例如(3,0),(0,4)之间的距离是5),如果一个炮台能攻击 到敌人,
#那么就会对敌人造成1×的伤害.黑默丁格将三个炮台放在N*M方格中的点上,
#并且给出敌人 的坐标. 问:那么敌人受到伤害会是多大?
#
#输入描述:
#第一行9个整数,R,x1,y1,x2,y2,x3,y3,x0,y0.R代表炮台攻击的最大距离,(x1,y1),(x2,y2),
#(x3,y3)代表三个炮台的坐标.(x0,y0)代表敌人的坐标.
#
#输出描述:
#输出一行,这一行代表敌人承受的最大伤害,(如果每个炮台都不能攻击到敌人,输出0×)
#
#输入例子:
#1 1 1 2 2 3 3 1 2
#
#输出例子:
#2x
#==============================================================================
import sys
def Dist(x1,y1,x0,y0):
    return ((x1 - x0)**2 + (y1 - y0)**2)**0.5
while True:
    try:
        harm = 0
        R,x1,y1,x2,y2,x3,y3,x0,y0 = [int(x) for x in sys.stdin.readline().strip().split()]
        boom_x = [x1,x2,x3]
        boom_y = [y1,y2,y3]
        for i in xrange(3):
            dis = Dist(boom_x[i],boom_y[i],x0,y0)
            if dis <= R:#可以攻击
                harm = harm + 1
        print repr(harm) + 'x'
    except:
        break

#==============================================================================
# 题目描述
在一组数的编码中，若任意两个相邻的代码只有一位二进制数不同，
则称这种编码为格雷码(Gray Code)，请编写一个函数，
使用递归的方法生成N位的格雷码。
给定一个整数n，请返回n位的格雷码，顺序为从0开始。
测试样例：
1
返回：["0","1"]
#==============================================================================
#思路
#递归的思路就是n位gray码是由n-1位gray码生成，举个例子简单一些：
#比如求n=3的gray码，首先知道n=2的gray码是(00,01,11,10)
#那么n=3的gray码其实就是对n=2的gray码首位添加0或1生成的，
#添加0后变成(000,001,011,010)
#添加1后需要顺序反向就变成(110,111,101,100)
#组合在一起就是(000,001,011,010,110,111,101,100)
# -*- coding:utf-8 -*-

class GrayCode:
    def getGray(self, n):
        # write code here
        if n==0:
            return 0
        elif n==1:
            return (["0","1"])
        else:
            pre_gray = self.getGray(n-1,res)
            new_gray = ['0' + x for x in pre_gray]
            new_gray += ['1' + x for x in pre_gray[-1::-1]]
        return new_gray
    
##调试
def getGray(n):
    # write code here
    if n==0:
        return 0
    elif n==1:
        return ["0","1"]
    else:
        pre_gray = getGray(n-1)
        new_gray = ['0' + x for x in pre_gray]
        new_gray += ['1' + x for x in pre_gray[-1::-1]]
    return new_gray
    
    
#==============================================================================
#题目描述
C市现在要转移一批罪犯到D市，C市有n名罪犯，按照入狱时间有顺序，
另外每个罪犯有一个罪行值，值越大罪越重。
现在为了方便管理，市长决定转移入狱时间连续的c名犯人，
同时要求转移犯人的罪行值之和不超过t，问有多少种选择的方式？ 

输入描述:
第一行数据三个整数:n，t，c(1≤n≤2e5,0≤t≤1e9,1≤c≤n)，
第二行按入狱时间给出每个犯人的罪行值ai(0≤ai≤1e9)

输出描述:
一行输出答案。

输入例子:
3 100 2
1 2 3

输出例子:
2
#==============================================================================
import sys
while True:
    try:
        n,t,c = [int(x) for x in sys.stdin.readline().strip().split()]
        sta = [int(x) for x in sys.stdin.readline().strip().split()]
        count = 0
        for i in xrange(n-c+1):
            if sum(sta[i:(i+c)]) <=t:#这样太复杂
                count+=1
        print count
    except:
        break
###改
import sys
while True:
    try:
        n,t,c = [int(x) for x in sys.stdin.readline().strip().split()]
        sta = [int(x) for x in sys.stdin.readline().strip().split()]
        count = 0
        ini = sum(sta[0:c])
        if ini > t:
            count = 0
        else:
            count = 1
        for i in xrange(n-c):
            ini = ini + sta[c+i] - sta[i]
            if ini <=t:#这样太复杂
                count+=1
        print count
    except:
        break
###正确答案
while True:
    try:
        a = raw_input("").split()
        b = raw_input("")
    except:
        break
    n,t,c = int(a[0]),int(a[1]),int(a[2])
    k = [int(i) for i in b.split()]
    p = sum(k[0:c])
    if p>t:
        num = 0
    else:
        num = 1
    for i in range(n-c):
        p = p+k[c+i]-k[i]
        if p <= t:
            num += 1
    print num
    

#==============================================================================
# 题目描述
#输入一个字符串，求出该字符串包含的字符集合
#
#输入描述:
#每组数据输入一个字符串，字符串最大长度为100，且只包含字母，
#不可能为空串，区分大小写。
#
#输出描述:
#每组数据一行，按字符串原有的字符顺序，输出字符集合，即重复出现并靠后的字母不输出。
#
#输入例子:
#abcqweracb
#
#输出例子:
#abcqwer
#==============================================================================
import sys
import collections
while True:
    try:
        res = collections.OrderedDict()
        line = sys.stdin.readline().strip()#换成raw_input()即可
        for x in line:
            res[x] = res.get(x,0) + 1
        print "".join(res.keys())
    except:
        break
###复杂度太高
import sys
while True:
    try:
        res = ""
        line = sys.stdin.readline().strip()#换成raw_input()即可
        for i in xrange(len(set(list(line)))):
            res = res + line[0]
            line = line.replace(line[0],"")
        print "".join(res)
    except:
        break
###复杂度太高，改为raw_input()就可以
while True:
    try:
        res = []
        line = raw_input()
        for x in line:
            if x not in res:
            	res.append(x)
        print "".join(res)
    except:
        break
###正确答案
while True:
    try:
        s = str(raw_input())
        L = []
        result = ''
        for i in s:
            if not i in L:
                L.append(i)
                result += i
        print result
    except:
        break

#==============================================================================
题目描述
在4x4的棋盘上摆满了黑白棋子，黑白两色的位置和数目随机其中左上角坐标为(1,1),
右下角坐标为(4,4),现在依次有一些翻转操作，
要对一些给定支点坐标为中心的上下左右四个棋子的颜色进行翻转，
请计算出翻转后的棋盘颜色。
给定两个数组A和f,分别为初始棋盘和翻转位置。其中翻转位置共有3个。请返回翻转后的棋盘。
测试样例：
[[0,0,1,1],[1,0,1,0],[0,1,1,0],[0,0,1,0]],[[2,2],[3,3],[4,4]]
返回：[[0,1,1,1],[0,0,1,0],[0,1,1,0],[0,0,1,0]]

#==============================================================================
# -*- coding:utf-8 -*-
def flip(x):
    if x==0:
        return 1
    else:
        return 0

class Flip:
    def flipChess(self, A, f):
        # write code here
        for i in xrange(len(f)):
            x = f[i][0]-1#中心点横坐标
            y = f[i][1]-1#中心点纵坐标
            if (y-1) >= 0:#上方存在
               A[x][y-1] = flip(A[x][y-1])
            if (y+1) < len(A):#下方存在
               A[x][y+1] = flip(A[x][y+1])
            if (x-1) >= 0:#左边存在
               A[x-1][y] = flip(A[x-1][y])
            if (x+1) < len(A[0]):#右边存在
               A[x+1][y] = flip(A[x+1][y])
        return A
###正确答案：翻转为 1-x
class Flip:
    def flipChess(self, A, f):
        # write code here
        for i in xrange(len(f)):
            x = f[i][0]-1#中心点横坐标
            y = f[i][1]-1#中心点纵坐标
            if (y-1) >= 0:#上方存在
               A[x][y-1] = 1-A[x][y-1]
            if (y+1) < len(A):#下方存在
               A[x][y+1] = 1-A[x][y+1]
            if (x-1) >= 0:#左边存在
               A[x-1][y] = 1-A[x-1][y]
            if (x+1) < len(A[0]):#右边存在
               A[x+1][y] = 1-A[x+1][y]
        return A
    
#==============================================================================
题目描述
有一楼梯共m级，刚开始时你在第一级，若每次只能跨上一级或者二级，要走上m级，
共有多少走法？注：规定从一级到一级有0种走法。
给定一个正整数int n，请返回一个数，
代表上楼的方式数。保证n小于等于100。为了防止溢出，请返回结果Mod 1000000007的值。
测试样例：
3
返回：2
###递推d[n] = d[n-1] + d[n-2]
#==============================================================================
# -*- coding:utf-8 -*-
class GoUpstairs:
    def countWays(self, n):
        # write code here
        if n <= 3:
            return n-1
        d = [1,2]#在1,2阶
        for i in xrange(3,n):
            d[0],d[1] = d[1],(d[0] + d[1])
        return d[1] % 1000000007

#==============================================================================
#题目描述
#小东和三个朋友一起在楼上抛小球，他们站在楼房的不同层，
#假设小东站的楼层距离地面N米，球从他手里自由落下，
#每次落地后反跳回上次下落高度的一半，并以此类推知道全部落到地面不跳，
#求4个小球一共经过了多少米？(数字都为整数)
#给定四个整数A,B,C,D，请返回所求结果。
#测试样例：
#100,90,80,70
#返回：1020
##解析
#除了最开始的下落高度只加一次外，以后的弹起下落都是两倍的距离。
#举个例子，下落高度是x米 ，则有下落后弹起的高度是x/2，再落下去，又走了一个x/2，
#依此类推，一直下去，也就是总距离为 x+2*x（1/2+1/4+1/8+……），
#而括号中的数列是一个等比数列，其极限求和的结果就是1。所以 总距离就是3*x。
#推理到这里，就发现写程序只需要一行关键代码就可以。
#==============================================================================
# -*- coding:utf-8 -*-

class Balls:
    def calcDistance(self, A, B, C, D):
        # write code here
        return 3*(A+B+C+D)
    
#==============================================================================
#题目描述
#小东所在公司要发年终奖，而小东恰好获得了最高福利，
#他要在公司年会上参与一个抽奖游戏，游戏在一个6*6的棋盘上进行，
#上面放着36个价值不等的礼物，每个小的棋盘上面放置着一个礼物，
#他需要从左上角开始游戏，每次只能向下或者向右移动一步，到达右下角停止，
#一路上的格子里的礼物小东都能拿到，请设计一个算法使小东拿到价值最高的礼物。
#给定一个6*6的矩阵board，其中每个元素为对应格子的礼物价值,左上角为[0,0],
#请返回能获得的最大价值，保证每个礼物价值大于100小于1000。
#
##解析
#平面上有N＊M个格子，每个格子中放着一定数量的苹果。你从左上角的格子开始，
#  每一步只能向下走或是向右走，每次走到一个格子上就把格子里的苹果收集起来， 这样下去，你最多能收集到多少个苹果。 
#  解这个问题与解其它的DP问题几乎没有什么两样。第一步找到问题的“状态”， 第二步找到“状态转移方程”，然后基本上问题就解决了。 
#  首先，我们要找到这 个问题中的“状态”是什么？我们必须注意到的一点是，
#  到达一个格子的方式最多只有两种：从左边来的(除了第一列)和从上边来的(除了第一行)。  因此为了求出到达当前格子后最多能收集到多少个苹果，
#  我们就要先去考察那些能到达当前这个格子的格子，到达它们最多能收集到多少个苹果。
#  (是不是有点绕，但这句话的本质其实是DP的关键：欲求问题的解，先要去求子问题的解) 
#  经过上面的分析，很容易可以得出问题的状态和状态转移方程。 状态S[i][j]表示我们走到(i,
#  j)这个格子时，最多能收集到多少个苹果。那么， 状态转移方程如下： 
#S[i][j]=A[i][j] + max(S[i-1][j], if i>0 ; S[i][j-1], if j>0)
#  其中i代表行，j代表列，下标均从0开始；A[i][j]代表格子(i, j)处的苹果数量。 
#  S[i][j]有两种计算方式：1.对于每一行，从左向右计算，然后从上到下逐行处理；2.
#  对于每一列，从上到下计算，然后从左向右逐列处理。
#  这样做的目的是为了在计算S[i][j]时，S[i-1][j]和S[i][j-1]都已经计算出来了。
#
#for i=0 to N-1:
#    for j=0 to M-1:
#        S[i][j]=A[i][j]+max(S[i][j-1],if j>0;S[i-1][j],if i>0)
#output S[n-1][m-1]
        
#==============================================================================
# -*- coding:utf-8 -*-

class Bonus:
    def getMost(self, board):
        # write code here
        #创建状态转移矩阵
        n = len(board)#行数
        m = len(board[0])#列数
        s = [[0]*m for i in xrange(n)]
        #s = [0]*m
        #s = [s]*n
        s[0][0] = board[0][0]
        #第一行,第一行的奖金只能来自第一行左边的格子
        for j in xrange(1,n):
            s[0][j] = s[0][j-1] + board[0][j]
        #第一列,第一列的奖金只能来自列的上面个格子
        for i in xrange(1,m):
            s[i][0] = s[i-1][0] + board[i][0]
        
        for i in xrange(1,n):
            for j in xrange(1,m):
                s[i][j] = board[i][j] + max(s[i][j-1],s[i-1][j])
        return s[n-1][m-1]

#==============================================================================
# 题目描述
请设计一个高效算法，再给定的字符串数组中，找到包含"Coder"的字符串(不区分大小写)，
并将其作为一个新的数组返回。结果字符串的顺序按照"Coder"出现的次数递减排列，
若两个串中"Coder"出现的次数相同，则保持他们在原数组中的位置关系。
给定一个字符串数组A和它的大小n，请返回结果数组。保证原数组大小小于等于300,
其中每个串的长度小于等于200。同时保证一定存在包含coder的字符串。
测试样例：
["i am a coder","Coder Coder","Code"],3
返回：["Coder Coder","i am a coder"]
#==============================================================================
# -*- coding:utf-8 -*-
import collections
class Coder:
    def findCoder(self, A, n):
        # write code here
        d = collections.OrderedDict()
        for i in xrange(n):
            target = A[i].lower()
            temp = target.find('coder')
            if temp != -1:
                d[i] = target.count('coder')
        d_sorted = sorted(d.items(),key = lambda x:x[1],reverse=True)
        res = []
        for i in xrange(len(d_sorted)):
            res.append(A[d_sorted[i][0]])
        return res
        
#正确答案

# -*- coding:utf-8 -*-
class Coder:
    def findCoder(self, A, n):
        t, p = [], 'Coder'.upper()
        for i in range(n):
            k = kmp(A[i].upper(), p)
            if k > 0:
                t.append((i, k))
            t.sort(key=lambda d: d[1], reverse=True)
        d = []
        for i in range(len(t)):
            d.append(A[t[i][0]])
        return d

def kmp(s, p):
    N = [0] * len(p)
    for i in range(2, len(p)):
        j = N[i - 1]
        while j > 0 and p[j] != p[i - 1]:
            j = N[j]
        if p[j] == p[i - 1]:
            N[i] = j + 1
    i, j, cnt = 0, 0, 0
    while i < len(s):
        if s[i] == p[j]:
            i, j = i + 1, j + 1
            if j == len(p):
                cnt, j = cnt + 1, 0
        elif j == 0:
            i += 1
        else:
            j = N[j]
    return cnt

#==============================================================================
# 题目描述
度度熊有一张网格纸，但是纸上有一些点过的点，每个点都在网格点上，
若把网格看成一个坐标轴平行于网格线的坐标系的话，每个点可以用一对整数x，y来表示。
度度熊必须沿着网格线画一个正方形，使所有点在正方形的内部或者边界。
然后把这个正方形剪下来。问剪掉正方形的最小面积是多少。 
输入描述:
第一行一个数n(2≤n≤1000)表示点数，接下来每行一对整数xi,yi(－1e9<=xi,yi<=1e9)
表示网格上的点
输出描述:
一行输出最小面积

输入例子:
2
0 0
0 3

输出例子:
9
#==============================================================================
import sys
while True:
    try:
        n = int(raw_input())#int(sys.stdin.readline().strip())
        a = []
        for i in xrange(n):
            x,y = [int(x) for x in raw_input().split()]#a.append(sys.stdin.readline().strip().split())
            if i == 0:
                dmm = [x,x,y,y]
            else:
                if x < dmm[0]:# xmin
                    dmm[0] = x
                if x > dmm[1]:# xmax
                    dmm[1] = x
                if y < dmm[2]:# ymin
                    dmm[2] = y
                if y > dmm[3]:# ymax
                    dmm[3] = y
        print max(dmm[1] - dmm[0],dmm[3] - dmm[2])**2
    except:
        break
                    