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
