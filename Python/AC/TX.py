# -*- coding: utf-8 -*-
"""
Created on Wed Mar 29 15:18:44 2017

@author: jsysley
"""
#Example
import sys
s = sys.stdin.readline().strip()
while(s):
    s_lst = list(s)
    lowerlst = []
    upperlst = []
    for i in range(len(s_lst)):
        if s_lst[i].islower():
            lowerlst.append(s_lst[i])
        else:
            upperlst.append(s_lst[i])
    print "".join(lowerlst)+"".join(upperlst)
    s = sys.stdin.readline().strip()
#==============================================================================
##先求s的反串reverse，然后求他们的最长的公共子序列，要删除的字符个数就能知道
##时间复杂度O(N^2)
def GetRemoveNumber(s):
    s1 = s[:]
    s2 = s[::-1]
    length = len(s1)
    a = [0]*(length+1)
    b = [a]*(length+1)
    for i in range(len(s1)):
        for j in range(len(s2)):
            if(s1[i]==s2[j]):
                b[i+1][j+1]=b[i][j]+1
            else:
                b[i+1][j+1]=max(b[i][j+1],b[i+1][j])
    return (length-b[length][length])

import sys
s = sys.stdin.readline().strip()
while(s):
    print GetRemoveNumber(s)
    s = sys.stdin.readline().strip()
#==============================================================================

#==============================================================================
from collections import defaultdict
from operator import itemgetter
def Move(s1):
    s = s1[:]
    upper_dict = defaultdict(int)
    upper_al = []
    i=0
    while True:
        upper = min(s)
        if not upper.isupper():#不是大写
            break
        else:
            upper_dict[i] = s.index(upper)
            upper_al.append(upper)
            s = s.replace(upper,"_",1)
            i+=1
    sorted_upper = sorted(upper_dict.items(), key=itemgetter(1), reverse=False)
    s_l = list(s.replace('_',''))
    index_al = map(lambda x:x[0],sorted_upper)
    for j in range(len(upper_al)):
        s_l.append(upper_al[index_al[j]])
    return s_l
        
import sys
s = sys.stdin.readline().strip()
while(s):
    print "".join(Move(s))
    s = sys.stdin.readline().strip()
#==============================================================================

#==============================================================================
#def Diff_Max_Min(array_all,N):
#    array = array_all[:]
#    min_diff = abs(array[0]-array[1])
#    max_diff = abs(array[0]-array[1])
#    count = {}
#    for i in range(N):
#        for j in range(i+1,N):
#                diff = abs(array_all[i]-array_all[j])
#                if diff<min_diff:
#                    count['min'] = 1
#                    min_diff = diff
#                elif diff==min_diff:
#                    count['min'] = count.get('min',0)+1
#                if diff>max_diff:
#                    count['max'] = 1
#                    max_diff = diff
#                elif diff==max_diff:
#                    count['max'] =count.get('max',0)+1
#    return count

#排序
def Diff(array_sort):
    x = array_sort[:-1]
    y = array_sort[1:]
    diff = [y[i]-x[i] for i in range(len(x))]
    return diff
    
def Diff_Max_Min(array_all,N):
    array_sort = array_all[:]
    array_sort.sort()
    count = {}
    #dif = map(lambda x,y:y-x,data[:-1],data[1:])
    #所有数相同
    if array_sort[0]==array_sort[N-1]:
        count['min'] = N*(N-1)/2
        count['max'] = N*(N-1)/2
        return count
    else:
        #处理最大数
        last = array_sort.count(array_sort[N-1])
        first = array_sort.count(array_sort[0])
        count['max'] = last*first
        #处理最小数
        diff = Diff(array_sort)
        diff_min = min(diff)
        if diff_min != 0:
            count['min'] = diff.count(diff_min)
        else:
            label = 0
            for i in range(len(diff)):
                if diff[i]==0 and label==0:
                    label = 1
                elif diff[i]==0 and label!=0:
                    label+=1
                elif diff[i]!=0 and label!=0:
                    count['min'] = count.get('min',0)+label*(label+1)/2
                    label=0
            if label!=0:
            	count['min'] = count.get('min',0)+label*(label+1)/2
        return count
        
import sys
N = sys.stdin.readline().strip()
while(N):
    array = sys.stdin.readline().strip()
    array_all = map(lambda x :int(x),array.split(" "))
    res = Diff_Max_Min(array_all,int(N))
    print res['min'],res['max']
    N = sys.stdin.readline().strip()

#==============================================================================
import sys
N = sys.stdin.readline().strip()
while(N):
    
    N = sys.stdin.readline().strip()
    
    
    
    
    
    
    
    