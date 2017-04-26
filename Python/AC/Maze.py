# -*- coding: utf-8 -*-
"""
Created on Mon Feb 13 13:46:00 2017

@author: jsysley
"""

M = 8#行数
N = 8#列数
MaxSize = 100#栈最多元素个数\
mg =[      #一个迷宫，其四周要加上均为1的外框
 [1,1,1,1,1,1,1,1,1,1],
 [1,0,0,1,0,0,0,1,0,1],
 [1,0,0,1,0,0,0,1,0,1],
 [1,0,0,0,0,1,1,0,0,1],
 [1,0,1,1,1,0,0,0,0,1],
 [1,0,0,0,1,0,0,0,0,1],
 [1,0,1,0,0,0,1,0,0,1],
 [1,0,1,1,1,0,1,1,0,1],
 [1,1,0,0,0,0,0,0,0,1],
 [1,1,1,1,1,1,1,1,1,1]]

class MiGong:
    def __init__(self, i, j, di):
        self.i = i
        self.j = j
        self.di = di

def MgPath(mg,start,end):#start:[1,1]->end[M,N]
    all_path = []#所有路径
    mg_copy = mg[:]
    path_min = []#最短路径
    minlen = M*100#最短路径长度
    count = 0
    top=0#入栈
    sentence = 'a'+repr(top)+'=MiGong('+repr(start[0])+','+repr(start[1])+',-1)'
    exec(sentence)
    mg_copy[1][1] = -1#初始节点进栈
    while(top>-1):#栈不空时循环
        exec('ii='+'a'+repr(top)+'.i')
        exec('jj='+'a'+repr(top)+'.j')
        exec('dii='+'a'+repr(top)+'.di')
        if (ii==end[0])&(jj==end[1]):#找到了出口
            temp_path = []
            for k in range(top+1):
                exec('temp_i = '+'a'+repr(k)+'.i')
                exec('temp_j = '+'a'+repr(k)+'.j')
                temp = [temp_i,temp_j]
                temp_path.append(temp)
            all_path.append(temp_path)
            count+=1
            if (top+1)<minlen:#比较输出最短路径
                path_min = temp_path[:]
                minlen = top+1
                
            exec('temp_i = '+'a'+repr(top)+'.i')
            exec('temp_j = '+'a'+repr(top)+'.j')
            mg_copy[temp_i][temp_j] = 0#让该位置变为其他路径的可走结点
            top-=1
            exec('ii='+'a'+repr(top)+'.i')
            exec('jj='+'a'+repr(top)+'.j')
            exec('dii='+'a'+repr(top)+'.di')
            
        find = 0
        while (dii<4) & (find==0):#找下一个可走结点
            dii+=1
            if dii==0:
                exec('ii='+'a'+repr(top)+'.i-1')
                exec('jj='+'a'+repr(top)+'.j')
            elif dii==1:
                exec('ii='+'a'+repr(top)+'.i')
                exec('jj='+'a'+repr(top)+'.j+1')
            elif dii==2:
                exec('ii='+'a'+repr(top)+'.i+1')
                exec('jj='+'a'+repr(top)+'.j')
            elif dii==3:
                exec('ii='+'a'+repr(top)+'.i')
                exec('jj='+'a'+repr(top)+'.j-1')
            
            if mg_copy[ii][jj]==0:
                find = 1
                
        if find == 1:#找到了下一个可走结点
            exec('a'+repr(top)+'.di'+'=dii')#修改原栈顶元素的di值
            top+=1#下一个可走结点进栈
            exec('a'+repr(top)+'=MiGong('+repr(start[0])+','+repr(start[1])+',-1)')#初始化实例
            exec('a'+repr(top)+'.i'+'=ii')
            exec('a'+repr(top)+'.j'+'=jj')
            exec('a'+repr(top)+'.di'+'=-1')
            mg_copy[ii][jj]=-1#避免重复走到该结点
        else:
            exec('temp_i = '+'a'+repr(top)+'.i')
            exec('temp_j = '+'a'+repr(top)+'.j')
            mg_copy[temp_i][temp_j] = 0#让该位置变为其他路径的可走结点
            top-=1
    print("all path %s" %all_path)
    print("path counts %d" %count)
    print("min path %s" %path_min)
    print("min path length %d" %minlen)
               
start=[1,1]    
end = [8,8]
MgPath(mg,start,end)
MgPath2(mg,start,end)
########################
def MgPath2(mg,start,end):#start:[1,1]->end[M,N]
    all_path = []#所有路径
    mg_copy = mg[:]
    path_min = []#最短路径
    minlen = M*100#最短路径长度
    count = 0
    top=0#入栈
    a = {}
    temp = {}
    temp['i'] = start[0]
    temp['j'] = start[1]
    temp['di'] = -1
    a[top] = temp
    mg_copy[1][1] = -1#初始节点进栈
    while(top>-1):#栈不空时循环
        ii = a[top]['i']
        jj= a[top]['j']
        dii = a[top]['di']
        if (ii==end[0])&(jj==end[1]):#找到了出口
            temp_path = []
            for k in range(top+1):
                temp = [a[k]['i'],a[k]['j']]
                temp_path.append(temp)
            all_path.append(temp_path)
            count+=1
            if (top+1)<minlen:#比较输出最短路径
                path_min = temp_path[:]
                minlen = top+1
            mg_copy[a[top]['i']][a[top]['j']] = 0#让该位置变为其他路径的可走结点
            top-=1
            ii = a[top]['i']
            jj = a[top]['j']
            dii = a[top]['di']
        find = 0
        while (dii<4) & (find==0):#找下一个可走结点
            dii+=1
            if dii==0:
                ii = a[top]['i']-1
                jj = a[top]['j']
            elif dii==1:
                ii = a[top]['i']
                jj = a[top]['j']+1
            elif dii==2:
                ii = a[top]['i']+1
                jj = a[top]['j']
            elif dii==3:
                ii = a[top]['i']
                jj = a[top]['j']-1
            
            if mg_copy[ii][jj]==0:
                find = 1
                
        if find == 1:#找到了下一个可走结点
            a[top]['di'] = dii#修改原栈顶元素的di值
            top+=1#下一个可走结点进栈
            temp = {}
            temp['i'] = ii
            temp['j'] = jj
            temp['di'] = -1
            a[top] = temp
            mg_copy[ii][jj]=-1#避免重复走到该结点
        else:
            mg_copy[a[top]['i']][a[top]['j']] = 0#让该位置变为其他路径的可走结点
            top-=1
    print("all path %s" %all_path)
    print("path counts %d" %count)
    print("min path %s" %path_min)
    print("min path length %d" %minlen)