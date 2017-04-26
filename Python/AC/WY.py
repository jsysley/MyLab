# -*- coding: utf-8 -*-
"""
Created on Sat Mar 18 13:06:11 2017

@author: jsysley
"""
#==============================================================================
#intput:AAAABCCDAA
#out:4A1B2C1D2A
sentence = raw_input().strip(" ")
count = 1
def sta(sentence):
    out = []
    dd = {}
    if(len(sentence)==1):#只有一个字符串情况
        out.extend(repr(1)+sentence)
    else:#有多个字符串的情况
        for i in range(len(sentence)):#不包括最后一个字符串的情况
            letter = sentence[i]
            #print letter
            if letter not in dd.keys(): 
                if(len(dd) > 0):
                    temp = str(dd.values()[0])
                    temp = temp+dd.keys()[0]
                    out.append(temp)
                dd = {}
                dd[letter] = 1
            else:
                dd[letter]+=1
    #对尾巴的处理
    if(len(dd)>0):
        temp = str(dd.values()[0])
        temp = temp+dd.keys()[0]
        out.append(temp)
    return out

out = sta(sentence)
print "".join(out)
#==============================================================================


#==============================================================================
#intput:
#4 2
#87 98 79 61
#10 27 95 70
#20 64 73 29
#71 65 15 0
#out:193
N,D = map(lambda x:int(x),raw_input().split(" "))
Matrix = []
for i in range(N):
    temp = map(lambda x:int(x),raw_input().split(" "))
    Matrix.append(temp)
#输入一个行为length的矩阵，length为连续length个数的和，
def sum_l(mat,length):
    n_row = len(mat)
    max_n = 0
    #左上到右下
    for j in range(length-n_row+1):
        temp = 0
        for i in range(n_row):#对每行 取数字相加
            temp = temp + mat[i][j+i]
        max_n = max(max_n,temp)
    #右上到左下
    mat_reverse=[]
    for k in range(len(mat)):
        mat_reverse.append(list(reversed(mat[k])))
    for j in range(length-n_row+1):
        temp = 0
        for i in range(n_row):#对每行 取数字相加
            temp = temp + mat_reverse[i][j+i]
        max_n = max(max_n,temp)
    return max_n
#对行列
def row_col(Matrix,N):
    max_num = 0
    for i in range(int(N)):#对每行/列循环
        for j in range(int(N)-int(D)+1):#每行/列的处理
            temp_row = sum(Matrix[i][j:(j+int(D))])
            max_num = max(max_num,temp_row)
            #对列处理
            temp_col = 0
            for k in range(j,j+int(D)):
                temp_col = temp_col + Matrix[k][i]
            max_num = max(max_num,temp_col)
    return max_num
#####
max_num = row_col(Matrix,N)
for j in range(N-D+1):#可以取得次数，每次取D行
   mat = Matrix[j:(j+int(D))]
   mat_reverse=[]
   temp = sum_l(mat,N)
   max_num = max(max_num,temp)

print max_num
#==============================================================================
#input:
#4 4
#....
#..*@
#....
#.X..
#6 6
#...#..
#......
##*##..
#..##.#
#..X...
#.@#...
#output:
#3
#11

N,M = map(lambda x:int(x),raw_input().split(" "))
Mg = []
for i in range(N):
    temp = raw_input().split(" ")
    Mg.extend(temp)

class MiGong:
    def __init__(self, i, j, di):
        self.i = i
        self.j = j
        self.di = di

def Trans(Mg):
    res = []
    man = []
    block = []
    terminal = []
    result = []
    for i in range(len(Mg)):
        temp = []
        for j in range(len(Mg[0])):
            if Mg[i][j]=='.':
                temp.append(0)
            else:
                if Mg[i][j]=='X':
                    man = [i+1,j+1]
                    temp.append(0)
                elif Mg[i][j]=='*':
                    block = [i+1,j+1]
                    temp.append(0)
                elif Mg[i][j]=='@':
                    terminal = [i+1,j+1]
                    temp.append(0)
                else:
                    temp.append(1)
        res.append(temp)
        #补齐墙
    temp = ['1']*(len(Mg[0])+2)
    temp = map(lambda x:int(x),temp)
    for i in range(len(res)):
        res[i].insert(0,1)
        res[i].insert(len(res[i]),1)
    result.append(temp)
    result.extend(res)
    result.append(temp)
    
    return result,man,block,terminal

def MgPath(mg,start,end):#start:[1,1]->end[M,N]
    all_path = []#所有路径
    mg_copy = mg[:]
    path_min = []#最短路径
    minlen = M*100#最短路径长度\\`
    count = 0
    top=0
    sentence = 'a'+repr(top)+'=MiGong('+repr(start[0])+','+repr(start[1])+',-1)'
    exec(sentence)
    mg_copy[start[0]][start[1]] = -1#初始节点进栈
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
    return minlen

Maze,man,block,terminal = Trans(Mg)        
#从人到箱子
min_man = MgPath(Maze,man,block)-1
min_block = MgPath(Maze,block,terminal)-1
print (min_man+min_block-1)
#==============================================================================
#input:
#1
#2
#output:
#1.0000
#1.5000
import random
N = int(raw_input().strip(" "))
#速度
def RanNum(a):
#    random.seed(a)
    return(random.random())

def Rank_Index(gg,rev=False):
    num = len(gg)
    index = range(num)
    so = zip(index,gg)
    if rev==False:
        so.sort(key = lambda x:x[1])#对第二个关键字排序，从小到大排序
    if rev==True:
        so.sort(key = lambda x:x[1],reverse=True)
    res = map(lambda x :x[0],so)#排序后的index
    return res
    
def Horse(N):
    speed = map(lambda x:RanNum(x),range(1,N+1))
    before_rank = Rank_Index(speed,rev=True)
    before = {}
    for i in range(len(before_rank)):
        before[before_rank[i]] = i#第before_rank[i]匹马排第i+1名

    deal = before.items()
    deal.reverse()
    top = N-1
    move = 0#被超过的马
    for i in range(len(deal)):
        if deal[i][0]<=top:#仅考虑前面的马
            if deal[i][0]>deal[i][1]:#前进了
                move = move + deal[i][0]-deal[i][1]
                top = deal[i][1]#仅考虑排名top及以后的马
    return (N-move)

result = [] 
for i in range(100000):
    result.append(Horse(N))

left = float(sum(result))/len(result)
print float("%.4f"%left)
#==============================================================================
try:
    while True:
        line = sys.stdin.readline().strip()
        if not line:
            break
except:
    break
#==============================================================================
#input:
#3
#3 1
#1 2 3 4 5 6
#3 2
#1 2 3 4 5 6
#2 2
#1 1 1 1
#output:
#1 4 2 5 3 6
#1 5 4 3 2 6
#1 1 1 1
 
T = int(raw_input().strip(" "))
Mat = []
for i in range(T):
    N,K = map(lambda x:int(x),raw_input().split(" "))
    mat = map(lambda x:int(x),raw_input().split(" "))
    temp = [N,K,mat]
    Mat.append(temp)
#洗一次
def Wash(mat,N):
    if len(mat)!=2*N:
        print("length of mat error")
        return 0
    low = mat[:N]
    high = mat[N:]
    low.reverse()
    high.reverse()
    res = []
    for temp in zip(high,low):
        res.append(temp[0])
        res.append(temp[1])
    res.reverse()
    return res

for j in range(T): 
    N = Mat[j][0]
    K = Mat[j][1]
    mat = Mat[j][2]
    result = []
    for i in range(K):
        if i==0:
            result = Wash(mat,N)
        else:
            result = Wash(result,N)
    result = map(lambda x:str(x),result)
    print " ".join(result)
#==============================================================================

#==============================================================================
#input:
#3
#7 4 7
#2 50
#output:
#49
N = int(raw_input().strip(" "))
A = map(lambda x: int(x),raw_input().split(" "))
K,D = map(lambda x:int(x),raw_input().split(" "))
if isinstance(A,int):
    A = [A]
#对一个local求其周围k个最大
def Product(A,local,K,D):
    if K==1:
        for i in range(len(A)):
            if i==0:
                mul_res = A[i]
            else: 
                if A[i]>mul_res:
                    mul_res = A[i]
    else:
        mat = []
        if local==0:
            mat.append(A[local])
        elif local==1:
            mat.append(A[local-1])
        elif (local-D)<0:
            mat.extend(A[:local])
        else:
            mat.extend(A[(local-D):local])
            
        if local!=0:
            mat.append(A[local])
            
        if local==(len(A)-1):
            pass
        elif local==(len(A)-2):
            mat.append(A[local+1])
        elif (local+D)>len(A):
            mat.extend(A[(local+1):])
        else:
            mat.extend(A[(local+1):(local+D)])
        local_local = mat.index(A[local])
        if local_local!=0:
            for i in range(len(mat)):
                if i!=local_local:
                    if i==0:
                        mul_res = A[i]*A[local_local]
                    else:
                        temp = A[i]*A[local_local]
                        if temp>mul_res:
                            mul_res = temp
        else:
            for i in range(1,len(mat)):
                if i!=local_local:
                    if i==1:
                        mul_res = A[i]*A[local_local]
                    else:
                        temp = A[i]*A[local_local]
                        if temp>mul_res:
                            mul_res = temp
    return mul_res
    
for i in range(len(A)):
    if i==0:
        res = Product(A,i,K,D)
    else:
        temp = Product(A,i,K,D)
        if temp>res:
            res = temp
print(res)
    
#==============================================================================

import sys 
for line in sys.stdin:
    a = line.split()
    print int(a[0]) + int(a[1])

    import sys

if __name__ == "__main__":
    # 读取第一行的n
    n = int(sys.stdin.readline().strip())
    ans = 0
    for i in range(n):
        # 读取每一行
        line = sys.stdin.readline().strip()
        # 把每一行的数字分隔后转化成int列表
        values = map(int, line.split())
        for v in values:
            ans += v
    print ans
###############################################################################

N,K = map(lambda x:int(x),raw_input().split(" "))
A = map(lambda x: int(x),raw_input().split(" "))

def Magic(A):
    leng = len(A)
    res = []
    for i in range(leng):
        if i ==(leng-1):#最后一个
            temp = A[leng-1]+A[0]
        else:
            temp = A[i]+A[i+1]
        res.append(temp)
    return res

for i in range(K):
    if i==0:
        result = Magic(A)
    else:
        result = Magic(result)

result = map(lambda x:str(x),result)
print " ".join(result)

#############################
K = raw_input().strip(" ")
N = int(raw_input().strip(" "))
b=True
def Possible(K):
    poss = []
    if isinstance(K,str):
        t_K = K[:]
        try:
            x_index = t_K.index('X')
        except:
            return 'no'
        if x_index==0:
            for i in range(1,10):
                temp = str(i)+t_K[:x_index]
                poss.append(temp)
        elif x_index==(len(t_K)-1):
            for i in range(10):
                temp = t_K[:x_index]+str(i)
                poss.append(temp)
        else:
            for i in range(10):
                temp = t_K[:x_index]+str(i)+t_K[x_index+1:]
                poss.append(temp)
        
    if isinstance(K,list):
        for i in range(len(K)):
            t_K = K[i]
            try:
                x_index = t_K.index('X')
            except:
                return 'no'
            if x_index==0:
                for i in range(1,10):
                    temp = str(i)+t_K[:x_index]
                    poss.append(temp)
            elif x_index==(len(t_K)-1):
                for i in range(10):
                    temp = t_K[:x_index]+str(i)
                    poss.append(temp)
            else:
                 for i in range(10):
                     temp = t_K[:x_index]+str(i)+t_K[x_index+1:]
                     poss.append(temp)
    return poss

def Get_All(K):
    temp = Possible(K)
    b=True
    if temp=='no':
        return K
    elif len(temp)==1:
        t_before = temp
        while b:
            t_after = Possible(t_before)
            if t_after=='no':
                return t_before
            else:
                t_before=t_after
    else:
        return Get_All(temp)
    
all_num = Get_All(K)
all_num = map(lambda x: int(x),all_num)
count = 0
for i in range(len(all_num)):
    if all_num[i]%N==0:
        count+=1
print count
 
#######################
li = map(lambda x:str(x),raw_input().strip(""))
#左G右B
def dealHuiwen(li,count=0):
    leng = len(li)
    li_str = "".join(li)
    if leng>1:
        last=leng-1
        if li[0]=='G' and li[last]=='B':
            del li[0]
            del li[last-1]
            count = dealHuiwen(li,count)#简化后,再进行递归处理
        else:
            if li[0]!='G' and li[last] !="B":
                step_left = li_str.index('G')
                count+=step_left
                li[0]='G'
                li[step_left]='B'
                
                step_right = (last-li_str.rindex('B'))
                count+=step_right
                li[last]='B'
                li[step_right]='G'
                
                del li[0]
                del li[last-1]   
            elif li[last] !="B":
                step_right = (last-li_str.rindex('B'))
                count+=step_right
                li[last]='B'
                li[step_right]='G'
                del li[last]
            elif li[0]!='G':
                step_left = li_str.index('G')
                count+=step_left
                li[0]='G'
                li[step_left]='B'
                del li[0]
            count=dealHuiwen(li,count)    
    return count

res = dealHuiwen(li,count=0)
li.reverse()
res2 = dealHuiwen(li,count=0)
print min(res,res2)      
####################################
class MiGong:
    def __init__(self, i, di, di_len,j):
        self.i = i#当前位置
        self.di_len = di_len#约数长度
        self.di = di#所有约数
        self.j = j#所在的约数的位置

def Find(N,M):
    top_find = int(N**0.5)+1
    res=[]
    for i in range(2,top_find):
        if ((N % i)== 0) & ((N+i)<=M):
            res.append(int(i))
        if (i * i != N) & (N % (N / i) == 0) & ((N+int(N / i))<=M):
            res.append(int(N / i))
    res = list(set(res))
    res_len = len(res)
    return res,res_len
            
def MgPath(start,end):#start:N -> M
    all_path = []#所有路径
    path_min = []#最短路径
    minlen = float('inf')#最短路径长度\\
    count = 0
    top = 0
    mg = [0]*(end+1)
    ####头结点
    di,di_len = Find(start,end)
    sentence = 'a'+repr(top)+'=MiGong('+repr(start)+','+repr(di)+','+repr(di_len)+',-1)'
    exec(sentence)
    mg[N] = -1
    while(top>-1):#栈不空时循环
        exec('ii='+'a'+repr(top)+'.i')#所在位置
        exec('dii='+'a'+repr(top)+'.di')#当前位置所有约数
        exec('dii_len='+'a'+repr(top)+'.di_len')
        exec('jj='+'a'+repr(top)+'.j')#已经遍历的约数
        if ii>end:
            return 0
        if ii==end:#找到了出口
            temp_path = []
            for k in range(top+1):
                exec('temp_i = '+'a'+repr(k)+'.i')
                temp_path.append(temp_i)
            all_path.append(temp_path)
            count+=1
            if (top+1)<minlen:#比较输出最短路径
                path_min = temp_path[:]
                minlen = len(path_min)
            
            #让该位置变为其他路径的可走结点
            exec('temp_ii ='+ 'a'+repr(top)+'.i')
            mg[temp_ii] = 0
            top-=1
            exec('ii='+'a'+repr(top)+'.i')
            exec('jj='+'a'+repr(top)+'.j')
            exec('dii_len='+'a'+repr(top)+'.di_len')
            exec('dii='+'a'+repr(top)+'.di')
            
        find = 0
        while (jj<(dii_len-1)) & (find==0):#找下一个可走结点
            jj+=1
            exec('temp_ii='+'a'+repr(top)+'.i+'+repr(dii[jj]))
            if mg[temp_ii]==0:
                find=1
                
        if find == 1:#找到了下一个可走结点
            exec('a'+repr(top)+'.j'+'=jj')#修改原栈顶元素的di值
            top+=1#下一个可走结点进栈
            exec('a'+repr(top)+'=MiGong('+repr(start)+','+repr(di)+','+repr(di_len)+',-1)')#初始化实例
            exec('a'+repr(top)+'.i'+'=temp_ii')
            di,di_len = Find(temp_ii,end)
            exec('a'+repr(top)+'.di'+'=di')
            exec('a'+repr(top)+'.di_len'+'=di_len')
            exec('a'+repr(top)+'.jj'+'=-1')
            mg[temp_ii] = -1
        else:
            exec('temp_ii ='+ 'a'+repr(top)+'.i')
            if temp_ii>end:
                return 0
            mg[temp_ii] = 0
            top-=1
    #print path_min
    return minlen

import sys
num = sys.stdin.readline().strip()
while num:
    N,M = map(lambda x:int(x),num.split(" "))
    print MgPath(N,M)-1

#####################################################
from copy import deepcopy
def Initialize(can_do,N,end):#初始化数据格式
    all_job = {}
    one_job = {}
    for i in range(N):#统计
        for j in range(len(can_do[i])):
            one_job[can_do[i][j]] = one_job.get(can_do[i][j],"") + repr(i)
    for i in range(end):#整理数据格式
        temp = {}
        temp['who'] = '0'
        temp['di'] = one_job[repr(i)]
        temp['di_len'] = len(one_job[repr(i)])
        temp['j'] = -1
        all_job[repr(i)] = temp
    return all_job
    
def Arrange_job(raw_job,N,end):
    all_path = []#所有安排
    count = 0#计数
    top = 0#指针
    all_job = deepcopy(raw_job)
    mg = [0]*N
    ##########
    while(top>-1):#栈不空时循环
        dii = all_job[repr(top)]['di']#所有可用工程师
        dii_len = all_job[repr(top)]['di_len']#可用工程师数量
        jj = all_job[repr(top)]['j']#已用
        if top==(end-1):#安排完毕
            temp_path = []
            for k in range(top):
                temp_i = all_job[repr(k)]['who']
                temp_path.append(temp_i)
            all_path.append(temp_path)
            count+=1#计数
            #让该工程师变为其他工作可用
            temp_i = all_job[repr(top)]['who']
            mg[int(temp_i)] = 0
            top-=1
            dii_len = all_job[repr(top)]['di_len']
            jj = all_job[repr(top)]['j']#已遍历工程师
            dii = all_job[repr(top)]['di']
        find = 0
        while (jj<(dii_len-1)) & (find==0):#找下一个可用工程师
            jj+=1
            temp_i = all_job[repr(top)]['di'][jj]
            if mg[int(temp_i)]==0:
                find=1 
        if find == 1:#找到了下一个可走结点
            all_job[repr(top)]['j'] = jj#修改原栈顶元素的di值
            top+=1#下一个可走结点进栈
            all_job[repr(top)]['who'] = temp_i
            mg[int(temp_i)] = -1
            all_job[repr(top)]['j'] = -1
        else:
            temp_i = all_job[repr(top)]['who']
            mg[int(temp_i)] = 0
            top-=1
    #print all_path
    return count
    
import sys
N = raw_input()#sys.stdin.readline().strip()
while N:
    N = int(N)
    can_do = []
    for i in range(N):
        one_do = raw_input()#sys.stdin.readline().strip()
        can_do.append(one_do)
    raw_job = Initialize(can_do,N,end)
    print Arrange_job(raw_job,N,end)
    N = sys.stdin.readline().strip()
    
##############
def Arrange(all_job_num,N,all_sum,dp):
    for i in range(N):
        k = all_sum/2
        while True:
            if k<all_job_num[i]:
                break
            dp[k] = max(dp.get(k,0),dp.get(k-all_job_num[i],0)+all_job_num[i])
            k-=1
    return dp

import sys
N = sys.stdin.readline().strip()
while N:
    N = int(N)
    all_job = sys.stdin.readline().strip()
    all_job_int = map(lambda x:int(x),all_job.split(" "))
    dp = {}
    arr = {}
    all_job_num = map(lambda x:x/1024,all_job_int)#转换成较小的数
    all_sum = sum(all_job_num)#求和
    res = Arrange(all_job_num,N,all_sum,dp)
    print (all_sum - dp[all_sum/2])*1024
    N = sys.stdin.readline().strip()

###################################3
import sys
N = sys.stdin.readline().strip()
while N:
    
    N = sys.stdin.readline().strip()








