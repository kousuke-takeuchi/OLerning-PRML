# -*- coding: utf-8 -*-
import numpy as np
import time

def add(A, B):
    a_nrow, a_ncol = A.shape
    b_nrow, b_ncol = B.shape
    if (a_nrow != b_nrow or a_ncol != b_ncol):
        raise Exception("can't add " + str(A.shape)  \
                        + " and" + str(B.shape) + "\n")
    C = np.zeros((a_nrow, a_ncol))
    for i in range(a_nrow):
        for j in range(a_ncol):
            C[i, j] = A[i, j] + B[i, j]
    return C

def dot(A, B):
    a_nrow, a_ncol = A.shape
    b_nrow, b_ncol = B.shape
    if (a_ncol != b_nrow):
        raise Exception("can't product " + str(A.shape)  \
                        + " and" + str(B.shape) + "\n")
        exit(1)
    C = np.zeros((a_nrow, b_ncol))
    for i in range(a_nrow):
        for j in range(b_ncol):
            for k in range(a_ncol):
                C[i, j] = C[i, j] + A[i, k] * B[i, j]
    return C

def t(A):
    a_nrow, a_ncol = A.shape
    C = np.zeros((a_nrow, a_ncol))
    for i in range(a_nrow):
        for j in range(a_ncol):
            C[i, j] = A[j, i]
    return C


def main():
    A = np.asmatrix(np.array([[1,2,3], [4,5,6], [7,8,9]]))
    B = np.asmatrix(np.array([[1,2,3], [4,5,6], [7,8,9]]))
    
    # self func
    start = time.clock()
    t(A)
    end = time.clock()
    self_time = end - start
    
    # numpy
    start = time.clock()
    A.T
    end = time.clock()
    np_time = end - start

    print "\nself_time: " + str(self_time) + ", np_time: " + str(np_time) + "\n"

if __name__ == '__main__':
    main()


"""
    実行結果
    add: self_time: 9.2e-05, np_time: 2.8e-05
    dot: self_time: 0.000235, np_time: 2.2e-05
    transpose: self_time: 6e-05, np_time: 1e-05
    
    逆行列に関しては、クラメールの公式やLU分解などを用いて関数を作成できるが今回は省略
"""