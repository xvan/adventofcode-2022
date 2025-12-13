
import numpy as np
from hsnf import smith_normal_form
import sympy as sp


def gaussian_elimination(A, b, demo_mode=False):
    n = len(b)
    m = A.shape[1]  # number of columns
    M = A.copy()
    b = b.copy()
    
    row_pivots = []  # Track which row was used as pivot
    col_pivots = []  # Track which column was used as pivot
    used_rows = set()  # Track which rows have been used as pivots
    
    for col in range(m):  # Process each column
        # Find a non-zero pivot in current column from unused rows
        pivot_row = None
        for row in range(n):
            if row not in used_rows and np.abs(M[row, col]) > 1e-10:
                pivot_row = row
                break
        
        if pivot_row is None:
            if demo_mode:
                print(f"Skipping column {col}, no pivot found")
            continue
        
        row_pivots.append(pivot_row)
        col_pivots.append(col)
        used_rows.add(pivot_row)
        
        if demo_mode:
            print(f"Using row {pivot_row} as pivot for column {col}")
        
        # Elimination - eliminate this column in ALL other rows (not just below)
        for j in range(n):
            if j != pivot_row and np.abs(M[j, col]) > 1e-10:
                factor = M[j, col] / M[pivot_row, col]
                M[j] = M[j] - factor * M[pivot_row]
                b[j] = b[j] - factor * b[pivot_row]
                if demo_mode:
                    print(f"Eliminated row {j} using row {pivot_row} with factor {factor}")
                    print(M)
    
    return M, b, sorted(row_pivots), sorted(col_pivots)




# Integer matrix to be decomposed

M=np.array(    [[0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1],
                [0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1],
                [0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0],
                [0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
                [0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0],
                [1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0],
                [1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1],
                [0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0],
                [0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0]], dtype=int) 


k=np.array([44,20,17,44,10,53,46,21,51,22])



import itertools

def bruteforce(M, k):
    r,c = M.shape
    best_objective = None
    best_solution = None
    
    all_solutions = []
    
    spM = sp.Matrix(M)
    spk = sp.Matrix(k)
    
    for col_base in itertools.combinations(range(c), r):
       spB = spM[:, col_base]
       if spB.det() == 0:
           continue 
       solution = spB.inv() @ spk
       all_solutions.append((col_base, solution))
                     
    return all_solutions


###################################

M_reduced, k_reduced, row_pivots, col_pivots = gaussian_elimination(M, k, demo_mode=False)

print("Reduced Matrix M:")
print(M_reduced)
print("Reduced k:")
print(k_reduced)
print("Row pivots:", row_pivots)
print("Column pivots:", col_pivots)

###################################

Mr = M[row_pivots,:]
kr = k[row_pivots]

#sp.pprint(spMr[:,col_pivots].inv()@spkr)
solutions = bruteforce(Mr, kr)

print("Total brute-force solutions found:", len(solutions))

integer_solutions = [(col_base, solution) for col_base, solution in solutions if all(x.is_integer for x in solution)]
print("Integer brute-force solutions found:", len(integer_solutions))

print("less negative integer solutions")
sorted_solutions = sorted([(sum(x for x in solution if x<0), col_base, solution) for col_base, solution in integer_solutions if any(x<0 for x in solution)], key=lambda t: t[0], reverse=True)
        

print("Best integer solutions:")
for neg_sum, col_base, solution in sorted_solutions[:5]:
    print("Negative sum:", neg_sum)
    print("Column base:", col_base)
    sp.pprint(solution)




ncnt, test_columns, test_reduced_solution = sorted_solutions[0]
x_particular = np.zeros(M.shape[1],dtype=int)
for i, col in enumerate(test_columns):
    x_particular[col] = test_reduced_solution[i]

print("Test solution check M @ x:")
residual = M @ x_particular - k
sp.pprint(residual)
        

# Smith normal form D, and unimodular matrices L and R such that L @ M @ R == D
D, L, R = smith_normal_form(M)

print("Smith Normal Form D:")
print(D)
# Output:
# Smith Normal Form D:
# [[   1    0    0    0]
#  [   0    3    0    0]
#  [   0    0 2079    0]]

# Verify the decomposition
assert np.allclose(L @ M @ R, D)


rank = sum(D[i, i] != 0 for i in range(min(D.shape)))

null_cols = range(rank, D.shape[1])
integer_null_basis = R[:, null_cols]

print("Integer null space basis:")
print(integer_null_basis)

general_solution = lambda zvals: x_particular + integer_null_basis @ np.array(zvals, dtype=int)


print( general_solution([-1, -1, 0]) > 0)

Z_max = np.max(k)

nns = []
for z0 in range(-Z_max, Z_max+1):
    for z1 in range(-Z_max, Z_max+1):
        for z2 in range(-Z_max, Z_max+1):            
            x = general_solution([z0, z1, z2])
            if np.all(x >= 0):
                nns.append(x)
                sp.pprint(x)

print("Total non-negative integer solutions found:", len(nns))
a_solution = np.argmin(np.sum(x) for x in nns)
print("Minimum objective value:", np.sum(nns[a_solution]))
print("Solution vector:", nns[a_solution])

assert(np.all(M @ nns[a_solution] == k))



# z = sp.symbols(f'z0:{M.shape[1]-rank}', integer=True)
# zvec = sp.Matrix(len(z), 1, z)

# x_general = sp.Matrix(x_particular) + integer_null_basis * zvec

# print("General integer solution:")
# sp.pprint(x_general)


# # x = x_general.subs({z[0]: 1, z[1]: 1, z[2]: 1})
# # sp.pprint(x)

# girder_solutions = []
# for z0 in range(-2, 3):
#     for z1 in range(-2, 3):
#         for z2 in range(-2, 3):            
#             x = x_general.subs({z[0]: z0, z[1]: z1, z[2]: z2})
#             girder_solutions.append(x)

# sp.pprint(girder_solutions)
# ############################################
# A = np.array([
#     [6, 10, 16],
#     [4, 14, 22]
# ],dtype=int)

# D, U, V = smith_normal_form(A)

# print("U =")
# print(U)
# print("D =")
# print(D)
# print("V =")
# print(V)


# # Verify the decomposition
# assert np.allclose(U @ A @ V, D)


# rank = sum(D[i, i] != 0 for i in range(min(D.shape)))

# null_cols = range(rank, D.shape[1])
# integer_null_basis = V[:, null_cols]

# print("Integer null space basis:")
# print(integer_null_basis)

# b = sp.Matrix([2, 4])

# c = U * b

# # Check divisibility
# for i in range(rank):
#     if c[i] % D[i, i] != 0:
#         raise ValueError("No integer solutions")

# # Particular solution
# y = sp.zeros(A.shape[1], 1)
# for i in range(rank):
#     y[i] = c[i] // D[i, i]

# x_particular = V * y
# print("One integer particular solution:")
# print(x_particular)

# # General solution
# z = sp.symbols(f'z0:{A.shape[1]-rank}', integer=True)

# print("this is z")
# sp.pprint(z)
# print("this is rank")
# print(rank)
# print("this is y before")
# sp.pprint(y)
# for k, zi in enumerate(z):
#     y[rank + k] = zi
# x_general = V * y

# print("General integer solution:")
# print(x_general)