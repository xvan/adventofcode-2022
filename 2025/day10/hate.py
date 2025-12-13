from exceptiongroup import catch
import numpy as np
import itertools
import pprint


def parse_line(line):
    # Find and extract the curly braces content (always at the end)
    curly_start = line.rfind('{')
    curly_end = line.rfind('}')
    curly_content = [int(x.strip()) for x in line[curly_start+1:curly_end].split(',')]
    
    # Remove curly braces part from line
    line_without_curly = line[:curly_start].strip()
    
    # Skip the first square brackets
    first_bracket_end = line_without_curly.find(']')
    remaining = line_without_curly[first_bracket_end+1:].strip()
    
    # Parse all parentheses groups
    parentheses_groups = []
    i = 0
    while i < len(remaining):
        if remaining[i] == '(':
            # Find matching closing parenthesis
            end = remaining.find(')', i)
            content = remaining[i+1:end]
            if content.strip():  # Not empty
                group = [int(x.strip()) for x in content.split(',')]
                parentheses_groups.append(group)
            i = end + 1
        else:
            i += 1
    
    return parentheses_groups, curly_content


def build_matrix_A(paren_lists, curly_list):
    total_rows = len(curly_list)
    total_cols = len(paren_lists)
    
    A = np.zeros((total_rows, total_cols))
    

    for col_idx, paren_group in enumerate(paren_lists):
        for row_idx in paren_group:
            A[row_idx, col_idx] = 1  # Adjust for 0-based index

    return A


def read_input(file_path):
    output = []
    with open(file_path, 'r') as f:
        for line in f:
            line = line.strip()
            if line:
                paren_lists, curly_list = parse_line(line)                
                A = build_matrix_A(paren_lists, curly_list)
                
                k = np.array(curly_list)
                
                output.append((A, k))
    return output         

# Test with your input

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


import numpy as np

def simplex_phase2(A, b, c, basis, demo_mode=False):
    A = np.array(A, float)
    b = np.array(b, float)
    c = np.array(c, float)
    basis = list(basis)

    m, n = A.shape
    it = 0

    while True:
        it += 1
        if demo_mode:
            print("\n=== Iteration", it, "===")
            print("Basis:", basis)

        B = A[:, basis]
        Binv = np.linalg.inv(B)

        xB = Binv @ b
        x = np.zeros(n)
        x[basis] = xB

        cB = c[basis]

        # Correct dual
        y = Binv.T @ cB  # y solves Bᵀ y = cB

        # Correct reduced costs
        reduced = c - A.T @ y

        # Correct optimality test for MIN
        if np.all(reduced >= -1e-12):
            return x, c @ x

        # Entering: most negative reduced cost
        entering = np.argmin(reduced)

        d = Binv @ A[:, entering]

        if np.all(d <= 1e-14):
            raise Exception("LP unbounded")

        ratios = np.array([
            xB[i] / d[i] if d[i] > 1e-14 else np.inf
            for i in range(m)
        ])

        leaving_row = np.argmin(ratios)
        basis[leaving_row] = entering


def bruteforce(A,k):
    
    m, n = A.shape
    combinations = list( itertools.combinations(range(n), m))
    
    solutions = []
    solidx = []
    retrace = []
    
    try:        
        #print(combinations)
        for id,cols in enumerate(combinations):
            submatrix = A[:, cols]
            if np.linalg.matrix_rank(submatrix) != m:
                retrace.append((cols, None))
                continue
                
                basis_sol = np.linalg.solve(submatrix, k)                
            else: 
                basis_sol = np.linalg.inv(submatrix) @ k
            retrace.append((cols, basis_sol))
            jj = [abs(x - round(x)) < 1e-4 and x >= -1e-4 for x in basis_sol.squeeze()]
            # print (jj)
            # print(basis_sol)
            if np.all(jj):
                solutions.append(np.sum(basis_sol))
                solidx.append(id)
                
        min_sol_idx =np.argmin(solutions)
        return (solutions[min_sol_idx], retrace[solidx[min_sol_idx]])

    except Exception as e:        
        np.set_printoptions(threshold=np.inf, linewidth=200, precision=2, suppress=True)
        msg = ("\nBrute force failed!\n"
            "Trace (all attempts):\n"
            f"{pprint.pformat(retrace, indent=2)}\n"
            f"Full trace length: {len(retrace)}\n")
        raise Exception(msg) from e
    
    
def diagnose(A, k, tol=1e-9):
    print("rank(A) =", np.linalg.matrix_rank(A))
    A_aug = np.hstack([A, k.reshape(-1,1)])
    print("rank([A|k]) =", np.linalg.matrix_rank(A_aug))
    # least squares / residual
    x, *_ = np.linalg.lstsq(A, k, rcond=None)
    res = np.linalg.norm(A.dot(x) - k)
    
    return x, res

import math

def solve_one(input, n=0):
    A, k = input
        # print("Matrix A:")
        # print(A)
        # print("Rank of A:")
        # print(np.linalg.matrix_rank(A))
        # print("Vector k:")
        # print(k)
    M, b, row_pivots, col_pivots = gaussian_elimination(A, k, demo_mode=False)
        # print("Row echelon form of A:")
        # print(M)
        # print("Transformed vector b:")
        # print(b)
        # print("Row pivots:", row_pivots)
        # print("Column pivots:", col_pivots)
        # print("Reduced submatrix:")
    Ar = A[row_pivots,:]
        # print(Ar)
        # print("Reduced Basis A")
        # print(Ar[:, col_pivots])
    kr = k[row_pivots]        
        # print("Reduced k")
        # print(kr)
    try:
        objective, other = bruteforce(Ar, kr)
    except Exception as e:            
            #x, res = diagnose(A, k)
        msg = (f"\nBrute force failed on input line {n+1}\n"
                   f"Matrix A:\n{pprint.pformat(A)}\n"
                   f"Vector k: {pprint.pformat(k)}\n"
                   f"Row pivots: {row_pivots}\n"                   
                   f"Rank of A: {np.linalg.matrix_rank(A)}\n"
                   f"Matrix A.T:\n{pprint.pformat(A.T)}\n"
                   #f"least-squares residual ||A x - k|| = {res}\n"
                   #f"Least-squares solution x:\n{pprint.pformat(x)}\n"
                   #f"Exception: {e}\n"
                   )
        raise Exception(msg) from e
        
    print(f"Input line {n+1}: Optimal objective = {objective}")
    print(other)
    return objective

if __name__ == "__main__":
    input_file = '2025/day10/input'
    #input_file = '2025/day10/test_input'
    inputs = read_input(input_file)
    
    print("Total input lines:", len(inputs))
    print("Sample input line 1 matrix A and vector k:", inputs[0])
    all_comb = [math.comb(len(b),len(c)) for b,c in inputs] # to test parsing
    max_dix = np.argmax(all_comb)
    print("Max combinations in input line", max_dix+1, "with", all_comb[max_dix], "combinations")
    
    solution = 0

    objective = solve_one(inputs[8], 0)    
    # for n,input in enumerate(inputs):        
    #     objective = solve_one(input, n)
    #     solution += objective
        
    # print("Final solution:", solution)