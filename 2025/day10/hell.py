
import numpy as np
from hsnf import smith_normal_form
import sympy as sp

debug_mode = True

def dpprint(*args, **kwargs):
    if debug_mode:
        sp.pprint(*args, **kwargs)

def dprint(*args, **kwargs):
    if debug_mode:
        print(*args, **kwargs)

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
                dprint(f"Skipping column {col}, no pivot found")
            continue
        
        row_pivots.append(pivot_row)
        col_pivots.append(col)
        used_rows.add(pivot_row)
        
        if demo_mode:
            dprint(f"Using row {pivot_row} as pivot for column {col}")
        
        # Elimination - eliminate this column in ALL other rows (not just below)
        for j in range(n):
            if j != pivot_row and np.abs(M[j, col]) > 1e-10:
                factor = M[j, col] / M[pivot_row, col]
                M[j] = M[j] - factor * M[pivot_row]
                b[j] = b[j] - factor * b[pivot_row]
                if demo_mode:
                    dprint(f"Eliminated row {j} using row {pivot_row} with factor {factor}")
                    dprint(M)
    
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


def test_integer(solution):
    dprint("Testing solution for integrality:")
    dpprint(solution)
    for val in solution:
        if not np.isclose(val, int(val),1e-7):
            return False
    return True

###################################
def solve_integer_program(M, k):
    M_reduced, k_reduced, row_pivots, col_pivots = gaussian_elimination(M, k, demo_mode=False)

    dprint("Reduced Matrix M:")
    dprint(M_reduced)
    dprint("Reduced k:")
    dprint(k_reduced)
    dprint("Row pivots:", row_pivots)
    dprint("Column pivots:", col_pivots)

    ###################################

    Mr = M[row_pivots,:]
    kr = k[row_pivots]

    #dpprint(spMr[:,col_pivots].inv()@spkr)
    solutions = bruteforce(Mr, kr)

    for solution in solutions:
        dpprint(solution)


    dprint("Total brute-force solutions found:", len(solutions))

    integer_solutions = [(col_base, solution) for col_base, solution in solutions if test_integer(solution)]
    dprint("Integer brute-force solutions found:", len(integer_solutions))
    dpprint(integer_solutions)
    #test x >= 0 with float tolerance
    non_negative_solutions = [(col_base, solution) for col_base, solution in integer_solutions if all(x >= -1e-4 for x in solution)]
    
    
    if non_negative_solutions:
        dprint("Non-negative integer solutions found:", len(non_negative_solutions))
        min_solution = np.argmin(np.sum(solution) for col_base, solution in non_negative_solutions)
        dprint("Minimum objective value (non-negative):", np.sum(non_negative_solutions[min_solution][1]))
        dprint("Solution vector (non-negative):", non_negative_solutions[min_solution][1])
        return np.sum(non_negative_solutions[min_solution][1])    
       

    dprint("less negative integer solutions")
    sorted_solutions = sorted([(sum(x for x in solution if x<0), col_base, solution) for col_base, solution in integer_solutions if any(x<0 for x in solution)], key=lambda t: t[0], reverse=True)
            

    dprint("Best integer solutions:")
    for neg_sum, col_base, solution in sorted_solutions[:5]:
        dprint("Negative sum:", neg_sum)
        dprint("Column base:", col_base)
        dpprint(solution)




    ncnt, test_columns, test_reduced_solution = sorted_solutions[0]
    x_particular = np.zeros(M.shape[1],dtype=int)
    for i, col in enumerate(test_columns):
        x_particular[col] = test_reduced_solution[i]

    dprint("Test solution check M @ x:")
    residual = M @ x_particular - k
    dpprint(residual)
            

    # Smith normal form D, and unimodular matrices L and R such that L @ M @ R == D
    D, L, R = smith_normal_form(M)

    dprint("Smith Normal Form D:")
    dprint(D)
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

    dprint("Integer null space basis:")
    dprint(integer_null_basis)

    general_solution = lambda zvals: x_particular + integer_null_basis @ np.array(zvals, dtype=int)


    dprint( general_solution([-1, -1, 0]) > 0)

    Z_max = np.max(k)

    nns = []
    for z0 in range(-Z_max, Z_max+1):
        for z1 in range(-Z_max, Z_max+1):
            for z2 in range(-Z_max, Z_max+1):            
                x = general_solution([z0, z1, z2])
                if np.all(x >= 0):
                    nns.append(x)
                    dpprint(x)

    dprint("Total non-negative integer solutions found:", len(nns))
    a_solution = np.argmin(np.sum(x) for x in nns)
    dprint("Minimum objective value:", np.sum(nns[a_solution]))
    dprint("Solution vector:", nns[a_solution])

    assert(np.all(M @ nns[a_solution] == k))

    return np.sum(nns[a_solution])




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

#print ( solve_integer_program(M, k))

if __name__ == "__main__":
    input_file = '2025/day10/input'
    #input_file = '2025/day10/test_input'
    inputs = read_input(input_file)
    solution = 0
    for n,input in enumerate(inputs):
        if n != 7:
            continue 
        print(f"Processing input #{n+1}")              
        objective = solve_integer_program(input[0], input[1])
        solution += objective
        
    print("Final solution:", solution)
