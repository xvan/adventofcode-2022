import numpy as np
import os

def parse_pages(fp):
    pages = []
    for line in fp:
        l = line.strip()
        if l == '':
            break
        pages.append([int(p) for p in l.split('|')])
    return pages        

def parse_manuals(fp):
    manuals = []
    for line in fp:
        manuals.append([int(p) for p in line.strip().split(',')])
    return manuals
        
        
def parse_day5(filepath):    
    with open (filepath) as f:
        pages = parse_pages(f)
        manuals = parse_manuals(f)
        
    return pages, manuals
            
                
def create_connectivity_matrix(pages):
    N = max( max(p) for p in pages)
    connectivity = np.eye(N+1, dtype=int)
    for p in pages:
        connectivity[p[0], p[1]] = 1
        
    #for _ in range(N):
    #    connectivity = np.clip( np.dot(connectivity, connectivity), 0, 1)
    return connectivity


def run_day5(filepath):
    pages, manuals = parse_day5(filepath)
    connectivity = create_connectivity_matrix(pages)    
    return sum([m[int(len(m)/2)] for m in manuals if not any([connectivity[x,y] for x,y in zip(m[-1:0:-1], m[-2::-1])])])
    




print(run_day5('2024/day5/test_input'))

print(run_day5('2024/day5/input'))
    
    