import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
k=pd.read_csv("2025/day9/input")

kk=k.to_numpy()
ss=np.array(list(zip(kk ,  kk[1:,:])))

ss[0]