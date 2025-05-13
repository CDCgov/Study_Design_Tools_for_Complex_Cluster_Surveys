#import sys

# execute code in another file
def myImport(path):
    x = sys.path[0]+path
    exec(open(x).read(),globals())



# Utility functions: wrapers for vectorization
def vR(x):
   return x[np.newaxis,:]

def vC(x):
   return x[:,np.newaxis]



# Similar to R expand.grid()
def expand_grid(*inputs):
    val = list(itertools.product(*inputs))
    return pd.DataFrame(val)