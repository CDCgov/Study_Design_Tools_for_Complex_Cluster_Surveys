
def nOutTab(d,p,m,icc=1/6,cv=0.50,nrr=0.15,alpha=0.05):
    x = expand_grid(d,p,m,icc,cv,nrr,alpha)
    x.columns = ('d','p','m','icc','cv','nrr','alpha')
    x['ess'] = x.apply(lambda x: ESS(x['p'],x['d']),axis=1)
    x['deff'] = x.apply(lambda x: DE(x['m'],x['icc'],x['cv']),axis=1)
    x['inff'] = x.apply(lambda x: INF(x['nrr']),axis=1)
    x['n'] = x.apply(lambda x: ceil(x['ess']*x['deff']*x['inff']),axis=1)
    x['nc'] = x.apply(lambda x: ceil(x['n']/x['m']),axis=1)
    return x


def dOutput(n,p,m,icc=1/6,cv=0.50,nrr=0.15,alpha=0.05):
    x = expand_grid(n,p,m,icc,cv,nrr,alpha)
    x.columns = ('n','p','m','icc','cv','nrr','alpha')
    x['d'] = x.apply(lambda x: halfWidthCI(x['n'],x['p'],x['m'],x['icc'],x['cv'],x['nrr'],x['alpha']),axis=1)
    return x




##https://stackoverflow.com/questions/12130883/r-expand-grid-function-in-python
#import sys
#a = [1,2,3]
#b = [4,5,6]
#ab = list(itertools.product(a,b))
#abdf = pd.DataFrame(ab,columns=("a","b"))
#print(abdf)

