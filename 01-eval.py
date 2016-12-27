'''
Variables are numbers/strings
'''

import pprint
from copy import copy

def my_scheme_eval(term, env=None):
    ## Initialization of ENV
    if env is None:
        env = []

    ## GREGNAME: REFERENCE BINDING
    if type(term) != list:

        env_dict = dict(env) # later values have prescedence
        value = env_dict.get(term)

        if value: # this variable exists in the environment; substitue
            return value # this is already a closure
        raise ValueError()

    ## GREGNAME: ClOSE OVER ENV
    if term[0] == 'lambda':
        return (term, env) # bsically don't do anything

    ## GREGNAME: APPLY CLOSURE
    if type(term[0]) == list:
        value = my_scheme_eval(term[0], env) #(term,env) pair
        value_u = my_scheme_eval(term[1], env) 
        func_a = value[0][2]
        env_f = copy(value[1])
        param = value[0][1] # x
        env_f.append((param, value_u),)
        return my_scheme_eval(func_a, env_f)


    ## What the hell is travesrseee environmenttt?
    # post processing?
    # deleting uselsess environments?


def print2(x):
    pprint.pprint(x, depth=8842386334636)

#Tests
print2(my_scheme_eval(['lambda', 'x', 'x']))

print2(my_scheme_eval([['lambda', 'x', 'x'], ['lambda', 'x', 'x']]))
print2(my_scheme_eval([['lambda', 'x', 'x'], ['lambda', 'y', 'y']]))
print2(my_scheme_eval([['lambda', 'y', 'y'], ['lambda', 'x', 'x']]))
print2(my_scheme_eval([['lambda', 'y', ['lambda', 'z', 'y']], ['lambda', 'x', 'x']]))
print2(my_scheme_eval([[['lambda', 'y', ['lambda', 'z', 'y']], ['lambda', 'x', 'x']], ['lambda', 'j,' 'j']]))

'''
prog = [['lambda', 'x', ['lambda', 'y', 'x']], 'z'] # not a lambda?
print my_scheme_eval(prog)

prog = [['lambda', 'x', ['lambda', 'y', 'x']], ['lambda', 'f', 'z']]
print my_scheme_eval(prog)

prog = [['lambda', 'x', ['lambda', 'y', 'x']], ['lambda', 'y', 'y']]

print my_scheme_eval(prog) # ['lambda', 'y', ['lambda', 'y', 'y']]

m = [prog, 'z']
print m
print my_scheme_eval(m)

'''
