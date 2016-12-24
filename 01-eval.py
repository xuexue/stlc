'''
Variables are numbers/strings
'''

from copy import copy

#def partial_scheme_eval(program, env):

def my_scheme_eval(term, env={}):
    ## VARIABLE
    if type(term) != list:
        if term in env and env[term] is not None:
            # this variable exists in the environment; substitue
            return env[term]
        # this variable does not exist in the environment; don't substittue
        return term
    ## FUNCTION DEFINITION
    if term[0] == 'lambda':
        _, param, program = term
        return ['lambda', param, my_scheme_eval(program, env)]
    ## FUNCTION APPLICATION
    if type(term[0]) == list and term[0][0] == 'lambda':
        _, param, program = term[0]
        arg = my_scheme_eval(term[1])
        new_env = copy(env)
        new_env[param] = arg
        return my_scheme_eval(program,new_env) 
    return term


#Tests
assert my_scheme_eval('x') == 'x'
assert my_scheme_eval(['lambda', 'x', 'y']) == ['lambda', 'x', 'y']

prog = [['lambda', 'x', ['lambda', 'y', 'x']], 'z']
assert my_scheme_eval(prog) == ['lambda', 'y', 'z']
