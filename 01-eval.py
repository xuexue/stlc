'''
Variables are numbers/strings
'''

from copy import copy

#def partial_scheme_eval(program, env):

class Environment(object):
    def __init__(self):
        self.values = []
    def push(self, var, arg):
        self.values.append((var, arg),)
    def pop(self):
        self.values.pop()
    def get_val(self, var):
        turn_into_dict = dict(self.values) # later values have prescedence
        return turn_into_dict.get(var)

def my_scheme_eval(term, env=None):
    ## Initialization of ENV
    if env is None:
        env = Environment()
    ## VARIABLE
    if type(term) != list:
        val = env.get_val(term)
        if val: # this variable exists in the environment; substitue
            return val
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
        env.push(param, arg) # add the new param/arg to env
        retval = my_scheme_eval(program, env) 
        env.pop() # remove it to continue computation
        return retval
    return term


#Tests
assert my_scheme_eval('x') == 'x'
assert my_scheme_eval(['lambda', 'x', 'y']) == ['lambda', 'x', 'y']

prog = [['lambda', 'x', ['lambda', 'y', 'x']], 'z']
assert my_scheme_eval(prog) == ['lambda', 'y', 'z']

prog = [['lambda', 'x', ['lambda', 'y', 'x']], ['lambda', 'f', 'z']]
assert my_scheme_eval(prog) == ['lambda', 'y', ['lambda', 'f', 'z']]

prog = [['lambda', 'x', ['lambda', 'y', 'x']], ['lambda', 'y', 'y']]
print my_scheme_eval(prog)

print my_scheme_eval([prog, 'z'])

