#nodes = {}

def forward(node, args):
    nodes[node].compute(*args)

class node1():

    def compute(self):
        forward('node2', ["hi"])

class node2():

    def compute(self, arg1):
        print arg1

def main():
    global nodes
    nodes = { 'node1': node1(), 'node2': node2() }
    nodes['node1'].compute()

main()
