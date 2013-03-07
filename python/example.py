import sys

nodes = {}
interfaces = {}

def forward(node, **kwargs):
    nodes[node].compute(kwargs)

class Node():
    pass

class Interface(Object):
    
    def __init__(self, ins, outs):
        self.ins = {}
        self.outs = {}

class MyCompStartl(Node):

    def __init__(self):
        self.image_filename = sys.argv[0]
        self.x = 0.0
        self.y = 0.0
        self.in_image = []

    def compute(self):
        """
        if an interface is on the left side of a forward we want to send its output to a node
        forward (IComplicatedEffect.out_image) to (next_step.in_image)
        """
        forward('NextStep', [interfaces['IComplicatedEffect'].outs['out_image']])

        """
        if an interface is on the right side of a forward we want to set its inputs.
        ie. forward (in_image, x, y) to IComplicatedEffect.(in_image, param1, param2)
        """
        interfaces['IComplicatedEffect'].ins['in_image'] = self.in_image
        interfaces['IComplicatedEffect'].ins['param1'] = self.x
        intefraces['IComplicatedEffect'].ins['param2'] = self.y

class NextStep(Node):
    
    def compute(self, in_image):
        pass
    
class IComplicatedEffect(Interface):

    def __init__(self):
        self.in_image = []
        self.param1 = 0.0
        self.param2 = 0.0
        self.out_image = []
        super().__init__({'in_image': self.in_image, 'param1': self.param1, 'param2': self.param2}, {'out_image': self.out_image})

class IBlur(Interface):    

    def __init__(self, image, strength):
        self.image = []
        self.strength = strength
        self.out_image = []
        super().__init__({'image': self.image, 'strength': self.strength}, {'out_image': self.out_image})
        
class Blur(Node):

    def __init__(self):
        self.new_image = []
        self.op1 = 0
        
    def compute(self, **kwargs):
        self.new_image = kwargs['image']
        
class CEStart1(Node):

    def __init__(self):
        self.new_image = []

    def compute(self, image, param1):
        forward('op1', [self.new_image])

class CEStart2(Node):
    
    def __init__(self):
        self.new_image = []

    def compute(self, image, param2):
        forward('op1', [self.new_image])
        
class op1(Node):
    
    def compute(self, image):
        pass

class CEFinalStep(Node):

    def compute(image, param3):
        forward('IComplicatedEffect', [image])
