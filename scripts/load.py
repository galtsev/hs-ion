
from amazon.ion import simpleion

def run():
    with open("data.ion", 'rb') as f:
        data = simpleion.load(f, single_value=False)
    print(data)

run()