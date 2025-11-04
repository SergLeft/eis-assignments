
class Coordinate:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def distance_to(self, other_coordinate):
        # check out math.hypot for a nicer implementation!
        x_diff = self.x - other_coordinate.x
        y_diff = self.y - other_coordinate.y
        return (x_diff * x_diff + y_diff * y_diff) ** 0.5

    def as_string(self):
        return f"({self.x}, {self.y})"

def origin():
    return Coordinate(0, 0)

def find_nearest_point(reference, option1, option2):
    if reference.distance_to(option1) < reference.distance_to(option2):
        return option1
    else:
        return option2
