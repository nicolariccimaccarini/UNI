class veichle():
    
    def __init__(self, speed=0.0, acceleration=0.0):
        self._speed = float(speed)
        self._acceleration = float(acceleration)
        

    def set_speed(self, value):
        _speed = float(value)

    def set_acceleration(self, value):
        _acceleration = float(value) 

    def print_speed(self):
        print("speed: " + str(self._speed))
    
    def print_acceleration(self):
        print("acceleration: " + str(self._acceleration))

    @staticmethod
    def compute_speed_increment(acceleration: float, seconds: int) -> float:
        return acceleration*seconds