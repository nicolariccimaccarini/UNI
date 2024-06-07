from Veichle import veichle

class bicycle(veichle):

    def __init__(self, speed: float=None, acceleration: float=None):
        if speed==None and acceleration==None:
            super().__init__()
        elif speed!=None and acceleration==None:
            super().__init__(speed)
        elif speed==None and acceleration!=None:
            super().__init__(acceleration)
        else:
            super().__init__(speed, acceleration)

    def pedal(self, n_hits: int, seconds: int):
        self.set_acceleration(n_hits/(seconds**2))
        self.set_speed(self._speed + super().compute_speed_increment(self._acceleration, seconds))

    @staticmethod
    def print_n_wheels():
        print("Ho 2 ruote!") 