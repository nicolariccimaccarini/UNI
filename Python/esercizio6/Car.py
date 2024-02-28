from Veichle import veichle

class car(veichle):

    def __init__(self, plate: str, running: bool=False, speed: float=None, acceleration: float=None):
        self.plate = plate
        self.running = running

        if not self.running:
            super().__init__()
        else:
            if speed==None and acceleration==None:
                super().__init__()
            elif speed!=None and acceleration==None:
                super().__init__(speed)
            elif speed==None and acceleration!=None:
                super().__init__(acceleration)
            else:
                super().__init__(speed, acceleration)

    def start(self):
        self.running = True

    def stop(self):
        self.running = False
        self.speed = 0.0
        self.acceleration = 0.0

    def accelerate(self, acceleration: float, seconds: int):
        if self.running:
            self.set_acceleration(acceleration)
            self.set_acceleration(self._speed + super().compute_speed_increment(acceleration, seconds))

    def set_speed(self, value):
        if self.running:
            return super().set_speed(value)
        
    def set_acceleration(self, value):
        if self.running:
            return super().set_acceleration(value)

    @staticmethod
    def print_n_wheels():
        print("Ho 4 ruote!")