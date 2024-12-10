import string
import random


class VehicelSpec:
    brand: str
    price: int 
    isEletric: bool

    def __init__(self, brand, price, isEletric) -> None:
        self.brand = brand
        self.price = price
        self.isEletric = isEletric

    def compute_tax(self):
        tax = 0.05
        if self.isEletric:
            tax = 0.02
        return tax * self.price

    def print(self):
        print(f"Brand: {self.brand}")
        print(f"Payable tax: {self.compute_tax()}")


class Vehicle:
    id: str
    license_plate: str
    spec: VehicelSpec 

    def __init__(self, id, plate, info) -> None:
        self.id = id
        self.license_plate = plate
        self.spec = info

    def print(self):
        self.spec.print()
        print(f"Id: {self.id}")
        print(f"License Plate: {self.license_plate}")


class VehicleRegistry:

    vehicle_info = { }

    def add_viehcle_info(self, brand, price, eletric):
        self.vehicle_info[brand] = VehicelSpec(brand, price, eletric)

    def __init__(self) -> None:
        self.add_viehcle_info("Tesla Model 3", 60000, True)
        self.add_viehcle_info("Volswagen ID3", 35000, True)
        self.add_viehcle_info("BMW 5", 35000, False)

    def create_vehicle(self, brand):
        vehicle_id = self.generate_vehicle_id(12)
        license_plate = self.generate_vehicle_license(vehicle_id)
        return Vehicle(vehicle_id, license_plate, self.vehicle_info[brand])

    def generate_vehicle_id(self, length):
        return ''.join(random.choices(string.ascii_uppercase, k=length))
    
    def generate_vehicle_license(self, id):
        return f"{id[:2]}-{''.join(random.choices(string.digits, k=2))}-{''.join(random.choices(string.ascii_uppercase, k=2))}"
    

class Application:

    def register_vehicle(self, brand: string):
        # create a registry entrance
        registry = VehicleRegistry()

        return registry.create_vehicle(brand)

app = Application()
vehicle = app.register_vehicle("BMW 5")
vehicle.print()