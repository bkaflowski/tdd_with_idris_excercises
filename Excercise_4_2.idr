module Excercise_4_2

import Data.Vect

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Unicycle : Vehicle Pedal
     Motorcyle : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcyle fuel) = 2
wheels Tram = 60

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
refuel Unicycle impossible
refuel (Motorcyle fuel) = Motorcyle 50
refuel Tram impossible

vectTake : (n : Nat) -> Vect (n + m) e -> Vect n e
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just ((index idx xs) + (index idx ys))
