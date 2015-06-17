module Metrics where

data MetricUnit = Meter | Liter | KiloGram deriving (Show, Eq)
data ImperialUnit = Yard | Gallon | Pound deriving Show
data Measurement = MetricMeasurement Double MetricUnit
                 | ImperialMeasurement Double ImperialUnit
                   deriving Show

-- "m" "L" "kg" - guards and pattern match (needs Eq)
symbol :: MetricUnit -> String
symbol Meter = "m"
symbol Liter = "L"
symbol KiloGram = "kg"

convert :: Measurement -> Measurement
convert (ImperialMeasurement y Yard)  = MetricMeasurement (y * 0.9144) Meter
convert (ImperialMeasurement y Gallon)= MetricMeasurement (y * 3.7854) Liter
convert (ImperialMeasurement y Pound) = MetricMeasurement (y * 0.4536) KiloGram
convert (MetricMeasurement x metric)
    | metric == Meter    = ImperialMeasurement (x * 1.0936) Yard
    | metric == Liter    = ImperialMeasurement (x * 0.2642) Gallon
    | metric == KiloGram = ImperialMeasurement (x * 2.2046) Pound
