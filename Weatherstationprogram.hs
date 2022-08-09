
import Text.Printf
import Text.Read
import System.Exit

data Station = Station {name :: String, north :: Float , east :: Float, temperature :: [Float] } deriving (Show, Eq, Read)

testData :: [Station]
testData = [
    Station "Mumbles Head" 51.565 (-3.981) [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09],
    Station "Greenwich Park" 51.477 0.004 [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85],
    Station "Solent" 50.807 (-1.208) [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17],
    Station "Ronaldsway" 54.085 (-4.632) [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17],
    Station "Baltasound" 60.749 (-0.850) [6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00],
    Station "St Austell" 50.337 (-4.787) [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18],
    Station "Heathrow" 51.479 (-0.449) [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79],
    Station "Hunstanton" 52.939 0.493 [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56],
    Station "Durham" 54.767 (-1.583) [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07],
    Station "Monks Wood" 52.400 (-0.233) [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85] ]

displayStations :: Station -> String
displayStations station =  
   printf "%25s" (name station) ++ 
   printf "%10s" (show $ north station) ++ 
   printf "%10s" (show $ east station) ++ 
   printf "%30s" (show $ temperature station) 

joinString :: [Station] -> String
joinString [] = lineCreator
joinString (x:xs) = displayStations x ++ "\n" ++ joinString xs

lineCreator :: String
lineCreator = "------------------------------------------------------------------------------------------------"

getStation :: [Station] -> String
getStation [] = []
getStation (station:stations) = name station  ++ "\n" ++ getStation stations

-- for inputStation to function correctly, station name has to be wrapped in quotation marks, negative numbers have to be in brackets, 
-- and temperature data has to be wrapped in square brackets
inputStation :: Station -> [Station] -> [Station]
inputStation station [] = [station]
inputStation station (x:xs) = if (name station) <= (name x)
    then station:x:xs
    else x:inputStation station xs

applyfahrenheitFormula :: [Float] -> [Float]
applyfahrenheitFormula [] = []
applyfahrenheitFormula (x:xs) = celsiustofahrenheitFormula x:applyfahrenheitFormula xs

celsiustofahrenheitFormula :: Float -> Float
celsiustofahrenheitFormula x = (1.8 * x) + 30

temptoFahrenheit :: Station -> Station
temptoFahrenheit (Station name north east temps) = Station name north east (applyfahrenheitFormula temps)

applytoTemp :: [Station] -> [Station]
applytoTemp [] = []
applytoTemp (x:xs) = temptoFahrenheit x:applytoTemp xs

checkaugustFormula :: [Float] -> Bool
checkaugustFormula [] = False
checkaugustFormula temps 
    | temps!! 7 > 20 = True 
    | otherwise = False

getTemp :: [Station] -> [String]
getTemp [] = []
getTemp ((Station name north east temps):xs) | checkaugustFormula temps == True = [] ++ name:getTemp xs
                                             | otherwise = getTemp xs

stationnametoString :: [String] -> String 
stationnametoString [] = ""
stationnametoString (x:xs) = printf x ++  "\n" ++ lineCreator ++ "\n" ++ stationnametoString xs

stationsToString :: [Station] -> String 
stationsToString list =  "\n" ++ (joinString list) ++ lineCreator 

applytempinMonth :: [Station] -> String -> Int -> Float -> [Station]
applytempinMonth [] _ _ _ = []
applytempinMonth stations "" _ _ = stations
applytempinMonth ((Station name north east temps):stations) stationName month temperature     
    | stationName == name = (Station name north east (updateTemp (tempList temps) month temperature)):stations
    | otherwise = (Station name north east temps)
      :applytempinMonth stations stationName month temperature 

updateTemp :: [(Int, Float)] -> Int -> Float -> [Float]
updateTemp [] _ _ = []
updateTemp ((d, x):xs) month temperature = 
    if d == month then temperature:updateTemp xs month temperature 
    else x:updateTemp xs month temperature

tempList :: [Float] -> [(Int, Float)]
tempList [] = []
tempList temps = zip [1..12] temps

main :: IO ()
main = do
  stationsList <- loadData
  menu stationsList

saveData :: [Station] -> IO ()
saveData = writeFile "stations.txt" . show

loadData :: IO [Station]
loadData = do
  fContents <- readFile "stations.txt"
  return (read fContents :: [Station])

menu :: [Station] -> IO ()
menu stringList = do
    putStrLn "Enter a number selection to be carried out: "
    putStrLn "1. output the names of all the weather stations"
    putStrLn "2. output the data after adding a new station"
    putStrLn "3. output the data with all temperature values converted to degrees Fahrenheit"
    putStrLn "4. output the names of weather stations with August temperature warmer than 20 degrees Celsius"
    putStrLn "5. output all stations to string (stationsToString testData)"
    putStrLn "6. output the data after changing the temperature of a selected month to a new selected temperature"
    putStrLn "7. Exit the program"
    putStrLn ""
    option <- getLine
    let maybechosenOption = readMaybe option :: Maybe Int
    case maybechosenOption of
      Just chosenOption -> do
        newList <- selectionManager chosenOption stringList
        menu newList
      _ -> do
        putStrLn "Please enter a valid option"
        menu stringList

selectionManager :: Int -> [Station] -> IO [Station]
selectionManager 1 stationList = do

    putStrLn $ getStation stationList
    putStrLn $ lineCreator
    return stationList

selectionManager 2 stationList = do
    putStrLn "Enter the new station's name:"
    stationInput <- getLine
    putStrLn "Enter the new station's north coordinate:"
    northInput <- getLine
    putStrLn "Enter the new station's east coordinate:"
    eastInput <- getLine
    putStrLn "Enter a list of the new station's temperature data:"
    inputList <- getLine
    putStrLn ""
    let ninput = read northInput :: Float
    let einput = read eastInput :: Float
    let tempinput = read inputList :: [Float]
    let newStationList = inputStation (Station stationInput ninput einput tempinput) stationList
    putStrLn $ lineCreator
    putStrLn $ joinString $ newStationList
    putStrLn $ lineCreator
    return newStationList

selectionManager 3 stationList = do
    putStrLn $ lineCreator
    putStrLn $ joinString $ applytoTemp stationList
    return stationList

selectionManager 4 stationList = do
    putStrLn $ lineCreator
    putStrLn $ stationnametoString $ getTemp stationList
    return stationList

selectionManager 5 stationList = do
    putStrLn $ lineCreator
    putStrLn (stationsToString stationList)
    return stationList

selectionManager 6 stationList = do
    putStrLn "Enter the station's name that you wish to change the temperature for:"
    stationInput <- getLine
    putStrLn "Enter the station's month that you wish to change the temperature for:"
    stationMonth <- getLine
    putStrLn "Enter the new station's temperature for that month"
    stationtemp <- getLine
    let smonth = read stationMonth :: Int
    let stemp = read stationtemp :: Float
    let newStationList = applytempinMonth stationList stationInput smonth stemp
    putStrLn $ lineCreator
    putStrLn $ joinString $ newStationList
    return newStationList

selectionManager 7 stationList = do
    saveData stationList
    exitSuccess

selectionManager _ stationList = do
    putStrLn "Please enter a valid option"
    return stationList