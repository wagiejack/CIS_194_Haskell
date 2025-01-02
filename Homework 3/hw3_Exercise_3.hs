module Golf where
    --two main tasks:- 1.frequency of each number, 2.highest frequency will be the number of lines, initial logic is that top line will be no. maxFrequency, we traverse the line and all the numbers from [0-9] having frequency>=lineNumber will be printed, at the end we will print a "=" 10 times and numbers from 0-9

    --Input will be a integer and we have to return a string, we will use two functions, 1.Will give us the frequencies of all the numbers from 0-9/ 2.Will print the frquencies of the strings in desired format.

    --function to get the count of a specefic number from the array
    countFreq :: (Integer,[Integer])->Integer
    countFreq (_,[]) = 0
    countFreq (n,x:xs)
        | n==x = 1 + countFreq (n,xs)
        | otherwise = countFreq (n,xs)

    --function to give frequencies for all the numbers from 0-9 in a [Int] array
    --functionality is that we will traverse the array and update the count int a 10-sized list
    getFreqAll :: [Integer]->[Integer]
    getFreqAll n = map (\i->countFreq (i,n)) [0..9]

    --now for printing, we will take the maximum value and go from 0-9, if any of the frequency matches i.e >= to rowValue then we will print it, rowValue will keep decreasing with time, the no. of rows to iterate will be maximumValue
    histogram :: [Integer]->String
    --unlines will add a new line at comma seperation, we add pattern+equals+numbers
    histogram n = unlines (stars ++ [baseline,numbers])
        where
            freqs = getFreqAll n
            max = maximum freqs
            --generating the content, first we generate a list of rows which will range from max-1,then at each row, we will operate on each frequency and evaluate if it is >=row-value at that row, if yes then we can print * else space,
            stars = [  [if freq>=rows then '*' else ' '|freq<-freqs]| rows<-[max,max-1..1]]
            baseline = replicate 10 '='
            numbers = "0123456789"
-- to get the output as in 