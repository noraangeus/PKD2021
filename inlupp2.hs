delbart acc n = 
	| 256217472 `mod` n == 0 && 597840768 `mod` n == 0 && 2017712592 `mod` n == 0 = (1+acc) delbart (n+1)
