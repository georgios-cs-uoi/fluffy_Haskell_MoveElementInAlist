--GIORGOS SIDIROPOULOS

move :: Eq u => [u] -> u -> Int -> [u]
move s x n 
	| (member x s) == False
		= s
	| n > 0 
		= if ((memberPosition x s)+n) >= (length s) then (moveEnd x s)
		else (rightMove x s n)
	| n < 0 
		= if (memberPosition x s) <= (abs n) then (moveBegin x s)
		else (leftMove x s (abs n))
	|otherwise 
		= (delete x s)


moveEnd :: Eq u =>  u -> [u] -> [u]
moveEnd x s = (delete x s) ++ [x] 

moveBegin ::  Eq u =>  u -> [u] -> [u]
moveBegin x s = x : (delete x s) 	

rightMove :: Eq u => u -> [u] -> Int -> [u]
rightMove x s n = (insert x (delete x s) ((memberPosition x s)+n))

leftMove :: Eq u =>  u -> [u] -> Int -> [u]
leftMove x s n = (insert x (delete x s) ((memberPosition x s)-n))

delete :: Eq u => u -> [u] -> [u] 
delete n (h:t)
	| n == h
		= t
	| otherwise
		= h:delete n t
delete n [] = []

member :: Eq u => u -> [u] -> Bool  
member n (h:t) = n == h || member n t
member n [] = False

memberPosition :: Eq u => u -> [u] -> Int  
memberPosition n (h:t)
	|n == h 
		= 0
	|otherwise = (memberPosition n t) + 1

insert :: u -> [u] -> Int -> [u]
insert x (h:t) p
    | p == 0 
		= x : h : t
    | p > 0 
		= h : (insert x t (p - 1)) 
		
	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

