-- Functional Implementation by maintaining two lists
-- One for enqueue and dequeue.
data Queue d = Queue [d] [d] deriving ( Eq, Show, Read)

emptyQueue :: Queue d
emptyQueue = Queue [] []

isEmpty :: Queue d -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

enqueue :: d -> Queue d -> Queue d
enqueue d (Queue xs ys) = Queue xs (d:ys)

dequeue :: Queue d -> Queue d
dequeue (Queue [] []) = error "empty queue"
dequeue (Queue [] ys) = dequeue (Queue (reverse ys) [])
dequeue (Queue (x:xs) ys) = Queue xs ys
                           
front :: Queue d -> d
front (Queue [] []) = error "Empty Queue"
front (Queue [] ys) = last ys
front (Queue (x:xs) ys) = x