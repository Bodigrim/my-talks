{-# LANGUAGE ScopedTypeVariables #-}

module AllSorts where

certainCmp :: Ord α => α -> α -> [Bool]
certainCmp a b = [a <= b]

uncertainCmp :: α -> α -> [Bool]
uncertainCmp _ _ = [True, False]

-------------------------------------------------------------------------------

insertSortBy :: forall μ α. Monad μ
             => (α -> α -> μ Bool) -> [α] -> μ [α]
insertSortBy cmp = insertSort
  where
    insertSort :: [α] -> μ [α]
    insertSort []       = return []
    insertSort (a : as) = do
                          bs <- insertSort as
                          insert a bs

    insert :: α -> [α] -> μ [α]
    insert a []       = return [a]
    insert a (b : bs) = do
                        t <- a `cmp` b
                        if t then return (a : b : bs)
                             else (b :) <$> insert a bs

-------------------------------------------------------------------------------

selectSortBy :: forall μ α. Monad μ
             => (α -> α -> μ Bool) -> [α] -> μ [α]
selectSortBy cmp = selectSort
  where
    selectSort :: [α] -> μ [α]
    selectSort []       = return []
    selectSort (a : as) = do
                          (b, bs) <- selectMin a as
                          (b :) <$> selectSort bs

    selectMin :: α -> [α] -> μ (α, [α])
    selectMin a []       = return (a, [])
    selectMin a (b : bs) = do
                           t <- a `cmp` b
                           let (a', b') = if t then (a, b)
                                               else (b, a)
                           (c, cs) <- selectMin a' bs
                           return (c, b' : cs)

-------------------------------------------------------------------------------

bubbleSortBy :: forall μ α. Monad μ
             => (α -> α -> μ Bool) -> [α] -> μ [α]
bubbleSortBy cmp = bubbleSort
  where
    bubbleSort :: [α] -> μ [α]
    bubbleSort []       = return []
    bubbleSort (a : as) = do
                          (b, bs) <- bubble a as
                          (b :) <$> bubbleSort bs

    bubble :: α -> [α] -> μ (α, [α])
    bubble a []       = return (a, [])
    bubble a (c : cs) = do
                        (b, bs) <- bubble c cs
                        t <- a `cmp` b
                        return $ if t then (a, b : bs)
                                      else (b, a : bs)

-------------------------------------------------------------------------------

quickSortBy :: forall μ α. Monad μ
            => (α -> α -> μ Bool) -> [α] -> μ [α]
quickSortBy cmp = quickSort
  where
    quickSort :: [α] -> μ [α]
    quickSort []       = return []
    quickSort (a : as) = do
                         (bs, cs) <- partitionBy (`cmp` a) as
                         xs <- quickSort bs
                         ys <- quickSort cs
                         return $ xs ++ (a : ys)

partitionBy :: Monad μ
            => (α -> μ Bool) -> [α] -> μ ([α], [α])
partitionBy predicate = partition
  where
    partition []       = return ([], [])
    partition (a : as) = do
                         (bs, cs) <- partition as
                         t <- predicate a
                         return $ if t then (a : bs, cs)
                                       else (bs, a : cs)

-------------------------------------------------------------------------------

mergeSortBy :: forall μ α. Monad μ
            => (α -> α -> μ Bool) -> [α] -> μ [α]
mergeSortBy cmp = mergeSort
  where
    mergeSort :: [α] -> μ [α]
    mergeSort []  = return []
    mergeSort [a] = return [a]
    mergeSort as  = do
                    let l = length as `div` 2
                    let (bs, cs) = splitAt l as
                    xs <- mergeSort bs
                    ys <- mergeSort cs
                    merge xs ys

    merge :: [α] -> [α] -> μ [α]
    merge as       []       = return as
    merge []       bs       = return bs
    merge (a : as) (b : bs) = do
                              t <- a `cmp` b
                              if t then (a :) <$> merge as (b : bs)
                                   else (b :) <$> merge (a : as) bs
