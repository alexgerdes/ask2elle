module FloatOperations where

main :: ([Float], [Bool])
main = ([1.0 + 2.5, 2.4 - 4.0, 8.8 * 3.1, 1.0 / 3.0, -. 4.5]
       ,[3.4 > 3.3, 1.2 == 1.2, 8.0 < 4.0, 3.5 >= 7.0
        , 1.2 <= 10.0, 4.0 /= 4.1, 3.0 /= 3.0 
        ]
       )