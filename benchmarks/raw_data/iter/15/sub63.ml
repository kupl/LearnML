let rec iter (n, f) v =
        if n <= 0 then v else iter (n - 1, f) (f v);;