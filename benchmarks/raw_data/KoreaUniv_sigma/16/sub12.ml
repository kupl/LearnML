let rec sigma f a b =
    if b = a then f b
    else f b + (sigma f a (b-1)) ;;
