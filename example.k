def sub(x,y)
    x - y

def facto(v)
    if v == 0 then
        1
   else
        v * facto(sub(v, 1))

def add(x,y)
    x + y


main do printd( facto(10) )