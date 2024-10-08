fun curry f=fn first => (fn second => f(first,second));
fun uncurry f=fn (element1,element2) => (f element1) element2;