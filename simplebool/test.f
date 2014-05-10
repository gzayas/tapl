
 lambda x:Bool. x;
 (lambda x:Bool->Bool. if x false then true else false) 
   (lambda x:Bool. if x then false else true); 

 (lambda x:Bool->Bool. (lambda y:Bool. x y))
   (lambda x:Bool. if x then false else true)
   false;

