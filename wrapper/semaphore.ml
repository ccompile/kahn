
type t = {count:int ref; mutex:Mutex.t; empty:Mutex.t}

exception SemaphoreUnlockException

let create count =
    let empty = Mutex.create () in
    if count <> 0 then
        Mutex.lock empty;
    {count= ref count; mutex=Mutex.create ();
      empty = empty}
    
let lock sem =
    Mutex.lock sem.mutex;
    incr sem.count;
    if !(sem.count) = 1 then
       Mutex.lock sem.empty;
    Mutex.unlock sem.mutex
    

let unlock sem =
     Mutex.lock sem.mutex;
     if !(sem.count) = 0 then
	begin
     	   Mutex.unlock (sem.mutex); 
           raise SemaphoreUnlockException
        end
     else
	begin
	   let oldval = !(sem.count) in
	   decr sem.count;
	   if oldval = 1 then
		Mutex.unlock sem.empty;
    	Mutex.unlock sem.mutex
	end

let wait_empty sem =
     Mutex.lock sem.empty;
     Mutex.unlock sem.empty

