module TreeExn : TREE = 
struct

  type 'a tree = Node of 'a * ('a tree) list 

  exception NotFound
  exception NotImplemented

  let find p t = raise NotImplemented

end
