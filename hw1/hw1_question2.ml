let square_root a = 
  	let rec findroot x acc =
  		let next = (a /. x +. x) /. 2.0 in
		let difference = abs_float(next -. x) in
  		if difference > acc then findroot next acc
  		else next
  in
  findroot 1.0 epsilon_float;;
