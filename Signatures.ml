(* We need to implement these methods, feel free to create separate files
    for the methods you implement. *)
(* Interface *)
class type linearprog_t =
object

  (* Vectors have a fixed length and will probably be implemented as an array
      so each part of the array can be easily indexed into. Vectors will be
      instance variables in children classes *)

  method io : Uinput -> LinearProgram

  method prep : LinearProgram -> Cinput

  method simplex : Cinput -> Coutput

  method unprep : Coutput -> float vector

  method branch_and_bound : float vector -> LinearProgram

  method geo_search : float vector -> LinearProgram -> int vector

end

class LinearProgram =
object(self)
type 'a Vector = 'a list (* Might model after stacks *)

end

(* Dummy classes *)
(* Will have a constructor and objects of these classes are to be passed into
    the methods above*)
class Uinput =
object(self)
end

class Cinput =
object(self)
end

class Coutput =
object(self)
end
