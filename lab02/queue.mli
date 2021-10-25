type 'a queue

  val empty : 'a queue

  val push : 'a -> 'a queue -> 'a queue

  val pop : 'a queue -> 'a queue

  val get_min : 'a queue -> 'a