import std/sequtils

func mapp*[A, B](fn: proc(a: A): B, s: seq[A]): seq[B] =
  ## just like map but arguments are in the reverse order
  map s, fn
