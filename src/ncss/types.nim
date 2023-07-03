import std/strutils

type
  Pixel* = distinct int
  Percent* = distinct float


func `%`*[N: SomeNumber](v: N): Percent =
  v.toFloat.Percent

func `$`*(v: Percent): string =
  $v.int & "%"


func `'px`*(v: string): Pixel =
  v.parseInt.Pixel

func `$`*(v: Pixel): string =
  $v.int & "px"
