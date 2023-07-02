import std/[macros, strtabs, unicode, sequtils, strutils]
import macroplus


type
  NCssNodeKind = enum
    ncnkWrapper
    ncnkBlock
    # ncnkValue
    # ncnkProperty
    # ncnkCall

  NCssContext = object

  NCssNode = ref object
    # case kind: NCssNodeKind
    # of ncnkBlock: discard
    # else: discard

    kind: NCssNodeKind
    selector: string
    props: StringTableRef
    children: seq[NCssNode]

  NCss = object


func un(s: string): Rune =
  s.runeAt 0

func normalizeNcssIdent*(s: string): string =
  var acc: seq[Rune]

  for c in s.toRunes:
    acc.add:
      case c
      of un"—": un"-"
      of un"․": un"."
      of un"꞉": un":"
      of un"＜": un"<"
      of un"＋": un"+"
      else: toLower c

  $acc


func unwrapNestedCommandsImpl(n: NimNode, result: var seq[NimNode]) =
  case n.kind
  of nnkCommand:
    result.add n[0]
    unwrapNestedCommandsImpl n[1], result
  else:
    result.add n

func unwrapNestedCommands(n: NimNode): seq[NimNode] =
  unwrapNestedCommandsImpl n, result

func mapp[A, B](fn: proc(a: A): B, s: seq[A]): seq[B] = 
  for i in s:
    result.add fn i

func parseNcss(nimTree: NimNode): NimNode =
  result = newTree(nnkBracket)

  for n in nimTree:
    case n.kind
    of nnkCall:
      let
        id = n[CallIdent]
        selectors = mapp normalizeNcssIdent:
          case id.kind
          of nnkIdent: @[id.strVal]
          of nnkTupleConstr: id.children.toseq.mapIt it.strVal
          else: raise newException(ValueError, "!+!")
          

        body = n[CallBody]

      debugecho selectors

      var acc = newTree(nnkTableConstr)

      for sett in body:
        let
          prop = sett[AsgnLeftSide]
          # prop = normalizeNcssIdent sett[AsgnLeftSide].strVal
          val = unwrapNestedCommands sett[AsgnRightSide]
          finalValue = block:
            var acc = newTree(nnkBracket)

            for v in val:
              acc.add inlineQuote $`v`

            inlineQuote `acc`.join " "

        acc.add newColonExpr(prop, finalValue)
        debugecho prop, ": ", repr finalValue

      let sss = selectors.join ", "

      result.add quote do:
        NCssNode(
          kind: ncnkBlock,
          selector: `sss`,
          props: newStringTable(`acc`))


    else:
      discard

  result = quote:
    NCssNode(
      kind: ncnkWrapper,
      children: @`result`)

  debugecho repr result
  debugecho "......................"



macro css(body): untyped =
  parseNcss body

type
  Pixel = distinct int
  Percent = distinct float


func `%`[N: SomeNumber](v: N): Percent =
  v.toFloat.Percent

func `$`(v: Percent): string =
  $v.int & "%"


func `'px`(v: string): Pixel =
  v.parseInt.Pixel

func `$`(v: Pixel): string =
  $v.int & "px"


func `$`(n: NCssNode): string =
  case n.kind
  of ncnkWrapper:
    for c in n.children:
      result.add $c
      result.add "\n"

  of ncnkBlock:
    result.add n.selector
    result.add " {\n"
    for p, v in n.props:
      result.add repeat(' ', 2)
      result.add p
      result.add ": "
      result.add v
      result.add ";\n"
    result.add "}"


macro defProp(ns: varargs[untyped]): untyped =
  result = newStmtList()

  for n in ns:
    let normalized = newLit normalizeNcssIdent n.strVal
    result.add quote do:
      const `n` = `normalized`


defProp border—radius, border—width,
        e—resize, cursor,
        left, right, top, bottom


# dumpTree:
let localStyles =
  css:
    # var globe = 2

    ․extender:
      border—width = 4'px
      cursor = pointer

    (․side—bar, ․btn):
      cursor = e—resize
      border—radius = 0 0 2'px 10'px

    ․tool—bar꞉hover＜p:
      left = 0
      top = %50
      # transform = translateY( - %50)


echo localStyles