import std/[macros, strtabs, unicode, sequtils]
import macroplus


type
  NCssNodeKind = enum
    ncnkWrapper
    ncnkProperty
    ncnkValue
    ncnkCall

  NCssContext = object

  NCssNode = ref object
    case kind: NCssNodeKind
    of ncnkCall: discard
    else: discard

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
      else: c

  $acc


func unwrapNestedInfixImpl(n: NimNode, infixLit: string, result: var seq[NimNode]) =
  if n.matchInfix infixLit:
    result.add n[InfixLeftSide]
    unwrapNestedInfixImpl n[InfixRightSide], infixLit, result
  else:
    result.add n

func unwrapNestedInfix(n: NimNode, infixLit: string): seq[NimNode] =
  unwrapNestedInfixImpl n, infixLit, result


func parseNcss(nimTree: NimNode): NCssNode =
  result = NCssNode(kind: ncnkWrapper)

  for n in nimTree:
    case n.kind
    of nnkInfix:

      let
        body = n[InfixBody]
        selectors =
          unwrapNestedInfix(n, "..").
          mapIt(normalizeNcssIdent it.strVal)

      debugecho selectors

    of nnkCall:
      let
        selector = n[CallIdent].strVal.normalizeNcssIdent
        body = n[CallArgs[0]]


      debugecho @[selector]

    else:
      discard

macro css(body): untyped =
  discard parseNcss body
  newlit 1


let localStyles = css:
  # var globe = 2

  ․extender:
    width: 4.px
    cursor: e—resize

  ․side—bar .. ․btn:
    right: 0
    top: 0

  # ․tool—bar꞉hover＜p＋[attribute = value]:
    # left: 0
    # top: %50
    # transform: translateY( - %50)


#[
  Call
    Ident "․side—bar"
    StmtList
      Call
        Ident "right"
        StmtList
          IntLit 0
      Call
        Ident "top"
        StmtList
          IntLit 0
  Infix
    Ident ".."
    BracketExpr
      Ident "․tool—bar꞉hover＜p＋"
      ExprEqExpr
        Ident "attribute"
        Ident "value"
    Ident "․btn"
    StmtList
      Call
        Ident "left"
        StmtList
          IntLit 0
      Call
        Ident "top"
        StmtList
          Prefix
            Ident "%"
            IntLit 50
      Call
        Ident "transform"
        StmtList
          Call
            Ident "translateY"
            Prefix
              Ident "-"
              Prefix
                Ident "%"
                IntLit 50
  Call
    Ident "․extender"
    StmtList
      Call
        Ident "width"
        StmtList
          DotExpr
            IntLit 4
            Ident "px"
      Call
        Ident "cursor"
        StmtList
          Ident "e—resize"
]#
