module Model

type Literal = Int of int | String of string
type Expression =
    | VariableReference of name: string
    | MemberReference of owner: Expression * memberName: string
    | ContextReference
    | And of Expression * Expression
    | Equal of Expression * Expression
    | Literal of Literal
type Statement =
    | Case of variableName: string * (Expression * Statement list) list
    | CreateTask of name: string
    | Associate
    | SendEmail
type InputDeclaration = InputDeclaration of caseName: string * contextName: string
type Preface = Preface of InputDeclaration
type Program = Program of Preface * Statement list

(*
Example program:

Inputs: Case Entity (case, ExecutionContext ctx)

ctx.user = "system"
                dsffsdfdsaf
ctx.user <> system
Case Create:
                when ctx.field1 == "abc"
                                Create Task with name = "sdfsdf"
                                Associate with case
                When ctx.field2 = 1 and field 4 = "b"
                                Send email

*)

// Manually construct a data structure in F# as an example of what the parser will give us
let example =
    Program(Preface(InputDeclaration("case", "ctx")),
        [
        Case("Create",
            [
                Equal(MemberReference(ContextReference, "field1"), Literal(String "abc")),
                    [
                        CreateTask("sdfsdf")
                        Associate
                    ]
                And(Equal(MemberReference(ContextReference, "field2"), Literal(Int 1)), Equal(MemberReference(ContextReference, "field4"), Literal(String "b"))),
                    [
                        SendEmail
                    ]
            ])
        ])

module Parse =
    open Packrat
    // For the sake of convention, I'm going to put a capital P for "Parser" at the end of all my parsing patterns but you could use another convention.
    type DurgaToyContext = {
        mutable executionContextName: string option
    }

    let (|DurgaToyContextP|_|) =
        Packrat.ExternalContextOf<DurgaToyContext>

    let (|LiteralP|_|) = function
        | Int(n, rest) -> Some (Literal (Int n), rest)
        | Str "\"" (CharsExcept (Set.ofList ['"'])(txt, Str "\"" rest)) ->
            Some (Literal (String txt), rest)
        | _ -> None
    let (|NameP|_|) = function
        | Chars alphanumeric (name, rest) -> Some(name, rest)
        | _ -> None
    let (|Expression|_|) = function
        | DurgaToyContextP { executionContextName = Some ctxName } & NameP(name, rest) when ctxName = name -> Some(ContextReference, rest)
        | NameP(name, rest) -> Some(VariableReference name, rest)
        | _ -> None
    let (|ProgramP|_|) = function
        | _ -> Some example

    let parse input =
        match ParseArgs.Init(input, { executionContextName = None }) with
        | ProgramP p -> p
        | _ -> failwithf "Invalid program: \n%s" input

module Execution =
    let execute input =
        printfn "Execution not implemented yet: \n%A" input