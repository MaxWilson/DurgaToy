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

let exampleTxt = """
Inputs: Case Entity (case, ExecutionContext ctx)

Case Create:
                when ctx.field1 == "abc"
                                Create Task with name = "sdfsdf"
                                Associate with case
                When ctx.field2 = 1 and field 4 = "b"
                                Send email"""

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

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#load "Packrat.fs"
#endif

#nowarn "40" // we're doing some recursive initialization to enable recursive patterns, but it's safe because we're not doing anything crazy like calling ctor function arguments from _inside_ the ctor, before the recursive arguments have been initialized. We're just storing the function arguments to be called later.
module Parse =
    open Packrat
    // For the sake of convention, I'm going to put a capital P for "Parser" at the end of all my parsing patterns but you could use another convention.
    type DurgaToyContext = {
        mutable executionContextName: string option
    }

    let (|DurgaToyContextP|_|) =
        Packrat.ExternalContextOf<DurgaToyContext>

    let (|StringLiteralP|_|) = function
        | Str "\"" (CharsExcept (Set.ofList ['"'])(txt, Str "\"" rest)) ->
            Some (String txt, rest)
        | _ -> None
    let (|LiteralP|_|) = function
        | Int(n, rest) -> Some (Int n, rest)
        | StringLiteralP(result) -> Some result
        | _ -> None
    let (|NameP|_|) = function
        | Chars alphanumeric (name, rest) -> Some(name, rest)
        | _ -> None
    let rec (|ExpressionP|_|) = pack <| function
        | ExpressionP(lhs, Word("and", ExpressionP(rhs,rest))) -> Some(Expression.And(lhs, rhs), rest) // notice the recursion! This is why we need packrat.
        | ExpressionP(lhs, OWS(Str "==" (OWS(ExpressionP(rhs,rest))))) -> Some(Expression.And(lhs, rhs), rest) // notice the recursion! This is why we need packrat.
        | LiteralP(l, rest) -> Some(Expression.Literal l, rest)
        | DurgaToyContextP { executionContextName = Some ctxName } & NameP(owner, Str "." (NameP(memberName, rest))) when ctxName = owner -> Some(MemberReference(ContextReference, memberName), rest)
        | ExpressionP(owner, Str "." (NameP(memberName, rest))) -> Some(MemberReference(owner, memberName), rest) // notice the recursion! This is why we need packrat, so we can say stuff like foo.bar.baz, which is a member reference inside another member reference
        | NameP(name, rest) -> Some(VariableReference name, rest)
        | _ -> None
    let rec (|CaseBranchesP|_|) = function
        | OWS(Str "when" (ExpressionP(expr, StatementsP(statements, rest)))) -> Some([expr, statements], rest)
        | _ -> None
    and (|StatementP|_|) = function
        | OWS(Str "Case" (Word(caseName, Str ":" (CaseBranchesP(branches, rest))))) ->
            Some(Case(caseName, branches), rest)
        | OWS(Str "Create Task with name =" (OWS(StringLiteralP(String(name), rest)))) -> // notice how we use a pattern here to deconstruct the String data back to a .NET string. We could also have written it differently, e.g. have StringLiteralP return System.String instead of Model.Literal.String. Not much difference between them so use whichever one you think is more readable.
            Some(CreateTask(name), rest)
        | OWS(Str "Associate with case" rest) -> // notice how we use a pattern here to deconstruct the String data back to a .NET string. We could also have written it differently, e.g. have StringLiteralP return System.String instead of Model.Literal.String. Not much difference between them so use whichever one you think is more readable.
            Some(Associate, rest)
        | OWS(Str "Send email" rest) -> // notice how we use a pattern here to deconstruct the String data back to a .NET string. We could also have written it differently, e.g. have StringLiteralP return System.String instead of Model.Literal.String. Not much difference between them so use whichever one you think is more readable.
            Some(SendEmail, rest)
        | _ -> None
    and (|StatementsP|_|) = pack <| function
        | StatementsP(prev, StatementP(statement, rest)) -> Some(prev@[statement], rest) // notice the recursion! In this case, packrat is going to recur through StatementP, StatementsP, and possibly CaseBranches too. They're all intertwined.
        | StatementP(statement, rest) -> Some([statement], rest)
        | _ -> None
    let (|PrefaceP|_|) = function
        | OWS(Str "Inputs: Case Entity" (OWS(Str "(" (NameP(caseName, OWS(Str "," (OWS(Str "ExecutionContext" (OWS(NameP(contextName, Str ")" (OWS rest)))))))))))) & DurgaToyContextP ctx ->
            ctx.executionContextName <- Some contextName // remember the context name so we can use it later on in parsing.
            Some(Preface(InputDeclaration(caseName, contextName)), rest)
        | _ -> None
    let (|ProgramP|_|) = function
        | PrefaceP(p, StatementsP(statements, rest)) -> Some(Program(p, statements), rest)
        | _ -> None
    let parse input =
        match ParseArgs.Init(input, { executionContextName = None }) with
        | ProgramP(p, End) -> p
        | PrefaceP(p, (args, ix as rest)) ->
            failwithf "Invalid program: too much input.\nParsed:%A\nLeftover input: %s" p (args.input.Substring(ix))
        | _ -> failwithf "Invalid program: \n%s" input
    let txt = """
                when ctx.field1 == "abc"
                                Create Task with name = "sdfsdf"
                                Associate with case"""
    match ParseArgs.Init("ctx.field1 == \"abc\"", { executionContextName = None }) with
    | ExpressionP(p, OWS(End)) -> p
    | ExpressionP(p, (args, ix as rest)) ->
        printfn "%A\nLeftover input: %s" p (args.input.Substring(ix))
        failwith "Invalid program"
    match ParseArgs.Init(txt, { executionContextName = None }) with
    | CaseBranchesP(p, OWS(End)) -> p
    | CaseBranchesP(p, (args, ix as rest)) ->
        printfn "%A\nLeftover input: %s" p (args.input.Substring(ix))
        failwith "Invalid program"
    | _ -> failwithf "Invalid program: \n%s" txt

Parse.parse exampleTxt = example


module Execution =
    let execute input =
        printfn "Execution not implemented yet: \n%A" input