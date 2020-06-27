module Model

type InputDeclaration = InputDeclaration of caseName: string * contextName: string
type Preface = Preface of InputDeclaration
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
