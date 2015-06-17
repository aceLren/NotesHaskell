struct ServArgs {
    1: bool myFlag,
    2: i32 num1,
    3: i32 num2
}

service Serv {
    void ping(),
    i32 myOp(1: ServArgs a, 2: string other)
}
