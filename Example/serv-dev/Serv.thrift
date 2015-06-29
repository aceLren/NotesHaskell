struct ServArgs {
    1: bool myFlag,
    2: i32 num1,
    3: i32 num2
}

service Serv {
    void ping(),
    i32 myOp(1: ServArgs a, 2: string other)
    bool saveFood(1:string nm, 2:double cost, 3:double a, 4:double c, 5:double b1, 6:double b2)
}
