Case For Functional
===

References
---
[f# article](http://fsharpforfunandprofit.com/posts/is-your-language-unreasonable/)

Languages Should Be Predictable
===

Example 1
---

What is the value of y?

    var x = 2;
    DoSomething(x);

    // What is the value of y?
    var y = x - 1;

---

The answer: -1.  This is javascript:

    function DoSomething(foo) { x = false; }

    var x = 2;
    DoSomething(x);
    var y = x - 1;

It accesses x directly, turns it to a bool which is cast automatically to 0.

How to make your language predictable
---

1. Variables should not be allowed to change their type.

Example 2
---

Are these equal?

    // create two customers
    var cust1 = new Customer(99, "J Smith")
    var cust2 = new Customer(99, "J Smith")

    // true or false?
    cust1.Equals(cust2)

---

Who knows?  It depends on how it is implemented.

-   How often would you NOT want the instances to be equal?
-   How often have you had to override the Equals method?
-   How often have you had a bug caused by forgetting to override the Equals method?
-   How often have you had a bug caused by mis-implementing GetHashCode (such as forgetting to change it when the fields that you compare on change)?

How to make your language predictable
---

1. Variables should not be allowed to change their type.
2. Objects containing the same values should be equal by default.

Example 3
---

Are these equal?

    // create a customer and an order
    var cust = new Customer(99, "J Smith")
    var order = new Order(99, "J Smith")

    // true or false
    cust.Equals(order);

How to make your language predictable
---

Who cares?  This shouldn't happen!  Definitely does when comparing subclasses.

1. Variables should not be allowed to change their type.
2. Objects containing the same values should be equal by default.
3. Comparing objects of different types is a compile-time error.

Example 4
---

What is this?

    // create a customer
    var cust = new Customer();

    // ?
    Console.WriteLine(cust.Address.Country);

How to make your language predictable
---

Who knows, depends on if the constructor has defaults, etc.

1. Variables should not be allowed to change their type.
2. Objects containing the same values should be equal by default.
3. Comparing objects of different types is a compile-time error.
4. Objects must always be initialized to a valid state. Not doing so is a compile-time error.

Example 5
---

Does this set have our customer?

    // create a customer
    var cust = new Customer(99, "J Smith");

    // add it to a set
    var processedCustomers = new HashSet<Customer>();
    processedCustomers.Add(cust);

    // process it
    ProcessCustomer(cust);

    // Does the set contain the customer? true or false?
    processedCustomers.Contains(cust);

---

Maybe. Maybe not.

It depends on two things:

1. the hash code of the customer depend on a mutable field, such as an id.
2. does ProcessCustomer change this field?

If it were immutable, no problemo.

How to make your language predictable
---

1. Variables should not be allowed to change their type.
2. Objects containing the same values should be equal by default.
3. Comparing objects of different types is a compile-time error.
4. Objects must always be initialized to a valid state. Not doing so is a compile-time error.
5. Once created, objects and collections must be immutable.

Example 6
---

What will be the output?

    // create a repository
    var repo = new CustomerRepository();

    // find a customer by id
    var customer = repo.GetById(42);

    // what is the expected output?
    Console.WriteLine(customer.Id);

How to make your language predictable
---

It depends.  Might return customer (signature) but also might be null.

1. Variables should not be allowed to change their type.
2. Objects containing the same values should be equal by default.
3. Comparing objects of different types is a compile-time error.
4. Objects must always be initialized to a valid state. Not doing so is a compile-time error.
5. Once created, objects and collections must be immutable.
6. No nulls allowed.
7. Missing data or errors must be made explicit in the function signature.

---

Other things!: globals, side-effects, casting, etc.

The above is in many cases anti-OOP.
