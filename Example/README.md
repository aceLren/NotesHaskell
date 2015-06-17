Example
===

Overview
---

Here is a sample project / problem.  It's a linear program - we're trying to
minimize our food cost while getting at least 700% total of each vitamin (if
you look in VitaminContent.csv it has the percentage of each vitamin for each
food).  Each food has to be greater than or equal to zero.

I've set it up like I like to do Haskell projects - including all kinds of libraries
just so we can get familiar (so yes, running cabal install will take a while).  At its core
it's a service with a web front end using browser technologies.
I've forked Thrift to make writing a websocket server easier. Included below is
how to install that version.

Also I've included an excel file and a csv.  In this problem we'll read both.
You could use a csv library, but for this use attoparsec (already in the cabal
dependencies file).  For excel, use [xlsx](https://hackage.haskell.org/package/xlsx-0.1.0.4/docs/Codec-Xlsx.html).

Database
---

I've included an ORM library called Persistent for automatically storing data in
the database.  It's data type definition looks different than normal because it
uses template Haskell.  We probably won't have time to get into that much, but
this should at least give you a simple look at what that looks like in projects.

Thrift
---

You can install my websocket library by cloning my thrift fork:

    git clone https://github.com/aceLren/thrift.git clone_dir

I've already used Thrift to generate source files for us, but if you want to do it
install thrift and then:

    thrift --gen hs -o lib/ Serv.thrift
    thrift --gen js -o lib/ Serv.thrift

Run Example
---
Then getting the Example to run should be as simple as:

    cd ExampleDir
    mkdir serv-dev/resources/db
    ln -s serv-dev/lib serv-dev/web

    cabal sandbox init
    cabal install clone_dir/lib/hs
    // This install will give you an error, ignore it
    cabal install
    cabal run

Certs
---

If you want to try to set up tls, create a crt and key using openssl. The commented
code looks for them in serv-dev/resources/certs/
