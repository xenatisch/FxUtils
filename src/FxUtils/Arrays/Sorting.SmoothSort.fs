namespace FxUtils.Arrays

open System
open System.Runtime.CompilerServices


#if NETSTANDARD2_0
namespace System.Runtime.CompilerServices

open System

[<Sealed; AttributeUsage(AttributeTargets.Struct)>]
type IsByrefLikeAttribute() = inherit Attribute()
#endif


[<AutoOpen>]
module Array =
    
    let inline private generatedIsAscending (sample: 'a[]) =
        match box sample.[0] with
        | :? String  -> fun a b -> String.Compare(unbox a, unbox b, false) <= 0
        | _          -> fun a b -> unbox a <= b



    let inline private up (ia: int byref) (ib: int byref) =
        let tmp = ia
        ia      <- ia + ib + 1
        ib      <- tmp


    let inline private down (ia: int byref) (ib: int byref) =
        let tmp = ib
        ib      <- ia - ib - 1
        ia      <- tmp


    [<Struct; IsByRefLike>]
    type private SmoothSortAttribs<'a when 'a : comparison> =
        {
            mutable q   : int
            mutable r   : int
            mutable p   : int
            mutable b   : int
            mutable c   : int
            mutable r1  : int
            mutable b1  : int
            mutable c1  : int
            length      : int
            mutable A   : 'a array
            isAscending : 'a -> 'a -> bool
        }

        member inline this.sift =
            let mutable r2 = 0
            let r0         = this.r1
            let t          = this.A.[r0]

            while this.b1 >= 3 do
                r2 <- this.r1 - this.b1 + this.c1

                match this.isAscending this.A.[this.r1 - 1] this.A.[r2] with
                | true   -> ()
                | _      -> r2 <- this.r1 - 1
                            down &this.b1 &this.c1

                match this.isAscending this.A.[r2] t with
                | true   -> this.b1          <- 1
                | _      -> this.A.[this.r1] <- this.A.[r2]
                            this.r1          <- r2
                            down &this.b1 &this.c1

                match Convert.ToBoolean (this.r1 - r0) with
                | true   -> this.A.[this.r1] <- t
                | _      -> ()


        // Sift down operation
        // -------------------
        // Restores the heap invariant if violated only at the root
        // node. When root is less than its children, it gets swapped
        // with the greatest child and the process is repeated with
        // the root node in its new subtree.
        member inline this.trinkle =
            let r0         =  this.r1
            let mutable r2 =  0
            let mutable r3 =  0
            let mutable p1 =  this.p
            let t          =  this.A.[r0]
            this.b1        <- this.b
            this.c1        <- this.c

            while p1 > 0 do
                while (p1 &&& 1) = 0 do
                    p1 <- p1 >>> 1
                    up &this.b1 &this.c1

                r3 <- this.r1 - this.b1

                if (p1 = 1) || this.isAscending this.A.[r3] t then
                    p1 <- 0
                else
                    p1 <- p1 - 1

                    if this.b1 = 1 then
                        this.A.[this.r1] <- this.A.[r3]
                        this.r1          <- r3
                    elif this.b1 >= 3 then
                        r2 <- this.r1 - this.b1 + this.c1

                        match not (this.isAscending this.A.[this.r1 - 1] this.A.[r2]) with
                        | false  -> ()
                        | _      -> r2 <- this.r1 - 1
                                    down &this.b1 &this.c1
                                    p1 <- p1 <<< 1

                        match this.isAscending this.A.[r2] this.A.[r3] with
                        | true   -> this.A.[this.r1] <- this.A.[r3]
                                    this.r1          <- r3
                        | _      -> this.A.[this.r1] <- this.A.[r2]
                                    this.r1          <- r2
                                    down &this.b1 &this.c1
                                    p1               <- 0

            if Convert.ToBoolean(r0 - this.r1) then this.A.[this.r1] <- t

            this.sift


        member inline this.semiTrinkle =
            this.r1 <- this.r - this.c

            match this.isAscending this.A.[this.r1] this.A.[this.r] with
            | true   -> ()
            | _      -> let tmp          =  this.A.[this.r]
                        this.A.[this.r]  <- this.A.[this.r1]
                        this.A.[this.r1] <- tmp
                        this.trinkle



    let rec private leftShiftDown (s: SmoothSortAttribs<'a> byref) =
        down &s.b &s.c
        s.p <- s.p <<< 1

        match s.b with
        | x when x > 1 -> leftShiftDown &s
        | _            -> ()


    // Build sorted array
    let rec private buildSortArray (s: SmoothSortAttribs<'a> byref) =
        match s.q with
        | v when v < 2  -> ()
        | _             ->  s.q <- s.q - 1
                            match s.b with
                            | 1                  -> s.r <- s.r - 1
                                                    s.p <- s.p - 1

                                                    while (s.p &&& 1) = 0 do
                                                        s.p <- s.p >>> 1
                                                        up &s.b &s.c

                            | bb when bb >= 3    -> s.p <- s.p - 1
                                                    s.r <- s.r - s.b + s.c

                                                    match s.p with
                                                    | pp when pp > 0 -> s.semiTrinkle
                                                    | _              -> ()

                                                    down &s.b &s.c

                                                    s.p <- (s.p <<< 1) + 1
                                                    s.r <- s.r + s.c

                                                    s.semiTrinkle
                                                    down &s.b &s.c

                                                    s.p <- (s.p <<< 1) + 1

                            | _                  -> ()

                            buildSortArray &s
    
    /// <summary>
    /// Native implementation of Smooth Sort algorithm in F#.
    /// </summary>
    ///
    /// <remarks>
    /// Smoothsort:  an alternative for sorting in situ
    /// Reference: Dijkstra, E. Smoothsort, an alternative for sorting in situ [https://www.cs.utexas.edu/users/EWD/ewd07xx/EWD796a.PDF]
    /// 
    /// Unlike Quicksort, worst-case scenario is O(n log n).
    /// Unliked Mergesort, sorting occurs in-place.
    /// Unlike Heapsort, sorting is adaptive.
    ///
    /// Disadvantages:
    /// Sorting is not stable: The constant in O(n log n) can be quite big.
    /// The algorithm needs to hold all of the trees in the Leonardo heap in memory.
    ///
    /// From Wikipedia:
    /// The smoothsort algorithm needs to be able to hold in memory the sizes of all of the
    /// trees in the Leonardo heap. Since they are sorted by order and all orders are distinct,
    /// this is usually done using a bit vector indicating which orders are present. Moreover,
    /// since the largest order is at most O(log n), these bits can be encoded in O(1) machine
    /// words, assuming a transdichotomous machine model.
    /// 
    /// Note that O(1) machine words is not the same thing as one machine word. A 32-bit vector
    /// would only suffice for sizes less than L(32) = 7,049,155. A 64-bit vector will do for
    /// sizes less than L(64) = 34,335,360,355,129 ≈ 245.
    /// In general, it takes 1/log2(φ) ≈ 1.44 bits of vector per bit of size.
    /// </remarks>
    /// 
    /// <param name="A">Array to be sorted</param>
    /// <returns name="A">Sorted array. Note that the original array will be sorted too.
    /// The value is returned to facilitate piping.</returns>
    [<CompiledName("Smoothsort")>]
    let smoothsort<'a when 'a : comparison> (A: 'a[]) =
        let mutable s =
            {
                q           = 1 
                r           = 0
                p           = 1 
                b           = 1  
                c           = 1
                r1          = 0
                b1          = 0
                c1          = 0
                A           = A
                length      = A.Length
                isAscending = generatedIsAscending A
            }


        // Build the tree
        while s.q < s.length do
            s.r1 <- s.r

            match s.p with
            | pp when (pp &&& 7) = 3 -> s.b1 <- s.b
                                        s.c1 <- s.c
                                        s.sift
                                        s.p  <- (s.p + 1) >>> 2

                                        // Two ups
                                        up &s.b &s.c
                                        up &s.b &s.c

            | pp when (pp &&& 3) = 1 -> match s.q + s.c < s.length with
                                        | true   -> s.b1 <- s.b
                                                    s.c1 <- s.c
                                                    s.sift
                                        | _      -> s.trinkle

                                        leftShiftDown &s
                                        s.p <- s.p + 1
            | _                      -> ()

            s.q <- s.q + 1
            s.r <- s.r + 1

        s.r1 <- s.r
        s.trinkle
        buildSortArray &s

        A
