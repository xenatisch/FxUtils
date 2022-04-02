module FxUtils.Arrays.Tests

open FxUtils.Arrays
open NUnit.Framework


[<TestFixture>]
type TestSmoothSort () =
        
    [<Test>]
    member this.TestFloatingPointArray() =
        let longDigit = 0.2 + 0.1  // 0.30000000000000004
        
        let arr      = [|9.; 3.; 5.; 0.3; 2.; 4.; 8.; 1.; 10.; 15.; 27.; -1.; 12.; 5.; longDigit; 13.|]
        let expected = [|-1.; 0.3; longDigit; 1.; 2.; 3.; 4.; 5.; 5.; 8.; 9.; 10.; 12.; 13.; 15.; 27.|]
        
        let result =
            arr
            |> Array.smoothsort
        
        Assert.That(result, Is.EqualTo(expected))
        
        // Ensure that original array has changed.
        Assert.That(result, Is.EqualTo(arr))
      
    [<Test>]  
    member this.TestStringArray() =
        let arr =
            "the quick brown fox jumps over the lazy dog"
                .Split " "
        
        let expected = [|"brown"; "dog"; "fox"; "jumps"; "lazy"; "over"; "quick"; "the"; "the"|]
        
        let result =
            arr
            |> Array.smoothsort
        
        Assert.That(result, Is.EqualTo(expected))
        
        // Ensure that original array has changed.
        Assert.That(result, Is.EqualTo(arr))
        
    [<Test>]  
    member this.TestIntegerArray() =
        let arr      = [|9; 3; 5; 2; 4; 8; 1; 10; 15; 27; -1; 12; 5; 13|]
        let expected = [|-1; 1; 2; 3; 4; 5; 5; 8; 9; 10; 12; 13; 15; 27|]
        
        let result =
            arr
            |> Array.smoothsort
        
        Assert.That(result, Is.EqualTo(expected))

        // Ensure that original array has changed.
        Assert.That(result, Is.EqualTo(arr))

    [<Test>]  
    member this.TestRandomArray() =
        let count = 100_000
        let rnd   = System.Random()
        let arr   = Array.init count (fun _ -> rnd.Next ())
        
        // Use built-in sorting algorithm to produce
        // a sorted version of the array.
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let expected =
            arr
            |> Array.sort
        stopWatch.Stop()
        printfn "internal %f" stopWatch.Elapsed.TotalMilliseconds

        
        // Ensure original array hasn't changed.
        Assert.AreNotEqual(arr, expected)

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let result =
            arr
            |> Array.smoothsort
        stopWatch.Stop()
        printfn "smooth %f" stopWatch.Elapsed.TotalMilliseconds
        Assert.That(result, Is.EqualTo(expected))
        
        // Ensure that original array has changed.
        Assert.That(result, Is.EqualTo(arr))

    
    [<Test>]
    member this.TestRandomStringArray() =
        let count = 100_000
        let rnd   = System.Random()
        let arr   = Array.init count (fun _ -> $"{rnd.Next ()}")
        
        // Use built-in sorting algorithm to produce
        // a sorted version of the array.
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let expected =
            arr
            |> Array.sort
        stopWatch.Stop()
        printfn "internal %f" stopWatch.Elapsed.TotalMilliseconds

        
        // Ensure original array hasn't changed.
        Assert.AreNotEqual(arr, expected)

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let result =
            arr
            |> Array.smoothsort
        stopWatch.Stop()
        printfn "smooth %f" stopWatch.Elapsed.TotalMilliseconds
        Assert.That(result, Is.EqualTo(expected))
        
        // Ensure that original array has changed.
        Assert.That(result, Is.EqualTo(arr))
