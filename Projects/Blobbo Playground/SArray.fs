module Prime.SArray

let init length initializer =
    let a = SArray.zeroCreate length
    for i in 0 .. dec length do a[i] <- initializer i
    a

let replicate length x =
    let a = SArray.zeroCreate length
    for i in 0 .. dec length do a[i] <- x
    a