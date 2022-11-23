# Step 1:

**Grab input from user, up to 10 numbers, each of those numbers can be random (-2) or can be the end of the array (-1)
These should be 16-bit signed integers. (Same as the indec proc given)**

### Persistent data:

```c
int[] array;
```

```c
int[] tempArray; // This is to store values for array functions.
```

```c
int arrLen;
```

# Step 2:

**Process each individual input in as many ways as possible. Make sure that each of these operations works on positive, negative, and 0. Pass values on stack, call function, then print result of function if applicable.**

- These should use as LITTLE data from the input as possible, focus on making them general purpose.

- Pass arguments on stack (x)

- Return values in ax, bx, cx, dx, in order, depending on how many words need to be returned.

- Return 1 for True in boolean functions, and 0 for False. 

- Don't save something like isNegative at a memory location, check the first bit, or create a macro for it.

- Don't save result to a memory location, return it in a register.

- Prioritize returning values where possible, only print directly in procedure when it's necessary (like converting bases)

- Last bit = least-significant

- First bit = most-significant

## NECESSARY:

```c
int square(int x) { if (x < 181) return x * x } 
// 181^2 is roughly the 16-bit signed integer limit
```

```c
int necessaryBits(int x) {
    if (x == 0) {return 1;}
    for (int i = 1; i < 16; i++) {
        if (2**i >= x) return i+1; // Add one for sign bit
    }
}
```

```c
void printBinary(int x) {
    for (int i = 1; i <= necessaryBits(x); i++) {
        firstBit = x & 0x8000; // Bit mask for first bit
        if (firstBit > 0) printf('1');
        else printf('0')
        x = x<<1;
    }
}
```

```c
void printOctal(int x) {
    // Adding 2 before division so that it rounds up instead of down.
    for (int i = 1; i <= (necessaryBits(x)+2)/3; i++) {
        firstChunk = x & 0xE000; // Bit mask for first 2 bits
        printf("%d", firstChunk>>13);
        x<<3
    }
} 

```

```c
void printHexadecimal(int x){
    // Adding a 3 before division so it rounds up instead of down.
    for (int i = 1; i <= (necessaryBits(x)+3)/4; i++) {
        firstChunk = x & 0xF000; // Bit mask for first 4 bits
        printf("%d", firstChunk>>12); // Move first 4 bits to last 4 bits
        x<<2
    }
}
```

```c
void printDecimal(int x) {
    indec(x);
}
```

## OPTIONAL:

```c
bool isPrime(int x) {
  if (x > 1) {
    for (int i = 2; i <= x/2; ++i) {
      if (x % i == 0) { 
        return 0;
      }
    }
  }
return 1;
}
```

```c
int isPerfectSquare(int x) {
if (x == 1) return true;
if (x == 0) return true;
  if (x > 1) {
    for (int i = 2; i <= x/2; ++i) {
      if (square(i) == x) { 
        return 1;
      }
    }
  }
return 0;
}
```

```c
int isEven(int x) {
    if ((x & 0x01) > 0) return 0;
    return 1
}
```

# Step 3:

**Process the array itself in as many ways as possible. (These are extra)**

```c
void printArr(int[] arr, int arrLen) {
    for (int i = 0; i < arrLen; i++) {
        printDecimal(arr[i]);
    }
}
```

```c
int sumArr(int[] arr, int arrLen) {
    int sum = 0;
    for (int i = 0; i < arrLen; i++) {
        sum += arr[i];
    }
}
```

```c
void reverseArr(int[] arr, int[] reverseArr, int arrLen) {
    for (int i = 1; i <= arrLen; i++) {
        reverseArr[arrLen-i] = arr[i-1];
    }
}
```



```c
void swapArr(int* num1 int* num2) {
    tempInt = *num1;
    *num1 = *num2;
    *num2 = tempInt;
}
```

```c
// This is ambitious but I feel like if we manage it, it'll make our project more impressive.
void sortArr(int[] arr, int arrLen) {
    for (int i = 0; i < arrLen-1; i++) {
        for (int j = 0; j < arrLen-i-1; i++) {
            if (arr[j] > arr[j+1]) swapArr(&arr[j], &arr[j+1])
        }
    }
}
```
