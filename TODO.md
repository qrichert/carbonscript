# TODO

Use slots for dataclasses:
https://stackoverflow.com/a/69661861

Use named tuples for frames and environment + add slots to scope/frames
(and any objects that may be instantiated a lot of times.)

https://stackoverflow.com/a/16571630

builtins.py (sqrt, stddev, etc.)

https://blog.jetbrains.com/pycharm/2013/06/textmate-bundles-in-pycharm/
https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide
https://blog.expo.dev/building-a-code-editor-with-monaco-f84b3a06deaf

## Code examples

```
func do_something()
    a = 2
    b = 4
    return a + b ** a


test a_plus_b_eq_c()
    a = 1
    b = 2
    c = 3
    expect a + b == c


while (true)
    ...
    for (x in [])
        ...

## test MUST have an expect keyword
OR contain subtests ##

test test_group()
    # setup

    test first_test()
        ...

    test other_test()
        ...

    # teardown
```
