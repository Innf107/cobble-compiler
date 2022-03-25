# White Space Sensitivity

Some tokens (notably `of`, `=`, and `do`) initiate a new block context, though that context actually starts at the next token. There is also an implicit global block context, which starts at indentation level 0.

Whenever a token is encountered at the same level of indentation as the start of the block context, a `Dedent` token is emitted.

Whenever a token is encountered at a lower level of indentation, the lexer emits an `EndBlock` token and closes the current block.

EOF is treated just like a new token at indentation level 0.

## Example
All following occurrences of `;` represent `Dedent` tokens that have been inserted by the lexer. The `;` are usually placed at the end of the previous non-whitespace line, to avoid changing indentation level, though keep in mind that they are emitted, when the *next* token is encountered.

Also, `}` denotes an `EndBlock` token, with the same caveats as `;`. `{` is added for clarity whenever a block is opened, even though it is not actually emitted by the lexer. 
```hs
{
module test;

f :: Int -> Int;
f x = 
    {
    f();
    g();
    let x = 5; -- let expressions can be closed by `in` or `;`.
    let y = 6 in; -- both `in` and `;` (in that order) are also allowed.
    x + 1
    };

g :: Eq Int => Int -> Int;
g x = case x of {
    1 -> 1;
    2 -> do { -- case expressions unfortunately need an explicit do block, since we can't simply start a new block on (->), which would conflict with function types
        test();
        something()
    };
    3 -> do {
        f 3;
        g 2
    }
};

h :: Int
  -> Int -- No dedent, since no block has been opened
  -> String;
h x y = {x + 1
}; -- EndBlock and Dedent, because of EOF.
}
```


