# Farnsworth Grammar

This represents the grammar of a Farnsworth variation that does not rely on S-Lists.

### Primitive Types


**Numbers:**

```scala
1
```

**Characters:**

```scala
'a'
```

**Booleans:**

```scala
True
False
```

**Lists:**

```scala
[]
[1]
[1, 2]
[1, 3, 5, 7, 9]
```

**Strings:**

```scala
"foo"
```

### Conditionals

```ruby
if condition then x else y end
```

### Functions

**Named Functions:**

```rust
fn one = 1
fn add l r = l + r
```

**Anonymous Functions:**

```haskell
\ -> 1
\ l r -> l + r 
```

**Function Application:**

Function application is performed using an exclamation mark.

```
one!
add! 1 2
(nested!)!
```

### Pattern Matching

```rust
match x where
case 1 2
case 2 4
case _ 0
end
```


