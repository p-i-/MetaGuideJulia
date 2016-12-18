π's Metaprogramming bits & bobs
===============================

(Note: most of the content/expertise is thanks to @fcard)

**Goals:**

 - Teach through minimal targeted functional/useful/non-abstract examples (e.g. `@swap` or `@assert`) that introduce concepts in suitable contexts

 - prefer Q&A style (or Socratic Dialogue style) -- it's the best for learning!

 - Prefer to let the code illustrate/demonstrate the concepts over paragraphs of explanation

 - Avoid linking 'required reading' to other pages -- it interrupts the narrative

 - Present things in a sensible order that will making learning easiest

ToDo:

 - more simple examples
 - exercises! 
   - "what would the following macro do?" 
   - design a macro to do this
   - diagnose and fix the following
   - etc.

**Resources:**

[julialang.org](http://docs.julialang.org/en/release-0.5/manual/metaprogramming/)  
[wikibook (@Cormullion)](https://en.wikibooks.org/w/index.php?title=Introducing_Julia/Metaprogramming)  
[5 layers (Leah Hanson)](http://blog.leahhanson.us/post/julia/julia-introspects.html)  
[SO-Doc Quoting (@TotalVerb)](http://stackoverflow.com/documentation/julia-lang/1945/metaprogramming/24364/quotenode-meta-quot-and-exprquote#t=201612052310514029068)  
[SO-Doc -- Symbols that are not legal identifiers (@TotalVerb)](http://stackoverflow.com/documentation/julia-lang/5817/string-macros/20504/symbols-that-are-not-legal-identifiers#t=201610090041530343729)  
[SO: What is a Symbol in Julia (@StefanKarpinski)](http://stackoverflow.com/questions/23480722/what-is-a-symbol-in-julia)  
[Discourse thread (@p-i-)](https://discourse.julialang.org/t/simple-metaprogramming-exercises-challenges)
http://stackoverflow.com/documentation/julia-lang/1945/metaprogramming#t=201612021739596755987  

Most of the material has come from the discourse channel, most of that has come from fcard... please prod me if I had forgotten attributions.

## **Symbol**

```julia
julia> mySymbol = Symbol("myName")  # or 'identifier'
:myName

julia> myName = 42
42

julia> mySymbol |> eval  # 'foo |> bar' puts output of 'foo' into 'bar', so 'bar(foo)'
42

julia> :( $mySymbol = 1 ) |> eval
1

julia> myName
1
```

**Passing flags into functions:**
```julia
function dothing(flag)
  if flag == :thing_one
    println("did thing one")
  elseif flag == :thing_two
    println("did thing two")
  end
end
julia> dothing(:thing_one)
did thing one

julia> dothing(:thing_two)
did thing two
```

**A hashkey example:**
```julia
number_names = Dict{Symbol, Int}()
number_names[:one] = 1
number_names[:two] = 2
number_names[:six] = 6
```

**(Advanced)**
(@fcard) `:foo` a.k.a. `:(foo)` yields a symbol if `foo` is a valid identifier, otherwise an expression.

```julia
# NOTE: Different use of ':' is:
julia> :mySymbol = Symbol('hello world')

#(You can create a symbol with any name with Symbol("<name>"), 
# which lets us create such gems as:
julia> one_plus_one = Symbol("1 + 1")
Symbol("1 + 1")

julia> eval(one_plus_one)
ERROR: UndefVarError: 1 + 1 not defined
...

julia> valid_math = :($one_plus_one = 3)
:(1 + 1 = 3)

julia> one_plus_one_plus_two = :($one_plus_one + 2)
:(1 + 1 + 2)

julia> eval(quote
           $valid_math
           @show($one_plus_one_plus_two)
       end)
1 + 1 + 2 = 5
...
```


Basically you can treat Symbols as lightweight strings. That's not what they're for, but you can do it, so why not. Julia's Base itself does it, `print_with_color(:red, "abc")` prints a red-colored abc .


## **Expr** (AST)

(Almost) everything in Julia is an expression, i.e. an instance of `Expr`, which will hold an [AST](http://docs.julialang.org/en/latest/devdocs/ast/).

```julia
# when you type ...
julia> 1+1
2

# Julia is doing: eval(parse("1+1"))
# i.e. First it parses the string "1+1" into an `Expr` object ...
julia> ast = parse("1+1")
:(1 + 1)

# ... which it then evaluates:
julia> eval(ast)
2

# An Expr instance holds an AST (Abstract Syntax Tree).  Let's look at it:
julia> dump(ast)
Expr
  head: Symbol call
  args: Array{Any}((3,))
    1: Symbol +
    2: Int64 1
    3: Int64 1
  typ: Any
  
# TRY: fieldnames(typeof(ast))
 
julia>      :(a + b*c + 1)  ==
       parse("a + b*c + 1") ==
       Expr(:call, :+, :a, Expr(:call, :*, :b, :c), 1)
true
```


**Nesting `Expr`s:**

```julia
julia> dump( :(1+2/3) )
Expr
  head: Symbol call
  args: Array{Any}((3,))
    1: Symbol +
    2: Int64 1
    3: Expr
      head: Symbol call
      args: Array{Any}((3,))
        1: Symbol /
        2: Int64 2
        3: Int64 3
      typ: Any
  typ: Any
  
# Tidier rep'n using s-expr
julia> Meta.show_sexpr( :(1+2/3) ) 
(:call, :+, 1, (:call, :/, 2, 3))
```

## multiline `Expr`s using **`quote`**

```julia
julia> blk = quote
           x=10
           x+1
       end
quote  # REPL[121], line 2:
    x = 10 # REPL[121], line 3:
    x + 1
end

julia> blk == :( begin  x=10; x+1  end )
true

# Note: contains debug info:
julia> Meta.show_sexpr(blk)
(:block,
  (:line, 2, Symbol("REPL[121]")),
  (:(=), :x, 10),
  (:line, 3, Symbol("REPL[121]")),
  (:call, :+, :x, 1)
)

# ... unlike:
julia> noDbg = :( x=10; x+1 ) 
quote 
    x = 10
    x + 1
end
```
... so `quote` is functionally the same but provides extra debug info.

(*) **TIP**: Use `let` to keep `x` within the block


## **`quote`** -ing a **`quote`**

`Expr(:quote, x)` is used to represent quotes within quotes.

    Expr(:quote, :(x + y)) == :(:(x + y))

    Expr(:quote, Expr(:$, :x)) == :(:($x))

`QuoteNode(x)` is similar to `Expr(:quote, x)` but it prevents interpolation.

    eval(Expr(:quote, Expr(:$, 1))) == 1

    eval(QuoteNode(Expr(:$, 1))) == Expr(:$, 1)

(http://stackoverflow.com/questions/41089019/disambiguate-the-various-quoting-mechanisms-in-julia-metaprogramming)

## Are **$** and **:(…)** somehow inverses of one another?

`:(foo)` means "don't look at the value, look at the expression"
`$foo` means "change the expression to its value"

`:($(foo)) == foo`. 
`$(:(foo))` is an error. 
`$(...)` isn't an operation and doesn't do anything by itself, it's an "interpolate this!" sign that the quoting syntax uses.  i.e. It only exists within a quote.


## Is **`$`**`foo` the same as **`eval(`**`foo`**`)`** ?

**No!** 
`$foo` is exchanged for the compile-time value
`eval(foo)` means to do that at runtime

`eval` will occur in the global scope
interpolation is local

`eval(:<expr>)` should return the same as just `<expr>` (assuming `<expr>` is a valid expression in the current global space)

    eval(:(1 + 2)) == 1 + 2

    eval(:(let x=1; x + 1 end)) == let x=1; x + 1 end



# **`macro`** s

Ready? :)

```julia
# let's try to make this!
julia> x = 5; @show x;
x = 5
```
## Let's make our own **`@show`** macro:

```julia
macro log(x)
  :(
    println( "Expression: ", $(string(x)), " has value: ", $x )
  )
end

u = 42
f = x -> x^2
@log(u)       # Expression: u has value: 42
@log(42)      # Expression: 42 has value: 42
@log(f(42))   # Expression: f(42) has value: 1764
@log(:u)      # Expression: :u has value: u
```

## **`expand`** to lower an **`Expr`**

[5 layers (Leah Hanson)](http://blog.leahhanson.us/post/julia/julia-introspects.html)  <-- explains how Julia takes source code as a string, tokenizes it into an `Expr`-tree (AST), expands out all the macros (still AST), ***lowers*** (lowered AST), then converts into LLVM (and beyond -- at the moment we don't need to worry what lies beyond!)

Q: `code_lowered` acts on functions. Is it possible to lower an `Expr`?
A: yup!
```julia
# function -> lowered-AST
julia> code_lowered(*,(String,String))
1-element Array{LambdaInfo,1}:
 LambdaInfo template for *(s1::AbstractString, ss::AbstractString...) at strings/basic.jl:84

# Expr(i.e. AST) -> lowered-AST
julia> expand(:(x ? y : z))
:(begin
        unless x goto 3
        return y
        3:
        return z
    end)

julia> expand(:(y .= x.(i)))
:((Base.broadcast!)(x,y,i))

# 'Execute' AST or lowered-AST
julia> eval(ast)
```

If you want to only expand macros you can use **`macroexpand`**:
```julia
# AST -> (still nonlowered-)AST but with macros expanded:
julia> macroexpand(:(@show x))
quote
    (Base.println)("x = ",(Base.repr)(begin  # show.jl, line 229:
                #28#value = x
            end))
    #28#value
end
```
...which returns a non-lowered AST but with all macros expanded.


## **`esc()`**

`esc(x)` returns an Expr that says "don't apply hygiene to this", it's the same as `Expr(:escape, x)`. Hygiene is what keeps a macro self-contained, and you `esc` things if you want them to "leak". e.g.

### Example: **`swap`** macro to illustrate **`esc()`**
```julia
macro swap(p, q)
  quote
    tmp = $(esc(p))
    $(esc(p)) = $(esc(q))
    $(esc(q)) = tmp
  end
end

x,y = 1,2
@swap(x,y)
println(x,y)  # 2 1
```
`$` allows us to 'escape out of' the `quote`.
So why not simply `$p` and `$q`?  i.e.
```julia
    # FAIL!
    tmp = $p
    $p = $q
    $q = tmp
```

Because that would look first to the `macro` scope for `p`, and it would find a local `p` i.e. the parameter `p` (yes, if you subsequently access `p` without `esc`-ing, the macro considers the `p` parameter as a local variable).

So `$p = ...` is just a assigning to the local `p`. it's not affecting whatever variable was passed-in in the calling context.

Ok so how about:
```julia
    # Almost!
    tmp = $p          # <-- you might think we don't 
    $(esc(p)) = $q    #       need to esc() the RHS
    $(esc(q)) = tmp
```

So `esc(p)` is 'leaking' `p` into the calling context.   *"The thing that was passed into the macro that **we receive as `p`**"*

```julia
julia> macro swap(p, q)                  
         quote                           
           tmp = $p                      
           $(esc(p)) = $q                
           $(esc(q)) = tmp               
         end                             
       end                               
@swap (macro with 1 method)              

julia> x, y = 1, 2                       
(1,2)                                    

julia> @swap(x, y);                      

julia> @show(x, y);                      
x = 2                                    
y = 1                                    

julia> macroexpand(:(@swap(x, y)))       
quote  # REPL[34], line 3:               
    #10#tmp = x # REPL[34], line 4:      
    x = y # REPL[34], line 5:            
    y = #10#tmp                          
end                                      
```
As you can see `tmp` gets the hygiene treatment `#10#tmp`, whereas `x` and `y` don't.
Julia is making a unique identifier for `tmp`, something you can manually do with `gensym`, ie:
```julia
julia> gensym(:tmp)
Symbol("##tmp#270")
```

But: **There is a gotcha:**

```julia
julia> module Swap
       export @swap

       macro swap(p, q)
         quote
           tmp = $p
           $(esc(p)) = $q
           $(esc(q)) = tmp
         end
       end
       end
Swap

julia> using Swap

julia> x,y = 1,2
(1,2)

julia> @swap(x,y)
ERROR: UndefVarError: x not defined
```
Another thing julia's macro hygiene does is, if the macro is from another module, it makes any variables (that were not assigned inside the macro's returning expression, like `tmp` in this case) globals of the current module, so `$p` becomes `Swap.$p`, likewise `$q` -> `Swap.$q`.

In general, if you need a variable that is outside the macro's scope you should esc it, so you should `esc(p)` and `esc(q)` regardless if they are on the LHS or RHS of a expression, or even by themselves.

people have already mentioned `gensym`s a few times and soon you will be seduced by the dark side of defaulting to escaping the whole expression with a few `gensym`s peppered here and there, but... Make sure to understand how hygiene works before trying to be smarter than it! It's not a particularly complex algorithm so it shouldn't take too long, but don't rush it! Don't use that power until you understand all the ramifications of it... (@fcard)

## Example: **`until`** macro

(@Ismael-VC)
```julia
"until loop"
macro until(condition, block)
    quote
        while ! $condition
            $block
        end
    end |> esc
end

julia> i=1;  @until(  i==5,  begin; print(i); i+=1; end  )
1234

```


(@fcard)  `|>` is controversial, however. I am surprised a mob hasn't come to argue yet. (maybe everyone is just tired of it).  There is a recommendation of having most if not all of the macro just be a call to a function, so:
```julia
macro until(condition, block)
    esc(until(condition, block))
end

function until(condition, block)
    quote
        while !$condition
            $block
        end
    end
end
```
...is a safer alternative.

##@fcard's simple macro challenge

Task: Swap the operands, so `swaps(1/2)` gives `2.00` i.e. `2/1`
```julia
macro swaps(e)
    e.args[2:3] = e.args[3:-1:2]   
    e
end
@swaps(1/2)
2.00
```

More macro challenges from @fcard [here](https://gist.github.com/fcard/b735e642e6613feffad2d00f3c4298bd)


---

## Interpolation and `assert` macro

http://docs.julialang.org/en/release-0.5/manual/metaprogramming/#building-an-advanced-macro
```julia
macro assert(ex)
    return :( $ex ? nothing : throw(AssertionError($(string(ex)))) )
end
```
Q: Why the last `$`?
A: It interpolates, i.e. forces Julia to `eval` that `string(ex)` as execution passes through the invocation of this macro.
i.e. If you just run that code it won't force any evaluation. But the moment you do `assert(foo)` Julia will ***invoke*** this macro replacing its 'AST token/Expr' with whatever it returns, and the `$` _will_ kick into action.

### A fun hack for using { } for blocks

(@fcard) I don't think there is anything technical keeping `{}` from being used as blocks, in fact one can even pun on the residual `{}` syntax to make it work:
```julia
julia> macro c(block)
         @assert block.head == :cell1d
         esc(quote
           $(block.args...)
         end)
       end
@c (macro with 1 method)

julia> @c {
         print(1)
         print(2)
         1+2
       }
123
```
*(unlikely to still work if/when the {} syntax is repurposed)

---


**_So first Julia sees the macro token, so it will read/parse tokens until the matching `end`, and create what?  An `Expr` with `.head=:macro` or something?  Does it store `"a+1"` as a string or does it break it apart into `:+(:a, 1)`?  How to view?_**

?

(@fcard) In this case because of lexical scope, a is undefined in `@M`s scope so it uses the global variable...
I actually forgot to escape the flipplin' expression in my dumb example, but the *"only works within the same module"* part of it still applies.
```julia
julia> module M
       macro m()
         :(a+1)
       end
       end
M

julia> a = 1
1

julia> M.@m
ERROR: UndefVarError: a not defined
```
The reason being that, if the macro is used in any module other than the one it was defined in, any variables not defined within the code-to-be-expanded are treated as globals of the macro's module.
```julia
julia> macroexpand(:(M.@m))
:(M.a + 1)
```

- - - 

# ***ADVANCED***

###@Ismael-VC
```julia
@eval begin
    "do-until loop"
    macro $(:do)(block, until::Symbol, condition)
        until ≠ :until && 
            error("@do expected `until` got `$until`")
        quote
            let
                $block
                @until $condition begin
                    $block
                end
            end
        end |> esc
    end
end
julia> i = 0            
0                       

julia> @do begin        
           @show i      
           i += 1       
       end until i == 5 
i = 0                   
i = 1                   
i = 2                   
i = 3                   
i = 4
```

### Scott's macro:

```
"""
Internal function to return captured line number information from AST

##Parameters
- a:     Expression in the julia type Expr

##Return

- Line number in the file where the calling macro was invoked
"""
_lin(a::Expr) = a.args[2].args[1].args[1]

"""
Internal function to return captured file name information from AST

##Parameters
- a:     Expression in the julia type Expr

##Return
- The name of the file where the macro was invoked
"""
_fil(a::Expr) = string(a.args[2].args[1].args[2])

"""
Internal function to determine if a symbol is a status code or variable
"""
function _is_status(sym::Symbol)
    sym in (:OK, :WARNING, :ERROR) && return true
    str = string(sym)
    length(str) > 4 && (str[1:4] == "ERR_" || str[1:5] == "WARN_" || str[1:5] == "INFO_")
end

"""
Internal function to return captured error code from AST

##Parameters
- a:     Expression in the julia type Expr

##Return
- Error code from the captured info in the AST from the calling macro
"""
_err(a::Expr) =
    (sym = a.args[2].args[2] ; _is_status(sym) ? Expr(:., :Status, QuoteNode(sym)) : sym)

"""
Internal function to produce a call to the log function based on the macro arguments and the AST from the ()->ERRCODE anonymous function definition used to capture error code, file name and line number where the macro is used

##Parameters
- level:     Loglevel which has to be logged with macro
- a:         Expression in the julia type Expr
- msgs:      Optional message

##Return
- Statuscode
"""
function _log(level, a, msgs)
    if isempty(msgs)
        :( log($level, $(esc(:Symbol))($(_fil(a))), $(_lin(a)), $(_err(a)) )
    else
        :( log($level, $(esc(:Symbol))($(_fil(a))), $(_lin(a)), $(_err(a)), message=$(esc(msgs[1]))) )
    end
end

macro warn(a, msgs...)  ; _log(Warning, a, msgs) ; end
```

# ***junk / unprocessed ...***

### **view**/**dump** a macro

(@p-i-) Suppose I just do `macro m(); a+1; end` in a fresh REPL. With no `a` defined. How can I ‘view’ it?
like, is there some way to ‘dump’ a macro?
Without actually executing it

(@fcard) All the code in macros are actually put into functions, so you can only view their lowered or type-inferred code.
```julia
julia> macro m()  a+1  end
@m (macro with 1 method)

julia> @code_typed @m
LambdaInfo for @m()
:(begin 
        return Main.a + 1
    end)

julia> @code_lowered @m  
CodeInfo(:(begin
        nothing
        return Main.a + 1
    end))
# ^ or: code_lowered(eval(Symbol("@m")))[1] # ouf!
```
    
Other ways to get a macro's function:
```julia
julia> macro getmacro(call) call.args[1] end
@getmacro (macro with 1 method)

julia> getmacro(name) = getfield(current_module(), name.args[1])
getmacro (generic function with 1 method)

julia> @getmacro @m
@m (macro with 1 method)

julia> getmacro(:@m)
@m (macro with 1 method)
```

```julia
julia> eval(Symbol("@M"))
@M (macro with 1 method)

julia> dump( eval(Symbol("@M")) )
@M (function of type #@M)

julia> code_typed( eval(Symbol("@M")) )
1-element Array{Any,1}:
 LambdaInfo for @M()

julia> code_typed( eval(Symbol("@M")) )[1]
LambdaInfo for @M()
:(begin 
        return $(Expr(:copyast, :($(QuoteNode(:(a + 1))))))
    end::Expr)

julia> @code_typed @M
LambdaInfo for @M()
:(begin 
        return $(Expr(:copyast, :($(QuoteNode(:(a + 1))))))
    end::Expr)
```
^ looks like I can use `code_typed` instead


### How to understand `eval(Symbol("@M"))`?

(@fcard) Currently, every macro has a function associated with it. If you have a macro called `M`, then the macro's function is called `@M`. Generally you can get a function's value with e.g. `eval(:print)` but with a macro's function you need to do `Symbol("@M")`, since just `:@M` becomes an `Expr(:macrocall, Symbol("@M"))` and evaluating that causes a macro-expansion.

## Why doesn't **`code_typed`** display params?

(@p-i-) 
```julia
julia> code_typed( x -> x^2 )[1]
LambdaInfo for (::##5#6)(::Any)
:(begin 
        return x ^ 2
    end)
```
^ here I see one `::Any` param, but it doesn't seem to be connected with the token `x`.
```julia
 julia> code_typed( print )[1]
LambdaInfo for print(::IO, ::Char)
:(begin 
        (Base.write)(io,c)
        return Base.nothing
    end::Void)
```
^ similarly here; there is nothing to connect `io` with the `::IO`
So surely this can't be a complete dump of the AST representation of that particular `print` method…?

(@fcard)  `print(::IO, ::Char)` only tells you what method it is, it's not part of the AST.
It isn't even present in master anymore:
```julia
julia> code_typed(print)[1]
CodeInfo(:(begin
        (Base.write)(io,c)
        return Base.nothing
    end))=>Void
```

(@p-i-) I don't understand what you mean by that. It seems to be dumping the AST for the body of that method, no?
I thought `code_typed` gives the AST for a function. But it seems to be missing the first step, i.e. setting up tokens for params.

(@fcard) `code_typed` is meant to only show the body's AST, but for now it does give the complete AST of the method, in the form of a `LambdaInfo` (0.5) or `CodeInfo` (0.6), but a lot of the information is omitted when printed to the repl. You will need to inspect the `LambdaInfo` field by field in order to get all the details. `dump` is going to flood your repl, so you could try:

```julia
macro method_info(call)
  quote
    method = @code_typed $(esc(call))
    print_info_fields(method)
  end
end

function print_info_fields(method)
  for field in fieldnames(typeof(method))
    if isdefined(method, field) && !(field in [Symbol(""), :code])
      println("  $field = ", getfield(method, field))
    end
  end
  display(method)
end

print_info_fields(x::Pair) = print_info_fields(x[1])
```

Which gives all the values of the named fields of a method's AST:

```julia
julia> @method_info print(STDOUT, 'a')
  rettype = Void
  sparam_syms = svec()
  sparam_vals = svec()
  specTypes = Tuple{Base.#print,Base.TTY,Char}
  slottypes = Any[Base.#print,Base.TTY,Char]
  ssavaluetypes = Any[]
  slotnames = Any[Symbol("#self#"),:io,:c]
  slotflags = UInt8[0x00,0x00,0x00]
  def = print(io::IO, c::Char) at char.jl:45
  nargs = 3
  isva = false
  inferred = true
  pure = false
  inlineable = true
  inInference = false
  inCompile = false
  jlcall_api = 0
  fptr = Ptr{Void} @0x00007f7a7e96ce10
LambdaInfo for print(::Base.TTY, ::Char)
:(begin
        $(Expr(:invoke, LambdaInfo for write(::Base.TTY, ::Char), :(Base.write), :(io), :(c)))
        return Base.nothing
    end::Void)
```
See the lil' `def = print(io::IO, c::Char)`? There you go! (also the `slotnames = [..., :io, :c]` part)
Also yes, the difference in output is because I was showing the results on master.

## ???

(@Ismael-VC) you mean like this?
http://stackoverflow.com/questions/39314925/generic-dispatch-with-symbols/39315024#39315024

You can do it this way:
```julia
julia> function dispatchtest{alg}(::Type{Val{alg}})
           println("This is the generic dispatch. The algorithm is $alg")
       end
dispatchtest (generic function with 1 method)

julia> dispatchtest(alg::Symbol) = dispatchtest(Val{alg})
dispatchtest (generic function with 2 methods)

julia> function dispatchtest(::Type{Val{:Euler}})
           println("This is for the Euler algorithm!")
       end
dispatchtest (generic function with 3 methods)

julia> dispatchtest(:Foo)
This is the generic dispatch. The algorithm is Foo

julia> dispatchtest(:Euler)
```

This is for the Euler algorithm!
I wonder what does @fcard thinks about generic symbol dispatch! ---^ :angel:



### Module Gotcha
```julia
@def m begin
  a+2
end

@m # replaces the macro at compile-time with the expression a+2
```

More accurately, only works within the toplevel of the module the macro was defined in.
```julia
julia> module M
       macro m1()
         a+1
       end
       end
M

julia> macro m2()
         a+1
       end
@m2 (macro with 1 method)

julia> a = 1
1

julia> M.@m1
ERROR: UndefVarError: a not defined

julia> @m2
2

julia> let a = 20
         @m2
       end
2
```
`esc` keeps this from happening, but defaulting to always using it goes against the language design.
A good defense for this is to keep one from using and introducing names within macros, which makes them hard to track to a human reader.
