ScalaStan
=========
A Scala DSL for [Stan](http://mc-stan.org).

Requirements
============
ScalaStan depends on [Scala](http://www.scala-lang.org), [SBT](http://www.scala-sbt.org), and
[CmdStan](http://mc-stan.org/users/interfaces/cmdstan).
On macOS, these can be installed via [Homebrew](https://brew.sh):
```
brew install cmdstan sbt
```

Project Structure
=================
 - `com.cibo.scalastan` contains the ScalaStan DSL (most importantly, the `ScalaStan` trait).
 - `com.cibo.scalastan.data` contains parsers for various data sources (R, for example).
 - `com.cibo.scalastan.models` contains reusable ScalaStan models.
 - Examples can be found in the `com.cibo.scalastan.examples` package.

Usage
=====
The ScalaStan DSL is accessed by extending the `com.cibo.scalastan.ScalaStan` trait.
Here's a simple example of linear regression:
```scala
import com.cibo.scalastan.ScalaStan

object MyModel extends App with ScalaStan {
  val n = data(int(lower = 0))
  val x = data(vector(n))
  val y = data(vector(n))

  val b = parameter(real())
  val m = parameter(real())
  val sigma = parameter(real(lower = 0))

  val model = new Model {
    y ~ Normal(m * x + b, sigma)
  }

  val xs: Vector[Double] = ???
  val ys: Vector[Double] = ???
  val results = model
    .withData(n, xs.length)
    .withData(x, xs)
    .withData(y, ys)
    .run()

  println("y = mx + b")
  println(s"m = ${results.best(m)}")
  println(s"b = ${results.best(b)}")
  println(s"sigma = ${results.best(sigma)}")
}
```

For comparison, the equivalent Stan program (without the data setup/output) is:
```stan
data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}
parameters {
  real b;
  real m;
  real<lower = 0> sigma;
}
model {
  y ~ normal(m * x + b, sigma);
}
```

Data Declarations
-----------------
Data declarations define the inputs to the model.
These go in the `data` section in Stan:
```stan
data {
  real<lower=0> x;
}
```

Using ScalaStan, this is encoded as:
```scala
val x = data(real(lower = 0))
```

The `lower` and `upper` bounds on values are optional in ScalaStan, just as in regular Stan.  ScalaStan supports
the following data types:

 - `int([lower], [upper])` // An integer
 - `real([lower], [upper])` // A real/double
 - `vector(length, [lower], [upper])` // A (column) vector of reals
 - `rowVector(length, [lower], [upper])` // A row vector of reals
 - `matrix(rows, cols, [lower], [upper])` // A matrix of reals

Arrays can be created for any data type with multiple dimensions by calling `apply` on the type.
For example, to create a 2-dimensional array of `int`:

```scala
int()(j, k)
```

Parameter Declarations
----------------------
Parameter declarations define the outputs of the model.  In Stan, these go in the `parameters` section:
```stan
parameters {
  real y;
}
```

Using ScalaStan, this is encoded as:
```scala
val y = parameter(real())
```

The types for parameters are the same as those for data.

The Model
---------
The model is encoded by extending the `Model` class. The body of this class supports a DSL to describe the model.

#### Local Declarations
Local values can be declared using the `local` function, which behaves like the `data` and `parameter` functions,
but is only available within the `Model` DSL (and other code DSLs), for example:
```scala
val z = local(real())
```

#### Operators
Most arithmetic operators behave as one would expect (and identically to Stan), for example `+`, `-`, `*`, `/`,
and `%`.

The logical operators `<`, `<=`, `>`, `>=` operate as expected, however, note that test for equality
is `===` (three `=`s instead of `==`) and test for inequality is `=/=` (instead of `!=`).

Stan supports element-wise multiplication and division, which are `:*` and `:/` in ScalaStan.

The power operator is `^`.

The transpose operator is `.t` (for example, `x.t` to transpose `x`).

#### Assignment
The assignment operator is `:=` (instead of `=`).

#### For Loops
For loops use the standard Scala syntax, but using the `range` function as a generator.  For example:
```scala
for (i <- range(1, n)) {
  // ...
}
```
This will cause `i` to take on the values `1` through `n` inclusive.
Note that arrays are indexed starting from one in Stan.

#### Conditionals
Conditions take the form `when` and `otherwise` to avoid conflicting with the Scala `if` and `else` statements.
For example:
```scala
when(x > 1) {
  // ...
} otherwise {
  // ...
}
```
The `otherwise` section is optional.  Additional `when` statements can be added to provide an "else if"
structure, but note the required `.`:
```scala
when(x > 1) {
  // ...
}.when(x < 0) {
  // ...
} otherwise {
  // ...
}
```

#### Distributions
Most of the built-in Stan distributions are available in ScalaStan.  The naming convention uses an initial
upper-case letter (for example, the Beta distribution is `Beta`).

To indicate that a value is sampled from a distribution, the `~` operator is used, for example:
```scala
y ~ Normal(0.0, 1.0)
```

This is equivalent to the following:
```scala
target += Normal(0.0, 1.0).lpdf(y)
```

#### Other Functions
Most of the built-in Stan functions are available in ScalaStan.  The naming convention uses camel-case instead
of underscores.


Data and Parameter Transforms
-----------------------------
Transformed versions of data and parameters can be declared via the `transformed data` and `transformed parameters`
sections in Stan. In ScalaStan, this is accomplished by extending the `DataTransform` or `ParameterTransform`
classes.  These classes take a parameter that is the type of the transformed value.  The body of the class
provides a DSL to encode the transformation using the same constructs as the model.  Inside this DSL, the
value is accessed using `result`.

Here is a simple data transform to add `1` to all elements of an array:
```scala
val xPlusOne = new ParameterTransform(vector(n)) {
  result := x + 1
}
```

Now, the transformed parameter can be referenced instead of the actual parameter.

User-Defined Functions
----------------------
User-defined functions are created by extending the `Function` class. This class takes an optional parameter to
determine the return type (it is assumed to return `void` if not specified).  The body of the class provides
a DSL for implementing the function.  Inputs to the function are specified using the `input` function,
which works much like the `local` function, but creates an input parameter instead of a local variable.
The `output` function sets the return value and returns from the function.

Here is an example to add `1` to all elements of an array:
```scala
val myFunc = new Function(vector(n)) {
  val x = input(n)
  output(x + 1)
}
```

Generated Quantities
--------------------
Generated quantities provide a means of deriving quantities from parameters, data, and random number generation.
In ScalaStan, such quantities are created by extending the `GeneratedQuantity` class. This class works like
the data and parameter transform blocks.  In addition to parameters and data, a generated quantity can use
random numbers drawn from a distribution.

Here is an example to draw a random number:
```scala
val rand = new GeneratedQuantity(real()) {
  result := Normal(0.0, 1.0).rng
}
```

Assigning Inputs
----------------
The values for data declarations are specified using the `withData` method on the model (technically, this method
is on the `CompiledModel` class, but there is an implicit conversion to compile the model).  So, for example,
given a data declaration for an integer called `x` and a model called `model`, one obtains an updated model
with the data filled in for `x` via:
```scala
model.withData(x, data)
```
Note that `data` in the above must be the appropriate Scala type for the model.  For example, an `int()` in
ScalaStan must be assigned an `Int` and a `real()` must be assigned a `Double`.  Vectors and higher
dimensional objects are represented `Vector` in Scala, which each dimension adding another `Vector`.
Note that Scala is indexed from zero whereas Stan (and ScalaStan) is indexed from one.

It is also possible to assign data from a `DataSource`.  For example, given a file (`data.R`) with _R_ formatted
data, we can assign `x` from the value named `X` in the R data file:
```scala
val source = com.cibo.scalastan.data.RDataSource.fromFile("data.R")
model.withData(source(x, "X"))
```

Before the model can be run, all inputs must be assigned.  However, the model may be run multiple times
with different inputs by using `withData` and replacing previously assigned data.

Running the Model
-----------------
Once all inputs are assigned, the model can be run using the `run` method.  This method takes an optional
parameter to determine what strategy to use (sample, optimize, etc.).  The return value of the `run` method
is a results object that can be used to extract values from the run.

Note that the first run of a model will take a long time since the Stan code will need to be built.  However,
the generated Stan executable is cached and reused, so additional runs will be fast. The SHA1 hash of the
generated code from ScalaStan is used to determine if the code has changed and needs to be rebuilt.


Getting Results
----------------
The `run` method on `CompiledModel` returns a `StanResults` object, which has several methods to extract
samples and statistics on the results of the run.  These methods take a parameter (or transformed
parameter) as an argument.

For example, to get the mean of the samples for parameter `y`:
```scala
val m = results.mean(y)
```

This will return the mean with the same shape as the input parameter.  Note that the Scala data structure
is indexed from zero (instead of one as it is in Stan).
