ScalaStan
=========
A Scala DSL for [Stan](http://mc-stan.org).

Requirements
============
ScalaStan depends on [Scala](http://www.scala-lang.org), [SBT](http://www.scala-sbt.org), and
[CmdStan](http://mc-stan.org/users/interfaces/cmdstan).

The `CMDSTAN_HOME` environment variable should be set to the location of the CmdStan installation.
Alternatively, ScalaStan will work if `PATH` contains `stanc` within a valid CmdStan installation.

Project Structure
=================
 - `com.cibo.scalastan` contains the ScalaStan DSL (most importantly, the `ScalaStan` trait).
 - `com.cibo.scalastan.ast` contains the ScalaStan abstract syntax tree.
 - `com.cibo.scalastan.data` contains parsers for various data sources (R, for example).
 - `com.cibo.scalastan.models` contains reusable ScalaStan models.
 - Examples can be found in the `com.cibo.scalastan.examples` package in the integration test (`it`) source directory.  Run an example using the command `sbt it:run` and choosing from the available examples.

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
    sigma ~ stan.cauchy(0, 1)
    y ~ stan.normal(m * x + b, sigma)
  }

  val xs: Seq[Double] = ???
  val ys: Seq[Double] = ???
  val results = model
    .withData(x, xs)
    .withData(y, ys)
    .run(chains = 5)

  results.summary(System.out)
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
  sigma ~ cauchy(0, 1);
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
 - `categorical()` // A categorical type (this is a ScalaStan extension to handle categorical data
   that turns into an `int`).

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

The types for parameters are the same as those for data, however, Stan does not allow integral parameters.

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

Stan supports element-wise multiplication and division, which are `*:*` and `/:/` in ScalaStan.

The power operator is `^`.

The transpose operator is `.t` (for example, `x.t` to transpose `x`).

#### Assignment
The assignment operator is `:=` (instead of `=`).

#### For Loops and Slices
For loops use the standard Scala syntax, but using the `range` function as a generator.  For example:
```scala
for (i <- range(1, n)) {
  // ...
}
```
This will cause `i` to take on the values `1` through `n` inclusive.
Note that arrays are indexed starting from one in Stan.

The `range` function can also be used for array slicing, for example:
```scala
x(range(1, 5)) := y(range(2, 6))
```

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
The built-in Stan distributions are available in ScalaStan from the `stan` object.

To indicate that a value is sampled from a distribution, the `~` operator is used, for example:
```scala
y ~ stan.normal(0.0, 1.0)
```

This is roughly equivalent to the following:
```scala
target += stan.normal(0.0, 1.0).lpdf(y)
```

#### Other Functions
The built-in Stan functions are available in ScalaStan from the `stan` object.


Transformed Data and Parameters
-----------------------------
Transformed versions of data and parameters can be declared via the `transformed data` and `transformed parameters`
sections in Stan. In ScalaStan, this is accomplished by extending the `TransformedData` or `TransformedParameter`
classes.  These classes take a parameter that is the type of the transformed value.  The body of the class
provides a DSL to encode the transformation using the same constructs as the model.  Inside this DSL, the
value is accessed using `result`.

Here is a simple data transform to add `1` to all elements of an array:
```scala
val xPlusOne = new TransformedParameter(vector(n)) {
  result := x + 1
}
```

The transformed parameter can now be referenced instead of the actual parameter in the model.

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
  result := stan.normal(0.0, 1.0).rng
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
dimensional objects are represented `Seq` in Scala, which each dimension adding another `Seq`.
Note that Scala is indexed from zero whereas Stan (and ScalaStan) is indexed from one.

It is also possible to assign data from a `DataSource`.  The following data sources are available:

 - `CsvDataSource`: A data source for parsing CSV.
 - `RDataSource`: A data source for parsing R-formatted data.
 - `TextDataSource`: A data source for parsing a text file with an array of values.

For example, given a file (`data.R`) with R-formatted
data, we can assign `x` from the value named `X` in the R data file using the `apply` method:
```scala
val source = com.cibo.scalastan.data.RDataSource.fromFile("data.R")
model.withData(source(x, "X"))
```

Using data sources, it is also possible to load the data in Scala format for manipulation using
the `read` method:
```scala
val source = com.cibo.scalastan.data.CsvDataSource.fromFile("data.csv")
val data: Seq[Double] = source.read(x, "X")
```

Before the model can be run, all inputs must be assigned.  To assign different inputs, the `reset` method
must be called.  Using the `reset` method will clear all assigned inputs, allowing the model to be run
multiple times with different inputs.

Running the Model
-----------------
Once all inputs are assigned, the model can be run using the `run` method.  This method takes the following
optional parameters:

 - chains: An integer specifying the number of chains to run in parallel.  This defaults to 4.
 - seed: An integer specifying the first random number seed to use or `-1` to use system time to select one.
   With multiple chains, the chain index is added to the seed to ensure each chain gets a different, but
   deterministic seed.
 - cache: A boolean specifying whether results should be cached.  This defaults to `true`.  When the
   results are cached, re-running the model with the same data set and seed as before will cause the results
   from the previous run to be returned rather than re-executing the model.
 - method: The method to use (`RunMethod.Sample()`, `RunMethod.Optimize()`, `RunMethod.Variational()`,
   or `RunMethod.Diagnose()`).  This defaults to `RunMethod.Sample()`.
   Note that each `RunMethod` is a case class that can accept additional parameters.

The return value of the `run` method is a results object that can be used to extract values from the run.

The first run of a model will take a long time since the Stan code will need to be built.  However,
the generated Stan executable is cached and reused, so additional runs will be fast. The SHA1 hash of the
generated code from ScalaStan is used to determine if the code has changed and needs to be rebuilt.  The
cached models and results are stored in `$HOME/.scalastan`.  Old models and results are removed in a
least-recently used fashion.

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

For convenience, there is also a `get` method on `StanResults` to access the input data.

The results object also has a `summary` method to output a summary of results like the `stansummary` program:
```scala
results.summary(System.out)
```

External Stan Code
------------------
It is possible to use ScalaStan to generate regular Stan code using the `emit` function on the model.
For example:
```scala
    val myModel = new Model {
      // ...
    }
    val writer = new java.io.PrintWriter("myModel.stan")
    myModel.emit(writer)
    writer.close()
```

It is also possible to use ScalaStan to run existing Stan models.  To use an existing model, the data
and parameters must be set up in ScalaStan, then the `loadFromFile` (or `loadFromString`) method on
`Model` can be used to load the model.  Once loaded, the model can be used just as a model implemented
directly in ScalaStan. For example:
```scala
    val n = data(int(lower = 0))
    val x = data(vector(n))
    val mu = parameter(real())
    val sigma = parameter(real(lower = 0))

    val model = Model.loadFromFile("normal.stan")

    // ...
```
