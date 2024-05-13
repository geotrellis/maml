# Map Algebra Modeling Language

[![CI](https://github.com/geotrellis/maml/actions/workflows/ci.yml/badge.svg)](https://github.com/geotrellis/maml/actions/workflows/ci.yml) [![Join the chat at https://gitter.im/geotrellis/geotrellis](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/geotrellis/geotrellis?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Maven Badge](https://img.shields.io/maven-central/v/com.azavea.geotrellis/maml-jvm_2.12?color=blue)](https://central.sonatype.com/search?q=g%3Acom.azavea.geotrellis&smo=true&name=maml-jvm_2.12)


Azavea has been developing Map Algebra Modeling Language (MAML) as part of a NASA grant in [Raster Foundry](https://www.rasterfoundry.com/). MAML is used to create a declarative structure that describes a combination of map algebra operations. This structure may be evaluated against a given collection of datasets to produce a result. Critically, the evaluation logic is not specified in MAML, only the semantic meaning of the operations. This separation allows for multiple interpreters to exist that operate in different computational contexts.

To keep the discussion of benefits of such scheme from being too academic, it is best to examine some use cases that this pattern enables.

## Table of Contents

- [Use Cases](#use-cases)
  - [Remote Evaluation](#remote-evaluation)
  - [Multiple Interpreters](#multiple-interpreters)
  - [Interactive Construction](#interactive-construction)
  - [GeoTrellis RDD Pipeline-ing](#geotrellis-rdd-pipeline-ing)
  - [Optimization](#optimization)
- [Representation](#representation)
  - [Tree Representation](#tree-representation)
  - [Pipeline Representation](#pipeline-representation)
- [Expressive Power of JSON Representation](#expressive-power-of-json-representation)
  - [what about parameter identity/re-use](#what-about-parameter-identityre-use)
- [API Components](#api-components)
  - [Scala MapAlgebra ADT](#scala-mapalgebra-adt)
  - [Scala MapAlgebra Interpreters](#scala-mapalgebra-interpreters)
  - [REST API](#rest-api)
  - [Client Libraries](#client-libraries)
- [Future Extensions](#future-extensions)

## Use Cases

### Remote Evaluation

Because a MAML expression is purely declarative it may be constructed remotely from where it is evaluated, for instance in a client library. It can then be sent to a remote service that evaluates the expression and instantiates a TMS endpoint to present the result.

Let there be a client library, for instance in JavaScript or Python. It has an object representing a catalog of raster data such that we can construct references to:

```python
nir_layer = GeoTrellisLayer("s3://raster-bucket/catalog", name = "LC8_nir", zoom = 23)
red_layer = GeoTrellisLayer("s3://raster-bucket/catalog", name = "LC8_red", zoom = 23)
```

The layer object exposes map algebra operations as methods:

```python
ndvi_layer = (nir_layer - red_layer) / (nir_layer + red_layer)
```

Instead of doing actual work the method calls accumulate a nested dictionary representing these operations such that later they may be given to remote process:

```python
service = GeoTrellisRemoteService("http://api.example.com/mapalgebra")
remote = service(ndvi_layer)
errors = remote.errors            # Do the layers I want exist? Can all operations be applied?
tms_url = remote.tms_url          # http://api.example.com/mapalgebra/tms/<ID>/{z}/{x}/{y}
raster = remote.fetch_raster(BBOX(...)) # Evaluate expression and clip bounding box, returning a numpy array here
```

What did we buy?

-   Remote service could have access to layers that don't fit on my machine
-   Remote service could have more computation power (EMR cluster)
-   Remote service could be closer to the data
-   Client can sample the answer through TMS or Clip without pulling full input layers
-   We can don't have to re-implement complex operations on the client

### Multiple Interpreters

If the MAML operation description avoids leaking implementation details multiple interpreters can exist in parallel:

1.  TMS Interpreter

    Responds to requests for a tile at Z/X/Y of an image pyramid. Because each requests covers a small amount of data, typically a 256x256 tile and possibly its neighbors, it can be computed quickly. In effect this creates a partial evaluation window of an expression over potentially large dataset.
    
    Naturally some operations, like cost distance, can not be computed per tile and would not be available in this context.

2.  RDD Interpreter

    Translates a MAML expression into a Spark RDD, which represents a batch job over a whole layers. The RDD can be evaluated by a Spark cluster with results saved for later use. Because RDD evaluation is capable of shuffle and iteration arbitrarily complex jobs can be described.

3.  Other Interpreters

    Other interpreters may exist where map algebra operations can be meaningfully performed. In practice they need not be limited to GeoTrellis operations or even executed on the JVM. Heterogeneous evaluation is possible, combining multiple tools to create the desired result.

### Interactive Construction

Expressions in nested tree structure map naturally and easily to a user interface representing them as graphical trees. This user interface allows an intuitive way for non-programmers to construct a complex nested expression. An expression can be visually evaluated fully, at its root, or partially, at a sub-tree creating an interactive workflow.

### GeoTrellis RDD Pipeline-ing

Spark performance is limited by the number of shuffle stages performed by a job where each record must be written to disk, and potentially sent across the network to another machine. Both of these IO operations are expensive and slow.

Unfortunately many common GIS operations imply a shuffle. For instance a focal operation requires information about geographically neighboring tiles, some or all of which may be on a different machine.

Given a MAML expression it can be inspected to decide the minimum buffer required per each tile. This buffer can be the basis for the first and only shuffle that provides sufficient neighborhood information for all focal operations to be evaluated as a pipeline.

In this manner MAML is to GeoTrellis operations as DataFrames are to RDD operations.

### Optimization

Because of the separation of declaration and evaluation it is possible to insert an optimization phase before evaluation that transforms a MAML expression to one that produces an equivalent result but with fewer computations.

Caching strategies may also be employed to re-used partially computed results in the expression or across subsequent evaluations.

## Representation

### Tree Representation

A natural form for an expression that may itself be recursively composed of other expressions is a tree. Such a tree maps directly to a JSON structure where each operation is a JSON object and its arguments are members of a field, containing other operation objects.

This presentation is unambiguous and can be easily constructed through an interactive interface.

1.  TODO <Insert Picture of RF model>
2.  TODO <Insert Corresponding JSON>

### Pipeline Representation

If the expression tree from above is flattened into a list, the operation-argument/parent-child relationship can be preserved through labeling each node and using the label to refer to it when it occurs as an argument.

This representation is potentially more human readable, due to its flat structure. It is also explicit about argument identity and when a sub-tree may be re-used and not recomputed.

1.  TODO <Insert Corresponding JSON>

## Expressive Power of JSON Representation

MapAlgebra operation is represented with a JSON object that has fields `apply` which names the function. Other fields may be present if they parameterize the function.

```json
{
    "apply": "-",
    "args": [ ... ]
}
```

The `args` field is a list of arguments to the MapAlgebra function they can be another operation or a parameter object:

```json
{
    "name": "LC8_0_NIR",
    "type": "raster",
    "id": "123e4567-e89b-12d3-a456-426655440000",
}
```

```json
{
    "id": "uasdfasdf",
    "input": {
        "project_id": "asdfasdf",
        "resolution": 15,
        "layers": ["I need color correction"]
        "tool_id": "GUID",
        "tool_run_params": {
            "execution_params"
        }
    },
    "output":{
        "source": "s3://bucket/sink"
    }
}
```

The parameter describes a source of raster. There is no one spec for parameter format because it may have many forms. It may be a URI for GeoTrellis layer in a catalog, a primary key for layer ID in a table, or some service endpoint URL. Agreement on possible parameter references is part of interface between client code building the JSON and the interpreter.

### what about parameter identity/re-use

MapAlgebra function application maps to nesting and forms an JSON forms Abstract Syntax Tree with internal nodes representing operations and leaf nodes representing parameters.

We can encode arbitrarily deep expressions with this structure. Covering conditionals and iteration requires expanding the spec to allow non-raster returns.

## API Components

### Scala MapAlgebra ADT

### Scala MapAlgebra Interpreters

- TMS
- Batch
- Window

### REST API

### Client Libraries

- Python
- JavaScript
- Scala

## Future Extensions

- Vector Inputs
- Vector Outputs
- Tabular Outputs (for zonal operations)
