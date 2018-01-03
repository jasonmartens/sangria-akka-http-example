## Dynamic GraphQL server

This is a fork of the [Sangria Akka HTTP example](https://github.com/sangria-graphql/sangria-akka-http-example) project. It is an
attempt to build a GraphQL server that can dynamically load SDL schema specifications and start serving requests from them during
runtime. It is an experiement, suggestions for improvement are welcome.

You can run the tests with `sbt test`. 

It still uses [Sangria](sangria-graphql.org), but bypasses the execution and marshalling layers and operates directly on the AST
generated by the schema and query parsers.