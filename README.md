This is meant to become a library for prototyping neo4j style DB applications, the reason for writing this are both to code something interesting, and to understand how a graph database may work, is my unnderstanding of how to implement something with the features of neo4j 2.0, but it is still in very early stages. Currently it allows for creation of nodes and links, the nodes can have one or more labels associated to it, an id is automatically assigned to them, and can have a list of setfable properties (simple plist). The links must have a type, a from-node and a to-node, and it checks that the from a to nodes are not the same, and it also can have a list of properties (simple-plist).

Currently tye nodes and links are stored in simple lists, with no persistency, which is ok for now, since I'm using this only for prototyping.

The properties in both nodes and links are represented by property lists, and it is assumed that the values can only be Strings, numbers or boolean, but it will also accept characters and some other types, but because of the implementation the values cannot be symbols, this could change in the future.

The current implementation allows for searching of nodes (with node-match), getting the list of nodes with a particular label (with has-label), and getting the list of links of a particular type (with has-type).

Inmediate things to follow are:
- Implementation of indexing (add and use indexes from the property list). (0%).
- Printing for the Link and Node objects (66%).
  - Complete Nodes print-object. [DONE]
  - Complete Linkd print-object.
  - Introduce function to dump properties. [DONE]
- Put everything in a package and select the exported functions.
  Node: Created the package and exported the disered already existing functions (node-create, link-create, node-match, get-prop, set-prop, dump-props)

Where am I going with this? My idea is to design a DSL to represent something similar to Cypher but lispier, and hopefully have that be a wrapper on top of the neo4j RESTAPI, (I know CL-NEO4J could be used for the same thing but I want it to be closer to Cypher and reflecting the latest 2.0 version), anyhow, I might end up taking a completely different direction.

Contributions and critics are welcomed, but bear in mind this is still very raw.
