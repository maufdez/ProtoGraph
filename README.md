This is meant to become a library for prototyping neo4j style DB applications, the reason for writing this are both to code something interesting, and to understand how a graph database may work, is my unnderstanding of how to implement something with the features of neo4j 2.0, but it is still in very early stages. Currently it allows for creation of nodes and links, the nodes can have one or more labels associated to it, an id is automatically assigned to them, and can have a list of setfable properties (simple plist). The links must have a type, a from-node and a to-node, and it checks that the from and to nodes are not the same, and it also can have a list of properties (simple-plist).

Currently the nodes and links are stored in simple lists, with no persistency, which is ok for now, since I'm using this only for prototyping.

The properties in both nodes and links are represented by property lists, and it is assumed that the values can only be Strings, numbers or boolean, but it will also accept characters and some other types.

The current implementation allows for searching of nodes (with node-match), getting the list of nodes with a particular label (with has-label), and getting the list of links of a particular type (with has-type).

Just added the functions for filtering links, links-with-type, links-from-node, links-to-node.

Inmediate things to follow are:
- Implementation of indexing (add and use indexes from the property list). (0%).
- Printing for the Link and Node objects (100%).
  - Complete Nodes print-object. [DONE]
  - Complete Link print-object. [DONE]
  - Introduce function to dump properties. [DONE]
- Put everything in a package and select the exported functions. [On-track]
  Note: Created the package and exported the desired already existing functions (get-keys, node-create, link-create, node-match, get-prop, set-prop, dump-props,liks-with-type,links-from-node,links-to-node)

Where am I going with this? My idea is to design a DSL to represent something similar to Cypher but lispier, and hopefully have that be a wrapper on top of the neo4j RESTAPI, (I know CL-NEO4J could be used for the same thing but I want it to be closer to Cypher and reflecting the latest 2.0 version), anyhow, I might end up taking a completely different direction.

I´ve been thinking on using manardb for persistency so I can use whatever I come up with instead of Neo4j for the project I'm working on, but I still need to get a better understanding of manardb, which seems to be what I want since it is pure lisp with no "SQL-DB" backend. With a prolog like query system like the one outlined in the ON-LISP book, it could be a usable system, and much more interesting to me than just wrapping something around neo4j.

Contributions and critics are welcomed, but bear in mind this is still very raw.

# Example usage (With the current functions)

Let's say we want to create a movie database, with the names of the actors, directors, movies, and maybe later some other information like locations, etc.

I will start adding one movie, with a couple of actors and the director:

```
(defvar *solo* (node-create :label :person :properties '(:name "Harrison" :surname "Ford")))
(defvar *leia* (node-create :label :person :properties '(:name "Carrie" :surname "Fisher")))
(defvar *lucas* (node-create :label :person :properties '(:name "George" :surname "Lucas")))
```

And then we can add the movie:

    (defvar *movie* (node-create :label :movie :properties '(:title "Star Wars")))

Now we can create links for our graph:

```
(link-create :acts-on *solo* *movie* :properties '(:plays "Han Solo"))
(link-create :acts-on *leia* *movie* :properties '(:plays "Leia Organa"))
(link-create :directs *lucas* *movie*)
```

With the database now in place, we can find the names of the cast:

    (links-with-type :acts-on (links-to-node *movie*))

Which will give the following result:

```
(#<PROTO-GRAPH::LINK
   #<NODE ID: 2 |NAME: Carrie |SURNAME: Fisher > [ACTS-ON]  #<NODE
                                                              ID: 4 |TITLE: Star Wars >>
 #<PROTO-GRAPH::LINK
   #<NODE ID: 1 |NAME: Harrison |SURNAME: Ford > [ACTS-ON]  #<NODE
                                                              ID: 4 |TITLE: Star Wars >>)
```

Of course, the result is ugly and not very legible, because it is merely using the print-unreadable-object, but we can see that we have the rudiments for querying a graph database.

Currently proto-graph does not check that an identical register already exists in the database before creating it, and there is no way to delete records from the database, modifying properties or adding new ones should be possible, set-prop is not really needed, since you can setf get-prop, so I will eliminated it. This just to make it CRUD, and later I need to make the DB persistent, and ad some sugar around stuff to make it easier to use.
