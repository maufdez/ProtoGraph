# Proto-Graph: A graph database.

**Note:** As of 7/27/2014 I am keeping my ramblings in the Wiki to keep the README cleaner and more relevant to the reader.

## Overview:

Proto-graph is a prototyping graph-database (ala Neo4j) written completely in Common lisp, although it now aspires to be more than just a prototyping tool.

Graph DBs are a form of no-SQL databases which are based on Math Graphs. In proto-graph nodes are called nodes, and edges are called links. Both edges and links are CLOS classes, derived from a more primitive class called thing.

Each node can have one ore several labels, represented by keyword parameters, if no label is provided the `:default` label is used. Nodes also have a list of properties, represented by a p-list where the keys are keyword parameters, and the values can be of any type, the matching is performed using an `equal` equality predicate.

Each link can must have a type which is also a keyword paramenter, a from-node and a to-node, which cannot be the same node. Links can also have a property list with the same characteristics as the properties for a node.

## Expected functionality:

The user should be able to find particular nodes, based on label and/or properties, and move to or from those nodes trough links to find other nodes related to it, this relation (type) and the properties of it can also be part of the querie, and any property from nodes and links, the node labels and link typeshould be easily extractable for use as the query results.

The user shold also be able to find a particular link, based on type and/or properties and move from there as described above.

Proto-graph should be able to traverse the whole graph without getting stuck on circlar references.

The database should be persistend, and fully CRUD.

## Other goals:

* Proto-graph should be fully programmed in Common Lisp, with no backend SQL database, and no calls to libaries written in other languages, it also will use only libraries that only depend on Common Lisp (no external DBs or FFIs).
* Proto-graph shuld have an s-expression based query language which does not look foreight to LISPers.
* Proto-graph is for LISPers, any API to make it accessible to other languages is not prioritary.

## Current functionality:

Currently proto-graph has can:
* Create Nodes with labels and properties, no sanity check is done on the user input. `(node-create ...)`
* Create directed Links each with type and optinally properties. `(link-create ...)`
* Modify the properties of both Nodes and Links. `(setf (get-property ...))`
* Find Nodes with particular labels and or properties. `(node-match ...)`
* Find Links of a particular type.  `(links-with-type ...)`
* Find Links which start at a particular node. `(links-from-node ...)`
* Find Links wich end at a particular node. `(links-to-node ...)`
* Nest Link finding functions to combine the functionality.
* Dumping properties of a node or a link. `(dump-props ...)`

Currently the DB is non persistent, and there are no functions to delete records. Functionality to modify labels in nodes, or types in links, is still missing.

## Notes:

Contributions and critics are welcomed, but bear in mind this is still very raw.

## Example usage (With the current functions)

Let's say we want to create a movie database, with the names of the actors, directors, movies, and maybe later some other information like locations, etc.

I will start adding one movie, with a couple of actors and the director:

```common-lisp
(defvar *solo* (node-create :label :person :properties '(:name "Harrison" :surname "Ford")))
(defvar *leia* (node-create :label :person :properties '(:name "Carrie" :surname "Fisher")))
(defvar *lucas* (node-create :label :person :properties '(:name "George" :surname "Lucas")))
```

And then we can add the movie:

```common-lisp
(defvar *movie* (node-create :label :movie :properties '(:title "Star Wars")))
```

Now we can create links for our graph:

```common-lisp
(link-create :acts-on *solo* *movie* :properties '(:plays "Han Solo"))
(link-create :acts-on *leia* *movie* :properties '(:plays "Leia Organa"))
(link-create :directs *lucas* *movie*)
```

With the database now in place, we can find the names of the cast:

```common-lisp
(links-with-type :acts-on (links-to-node *movie*))
```

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

Currently proto-graph does not check that an identical register already exists in the database before creating it, and there is no way to delete records from the database. Modifying properties or adding new ones should be possible, set-prop is not really needed, since you can setf get-prop, so I will eliminate it. These changes would be necessary just to make it CRUD, and later I need to make the DB persistent, and add some sugar around stuff to make it easier to use.
