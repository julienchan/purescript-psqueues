### Priority Search Queues for Purescript

Priority Search Queues implemented using Ralf Hinze's priority search pennants, see R. Hinze, A Simple Implementation Technique for Priority Search Queues.

Priority Search Queues (PSQ) are sorted maps that recognize an independent ordering on their entries' values – in this context known as priorities – in addition to the ordering on keys. It's mean, **PSQ** are the data type of choice when an efficient access to the smallest element of a varying collection of elements is required. It's common used by application
such as discrete event simulation and job scheduling.

Currently this library implements **PSQ** which uses the **Ord** k instance for ordering their elements.
