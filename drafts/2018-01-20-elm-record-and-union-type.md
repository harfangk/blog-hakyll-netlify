---
layout: post
ref: 
date: 2017-02-25 00:00:00 +0900
title: 
lang: en
---

Elm record type vs union type

similar use case when record fields are few and union type constructors or argument types are few

record provides builtin functions for accessing fields and syntactic sugar for updating fields

union type does not do that, but provides wrapper around the values so you need to define the getter
setter

when there's a lot of fields, use record because union type becomes clumsy

record is for representing shallow data 

union type is better for logic branching because of opaqueness and pattern matching
