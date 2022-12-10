# 🆉🅽 - The Znutar programming language

Znutar is a functional language for the .NET platform.

## Goals

This is a hobby project, so take the following with large grains of salt.

### Short term

* Integrate into the .NET ecosystem cleanly. Build Znutar projects from Visual Studio, Visual Studio Code, or the `dotnet` command line.
* Consume .NET assemblies written in C# or F#. For example, we want to be able to call `System.Console.WriteLine` or use [FParsec](https://www.quanttec.com/fparsec/) from Znutar. (However, the reverse is **not** a goal.)
* Write strict, pure functional programs. Mutability and other side effects won't be supported directly in Znutar (but can occur in .NET assemblies called from Znutar).

### Long term

* Provide an alternative to F#. There's room for more than one functional language in .NET!
* Explore the possibility of type classes, higher-kinded types, and other powerful functional concepts.

## Frequently asked questions

Q. Where does the name come from?

A. [🆉🅽](http://www.sjgames.com/awfulgreen/)
