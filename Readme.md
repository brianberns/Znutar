# 🆉🅽 - The Znutar programming language

Znutar is a hobby language for the .NET platform. I created it because I wanted to learn about:

* Hindley–Milner type inference.
* Creating a new .NET language from scratch.

## Examples

Znutar is a functional programming language that can also call methods in external .NET libraries. Here's "hello world":

```znutar
System.Console.WriteLine("Hello world")
```

## Getting started

Here's how to implement factorial:

1. Create a `Factorial.zn` file containing the source code of your program:

```znutar
let rec factorial n =
    if n = 0 then 1
    else n * factorial (n - 1);

factorial 6
```

2. In the same directory, create a `Factorial.znproj` file that looks like this:

```
<Project Sdk="Znutar.Sdk/1.0.0">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net7.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="Factorial.zn" />
  </ItemGroup>

</Project>
```

3. Compile your program: `dotnet build`.
4. Run your program: `.\bin\Debug\net7.0\Factorial.exe`. Output is `720`.

## Goals

* Provide an alternative to F#.
* Explore the possibility of type classes, higher-kinded types, and other  functional programming concepts.
* Self-host the Znutar compiler. (It's currently written in F#.)

## Frequently asked questions

Q. Where does the name come from?

A. [🆉🅽](http://www.sjgames.com/awfulgreen/)
