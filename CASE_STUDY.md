# ArtScript: A Case Study in DSL Creation

## 🎨 Overview
ArtScript is a Domain-Specific Language (DSL) designed for creating geometrically intricate artwork with a focus on simplicity and accessibility. Originally conceived as a collaborative project for a Programming Languages course, I later extended the language independently to transform it from a basic drawing tool into a fully capable generative art platform.

## 💡 The Motivation
The inspiration for ArtScript stemmed from a desire to blend mathematical precision with creative expression. Drawing heavily from the minimalist, geometric artwork at the Williams College Museum of Art (specifically works by Josef Albers), the goal was to create a language that makes programmatic art creation intuitive for beginners, aspiring artists, and children. 

## 👨‍💻 My Role & Journey

**Phase 1: Course Project (Collaborative)**
Initially, I co-developed ArtScript alongside my peer, Henok Misgina Fisseha, for our CSCI 334 class at Williams College. Our initial focus was on establishing the core syntax, a custom parser, and a basic evaluator capable of handling sequential commands (moving a pen, drawing basic shapes) and compiling them to SVG.

**Phase 2: Independent Expansion**
After the course concluded, I recognized the language's potential for generative art and independently implemented the remaining planned features and more. By diving deeper into the parser and abstract syntax tree (AST), I introduced programming constructs like variables, loops, state management, and geometric transformations.

## 🛠️ Tech Stack & Architecture
- **Language**: F#
- **Platform**: .NET
- **Output**: SVG (Scalable Vector Graphics)
- **Core Architecture**: The system utilizes a functional programming approach to parse custom syntax into an Abstract Syntax Tree (AST). The `Evaluator` recursively walks the AST, maintaining a state environment (for pen position, direction, and variables) and mapping the state changes into raw SVG string outputs.

## ✨ Key Features

### 1. The Core Language (Phase 1)
- **Turtle Graphics Engine**: Commands like `go`, `setlocation`, `toright`, `toleft`, `penup`, and `pendown` manage a stateful pen.
- **Basic Shapes**: Built-in primitives for `rect`, `circle`, and `poly`.
- **Repeat Blocks**: A macro-like `repeat` function that duplicates drawing expressions for easy pattern creation.

### 2. Generative Art Capabilities (Phase 2 - Solo)
I significantly refactored the environment map to support dynamic state, enabling:
- **Variables & Scoping**: Implemented a `set` command to store variables.
- **For-Loops**: Added bounded iteration to generate complex, algorithmically driven patterns.
- **Geometric Transformations**: `rotate` and `scale` blocks allow manipulation of grouped shapes.
- **Extended Primitives**: Added support for text rendering and ellipses.
- **Grid Tool**: Created a development helper to render coordinate axes, aiding in design alignment.

## 🧠 Technical Challenges & Solutions
1. **State Management**: Moving from static drawing commands to a system with variables and loops required migrating the evaluator's state from a simple coordinate/direction record to an environment map capable of holding variable bindings during execution.
2. **SVG Grouping for Transformations**: To implement rotations and scaling across multiple objects, I had to ensure the AST and Evaluator could properly wrap sequential expressions in SVG `<g>` (group) tags with the corresponding transform attributes, without breaking the sequence of the surrounding code.

## 🚀 Outcomes & Learnings
The evolution of ArtScript showcases the transition from a simple class project to a robust DSL. The language is now capable of producing stunning generative patterns, replicating museum artworks, and serving as a fun, educational tool for geometric programming. By driving the second phase of development individually, I deepened my understanding of parsing, recursive AST evaluation, and functional state management in F#.

*Check out the [main README](./README.md) for visual examples and instructions on running ArtScript.*
