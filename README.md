bindings-GLFW
=============

## Description

[![Hackage](https://img.shields.io/hackage/v/bindings-GLFW.svg)](http://hackage.haskell.org/package/bindings-GLFW)

Low-level [Haskell][1] bindings to [GLFW][2], an open source, multi-platform
library for creating windows with OpenGL contexts and managing input and
events.

The binding is to [GLFW 3.1 released 2015-01-18][3].

*These bindings are too low-level for normal use.* For higher-level GLFW
bindings, see [GLFW-b][4].

## Contributing

This package uses git-flow as development model, in short that means that:

1. New features should be added to "develop" branch.
2. "master" branch is reserved for stable releases.
3. Patches for bugs related with previous releases should always be done in
    "hotfixes" branch.
4. All merge commits to master from "hotfixes" should be done
    using the "--no-ff" flag and from "develop" should avoid merging commits.

Until we have a defined road-map we are going to leave out "release
"branches" and "feature branches". For more information about this development
model please refer to [this site.][5]

Thanks you, and happy coding.

[1]: http://www.haskell.org/
[2]: http://www.glfw.org/
[3]: http://www.glfw.org/Version-3.1-released.html
[4]: https://github.com/bsl/GLFW-b
[5]: http://nvie.com/posts/a-successful-git-branching-model/
