# Overview

This system defines functions, variables, packages, and classes to fill a [Clostrum](https://github.com/s-expressionists/Clostrum) environment with a mostly complete Common Lisp. The contents of this environment refer to this environment properly rather than the host Lisp's environment, so for example `(funcall 'foo ...)` will look up FOO in the Clostrum environment and not the host. This kind of "extrinsic" environment is useful for cross-compilation type operations and may be useful for sandboxing.

# Use

Load the `extrinsicl` ASDF system. `(extrinsicl:install-cl client environment compilation-environment)` installs CL into the given Clostrum environment, using the given client. This is the main entry point. The compilation environment is optional, but required to install `cl:proclaim`.

There is also `(extrinsicl:install-compilation-cl client environment)` to fill up a compilation environment rather than a runtime environment.

The `extrinsicl/maclina` subsystem can optionally be loaded. This provides `(extrinsicl:install-eval client environment compilation-environment)` which defines `cl:eval`, etc. This subsystem is optional because you may want to use Extrinsicl with some other evaluator.

If anything Extrinsicl installs doesn't work for your purposes, you can of course swap out function definitions etc. out of the resulting environment. Extrinsicl does not impose any kind of package locks.

# Ethos

Wherever possible, the host Lisp and host objects are used. This system therefore does not reimplement CL as a whole, though a few parts do necessitate some reimplementation.

Macros are defined only if they expand into entirely standard code. This is so that code compiled in an Extrinsicl environment can be loaded in any standard environment.

# Limitations and quirks

The following functions are missing:

* `load`, `compile-file`, `compile-file-pathname`, `symbol-value`, `boundp`, `set`, `disassemble`, `eval`, `compile`, `makunbound`: These are part of the evaluator and you are expected to know what you want to define here. As mentioned above, you can load `extrinsicl/maclina` which can install definitions for all of these except `load`.
* `ed`, `require`: These have implementation defined behavior.
* `documentation`: There is no reasonable way to use the host `documentation`, so it would require reimplementation.
* `apropos`, `apropos-list`, `inspect`: These use the environment heavily and are supposed to be at best marginally useful for uses of Extrinsicl.
* `make-synonym-stream`: Synonym streams store a symbol and get its value from the host environment. Extrinsicl would have to implement its own stream class to implement this.
* `make-package`: Lisp packages cannot be anonymous, so Extrinsicl can't make packages that are anonymous in the host environment but available in its environment.

The following macros are missing:

* `untrace`, `trace`, `with-hash-table-iterator`, `time`, `step`, `with-package-iterator`: These operators necessarily must expand into implementation defined code (or into nothing useful).
* `pprint-logical-block` and local macros: Cannot be defined without totally reimplementing the pretty printer, which would in turn make it impossible to use host streams. Might be done in the future with Gray streams.
* `formatter`: Can't reasonably be implemented without expanding into implementation defined code.
* `with-standard-io-syntax`: Needs a standard pprint dispatch table which there is no standard way to get.
* `handler-bind`, `assert`, `restart-bind`, `with-condition-restarts`: See "Condition System" below.
* `define-symbol-macro`, `defconstant`, `define-method-combination`, `define-modify-macro`, `defstruct`, `print-unreadable-object`, `with-output-to-string`: TODO, possibly.

The following constants are missing:

* `call-arguments-limit`, `lambda-list-keywords`, `lambda-parameters-limit`, `multiple-values-limit`: These are determined by the evaluator. `extrinsicl/maclina` does define them appropriately.

## Defining Macros

Most of the defining macros (e.g. `defun`) are imported from [Common Macros](https://github.com/robert-strandh/Common-macros/). Common Macros defines a few interface functions that are helpful or required for using them. Presently the required ones are:

* `wrap-in-method-lambda`, `wrap-in-make-method-lambda`, `wrap-in-ensure-method` for `defmethod`
* `defclass-compile-time-action`, `ensure-class-name` for `defclass`
* `defgeneric-compile-time-action`, `ensure-generic-function` for `defgeneric`
* `wrap-in-setf-type-function` for `deftype`
* `wrap-in-setf-setf-expander` for `define-setf-expander`

`defun-compile-time-action` can optionally be specialized to make toplevel `defun` do something in the compiler. `proclaim` can be specialized to have `declaim` expand into something other than the obvious `cl:proclaim` calls.

(Also, as of this writing, the changes I've made to Common Macros to facilitate some of this haven't yet been upstreamed.)

## Condition System

First off, the Extrinsicl environment's `*break-on-signals*` will not work, at all. `*break-on-signals*` can be (indirectly) referenced by essentially any function in the standard, and using the environment's variable instead of the host's would be prohibitively complicated, especially given that `*break-on-signals*` is just a debugging tool.

Secondly, there is no portable way to expand `handler-bind`, etc. It would be possible to reimplement the condition system portably, but this would entail using ersatz condition and restart objects rather than the host's, which would in turn cripple the debugging experience, etc. As a matter of pragmatism, Extrinsicl does import the condition system _functions_, e.g. `error`. These properly refer to the Extrinsicl environment as far as types go, but otherwise use the host condition system.
