# S7 Method Registration and Package Initialization

This file handles the registration of S7 methods when the package is
loaded. S7 requires explicit method registration in `.onLoad()` for
proper dispatch.
