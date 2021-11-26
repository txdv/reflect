load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test", "scala_macro_library")
package(default_visibility = ["//visibility:public"])

scala_macro_library(
    name = "reflect",
    srcs = ["MyFilter.scala"])

scala_binary(
    name = "main",
    main_class = "Main",
    srcs = ["Main.scala"],
    deps = [":reflect"])

scala_binary(
    name = "main2",
    main_class = "Main",
    srcs = ["Main2.scala"],
    deps = [])

scala_test(
    name = "test",
    srcs = ["Test.scala"],
    deps = [":reflect"])
