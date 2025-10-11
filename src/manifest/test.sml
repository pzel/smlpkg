
open Manifest

fun test s f =
    (if f() then print ("OK : " ^ s ^ "\n")
     else print ("ERR: " ^ s ^ "\n"))
    handle Fail e => print ("EXN: " ^ s ^ " raised Fail \"" ^ e ^ "\"\n")

val m = "require {}"

val () = test "empty-m" (fn () => null(requires(fromString "str" m)))

val mp = "package github.com/owner/repo require {}"
val () = test "empty-mp" (fn () => null(requires(fromString "str" mp)))
val () = test "empty-mp-host" (fn () => SOME "github.com" = Option.map #host (package(fromString "str" mp)))
val () = test "empty-mp-owner" (fn () => SOME "owner" = Option.map #owner (package(fromString "str" mp)))
val () = test "empty-mp-repo" (fn () => SOME "repo" = Option.map #repo (package(fromString "str" mp)))

val mp2 = "package git@git.sr.ht:~user/foobar.git require {}"
val mp2package = package(fromString "str" mp2)
val () = test "requires-are-null" (fn () => null(requires(fromString "str" mp2)))

val () = test "host-is-parsed" (fn () => SOME "git.sr.ht" = Option.map #host mp2package)
val () = test "empty-mp-owner" (fn () => SOME "~user" = Option.map #owner mp2package)
val () = test "empty-mp-repo" (fn () => SOME "foobar" = Option.map #repo mp2package)


val mr1 = "require { github.com/owner/repo 1.2.3 #asdefsde }"
val () = test "empty-mr1-len" (fn () => 1 = length(requires(fromString "str" mr1)))
val () = test "empty-mr1" (fn () => SOME "asdefsde" = #3(List.hd(requires(fromString "str" mr1))))

val mr2 = "require { github.com/owner/repo 1.2.3 #asdefsde github.com/owner2/repo8 43.3.2-alpha #523424abcd }"
val () = test "empty-mr2" (fn () => 2 = length(requires(fromString "str" mr2)))
val () = test "empty-mr2-version" (fn () => "1.2.3" = SemVer.toString(#2(List.hd(requires(fromString "str" mr2)))))


val res1 = pkgpathFromString "github.com/sturluson/testpkg"
val () = test "parsed" (fn () => Option.isSome res1)

val res2 = pkgpathFromString "github.com/sturluson/testpkg 0.1.0"
val () = test "parsed with version" (fn () => Option.isSome res2)

val res3 = pkgpathFromString "git@git.sr.ht:~pzel/sqlite3.git 0.1.0"
val () = test "parsed with version and .git suffix" (fn () => Option.isSome res3)

val res4 = pkgpathFromString "git@git.sr.ht:~pzel/sqlite3 0.1.0"
val () = test "parsed with version and no suffix" (fn () => Option.isSome res4)
