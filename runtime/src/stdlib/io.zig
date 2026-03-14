// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

//! Standard I/O effects: Console, FileSystem, Log.
//! See spec/axiom-overview-draft.md §5.7 for effect signatures.
//!
//! Console effect operations:
//!   print(msg: String) -> Unit
//!   read_line()        -> String
//!
//! FileSystem effect operations:
//!   read_file(path: Path)               -> Bytes
//!   write_file(path: Path, data: Bytes) -> Unit
//!   delete_file(path: Path)             -> Unit
//!   list_dir(path: Path)                -> List<Path>
//!
//! Log effect operations:
//!   log(level: Level, msg: String) -> Unit
//!   (Level = Debug | Info | Warn | Error)
//!
//! TODO: implement Console evidence + default handler (wraps std.io)
//! TODO: implement FileSystem evidence + default handler (wraps std.fs)
//! TODO: implement Log evidence + default handler (wraps std.log)

test "io: placeholder compiles" {}
