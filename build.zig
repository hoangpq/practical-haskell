const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const lib = b.addSharedLibrary("example", "src/main.zig", b.version(0, 0, 1));
    lib.setBuildMode(mode);
    lib.linkLibC(); // Needed for linking with libc
    switch (mode) {
        // This includes the compiler runtime (Zig runtime checks):
        .Debug, .ReleaseSafe => lib.bundle_compiler_rt = true,
        // This turns off the checks completely for release mode:
        .ReleaseFast, .ReleaseSmall => lib.disable_stack_probing = true,
    }
    lib.install();

    const main_tests = b.addTest("src/main.zig");
    // Depending on how you set up your Zig code,
    // you may need to link with libc in your tests as well:
    main_tests.linkLibC();
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
