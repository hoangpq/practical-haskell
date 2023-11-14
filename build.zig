const std = @import("std");
const FileSource = std.build.FileSource;
const OptimizeMode = std.builtin.OptimizeMode;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // const lib = b.addSharedLibrary("example", "src/main.zig", b.version(0, 0, 1));
    const lib = b.addSharedLibrary(.{
        .name = "example",
        .root_source_file = .{ .path = "src/main.zig" },
        .optimize = optimize,
        .target = target
    });

    lib.linkLibC(); // Needed for linking with libc
    b.installArtifact(lib);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test","Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
