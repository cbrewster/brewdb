const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zbench_module = b.dependency("zbench", .{
        .target = target,
        .optimize = optimize,
    }).module("zbench");

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    exe_mod.addImport("zbench", zbench_module);

    const exe = b.addExecutable(.{
        .name = "brewdb",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_filters = b.option([]const []const u8, "test-filters", "Skip tests that do not match the filter") orelse &.{};
    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
        .filters = test_filters,
    });

    const lldb = b.addSystemCommand(&.{ "gdb", "-tui", "--" });
    lldb.addArtifactArg(exe_unit_tests);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const test_debug_step = b.step("test-debug", "Run unit tests under lldb");
    test_debug_step.dependOn(&lldb.step);
}
